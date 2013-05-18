(defpackage :db.io
  (:use :common-lisp :util.util)
  (:import-from :sb-kernel system-area-pointer system-area-ub32-copy system-area-ub32-fill)
  (:import-from :sb-sys sap-ref-32 int-sap sap+ sap- with-pinned-objects vector-sap)
  (:import-from :sb-thread make-mutex with-mutex)
  (:export *filestream* !initialize 
	   +buffers+ +nbuffers+ sysbuffer resbuffer
	   +page-bytes+ +page-words+ +page-himask+ +page-lomask+ 
	   +header-sectors+ +journal-sectors+ +alloc-sectors+ +reserve-sectors+
	   +header-start+ +journal-start+ +journal-end+ +alloc-start+ +reserve-start+ +user-start+
	   
	   nobuffers
	   make-buffer buffer watchers dirty-p diskversion-p sector sap bnum
	   page-in page-out
	   find-sector dump contents cache isolate acquire-buf release-buf
	   lru debug-watchers flush))
	   
(in-package :db.io)

;; There is only 1 database and it is subdivided into 4096 byte blocks which are paged into memory.  I use a copy-on-write
;; system so I don't need locks.  Readers acquire-buf the blocks to prevent them from being paged out. Writers create
;; a copy (using isolate) before they write to ensure nobody but the writers thread sees the changes.

;; I keep the first two buffers permanently locked to reserve them for system routines. Preallocating them means I never
;; have to worry about these routines not being able to acquire a buffer.

;; My database has a 64mb journal starting at sector 1. This file deals with hardware while db.txn deals more with
;; transactions.

(defvar *filestream* nil)       ;; Bind to a binary IO stream.
(defconstant +nbuffers+ 8192) ;; 32768) ;; Less pages, higher chance for corruption to show.
(defconstant +page-bytes+ 4096) ;; 4096 bytes -- see getconf PAGESIZE
(defconstant +page-words+ (ash +page-bytes+ -2))
(defconstant +page-himask+ #b11111111111111111111000000000000)
(defconstant +page-lomask+ #b00000000000000000000111111111111)
;; 32764 * 4096 bytes = 128 megs of memory.

(defconstant +header-sectors+  #x0010) ;; 16 sectors reserved for a header (currently, there is no header)
(defconstant +journal-sectors+ #x4000) ;; 64 megs of disk space to store the block-level journal
(defconstant +alloc-sectors+   #x0100) ;; 01 megs of disk space to hold freespace list.
(defconstant +reserve-sectors+ #x0800) ;; 08 megs of disk space reserved for future use.

(defconstant +header-start+  0)
(defconstant +journal-start+ (+ +header-start+  +header-sectors+))
(defconstant +alloc-start+  (+ +journal-start+ +journal-sectors+))
(defconstant +reserve-start+ (+ +alloc-start+  +alloc-sectors+))
(defconstant +user-start+    (+ +reserve-start+ +reserve-sectors+))

(defconstant +journal-end+ +alloc-start+)

(define-condition nobuffers (error) ())

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (buffer (:conc-name nil) (:copier nil))
    (watchers 0 :type fixnum)            ;; Must be fixnum for compare-and-swap, 0 means unused, -1 means being setup.
    (diskversion-p nil :type boolean)
    (dirty-p nil :type boolean)
    (sector 0 :type (unsigned-byte 32))  ;; Big enough for 16 terabytes.
    (sap nil :type system-area-pointer)
    (bnum 0 :type (unsigned-byte 32))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defconstant +buffers+ (make-array +nbuffers+ :element-type 'buffer :initial-contents
			    (loop for i upfrom 0 repeat +nbuffers+ collecting (make-buffer :sap (int-sap 0) :bnum i))))
  (define-symbol-macro sysbuffer (elt +buffers+ 0))
  (define-symbol-macro resbuffer (elt +buffers+ 1)))

(defun !initialize ()
  (setf (watchers sysbuffer) 1 (watchers resbuffer) 1) ;; Reserved for system use.
  (let ((base (malloc (* +nbuffers+ +page-bytes+))))
    (loop for buffer across +buffers+ 
          for sap = base then (sap+ sap +page-bytes+)
          do (setf (sap buffer) sap)
	  do (system-area-ub32-fill 0 sap 0 +page-words+))))

(defun buffer-vals (fn &optional (start 0) (end +nbuffers+)) (map 'list fn (subseq +buffers+ start end)))
(defun debug-watchers () (buffer-vals #'watchers))

(let ((page-plru (make-array (1- +nbuffers+) :element-type 'boolean :initial-element nil)))
  (defun lru ()
    "Makes the least-recently-used slot the most-recently-used, and returns it"
    (labels ((inner (pos)
	       (cond ((>= pos (1- +nbuffers+)) (- pos (1- +nbuffers+)))
		     ((elt page-plru pos)
		      (setf (elt page-plru pos) nil)
		      (inner (1+ (1+ (* pos 2)))))
		     (t (setf (elt page-plru pos) t) (inner (1+ (* pos 2)))))))
      (elt +buffers+ (inner 0)))))

(let ((io-mutex (make-mutex :name "page io mutex")))
  (defun page-in (sector b)
    (let ((temp (make-array +page-words+ :element-type '(unsigned-byte 32))))
      (declare (dynamic-extent temp))
      (with-mutex (io-mutex)
	(file-position *filestream* (* +page-words+ sector))
	(read-sequence temp *filestream* :start 0 :end +page-words+))
      (with-pinned-objects (temp) (system-area-ub32-copy (vector-sap temp) 0 (sap b) 0 +page-words+)))) ;; No mutex.
  (defun page-out (sector b)
    (let ((temp (make-array +page-words+ :element-type '(unsigned-byte 32))))
      (declare (dynamic-extent temp))
      (with-pinned-objects (temp) (system-area-ub32-copy (sap b) 0 (vector-sap temp) 0 +page-words+)) ;; No mutex.
      (with-mutex (io-mutex)
	(file-position *filestream* (* +page-words+ sector))
	(write-sequence temp *filestream* :start 0 :end +page-words+)
	(finish-output *filestream*)))))

(defun dump (b) (setf (diskversion-p b) nil (dirty-p b) nil (sector b) 0))

(defun contents (stream b &optional (sector 0))
  (do ((i 0 (+ i 4))
       (sector (* sector +page-bytes+) (+ sector 4))
       (sap (sap b) (sap+ sap 4)))
      ((= i +page-bytes+) (princ #\Newline stream))
    (let ((val (sap-ref-32 sap 0)))
      (when (zerop (mod i 16))
	(princ #\Newline stream)
	(format stream " ~8,'0X:" sector))
      (format stream " ~2,'0X" (ldb (byte 8 0) val))
      (format stream " ~2,'0X" (ldb (byte 8 8) val))
      (format stream " ~2,'0X" (ldb (byte 8 16) val))
      (format stream " ~2,'0X"  (ldb (byte 8 24) val)))))
(defun debug-idx (b) (ash (sap- (sap b) (sap (elt +buffers+ 0))) -12))
(defun debug-buffer (b &optional (stream t) contents)
  (format stream "~%BUFFER~d~% watchers: ~a~% sector: ~a~% disk: ~a~% dirty: ~a~%" (debug-idx b)
	  (watchers b) (sector b) (diskversion-p b) (dirty-p b))
  (when contents (format stream "DATA:") (contents stream b
						   (* +page-bytes+
						      (if (numberp contents) contents (and contents (sector b)))))))

(defun release-buf (b)
  (do* ((current (watchers b) (watchers b))
	(desired (1- current) (1- current))) (nil)
    (when (<= current 0) (error "Tried to release buffer with ~d watchers" current))
    (when (= current (sb-ext:compare-and-swap (watchers b) current desired)) (return-from release-buf nil))))

(defun acquire-buf (b)
  (do* ((current (watchers b) (watchers b))
	(desired (1+ current) (1+ current))) (nil)
    (when (< current 0) (return-from acquire-buf nil))
    (when (= current (sb-ext:compare-and-swap (watchers b) current desired)) (return-from acquire-buf b))))

(let ((lock (sb-thread:make-mutex :name "cache lock")))
  (defun next ()
    (do ((try 0 (1+ try))
	 (b (lru) (lru)))
	((= 0 (sb-ext:compare-and-swap (watchers b) 0 -1))
	 (progn (when (and (diskversion-p b) (dirty-p b)) (page-out (sector b) b)) b))
      (when (> try +nbuffers+) (error 'nobuffers))))
  
  ;; This linear search probably slows things a lot. Consider implementing a search tree to speed it up.
  (defun find-sector (sector) (find-if #'(lambda (b) (and (diskversion-p b) (= sector (sector b)))) +buffers+))

  (defun cache (sector)
    (or (aand (find-sector sector) (acquire-buf it) (if (= sector (sector it)) it (release-buf it)));; Try lockless acquire.
	(sb-thread:with-mutex (lock) 
	  (or (aand (find-sector sector) (acquire-buf it))                                       ;; Search, serialized.
	      (let ((b (next))) 	                                                     ;; Load it from disk.
		(setf (diskversion-p b) t (dirty-p b) nil (sector b) sector)
		(page-in (sector b) b)
		(setf (watchers b) 1)
		b)))))

  (defun isolate (b)
    (sb-thread:with-mutex (lock)
      (let ((n (next)))
	(setf (diskversion-p n) nil (dirty-p n) nil (sector n) (sector b))
	(system-area-ub32-copy (sap b) 0 (sap n) 0 +page-words+)
	(setf (watchers n) 1)
	n))))
  
(defun flush ()
  ;; Flush all buffers to disk. Note, I can't make the OS/Disk hardware write, power outs could still lose the data.
  ;; But this should be called before closing, or resetting the journal.
  (assert (not (diskversion-p sysbuffer)))
  (assert (not (diskversion-p resbuffer)))
  (loop for b across +buffers+
        when (and (diskversion-p b) (dirty-p b))
        do (page-out (sector b) b)
        and do (setf (dirty-p b) nil)))
       

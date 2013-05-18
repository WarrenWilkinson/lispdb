(defpackage :db.cursor
  (:use :common-lisp :util.util :db.io :db.txn :db.bt :db.threads)
  (:import-from :sb-sys sap-int int-sap sap- sap+ sap-ref-8 sap-ref-16 sap-ref-32 sap-ref-64 with-pinned-objects vector-sap)
  (:import-from :sb-kernel system-area-ub8-copy)
  (:shadow count)
  (:export copy-data
	   ghost-cursor next
	   run run! skip
	   leaflength leaf-start run-string
   	   past-end? start-of-next
	   leafcount do-write reposition
	   ))

(in-package :db.cursor)

(defthreadlocal copy-data 
  (copy-data   pointer)) ;; ;; An extra 'hand' PTR that holds data and provides a cursor.

(defun leaf-start (source) (let ((keys (keys source))) (values keys (sap+ keys (leaf-keybytes)))))
(defun leaf-datum (source) (sap+ (keys source) (leaf-keybytes)))

(defun leaflength   (bytes) (ceiling (+ bytes 4) (leaf-u8s)))

(flet ((leafbytes (target)
	 (let ((buffer (lookup target)))
	   (if (found)
	       (prog1 (sap-ref-32 (origin-data) 0) (when buffer (release buffer)))
	       (if (zerop (sap-int (origin-key)))
		   (error "Found Nothing! expected ~x ~x ~x ~x."
			  (sap-ref-32 (down-key) 0) (sap-ref-32 (down-key) 4)
			  (sap-ref-32 (down-key) 8) (sap-ref-32 (down-key) 12))
		   (error "Found ~x ~x ~x ~x, expected ~x ~x ~x ~x."
			  (sap-ref-32 (origin-key) 0) (sap-ref-32 (origin-key) 4)
			  (sap-ref-32 (origin-key) 8) (sap-ref-32 (origin-key) 12)
			  (sap-ref-32 (down-key) 0) (sap-ref-32 (down-key) 4)
			  (sap-ref-32 (down-key) 8) (sap-ref-32 (down-key) 12)))))))
  (defun leafcount (target) (leaflength (leafbytes target))))

(defun ghost-cursor (buffer)
  "Ghosts the buffer AND moves origin-key & origin-data over to the new buffer."
  (let ((new (db.txn:ghost buffer)))
    (setf (origin-key) (sap+ (sap new) (sap- (origin-key) (sap buffer)))
	  (origin-data) (sap+ (sap new) (sap- (origin-data) (sap buffer))))
    new))

(flet ((fetch-linked-buffer (buffer)
	 (let ((nbuffer (link buffer)))
	   (prog1 
	       (unless (zerop nbuffer)
		 ;(release buffer)
		 (prog1 (acquire nbuffer)
		   (assert (and (not (eq nbuffer buffer)) "db.cursor:fetch-linked-buffer got same buffer!"))))
	     (release buffer)))))
  (defun start-of-next (buffer)
    (aif (fetch-linked-buffer buffer)
	 (multiple-value-bind (ks ds) (leaf-start it)
	   (setf (origin-key) ks (origin-data) ds)
	   it)
	 (progn (setf (origin-key) (int-sap 0) (origin-data) (int-sap 0)) nil))))

(defun past-end? (buffer v) (sb-sys:sap<= (sap+ (leaf-datum buffer) (* (packed buffer) (leaf-u8s))) v))
(defun past-last-block? (buffer v) (sb-sys:sap<= (sap+ (leaf-datum buffer) (* (1- (packed buffer)) (leaf-u8s))) v))

(defun next (buffer)
  (if (past-last-block? buffer (origin-data))
      (start-of-next buffer)
      (progn (setf (origin-key) (sap+ (origin-key) (key-u8s)) (origin-data) (sap+ (origin-data) (leaf-u8s)))
	     buffer)))

(defun reposition (n) (setf (origin-data) (sap+ (origin-data) n)))

;; It would be nicer to pass a SAP instead of an ARRAY+OFFSET, but because I acquire locks (on buffers) I can't use 
;; with-pinned-objects --- because it disable GC, and other threads acquire or release locks with GC... so deadlock.
(labels ((do-read (buffer dest offset len)
	   (if (> len 0)
	       (progn (with-pinned-objects (dest) ;; No mutex.
			(system-area-ub8-copy (origin-data) 0 (vector-sap dest) offset (min len (leaf-u8s))))
		      (do-read (next buffer) dest (+ offset (leaf-u8s)) (- len (leaf-u8s))))
	       buffer))
	 (do-write (buffer source offset len)
	   (if (> len 0)
	       (progn (with-pinned-objects (source) ;; No mutex.
			(system-area-ub8-copy (vector-sap source) offset (origin-data) 0 (min len (leaf-u8s))))
		      (do-write (awhen (next buffer) (ghost-cursor it)) source (+ offset (leaf-u8s)) (- len (leaf-u8s))))
	       buffer))
	 (do-skip (buffer len) (if (> len 0) (do-skip (next buffer) (- len (leaf-u8s))) buffer))
	 (read-len ()
	   (let ((length (sap-ref-32 (origin-data) 0)))
	     (make-array length :element-type '(unsigned-byte 8))))
	 (write-len (bytes) (setf (sap-ref-32 (origin-data) 0) (length bytes)))
	 ;;(reposition (n) (setf (origin-data) (sap+ (origin-data) n)))
	 (args-first (bytes n) (values bytes 0 (min (length bytes) (- (leaf-u8s) n))))
	 (args-rest (bytes n) (let ((already (- (leaf-u8s) n))) 
				(values bytes already (- (length bytes) already)))))

  (defun run (buffer)
    (let ((bytes (read-len)))
      (assert (< (length bytes) 12000)) ;; Just a debug check to test value reasonably ness... will be increased.
      (values bytes
	      (if (zerop (length bytes))
		  (next buffer)
		  (progn
		    (reposition 4)
		    (let ((nbuffer (multiple-value-call #'do-read buffer (args-first bytes 4))))
		      (when (eq nbuffer buffer) (reposition -4))
		      (multiple-value-call #'do-read nbuffer (args-rest bytes 4))))))))
  (defun run! (buffer bytes)
    (assert (< (length bytes) 12000)) ;; just a debug check.
    (if (zerop (write-len bytes))
	(awhen (next buffer) (ghost-cursor it))
	(progn (reposition 4)
	       (let ((nbuffer (multiple-value-call #'do-write buffer (args-first bytes 4))))
		 (when (eq nbuffer buffer) (reposition -4))
		 (multiple-value-call #'do-write nbuffer (args-rest bytes 4))))))
  (defun skip-inner (buffer count)
    (assert (< count 12000)) ;; just a debug check
    (if (zerop count)
	(next buffer)
	(progn (reposition 4)
	       (let ((nbuffer (do-skip buffer (min count (- (leaf-u8s) 4)))))
		 (when (eq nbuffer buffer) (reposition -4))
		 (do-skip nbuffer (- count (- (leaf-u8s) 4)))))))
  (defun skip (buffer) (skip-inner buffer (sap-ref-32 (origin-data) 0))))

(defun do-write (str target)
  (let ((wbuffer (ghost-cursor (lookup target))))
    (setf wbuffer (run! wbuffer str))
    (when wbuffer (release wbuffer))))

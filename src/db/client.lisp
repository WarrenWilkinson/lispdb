(defpackage :db.client
  (:use :common-lisp :util.util :db.io :db.txn :db.threads :db.bt :db.dir :db.resources :db.acl)
  (:import-from :sb-sys sap+ sap- sap-ref-8 sap-ref-32 sap-ref-64 with-pinned-objects vector-sap)
  (:import-from :sb-kernel system-area-ub8-copy)
  (:shadow directory)
  (:export +super-block+ *client-info* 
	   client-buffer

	   base36encode client? subscribe affiliate noclient ghost-client
	   next-oid gen-oid company caste registered directory page content diff archives acl membership +vcache+ 
	   clients
	   
	   with-client with-directory-tree with-page-tree with-content-tree with-diff-tree with-archives-tree with-acl-tree
	   with-membership-tree))

(in-package :db.client)

(defconstant +super-block+ 15) ;+user-start+)
(defconstant +client-root+ 0)

;(defvar *client-info*) ;; 4 74
(defbtree *client-info* 4 74)

(defthreadlocal loaded-client
  (client-ptr    pointer)      ;; PTR to in memory client details.
  (client-buffer integer 32)   ;; Holds the (locked, but released) buffer# that has our data.
  (client-space  memory  296)) ;; 296 bytes space to manipulate or hold in-memory client.

(defun client () 
  (btree *client-info*)
  (let ((hb (acquire +super-block+)))
    (prog1 (sap-ref-32 (sap hb) +client-root+) (release hb))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +client-oid+         0) ;; Next available OID.
(defconstant +client-caste+       4) ;; 0 = MY SITES, 1 = demo, 2 = subscriber.
(defconstant +client-company+     8) ;; UTF8, 1byte len, and then up to 63 bytes of UTF-8 info
(defconstant +client-dir+        72) ;; TREE BLOCK-REFS
(defconstant +client-page+       76)
(defconstant +client-content+    80)
(defconstant +client-diff+       84) 
(defconstant +client-archives+   88)
(defconstant +client-acl+        92)
(defconstant +client-membership+ 96) 

(defconstant +client-vcache+     -4) ;; NO SPACE LEFT!
(defconstant +client-registered+ 100) ;; Account creation date
;(defconstant +next... 92)
 ;; Account INFO... preferences... statistic write-back areas... COOKIE, LAST-LOGIN, etc (other DEMO things).

(defmacro next-oid ()    '(sap-ref-32 (client-ptr) +client-oid+))
(defmacro caste   ()    '(sap-ref-32 (client-ptr) +client-caste+))
(defun company    ()
  (let ((arr (make-array (sap-ref-8 (client-ptr) +client-company+) :element-type '(unsigned-byte 8))))
    ;; NO mutex. If some threads call with-pinned-objects while holding a mutex, and some call without, you can deadlock. 
    (with-pinned-objects (arr) (system-area-ub8-copy (client-ptr) (+ +client-company+ 1) (vector-sap arr) 0 (length arr)))
    (sb-ext:octets-to-string arr :external-format :utf8)))
(defun company!   (str)
  (let ((bytes (sb-ext:string-to-octets str :external-format :utf8)))
    (assert (<= (length bytes) 63))
    (setf (sap-ref-8 (client-ptr) +client-company+) (length bytes))
    ;; No mutex.
    (with-pinned-objects (bytes) (system-area-ub8-copy (vector-sap bytes) 0 (client-ptr) (+ +client-company+ 1) (length bytes)))))
(defsetf company company!)
(defmacro registered () '(sap-ref-64 (client-ptr) +client-registered+))

(defun directory () (btree *dir-info*) (sap-ref-32 (client-ptr) +client-dir+))
(defun set-directory (value) (setf (sap-ref-32 (client-ptr) +client-dir+) value))
(defun page () (btree *page-info*) (sap-ref-32 (client-ptr) +client-page+))
(defun set-page (value) (setf (sap-ref-32 (client-ptr) +client-page+) value))
(defun content () (btree *view-info*) (sap-ref-32 (client-ptr) +client-content+))
(defun set-content (value) (setf (sap-ref-32 (client-ptr) +client-content+) value))
(defun diff () (btree *diff-info*) (sap-ref-32 (client-ptr) +client-diff+))
(defun set-diff (value) (setf (sap-ref-32 (client-ptr) +client-diff+) value))
(defun archives () (btree *archives-info*) (sap-ref-32 (client-ptr) +client-archives+))
(defun set-archives (value) (setf (sap-ref-32 (client-ptr) +client-archives+) value))
(defun acl () (btree *acl-info*) (sap-ref-32 (client-ptr) +client-acl+))
(defun set-acl (value) (setf (sap-ref-32 (client-ptr) +client-acl+) value))
(defun membership () (btree *membership-info*) (sap-ref-32 (client-ptr) +client-membership+))
(defun set-membership (value) (setf (sap-ref-32 (client-ptr) +client-membership+) value))

(defsetf directory set-directory)
(defsetf page set-page)
(defsetf content set-content)
(defsetf diff set-diff)
(defsetf archives set-archives)
(defsetf acl set-acl)
(defsetf membership set-membership)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun base36encode (str)
  (assert (< 0 (length str) 25))
  (assert (not (eq #\0 (elt str (1- (length str))))))
  (parse-integer (concatenate 'string str (make-string (- 24 (length str)) :initial-element #\0)) :radix 36))

(defun base36decode (number)
  (string-right-trim '(#\0) (format nil "~36R" number)))

(defun base36down-key (str)
  (let ((v (base36encode str))
	(sap (down-key)))
    (setf (sap-ref-32 sap 0)  (ldb (byte 32 96) v)
	  (sap-ref-32 sap 4)  (ldb (byte 32 64) v)
	  (sap-ref-32 sap 8)  (ldb (byte 32 32) v)
	  (sap-ref-32 sap 12) (ldb (byte 32 0)  v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun noclient () (release (elt +buffers+ (client-buffer))) (setf (client-buffer) 0))

(defun ghost-client ()
  (unless (zerop (client-buffer))
    (let* ((buffer (elt +buffers+ (client-buffer)))
	   (nbuffer (ghost buffer))
	   (nsap (sap+ (sap nbuffer) (sap- (client-ptr) (sap buffer)))))
      (setf (client-ptr) nsap (client-buffer) (bnum nbuffer)))))

(defun client? (hostname)
  (let ((client-root (client)))
    (unless (zerop client-root)
      (base36down-key hostname)
      (let ((buffer (lookup client-root)))
	(prog1 (found) (release buffer))))))

(defun clients ()
  (with-reading 
    (setf (sap-ref-32 (down-key) 0) 0)
    (let* ((client-root (client))
	   (buffer (lookup client-root))
	   (acc nil))
      (tagbody :start
	 (unless (valid) (and buffer (release buffer)) (return-from clients (nreverse acc)))
	 (push (base36decode
		(boole boole-ior (boole boole-ior (boole boole-ior (ash (sap-ref-32 (origin-key) 0) 96)
							           (ash (sap-ref-32 (origin-key) 4) 64))
					                           (ash (sap-ref-32 (origin-key) 8) 32))
		                                                   (ash (sap-ref-32 (origin-key) 12) 0))) acc)
	 (setf buffer (db.cursor:next buffer))
	 (go :start)))))
  
(defun subscribe (hostname company caste &optional (registered (2010now)))
  (and (client? hostname) (error "The client ~a is already registered." hostname))
  (let ((oldptr (client-ptr)))
    (setf (client-ptr) (client-space) (down-data) (client-space))
    (setf (next-oid) 1024 (caste) caste (company) company (directory) 0 (page) 0 (content) 0 (diff) 0 (archives) 0
	  (registered) registered)
    (base36down-key hostname)
    (let* ((originally (client))
	   (target (insert (creating originally))))
      (unless (= originally target)
	(let ((hb (ghost (acquire +super-block+))))
	  (setf (sap-ref-32 (sap hb) +client-root+) target)
	  (release hb))))
    (setf (client-ptr) oldptr)))

(flet ((become (buffer) (setf (client-buffer) (bnum buffer) (client-ptr) (origin-data))))
  (defun affiliate (hostname)
    (assert (zerop (client-buffer)))
    (base36down-key hostname)
    (let ((buffer (lookup (client))))
      (if (found) (become buffer) (progn (release buffer) (error "No such client ~a" hostname))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-client (name &rest code)
  `(progn (affiliate ,name)
	  (unwind-protect (progn ,@code)
	    (noclient))))
(defmacro make-wrapper (field)
  `(defmacro ,(intern (format nil "WITH-~a-TREE" field)) ((var) &body body)
     (let ((osym (gensym)))
       `(let* ((,osym (,',field))
	       (,var ,osym))
	  (prog1 (progn ,@body)
	    (unless (= ,osym ,var)
	      (ghost-client)
	      (setf (,',field) ,var)))))))
(make-wrapper directory)
(make-wrapper page)
(make-wrapper content)
(make-wrapper diff)
(make-wrapper archives)
(make-wrapper acl)
(make-wrapper membership)
;; See also, with-content-tree defined in db.db. 

(defun gen-oid     () (let ((v (next-oid))) (assert *txn*) (ghost-client) (setf (next-oid) (1+ v)) v))

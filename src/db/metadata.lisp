(defpackage :db.metadata
  (:use :common-lisp :util.util :db.threads :db.bt)
  (:import-from :sb-sys sap-ref-8 sap-ref-32 sap-ref-64 with-pinned-objects vector-sap)
  (:import-from :sb-kernel system-area-ub8-copy)
  (:export oid copy-oid pair copy-pair unoid unpair
	   ;;
           write-metadata read-metadata make-metadata 
	   meta-raw +type+ +created+ +creator+ +owner+ +updated+ +updater+ +metadata-length+ ;; +updater+ is used in a test.
	   metatype secure-bit created creator owner updated updater make-metadata))

(in-package :db.metadata)

(defthreadlocal relation-data 
  (mark  memory 16)) ;; 16 bytes  space to store a 2 oids. 

(defun oid (oid)
  (let ((d (mark)))
    (setf (sap-ref-32 d 0)  (ldb (byte 32 24) oid)
	  (sap-ref-32 d 4)  (ash (ldb (byte 24 0) oid) 8)
	  (sap-ref-32 d 8)  #x8BADF00D
	  (sap-ref-32 d 12) #x8BADF00D)))

(defun copy-oid ()
  (let ((s (mark)) (d (down-key)))
    (setf (sap-ref-32 d 0) (sap-ref-32 s 0)
	  (sap-ref-32 d 4) (sap-ref-32 s 4)
	  (sap-ref-32 d 8) #xDEADBEEF
	  (sap-ref-32 d 12) #xDEADBEEF)))

(defun pair (oid parent-oid) 
  (let ((d (mark)))
    (setf (sap-ref-32 d 0)  (ldb (byte 32 24) parent-oid)
	  (sap-ref-32 d 4)  (boole boole-ior (ash (ldb (byte 24 0) parent-oid) 8) (ldb (byte 8 48) oid))
	  (sap-ref-32 d 8)  (ldb (byte 32 16) oid)
	  (sap-ref-32 d 12) (ash (ldb (byte 16 0) oid) 16))))
(defun copy-pair () 
  (let ((s (mark)) (d (down-key)))
    (setf (sap-ref-32 d 0) (sap-ref-32 s 0)
	  (sap-ref-32 d 4) (sap-ref-32 s 4)
	  (sap-ref-32 d 8) (sap-ref-32 s 8)
	  (sap-ref-32 d 12) (sap-ref-32 s 12))))

(defun unoid ()
  (let ((s (origin-key)))
    (boole boole-ior (ash (sap-ref-32 s 0) 24) (ldb (byte 24 8) (sap-ref-32 s 4)))))

(defun unpair ()
  "Returns parent then oid"
  (let ((s (origin-key)))
    (values (boole boole-ior (ash (sap-ref-32 s 0) 24) (ldb (byte 24 8) (sap-ref-32 s 4)))
	    (boole boole-ior (boole boole-ior (ash (ldb (byte 8 0) (sap-ref-32 s 4)) 48)
				    (ash (sap-ref-32 s 8) 16))
		   (ash (sap-ref-32 s 12) -16)))))

;; Meta data is used by DIRECTORY (with metatype), and by DOCUMENTS (+type+ = bytes of payload) 
;; secure-bit is used to record when an object has custom security information.

(defconstant +type+ 0)
(defconstant +created+ 4)
(defconstant +creator+ 12)
(defconstant +owner+ 20)
(defconstant +updated+ 28)
(defconstant +updater+ 36)
(defconstant +metadata-length+ 44)

(defmacro meta-raw (position)
  (if (or (eq position '+type+) (eq position 0))
      `(sap-ref-32 (origin-data) +type+)
      `(sap-ref-64 (origin-data) ,position)))

;; This would be cooler if I could just pass pointers. Instead I have to pick one or the other interface.
(defun write-metadata (metadata)
  (with-pinned-objects (metadata)
    (system-area-ub8-copy (vector-sap metadata) 0 (origin-data) 0 +metadata-length+)))

(defun read-metadata ()
  (let ((data (make-array (+ 4 8 8 8 8 8) :initial-element 0 :element-type '(unsigned-byte 8))))
    (with-pinned-objects (data)
      (system-area-ub8-copy (origin-data) 0 (vector-sap data) 0 +metadata-length+))
    data))

(defun make-metadata (metatype created creator owner updated updater)
  (let ((data (make-array (+ 4 8 8 8 8 8) :initial-element 0 :element-type '(unsigned-byte 8))))
    (with-pinned-objects (data)
      (setf (origin-data) (vector-sap data)
	    (sap-ref-32 (vector-sap data) +type+) metatype
	    (sap-ref-64 (vector-sap data) +created+) created
	    (sap-ref-64 (vector-sap data) +creator+) creator
	    (sap-ref-64 (vector-sap data) +owner+)   owner
	    (sap-ref-64 (vector-sap data) +updated+) updated
	    (sap-ref-64 (vector-sap data) +updater+) updater))
    data))

(defun metatype   (metadata) (with-pinned-objects (metadata) (ldb (byte 31 0) (sap-ref-32 (vector-sap metadata) +type+))))
(defun secure-bit (metadata) (with-pinned-objects (metadata) (ldb (byte 1 31) (sap-ref-32 (vector-sap metadata) +type+))))
(defun created    (metadata) (with-pinned-objects (metadata) (sap-ref-64 (vector-sap metadata) +created+)))
(defun creator    (metadata) (with-pinned-objects (metadata) (sap-ref-64 (vector-sap metadata) +creator+)))
(defun owner      (metadata) (with-pinned-objects (metadata) (sap-ref-64 (vector-sap metadata) +owner+)))
(defun updated    (metadata) (with-pinned-objects (metadata) (sap-ref-64 (vector-sap metadata) +updated+)))
(defun updater    (metadata) (with-pinned-objects (metadata) (sap-ref-64 (vector-sap metadata) +updater+)))

(defun set-owner   (metadata nv) (with-pinned-objects (metadata) (setf (sap-ref-64 (vector-sap metadata) +owner+) nv)))
(defun set-updated (metadata nv) (with-pinned-objects (metadata) (setf (sap-ref-64 (vector-sap metadata) +updated+) nv)))
(defun set-updater (metadata nv) (with-pinned-objects (metadata) (setf (sap-ref-64 (vector-sap metadata) +updater+) nv)))

(defsetf owner set-owner)
(defsetf updated set-updated)
(defsetf updater set-updater)

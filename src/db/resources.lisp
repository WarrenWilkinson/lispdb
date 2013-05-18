(defpackage :db.resources
  (:use :common-lisp :util.util :db.threads :db.txn :db.bt :db.cursor :db.metadata)
  (:import-from :sb-sys sap-ref-8)
  (:export *zeros* 
	   *page-info* *view-info* *diff-info* *archives-info* *cartulary-info*
	   rz-start make-rz delete-rz rz! rz@ deserialize serialize sexp@ sexp!
	   +document+ pack-document unpack-document))

(in-package :db.resources)

(defvar *zeros* (load-time-value (malloc 568)))

;; Pages, Views, ACL and Groups are read/written using a cursors. Documents always have metadata prepended to their data.

(defbtree *page-info* 2 142)
(defbtree *view-info* 2 82)
(defbtree *diff-info* 3 32)
(defbtree *archives-info* 2 2)
(defbtree *cartulary-info* 2 27)

(defun rz-key (i) (copy-oid) (setf (sap-ref-8 (down-key) 4) i))
(defun rz-start () (rz-key 0))
(defun make-rz (target oid) (oid oid) (rz-start) (setf (down-data) *zeros*) (insert (creating target)))
(defun delete-rz (target) (rz-start) (dotimes (i (leafcount target) target) (rz-key i) (setf target (rubout target))))
(defun rz@ (buffer) (run buffer))

(flet ((rz-alloc (count desired target)
	 (if (>= count desired)
	     (dotimes (i (- count desired)) (rz-key (- count i 1)) (setf target (rubout target)))
	     (dotimes (i (- desired count)) (rz-key (+ count i)) (setf target (insert target)))) target))
  (defun rz! (target ncontent)
    (rz-start) (setf target (rz-alloc (leafcount target) (leaflength (length ncontent)) target))
    (rz-start) (do-write ncontent target) target))

(defun deserialize (bin &optional (start 0))
  (let ((*package* (find-package :db.db)))
    (read-from-string (sb-ext:octets-to-string bin :external-format :utf8 :start start))))
(defun serialize (str)
  (let ((*package* (find-package :db.db)))
    (sb-ext:string-to-octets (prin1-to-string str) :external-format :utf8)))
(defun sexp@ (buffer) (assert buffer) (multiple-value-bind (bytes buffer) (rz@ buffer) (values (deserialize bytes) buffer)))
(defun sexp! (target sexp) (rz! target (serialize sexp)))

(defconstant +document+ 0)
(defun pack-document (meta sexp) (concatenate '(vector (unsigned-byte 8)) (subseq meta 4) (serialize sexp)))
(defun unpack-document (buffer)
  (multiple-value-bind (bytes buffer) (rz@ buffer)
    (values (concatenate '(vector (unsigned-byte 8)) 
		(make-array 4 :element-type '(unsigned-byte 8) :initial-contents (list +document+ 0 0 0))
		(subseq bytes 0 (- +metadata-length+ 4)))
	    (deserialize bytes (- +metadata-length+ 4)) buffer)))

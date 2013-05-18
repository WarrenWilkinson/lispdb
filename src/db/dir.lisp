(defpackage :db.dir
  (:use :common-lisp :util.util :db.txn :db.threads :db.bt :db.cursor :db.metadata :db.resources)
  (:import-from :sb-sys sap-ref-16 sap-ref-32)
  (:export *dir-info* make-entry delete-entry entry-key move
	   name@ name! title@ title! description@ description!))

(in-package :db.dir)

(defbtree *dir-info* 4 11)

(defun entry-key () (copy-pair) (setf (sap-ref-16 (down-key) 12) 0))
(defun name-key (i) (copy-pair) (setf (sap-ref-16 (down-key) 12)  (boole boole-ior #x0100 i)))
(defun title-key (i) (copy-pair) (setf (sap-ref-16 (down-key) 12) (boole boole-ior #x0200 i)))
(defun desc-key (i) (copy-pair) (setf (sap-ref-16 (down-key) 12)  (boole boole-ior #x0300 i)))

(flet ((run-string (buffer)
	 (multiple-value-bind (str buffer) (run buffer)
	   (values (sb-ext:octets-to-string str :external-format :utf8) buffer))))
  (defun name@ (buffer) (run-string buffer))
  (defun title@ (buffer) (run-string buffer))
  (defun description@ (buffer) (run-string buffer)))

(defun dir-meta@ (buffer) 
  (multiple-value-bind (name buffer) (name@ buffer)
    (multiple-value-bind (title buffer) (title@ buffer)
      (multiple-value-bind (desc buffer) (description@ buffer)
	(values name title desc buffer)))))

(defthreadlocal temporary-meta
    (temp-meta  memory  44)) ;; Same size as metadata.

(defun make-entry (target oid parent-oid meta);owner creator type)
  ;; Secure bit defaults to 0 (off) for all new entrys.
  (let ((sap (temp-meta)));scratch)))
    (setf (origin-data) sap (down-data) sap)
    (write-metadata meta)
    ;(setf (secure-bit) 0 (metatype) type (created) now (creator) creator (owner) owner (updated) now (updater) creator)
    (pair oid parent-oid)
    (entry-key) (setf target (insert (creating target)))
    (name-key 0) (setf (down-data) *zeros*) (setf target (insert target))
    (title-key 0) (setf (down-data) *zeros*) (setf target (insert target))
    (desc-key 0) (setf (down-data) *zeros*) (setf target (insert target))
    (assert (zerop (sap-ref-32 *zeros* 0)))
    target))

(defun delete-entry (target)
  (name-key 0)
  (let* ((buffer (lookup target))
	 (exist-nl (prog1 (leaflength (sap-ref-32 (origin-data) 0)) (setf buffer (db.cursor:skip buffer))))
	 (exist-tl (prog1 (leaflength (sap-ref-32 (origin-data) 0)) (setf buffer (db.cursor:skip buffer))))
	 (exist-dl (prog1 (leaflength (sap-ref-32 (origin-data) 0)) (setf buffer (db.cursor:skip buffer)))))

    ;; Added just for testing as an early warning...
    (when (or (> exist-nl 50) (> exist-tl 59) (> exist-dl 50))
      (error "Delete entry ~a too big! got ~a ~a ~a" target exist-nl exist-tl exist-dl))
    (when buffer (release buffer))
    (entry-key) (setf target (rubout target))
    (dotimes (i exist-nl) (name-key i) (setf target (rubout target)))
    (dotimes (i exist-tl) (title-key i) (setf target (rubout target)))
    (dotimes (i exist-dl) (desc-key i) (setf target (rubout target)))
    target))

(flet ((name-alloc (count desired target)
	 (if (>= count desired)
	     (dotimes (i (- count desired)) (name-key (- count i 1)) (setf target (rubout target)))
	     (dotimes (i (- desired count)) (name-key (+ count i)) (setf target (insert target)))) target))
  (defun name! (target nname &aux (nname (sb-ext:string-to-octets nname :external-format :utf8)))
    (name-key 0) (setf target (name-alloc (leafcount target) (leaflength (length nname)) target))
    (name-key 0) (do-write nname target) target))

(flet ((title-alloc (count desired target)
	 (if (>= count desired)
	     (dotimes (i (- count desired)) (title-key (- count i 1)) (setf target (rubout target)))
	     (dotimes (i (- desired count)) (title-key (+ count i)) (setf target (insert target)))) target))
  (defun title! (target ntitle &aux (ntitle (sb-ext:string-to-octets ntitle :external-format :utf8)))
    (title-key 0) (setf target (title-alloc (leafcount target) (leaflength (length ntitle)) target))
    (title-key 0) (do-write ntitle target) target))

(flet ((desc-alloc (count desired target)
	 (if (>= count desired)
	     (dotimes (i (- count desired)) (desc-key (- count i 1)) (setf target (rubout target)))
	     (dotimes (i (- desired count)) (desc-key (+ count i)) (setf target (insert target)))) target))
  (defun description! (target ndesc &aux (ndesc (sb-ext:string-to-octets ndesc :external-format :utf8)))
    (desc-key 0) (setf target (desc-alloc (leafcount target) (leaflength (length ndesc)) target))
    (desc-key 0) (do-write ndesc target) target))


;; Instead of move, I should just do (transactionally) delete and make.... MUCH SIMPLER.
(defun move (target new-parent)
  (entry-key)
  (let ((buffer (lookup target)))
    (assert (found))
    (multiple-value-bind (old-parent oid) (unpair)
      (when (= new-parent old-parent)
	(format *loop* "~%Tried to move ~d to ~a which is their old parent!" oid new-parent)); (get-parent)))
      (assert (not (= new-parent old-parent)))
      (let ((meta (read-metadata)))
	(multiple-value-bind (name title desc buffer) (dir-meta@ (next buffer))
	  (when buffer (release buffer))
	  (let ((target (make-entry (delete-entry target) oid new-parent meta)))
	    (setf target (db.dir:name! target name))
	    (setf target (db.dir:title! target title))
	    (setf target (db.dir:description! target desc))))))))
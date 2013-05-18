(defpackage :db.archives
  (:use :common-lisp :util.util :db.io :db.threads :db.txn :db.bt :db.metadata :db.resources :db.client)
  (:import-from :sb-sys sap+ sap- sap-ref-32)
  (:shadowing-import-from :db.client directory)
  (:export open-archives close-archives write-archives ghost-archives
	   cartulary next-doid gen-doid
	   with-archives/writing with-archives/reading with-cartulary))

(in-package :db.archives)

(defthreadlocal cartulary-data
  (cartulary-ptr    pointer)     ;; PTR to in memory cartulary details.
  (cartulary-buffer integer 32)  ;; buffer# that is opened to the cartulary data.
  (cartulary-poid   integer 64)  ;; Holds the page oid that the cartulary is under.
  (cartulary-space  memory  12)) ;; 12 bytes  space to manipulate or hold in-memory cartulary.

(defun cartulary () (btree *cartulary-info*) (sap-ref-32 (cartulary-ptr) 0))
(defun set-cartulary (nv) (setf (sap-ref-32 (cartulary-ptr) 0) nv))
(defsetf cartulary set-cartulary)
(defmacro next-doid () '(sap-ref-32 (cartulary-ptr) 4))

;; Maybe I should call all of these open-cartulary... 

(flet ((new-cartulary ()
	 (let ((space (cartulary-space)))
	   (setf (cartulary-buffer) 0 (cartulary-ptr) space
		 (sb-sys:sap-ref-32 space 0) 0  ;; block node where documents are stored.
		 (sb-sys:sap-ref-32 space 4) 1  ;; next oid (defaults to 1, because 0 is a special value)
		 (sb-sys:sap-ref-32 space 8) 0)))  ;; I don't think this is used, perhaps for last modified? 
       (store-cartulary (buffer)
	 (format *loop* " (packing ~a cartularies)" (packed buffer))
	 (setf (cartulary-buffer) (bnum buffer) (cartulary-ptr) (origin-data))))
  (defun open-archives (page-oid)
    (setf (cartulary-poid) page-oid)
    (oid page-oid) (copy-oid)
    (let ((page (archives)))
      (format *loop* " (archives: ~a)" page)
      (if (zerop page)
	  (new-cartulary)
	  (let ((buffer (lookup page)))
	    (if (found)
		(store-cartulary buffer)
		(progn (format *loop* "~%No cartulary for ~a in tree@~a" page-oid page) (release buffer) (new-cartulary))))))))

;; Maybe ghost-cartulary? 
(defun ghost-archives ()
  (unless (zerop (cartulary-buffer))
    (let* ((buffer (elt +buffers+ (cartulary-buffer)))
	   (nbuffer (ghost buffer))
	   (nsap (sap+ (sap nbuffer) (sap- (cartulary-ptr) (sap buffer)))))
      (setf (cartulary-ptr) nsap (cartulary-buffer) (bnum nbuffer)))))

(flet ((ins ()
	 (oid (cartulary-poid)) (copy-oid)
	 (setf (down-data) (cartulary-space))
	 (with-archives-tree (arch) (setf arch (insert (creating arch)))))
       (del () (oid (cartulary-poid)) (copy-oid) (with-archives-tree (arch) (setf arch (rubout arch))))
       (rel () (release (elt +buffers+ (cartulary-buffer)))))
  (defun close-archives () (unless (zerop (cartulary-buffer)) (rel)))
  (defun write-archives ()
    (assert *txn*)
    (cond ((and (zerop (cartulary-buffer)) (zerop (cartulary))) nil)
	  ((zerop (cartulary-buffer)) (ins))
	  ((zerop (cartulary)) (del) (rel))
	  (t (rel)))))

(defmacro with-archives/writing (oid &rest body)
  (let ((success (gensym)))
    `(let ((,success nil))
       (unwind-protect (progn (open-archives ,oid)
			      (ghost-archives)
			      (prog1 (progn ,@body) (setf ,success t)))
	 (if ,success (write-archives) (close-archives))))))

(defmacro with-archives/reading (oid &rest body)
  `(unwind-protect (progn (open-archives ,oid)
			  ,@body)
     (close-archives)))

(defmacro with-cartulary ((var) &rest body)
  (let ((originally (gensym)))
    `(let* ((,originally (cartulary))
	    (,var ,originally))
       (prog1 (progn ,@body)
	 (unless (= ,originally ,var)
	   (setf (cartulary) ,var))))))

(defun gen-doid () (let ((v (next-doid))) (assert *txn*) (ghost-archives) (setf (next-doid) (1+ v)) v))

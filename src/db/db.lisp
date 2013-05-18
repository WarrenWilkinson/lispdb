(defpackage :db.db
  (:use :common-lisp :util.util :db.io :db.threads :db.txn :db.bt :db.cursor :db.metadata
	:db.dir :db.resources :db.acl :db.client :db.archives)
  (:shadowing-import-from :db.client directory)
  (:export +document+ +page+ +view+ +chart+ *thread*
	   with-reading with-writing with-client clients client? base36encode
	   metatype with-page-tree with-content-tree with-hack/content-tree
	   
	   unpair name@ title@ description@ next skip sexp@ sexp!
	   created creator owner updated updater
	   page-data view-data unpair pair copy-pair oid copy-oid lookup directory found save-dir

	   gen-oid *txn*

	   with-directory-tree ghost ghost-cursor entry-key  ;; Do I have to export these?

	   named-user
	   
	   with-archives/writing with-archives/reading gen-doid with-cartulary

	   ;; ACL STUFF...
	   with-acl-tree set-acl role permissions role-bits delete-acl
	   +role-administrator+ +role-designer+ +role-editor+ role-number
	   +role-user+ +role-reader+ +role-submitter+ +role-no-access+
	   +custom+ +administrator+ +designer+ +editor+ +user+ +submitter+ +reader+ +no-access+
	   +read+ +edit+ +create+ +delete+ +secure+ +reassign+ +export+
	   +permit+ +constraint-owner+ +constraint-creator+ +constraint-recent+ +constraint-mask+ 
	   
	   enabled-users enabled-groups recorded-memberships complete-memberships
	   group->shifted user->shifted shifted->user shifted->group group-p user-p enroll expell groups members
	   
	   meta-raw +type+ +created+ +creator+ +owner+ +updated+ +updater+ +metadata-length+

	   next skip name! title! description!
	   topdir chdir chfile chpath children/type oid-path
	   dir-has-pair-p restore-dir make-client release
	   
	   badpath baddocument doid

	   checkpoint shutdown initialize

	   make-entry make-metadata create-document update-document
	   document-meta+content entry-meta entry-content
	   create-entry-meta update-entry-meta
	   create-entry-content update-entry-content

	   +top-page+ +system-page+ +user-page+ +user-alphabetical-view+ +group-page+ +import-page+ +root+ +guest+
	   ))

(in-package :db.db)

(defconstant +page+  1) 
(defconstant +view+  2)
(defconstant +chart+ 3)

(defconstant +all-registered-group+ 0)

(defconstant +top-page+ 1)
(defconstant +system-page+ 2)
(defconstant +user-page+ 3)
(defconstant +group-page+ 4)
(defconstant +import-page+ 5)

(defconstant +user-alphabetical-view+ 16)
(defconstant +group-alphabetical-view+ 18)

(defconstant +guest+ 0)
(defconstant +root+ 1)

(define-condition badpath (error) ())

(defthreadlocal current-working-directory
  (cwd-sector  integer 32)   ;; Holds the sector# that has our data. I do it this way rather than buffer#  
  (cwd-offset  integer 32))  ;; because then I can close the buffer.

(defun restore-dir () ;; Restore the saved directory to the cursor...
  (let ((buffer (acquire (cwd-sector))))
    (leaf) (set-origin buffer (cwd-offset)) (node)
    buffer))

(defun save-dir (buffer) ;; Save the current cursor to the directory.
  (setf (cwd-sector) (sector buffer))
  (setf (cwd-offset) (truncate (- (sb-sys:sap- (origin-key) (sap buffer)) +header-bytes+) (key-u8s))))

(defun topdir () 
  (pair 1 0) (copy-pair) ;; Seek first OID with parent 0.. 
  (let ((buffer (lookup (directory))))
    (assert (found))
    (save-dir buffer)
    buffer))

(labels ((seek (name parent cursor)
	   (if (and cursor (eq parent (unpair)))
	       (let ((oldcursor cursor)
		     (data (origin-data))
		     (key (origin-key)))
		 (acquire-buf oldcursor)
		 (multiple-value-bind (string cb) (name@ (next cursor))
		   (release cb)
		   (setf cursor oldcursor)
		   (setf (origin-data) data)
		   (setf (origin-key) key)
		   (if (string-equal string name) cursor
		       (progn (setf cursor (next cursor)) ;; Skip meta, name, title, desc
			      (setf cursor (skip cursor))
			      (setf cursor (skip cursor))
			      (setf cursor (skip cursor))
			      (seek name parent cursor)))))
	       (and cursor (release cursor) nil))))
  (defun chlist (cursor path-list)
    "Changes the current working directory according to the path's pathname-directory."
    (if (null path-list)
	cursor
	(multiple-value-bind (current-parent new-parent) (unpair)
	  (declare (ignore current-parent))
	  (pair 0 new-parent) (copy-pair)
	  (when cursor (release cursor))
	  (setf cursor (lookup (directory))) ;; Lookup the first child of the parent.
	  (if (and (valid) (setf cursor (seek (car path-list) new-parent cursor)));; Succesful seek puts us in good position.
	      (progn (save-dir cursor) (chlist cursor (cdr path-list)))
	      (progn (when cursor (release cursor)) (error 'badpath))))))
  (defun oid-list (cursor path-list oid-list)
    (multiple-value-bind (current-parent new-parent) (unpair)
      (declare (ignore current-parent))
      (push new-parent oid-list)
      (if (null path-list)
	  (values cursor oid-list)
	  (progn (pair 0 new-parent) (copy-pair)
		 (when cursor (release cursor))
		 (setf cursor (lookup (directory))) ;; Lookup the first child of the parent.
		 (if (and (valid) (setf cursor (seek (car path-list) new-parent cursor)))
		     (progn (save-dir cursor) (oid-list cursor (cdr path-list) oid-list))
		     (progn (when cursor (release cursor)) (error 'badpath))))))))
	  
(defun chdir (path)
  (chlist (if (eq (car (pathname-directory path)) :absolute) (topdir) (restore-dir)) (cdr (pathname-directory path))))

(defun chfile (path)
  (if (or (pathname-name path) (pathname-type path))
      (chlist (restore-dir) (list (if (pathname-type path)
				      (concatenate 'string (pathname-name path) "." (pathname-type path))
				      (pathname-name path))))
      (restore-dir)))
(defun chpath (path) (aand (chdir path) (release it)) (chfile path))

(defun oid-dir (path)
  (oid-list (if (eq (car (pathname-directory path)) :absolute) (topdir) (restore-dir)) (cdr (pathname-directory path)) nil))
   
(defun oid-file (path oid-list)
  (if (or (pathname-name path) (pathname-type path))
      (multiple-value-bind (buffer oid-list)
	  (oid-list (restore-dir) (list (if (pathname-type path)
					    (concatenate 'string (pathname-name path) "." (pathname-type path))
					    (pathname-name path))) oid-list)
	(release buffer)
	oid-list)
      oid-list))

(defun oid-path (path)
  (multiple-value-bind (buffer oid-list) (oid-dir path)
    (release buffer)
    (oid-file path oid-list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ACL stuff...  

;; Use like: (with-acl-tree (acl) (oid page) (setf acl (set-acl acl inherit non-inherit)))
(defun set-acl (acl oid inherit non-inherit)
  (oid oid) (copy-oid)
  (let ((buffer (lookup acl)))
    (when (and (null inherit) (null non-inherit))
      (if (found)
	  (return-from set-acl (delete-rz acl))
	  (return-from set-acl acl)))
    (unless (found) (setf acl (db.resources:make-rz acl oid)))
    (and buffer (release buffer)))
  (oid oid) (rz-start)
  ;;(format *loop* "~%OID: ~a~%I: ~a~%N: ~a" oid inherit non-inherit)
  (db.acl:acl! acl inherit non-inherit))

(defun role-membership (membership-list role-associations)
  (dolist (role role-associations nil)
    (when (intersection membership-list (cdr role))
      (return-from role-membership (car role)))))

(defun permissions (oid)
  (oid oid) (copy-oid)
  (let ((buffer (lookup (acl))))
    (when (found)
      (reposition 4)
      (multiple-value-bind (inherit nb) (acl@ buffer)
	(setf buffer (skip-null nb))
	(multiple-value-bind (non-inherit nb) (acl@ buffer)
	  (and nb (release nb))
	  (return-from permissions (cons inherit non-inherit)))))
    (and buffer (release buffer))
    (cons nil nil)))

(defun role (membership-list path)
  (let* ((oids (oid-path path))
	 (acl-root (acl)))
    ;;(format *loop* "~%ROLE: ~a ---> ~a" path oids)
    (oid (car oids)) (copy-oid)
    ;;(format *loop* "~%FROLE: ~d" (car oids))
    (let ((buffer (lookup acl-root)))
      (when (found)
	(reposition 4)
	(multiple-value-bind (inherit nb) (acl@ buffer)
	  (setf buffer (skip-null nb))
	  (multiple-value-bind (non-inherit nb) (acl@ buffer)
	    (setf buffer nb)
	    (awhen (or (role-membership membership-list non-inherit)
		       (role-membership membership-list inherit))
	      (and buffer (release buffer))
	      (return-from role (values it (length oids)))))))
      (and buffer (release buffer)))
    (dolist (oid (cdr oids))
      ;;(format *loop* "~%ROLE: ~d" oid)
      (oid oid) (copy-oid)
      (let ((buffer (lookup acl-root)))
	(when (found) 
	  (reposition 4)
	  (multiple-value-bind (inherit nb) (acl@ buffer)
	    (setf buffer nb)
	    (awhen (role-membership membership-list inherit)
	      (and buffer (release buffer))
	      (return-from role (values it (- (length oids) (position oid oids)))))))
	(and buffer (release buffer))))
    (values +role-no-access+ 0)))

(defun enabled-users ()
  ;; Return the (name shifted-OID) of all enabled users... EXCEPT root and guest.
  (with-archives/reading +user-page+ 
    (oid 0) (copy-oid) 
    (let ((buffer (skip (lookup (cartulary)))) ;; The first will be root... so skip it.
	  (results nil))
      (loop until (not (valid))
	    for oid = (unoid)
	    do (multiple-value-bind (meta data nbuffer) (unpack-document buffer)
		 (declare (ignore meta))
		 (setf buffer nbuffer)
		 ;;(format *loop* "~%User data: ~s~%enabled: ~a" data (util.tree:tree-find data '(("Enabled" . 0))))
		 (when (string-equal "Enabled" (cdr (util.tree:tree-find data '(("Enabled" . 0)))))
		   (push (list (cdr (util.tree:tree-find data '(("Username" . 0)))) (user->shifted oid)) results)))
	    finally (progn (and buffer (release buffer)) (return (nreverse results)))))))

(defun enabled-groups () 
  ;; Return the shifted-OID of all groups...? (INCLUDING the all-registered group...)"
  ;; NOT including all-registered-group.
  nil)
;  (list 1))

(defun recorded-memberships ()
  (with-membership-tree (mbs) 
    (oid 0) (copy-oid)
    (let ((buffer (lookup (membership))))
      (loop until (not (valid))
	    collecting (multiple-value-bind (nbuffer data) (membership@ buffer)
			 (setf buffer nbuffer)
			 (cons (group->shifted +all-registered-group+) data)) into all-data
	    finally (progn (and buffer (release buffer)) (return all-data))))))

(defun complete-memberships ()
  (let ((eu (enabled-users))
	(eg (enabled-groups))
	(ms (recorded-memberships)))
    (setf ms (mapcar #'(lambda (m) (cons (car (or (assoc (car m) eu) (assoc (car m) eg))) m)) ms))
    (setf eu (mapcar #'(lambda (u) (append u (list (group->shifted +all-registered-group+)))) eu))
    (adjoin (list "Guests" (user->shifted +guest+))
	    (adjoin (list "Registered Users" (group->shifted +all-registered-group+))
		    (union eg (union eu ms :key #'second) :key #'second)))))
					;				(recorded-memberships) :key #'car) :key #'car)
;		  :key #'car) :key #'car))

  
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun children/type (child-type oid)
  (pair 0 oid) (copy-pair)
  (with-directory-tree (tree)
    (let ((buffer (lookup tree))
	  (place nil))
      (tagbody :start
	 (when (or (not (valid)) (not (= oid (unpair))))
	   (and buffer (release buffer))
	   (return-from children/type (nreverse place)))
	 (let ((metadata (read-metadata)))
	   (if (= (metatype metadata) child-type);+view+)
	       (multiple-value-bind (p o) (unpair)
		 (declare (ignore p))
		 (multiple-value-bind (name nbuffer) (name@ (next buffer))
		   (multiple-value-bind (title nbuffer) (title@ nbuffer)
		     (multiple-value-bind (description nbuffer) (description@ nbuffer)
		       ;;(format *loop* "~%Read ~s ~s ~s" name title description)
		       (setf buffer nbuffer)
		       (push (list o name title description (owner metadata) (creator metadata)) place)))))
	       (setf buffer (skip (skip (skip (next buffer)))))))
	 (go :start)))))

(defun !!!dump-documents (page-oid)
  ;; Delete all the documents. THIS WILL AUTOMATICALLY delete the archive entry, thus resetting the document-oid count.
  ;; Its not terribly efficient... but this operation is rare anyway.
  (with-archives/writing page-oid
    (with-cartulary (cart)
      (loop until (zerop cart)
	    for buffer = (progn (oid 0) (copy-oid) (lookup cart))
	    for real-oid = (prog1 (unoid) (assert (valid)) (assert buffer) (release buffer))
	    do (progn (oid real-oid) (copy-oid) (setf cart (delete-rz cart)))))))
	     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; I reserve about 256 pages (by setting the genoid) and 16 users (by setting doc-genoid).
;; The reserve space allows me to patch new functionality into existing systems.


(defun quick-entry (root oid parent owner type &aux (now (2010now)))
  (setf owner (user->shifted owner))
  (make-entry root oid parent (make-metadata type now owner owner now owner)))

(defun make-top-page (title)
  (with-directory-tree (dir)
    (setf dir (quick-entry dir +top-page+ 0 +root+ +page+))
    (setf dir (name! dir "/")) ;; Not used in searches, setting it to / makes top-level error messages say @ "/".
    (setf dir (title! dir title))
    (setf dir (description! dir (format nil "This is the top page for ~a. It's a good place for links to top-level pages. For example 'Safety Division' or 'Calgary Office' might be broad enough to have their own page under the top page, while 'Calgary Meeting Minutes 2010' is very specific and should be put under Top -> Calgary Office -> Minutes -> Calgary Meeting Minutes 2010." title))))
  (with-page-tree (page) 
    (setf page (make-rz page +top-page+))
    (setf page (sexp! page '(("!Page" . "FormLis is a flexible, powerful, easy to use wiki and online database. It is an effective online collaboration platform, database, knowledge repository, and groupware tool. It facilitates project development, document management, and data tracking with tools that let users create webpages and online databases without programming; FormLis supports multiple authors, changing requirements and iterative development, and it's sophisticated reporting engine makes FormLis the perfect tool for enterprise data.   FormLis lets seperate offices & teams collaborate in a central, web-accessible, place. "))))))

(defun make-system-page () 
  (with-directory-tree (dir)
    (setf dir (quick-entry dir +system-page+ +top-page+ +root+ +page+))
    (setf dir (name! dir "System"))
    (setf dir (title! dir "System"))
    (setf dir (description! dir "The system page is is dynamically generated; users will never see this.")))
  (with-page-tree (page) 
    (setf page (make-rz page +system-page+))
    (setf page (sexp! page '(("!Page" . "System Page"))))))

(defun make-user-page ()
  (with-directory-tree (dir) 
    (setf dir (quick-entry dir +user-page+ +system-page+ +root+ +page+))
    (setf dir (name! dir "Users"))
    (setf dir (title! dir "Users"))
    (setf dir (description! dir "Users are the documents created on this page. Users are editing by changing their underlying document, and potential users register by filling out this form.  You should consult the manual before making changes to this page.")))
  (with-page-tree (page)
    (setf page (make-rz page +user-page+))
    (setf page (sexp! page '(("!Page" . "[label]Username[/label] [field \"Username\"][br]
[label]Password[/label] [field \"Password\"][br]
[label]Confirm[/label] [field \"Confirm\"][br]
[field \"Enabled\"]")
			      ("FUsername" . (#x08001000 "")) ;; Required text field, 16 chars wide
			      ("FPassword" . (#x00002000 "")) ;; Text field, 32 chars wide.
			      ("FConfirm"  . (#x00002000 ""))
			      ("FEnabled"  . (#x400B0100 "Enabled" "Enabled" ""))))))) ;; Enabled Checkbox.

(defun make-user-alphabetical-view ()
  (with-directory-tree (dir) 
    (setf dir (quick-entry dir +user-alphabetical-view+ +user-page+ +root+ +view+))
    (setf dir (name! dir "Alphabetical"))
    (setf dir (title! dir "Alphabetical"))
    (setf dir (description! dir "Registered users in alphabetical order.")))
  (with-content-tree (view)
    (setf view (make-rz view +user-alphabetical-view+))
    (setf view (sexp! view "COLUMN0 \"Username\" ; 
COLUMN1 \"Enabled\" ;
VALUE0 [/Username] ;
VALUE1 [/Enabled] ;

SORT0 ascending ;"))))

(defun make-group-page ()
  (with-directory-tree (dir) 
    (setf dir (quick-entry dir +group-page+ +system-page+ +root+ +page+))
    (setf dir (name! dir "Groups"))
    (setf dir (title! dir "Groups"))
    (setf dir (description! dir "Groups are the documents created on this page. You should consult the manual before making changes to this page.")))
  (with-page-tree (page)
    (setf page (make-rz page +group-page+))
    (setf page (sexp! page '(("!Page" . "[label \"Name\"]Group Name[/label] [field \"Name\"]")
			     ("FName" . (#x08001000 ""))))))) ;; Required text field, 16 chars wide

(defun make-group-alphabetical-view ()
  (with-directory-tree (dir) 
    (setf dir (quick-entry dir +group-alphabetical-view+ +group-page+ +root+ +view+))
    (setf dir (name! dir "Alphabetical"))
    (setf dir (title! dir "Alphabetical"))
    (setf dir (description! dir "Groups in alphabetical order.")))
  (with-content-tree (view)
    (setf view (make-rz view +group-alphabetical-view+))
    (setf view (sexp! view "COLUMN0 \"Name\" ; VALUE0 [/Name] ; SORT0 ascending ;"))))

(defun make-import-page ()
  (with-directory-tree (dir) 
    (setf dir (quick-entry dir +import-page+ +system-page+ +root+ +page+))
    (setf dir (name! dir "Import"))
    (setf dir (title! dir "Import Wizard"))
    (setf dir (description! dir "This wizard can build wiki pages from word files, or bulk-load data from CSV, database exports or excel files.")))
  (with-page-tree (page)
    (setf page (make-rz page +import-page+))
    (setf page (sexp! page '(("!Page" . "This content should have been replaced..."))))))

;; By convention any tree-element that is a keyword is NOT DISPLAYABLE in views.  This lets me to keep PASHHASH hidden.

(defun make-super-user (&aux (now (2010now)))
  (with-archives/writing +user-page+
    (with-cartulary (cartulary)
      (setf cartulary (make-rz cartulary +root+)
	    cartulary (rz! cartulary (pack-document
				      (db.metadata:make-metadata +document+ now +root+ +root+ now +root+)
				      '(:doc ("Username" . "root") (:passhash . "unloginable")))))
      )))

(defun set-user-reserve ()
  (with-archives/writing +user-page+
    (with-cartulary (cartulary)
      (setf (next-doid) 16))))
(defun set-page-reserve () (setf (next-oid) 256))

(defun set-default-security ()
  (with-acl-tree (acl)
    (setf acl (set-acl acl +top-page+ (list (list +role-designer+ 1)) nil))))
  
(defun make-client (name title caste)
  (assert *txn*)
  (assert (zerop (client-buffer)))
  (subscribe name title caste)
  (with-client name
    (make-top-page title)
    (make-system-page)
    (make-user-page)
    (make-user-alphabetical-view)
    (make-group-page)
    (make-group-alphabetical-view)
    (make-import-page)

    (make-super-user)

    (set-user-reserve)
    (set-page-reserve)
    (set-default-security)))

(defun initialize ()
  (db.io:!initialize)
  ;(db.threads:!threads.init)
  ;; Setup INFOs (I could maybe compile them to static lisp low level space.. would be better...
;  (setf *dir-info* (malloc 16))
;  (setf *page-info* (malloc 16))
;  (setf *view-info* (malloc 16))
;  (setf *diff-info* (malloc 16))
;  (setf *archives-info* (malloc 16))
;  (setf *cartulary-info* (malloc 16))
;  (setf *client-info* (malloc 16))
;  (setf *acl-info* (malloc 16))
;  (setf *membership-info* (malloc 16))
  
;  (db.bt:custom *dir-info* 4 11)
;  (db.bt:custom *page-info* 2 142) ;; 7 entries per page, around 81 words, max ~145,408 characters.
;  (db.bt:custom *view-info* 2 82) ;; 12 entries per page, around 46 words.
;  (db.bt:custom *diff-info* 3 32)  ;; 29 entries per page, around 18 words. 
;  (db.bt:custom *archives-info* 2 2)   ;; 254 entries per page (store both AVAIL-OID and ADDR)
;  (db.bt:custom *cartulary-info* 2 27) ;; Roughly the meta-data and 68 bytes of content.  35 entries per page.
;  (db.bt:custom *client-info* 4 74)
;  (db.bt:custom *acl-info* 2 8)  ;; 102 entries per page.
;  (db.bt:custom *membership-info* 2 2)  ;; 511 entries per page, no wasted space.
  )

;; Some printing routines... makes a nice table of client database offsets.
  
(defun client-vitals (name)
  ;(with-reading 
  (with-client name
    (list name (next-oid) (caste) (company) (directory) (page) (content) (diff) (archives))))

(defun client-offsets (&rest names) 
    (format *loop* "~%        Name  NextOid  Caste                          Company    0xDir   0xPage   0xView   0xDiff ~
0xArchiv")
  (format *loop* "~%------------ -------- ------ -------------------------------- -------- -------- -------- -------- ~
--------")
  (format *loop* "~:{~%~12@A ~8D ~6D ~32@A ~8X ~8X ~8X ~8X ~8X~}" 
	  (with-reading (loop for name in names collecting (client-vitals name)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; INTERFACE FOR READING/WRITING ENTRIES (and entry is a record in the DIR tree... the name isn't good).


;; 2011/04/22: This is a HACK because eventually PAGE data will be merged into the real content tree (with VIEW & CHART code)
;;             for now this routine masks the fact that it hasn't already happened. 
(defmacro with-hack/content-tree ((var content-tree) &body body)
  (let ((osym (gensym))
	(type (gensym)))
    `(let* ((,type ,content-tree)
	    (,osym (ncase ,type
		     (+page+ (page))
		     (+view+ (content))
		     (+chart+ (content))))
	    (,var ,osym))
       (prog1 (progn ,@body)
	 (unless (= ,osym ,var)
	   (ghost-client)
	   (ncase ,type
	     (+page+ (setf (page) ,var))
	     (+view+ (setf (content) ,var))
	     (+chart+ (setf (content) ,var))))))))
  
(defun named-user (name)
  "Given a user name returns its OID"
;  (with-reading 
;    (with-client (client)
  (open-archives +user-page+)
  (oid 0) (copy-oid)
  (do ((buffer (lookup (cartulary))))
      ((not (valid)) (and buffer (release buffer)) (close-archives) nil)
    (let ((oid (unoid)))
      (multiple-value-bind (meta content nbuffer) (unpack-document buffer)
	(declare (ignore meta))
	(setf buffer nbuffer)
	(when (string-equal name (util.tree:payload (util.tree:tree-find content '(("Username" . 0)))))
	  (and buffer (release buffer))
	  (close-archives)
	  (return-from named-user (values oid content)))))))

(defun entry-meta (buffer)
  (let ((meta (read-metadata)));rz (make-rz oid (metatype) (created) (creator) (owner) (updated) (updater))))
    (setf buffer (next buffer))
    (multiple-value-bind (name buffer) (name@ buffer)
      (multiple-value-bind (title buffer) (title@ buffer)
	(multiple-value-bind (description buffer) (description@ buffer)
	  ;(when buffer (release buffer))
	  (values meta name title description buffer))))))

(defun entry-content (root oid)
  (oid oid) (copy-oid)
  (multiple-value-bind (sexp buffer) (sexp@ (lookup root))
    (when buffer (release buffer))
    sexp))

(define-condition baddocument (error) ((doid :accessor doid :initarg :doid)))

(defun document-meta+content (doid)
  (with-cartulary (cart)
    (oid doid) (copy-oid)
    (let ((buffer (lookup cart)))
      (if (found)
	  (multiple-value-bind (meta content buffer) (unpack-document buffer)
	    (when buffer (release buffer))
	    (cons meta content))
	  (progn (and buffer (release buffer)) (error 'baddocument :doid doid))))))

(defun update-document (doid meta content)
  ;(with-archives/writing resource-oid
  (with-cartulary (cart) (oid doid) (setf cart (rz! cart (pack-document meta content)))))

;(defun write-meta (rz); buffer)
;  (setf (created) (rz-created rz) (creator) (rz-creator rz) (owner) (rz-owner rz)
;	(updated) (2010now) (updater) (rz-updater));(ash (session-var 'login) 1))

(defun create-document  (doid meta content)
  ;(with-archives/writing resource-oid
  ;(let ((doid (gen-doid)))
  (with-cartulary (cart)
    (setf cart (make-rz cart doid))
    (setf cart (rz! cart (pack-document meta content))))
  doid)

(flet ((quick-check (oid parent dir)
	 (pair oid parent) (copy-pair)
	 (let ((buffer (lookup dir)))
	   (if (found)
	       (progn (release buffer) (error "Couldn't created resource because the oid ~d is already in use..." oid))
	       (and buffer (release buffer))))))
  (defun create-entry-meta (parent oid meta name title description)
    ;(let ((oid (gen-oid)))
    (with-directory-tree (dir)
      (quick-check oid parent dir) ;; Make sure I don't double write something... thats really bad.
      (setf dir (make-entry dir oid parent meta))
      ;;dir oid parent resource-parent> (rz-owner resource>) (rz-creator resource>) (rz-metatype resource>)))
      (setf dir (name! dir name)); resource-name>))
      (setf dir (title! dir title));resource-title>))
      (setf dir (description! dir description)));resource-description>)))
    oid))

(defun update-entry-meta (buffer meta name title description)
  (assert (found))
  (setf buffer (ghost-cursor buffer))
;      (format *loop* "~%before update... meta: ~a, create: ~a ~a, own: ~a, update: ~a ~a"
;	      (metatype) (created) (creator) (owner) (updated) (updater))
  ;(let ((rz resource>))
  (assert (= (metatype meta) (metatype (read-metadata))));rz-metatype resource>)))
  (write-metadata meta)
  ;;(error "How do I write my meta?")
;  (setf (created) (rz-created rz) (creator) (rz-creator rz) (owner) (rz-owner resource>)
;	(updated) (2010now) (updater) (ash (session-var 'login) 1)))
  (release buffer)
  (with-directory-tree (dir)
    (setf dir (name! dir name));resource-name>))
    (setf dir (title! dir title));resource-title>))
    (setf dir (description! dir description))));resource-description>))))

(defun create-entry-content (root oid content)
  (setf root (make-rz root oid))
  (sexp! root content))
(defun update-entry-content (root oid content) (oid oid) (sexp! root content))

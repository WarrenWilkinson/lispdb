(defpackage :db.acl
  (:use :common-lisp :util.util :db.io :db.txn :db.threads :db.bt :db.cursor :db.metadata :db.resources)
  (:import-from :sb-kernel system-area-ub8-copy)
  (:import-from :sb-sys sap+ sap-ref-8 sap-ref-32 sap-ref-64 with-pinned-objects vector-sap)
  (:export *acl-info*
	   +role-administrator+ +role-designer+ +role-editor+
	   +role-user+ +role-reader+ +role-submitter+ +role-no-access+ acl! acl@ skip-null
	   +custom+ +administrator+ +designer+ +editor+ +user+ +submitter+ +reader+ +no-access+ role-number

	   +read+ +edit+ +create+ +delete+ +secure+ +reassign+ +export+	   
	   +permit+ +constraint-owner+ +constraint-creator+ +constraint-recent+ +constraint-mask+ 
	   role-bits

	   *membership-info* group->shifted user->shifted shifted->user shifted->group group-p user-p
	   membership@ groups members set-groups set-members superset
	   ancestors descendants
	   enroll expel))

(in-package :db.acl)

;(defvar *acl-info*)
(defbtree *acl-info* 2 8)


;; Security is stored in a btree keyed by resource OID. The payloads format is multiple tags of:

;;             [00|00]                                       {END OF TAGS}
;;             [01|ngroups] <custom-role> <group-ids>...     {used to assign groups to a new custom level}
;;OR           [02|ngroups] <group-ids>...                   {used to assign more groups to the custom level}
;;   [access-level|ngroups] <group-ids>...                   {used to assign groups to an access level}
;;
;; It always ends with that zero byte end of tags.  A security entry will have 2 sets, and the first one is the
;; inheritable permissions.

(defconstant +custom+ 1)
(defconstant +custom/continue+ 2)
(defconstant +administrator+ 3) ;; Access Levels
(defconstant +designer+ 4)
(defconstant +editor+ 5)
(defconstant +user+ 6)
(defconstant +submitter+ 7)
(defconstant +reader+ 8)
(defconstant +no-access+ 9)

;;A <role> is [64bit reassign-to] [4READ, 4EDIT], [4CREATE, 4DELETE], [4SECURE, 4REASSIGN], [EXPORT, 4unused]
;;The 4 bits are flags |POCR| that stand for permit, owner-condition, creator-condition and recent condition. 

(deftype role () '(simple-array (unsigned-byte 32) 5))
(defconstant +read+     0)
(defconstant +edit+     1)
(defconstant +create+   2)
(defconstant +delete+   3)
(defconstant +secure+   4)
(defconstant +reassign+ 5)
(defconstant +export+   6)

(defconstant +permit+             #b1000)
(defconstant +constraint-owner+   #b0100)
(defconstant +constraint-creator+ #b0010)
(defconstant +constraint-recent+  #b0001)
(defconstant +constraint-mask+    #b0111)

;; Here are the predefined roles,  8 = always, C = if own, d = own+recent, 0 = no:
;;                                                  Doc     Page    Resources
;;                                                X_SACDRE   X_SACDRE   X_SACDRE   
(defconstant +role-administrator+ (coerce #(0 0 #x80888888 #x80888888 #x80888888) '(vector (unsigned-byte 32) 5)))
(defconstant +role-designer+      (coerce #(0 0 #x80c88c88 #x80c88c88 #x80c88c88) '(vector (unsigned-byte 32) 5)))
(defconstant +role-editor+        (coerce #(0 0 #x80088c88 #x80088C88 #x8008cc88) '(vector (unsigned-byte 32) 5)))
(defconstant +role-user+          (coerce #(0 0 #x80008c88 #x80008c8c #x8000cc8c) '(vector (unsigned-byte 32) 5)))
(defconstant +role-submitter+     (coerce #(0 0 #x00008dcd #x00008dcd #x00008dcd) '(vector (unsigned-byte 32) 5)))
(defconstant +role-reader+        (coerce #(0 0 #x00000080 #x00000080 #x00000080) '(vector (unsigned-byte 32) 5)))
(defconstant +role-no-access+     (coerce #(0 0 #x00000000 #x00000000 #x00000000) '(vector (unsigned-byte 32) 5)))

(defun role-number (role) (aif (position role (list +role-administrator+ +role-designer+ +role-editor+ +role-user+
						    +role-submitter+ +role-reader+ +role-no-access+) :test #'eq)
			       (+ it 3) 1))

(defun role-assign-to (role) (boole boole-ior (ash (ldb (byte 24 0) (elt role 0)) 32) (elt role 1)))
(defun role-bits (role resource operation)
  (setf resource (min resource 2)) ;; All resources (charts/views share the same permission set).
  (assert (<= 0 resource 2))
  (assert (<= 0 operation 6))
  (format *loop* "~%ROLE-BITS: ~a ~a ~a" role resource operation)
  (with-pinned-objects (role)
    (let ((data (sap-ref-8 (vector-sap role) (+ 8 (truncate operation 2) (* resource 4))))); 8 (/ operation 2)))))
      ;;(format *loop* "~%data: ~b" data)
      (if (oddp operation)
	  (ldb (byte 4 0) data)
	  (ldb (byte 4 4) data)))))

;; I actually decode this while reading it from database.  This is probably dumb, and I should just rz@ it and decode it
;; in memory. Add this to my todo list.

(defun @+8 (buffer)
  (let ((value (sap-ref-8 (origin-data) 0)))
    ;; (format *loop* "~%(r: ~d): ~a ~a" value (origin-data) buffer)
    (setf (origin-data) (sap+ (origin-data) 1))
    (values value (if (past-end? buffer (origin-data)) (start-of-next buffer) buffer))))

(defun @+32 (buffer)
  (multiple-value-bind (lb buffer) (@+8 buffer)
    (multiple-value-bind (mb1 buffer) (@+8 buffer)
      (multiple-value-bind (mb2 buffer) (@+8 buffer)
	(multiple-value-bind (hb buffer) (@+8 buffer)
	  (values (boole boole-ior (boole boole-ior (boole boole-ior (ash hb 24) (ash mb2 16)) (ash mb1 8)) lb) buffer))))))

(defun @+64 (buffer)
  (multiple-value-bind (lb buffer) (@+32 buffer)
    (multiple-value-bind (hb buffer) (@+32 buffer)
      (values (boole boole-ior (ash hb 32) lb) buffer))))

(defun make-role () (make-array 5 :element-type '(unsigned-byte 32)))
(defun read-role (buffer) ;; Read the next 5 words.
  (let ((r (make-role)))
    (multiple-value-bind (data buffer) (@+32 buffer)
      (setf (elt r 0) data)
      (multiple-value-bind (data buffer) (@+32 buffer)
	(setf (elt r 1) data)
	(multiple-value-bind (data buffer) (@+32 buffer)
	  (setf (elt r 2) data)
	  (multiple-value-bind (data buffer) (@+32 buffer)
	    (setf (elt r 3) data)
	    (multiple-value-bind (data buffer) (@+32 buffer)
	      (setf (elt r 4) data)
	      (values r buffer))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun count+class (&aux (b (sap-ref-8 (origin-data) 0))) (values (ldb (byte 4 0) b) (ldb (byte 4 4) b)))

(defun decode-member (buffer old-member)
  (let ((b (sap-ref-8 (origin-data) 0)))
    (if (= b 255); zerop b)
	(multiple-value-bind (value buffer) (@+64 buffer) ;; Read a 64 bit guy OID.
	  (values (ash value -8) 8 buffer))
	(multiple-value-bind (b buffer) (@+8 buffer)
	  (values (+ old-member b) 1 buffer)))))

(defun decode-members (buffer count)
  "Decode a set number of members."
  (if (> count 0)
      (loop repeat count 
	    for (member bytes nbuffer) = (multiple-value-list (decode-member buffer 0)) then
	                                 (multiple-value-list (decode-member nbuffer member))
	    collecting member into members
	    finally (return (values members nbuffer)))
      (values nil buffer)))

(defun @+8-drop (buffer) 
  (multiple-value-bind (byte buffer) (@+8 buffer)
    (declare (ignore byte))
    buffer))
  
;; Consumes custom rules (but not  0+0 marker, which stops it.). 
(flet ((read-custom-rule (buffer role)
	 (multiple-value-bind (count class) (count+class);  buffer)
	   (ncase class 
	     (0 (values nil buffer))
	     (+custom+ (multiple-value-bind (role buffer) (read-role (@+8-drop buffer))
			 (multiple-value-bind (members buffer) (decode-members buffer count)
			   (values (cons role members) buffer))))
	     (+custom/continue+ (multiple-value-bind (members buffer) (decode-members (@+8-drop buffer) count)
				  (values (cons role members) buffer)))
	     (otherwise (values nil buffer))))))
  (defun fetch/custom (buffer)
    (loop with acc = nil
          for (role.groups nbuffer) = (multiple-value-list (read-custom-rule buffer  +role-no-access+))
                                 then (multiple-value-list (read-custom-rule nbuffer (caar acc)))
          while role.groups
	  do (setf buffer nbuffer)
          if (eq (caar acc) (car role.groups))
          do (rplacd (car acc) (nconc (cdr role.groups) (cdar acc)))
          else
          do (setf acc (cons role.groups acc))
	  finally (return (values acc buffer)))))

;; Will NOT consume custom rules.
(flet ((read-rule (buffer)
	 (multiple-value-bind (count class) (count+class)
	   (ncase class
	     (0               (values nil buffer))
	     (+administrator+ (multiple-value-bind (members buffer) (decode-members (@+8-drop buffer) count) 
				(values (cons +role-administrator+ members) buffer)))
	     (+designer+      (multiple-value-bind (members buffer) (decode-members (@+8-drop buffer) count) 
				(values (cons +role-designer+ members) buffer)))
	     (+editor+        (multiple-value-bind (members buffer) (decode-members (@+8-drop buffer) count)
				(values (cons +role-editor+ members) buffer)))
	     (+user+          (multiple-value-bind (members buffer) (decode-members (@+8-drop buffer) count)
				(values (cons +role-user+ members) buffer)))
	     (+submitter+     (multiple-value-bind (members buffer) (decode-members (@+8-drop buffer) count)
				(values (cons +role-submitter+ members) buffer)))
	     (+reader+        (multiple-value-bind (members buffer) (decode-members (@+8-drop buffer) count)
				(values (cons +role-reader+ members) buffer)))
	     (+no-access+     (multiple-value-bind (members buffer) (decode-members (@+8-drop buffer) count)
				(values (cons +role-no-access+ members) buffer)))))))
  (defun fetch (buffer)
    (loop with acc = nil
          for (role.groups nbuffer) = (multiple-value-list (read-rule buffer))
                                 then (multiple-value-list (read-rule buffer))
          while role.groups
	  do (setf buffer nbuffer)
          if (eq (caar acc) (car role.groups))
          do (rplacd (car acc) (nconc (cdr role.groups) (cdar acc)))
          else
          do (setf acc (cons role.groups acc))
          finally (return (values (nreverse acc) buffer)))))

(defun skip-null (buffer) (multiple-value-bind (val buffer) (@+8 buffer)
			    (assert (zerop val)) buffer))

;; A full set of permissions is CUSTOM INHERITED 0+0 CUSTOM NONINHERITED 0+0, 
;; so you would call this method twice to get both. (Remember, I also store they byte length first... so skip that).
(defun acl@ (buffer)
  (multiple-value-bind (custom buffer) (fetch/custom buffer)
    (multiple-value-bind (standard buffer) (fetch buffer)
      (values (nconc custom standard) buffer))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Even though I decode while reading from the database, I fully encode in memory and then write to the database.
;; Thats what this set of routines does.

(labels ((organize (permissions)
	   ;; Take the permissions, combine the same ones, remove empty ones and sort them all...
	   (sort 
	    (remove-if #.(lambda (item) (and (not (= (car item) +custom+)) (null (cddr item))))
	     (remove-duplicates 
	      (loop for rest on permissions 
		    collect (list* (role-number (caar rest)) (caar rest)
				   (sort(remove-duplicates
					 (apply #'append (mapcar #'cdr (remove (caar rest) rest :key #'car :test-not #'eq))))
					#'<))) :key #'second :test #'eq :from-end t)) #'< :key #'car))
	 (needed-headers (class.perm.members) (max 1 (ceiling (length (cddr class.perm.members)) 16)))
	 (needed-membership-bytes (ids);class.perm.members)
	   ;; At least 1 byte per entry. If more than 254 from previous, we need to fully encode it.
	   (+ (length ids) (* 7 (count 254 (mapcar #'- ids (cons 0 ids)) :test #'<))))
	 (needed-size (permissions)
	   (+ (* (count +custom+ permissions :key #'first) 20)      ;; Each custom set needs 20 bytes. 
	      (reduce #'+ permissions :key #'needed-headers) ;; Enough headers for each permission
	      (reduce #'+ permissions :key (compose #'needed-membership-bytes #'cddr)) ;; Enough bytes to store memberships
	      1)) ;; ZERO Byte terminator.
	 (write-role (sap role)
	   (with-pinned-objects (role)
	     (system-area-ub8-copy (vector-sap role) 0 sap 0 20))
	   (sap+ sap 20))
	 (write-members (sap members previous)
	   (cond ((null members) sap)
		 ((< (- (car members) previous) 255)
		  (setf (sap-ref-8 sap 0) (- (car members) previous))
		  (write-members (sap+ sap 1) (cdr members) (car members)))
		 (t (assert (= (ldb (byte 56 0) (car members)) (car members)))
		    (setf (sap-ref-64 sap 0) (boole boole-ior #xFF (ash (car members) 8)))
		    (write-members (sap+ sap 8) (cdr members) (car members)))))
	 (write-permissions (sap permissions)
	   (loop for (code role . members) in permissions
	         do (loop for submembers in (or (group members 15) '(nil))
			  do (setf (sap-ref-8 sap 0) (boole boole-ior (ash code 4) (length submembers)))
			  do (setf sap (sap+ sap 1))
		          when (= code +custom+)
		          do (setf sap (write-role sap role))
		          and do (setf code +custom/continue+)
		          do (setf sap (write-members sap submembers 0))))))
  (defun prepare-permissions (inherited non-inherited)
    (assert (or inherited non-inherited)) ;; You shouldn't write NULL permissions -- just delete the permission instead.

    ;; You shouldn't write a single permission to multiple groups.
    ;; Actually... it could be useful, but it makes the UI much harder, so I'm forbidding it.
    (let ((all-oids (append (mapcan (compose #'copy-list #'cdr) inherited)
			    (mapcan (compose #'copy-list #'cdr) non-inherited))))
      (assert (null (set-difference all-oids (remove-duplicates all-oids)))))
    

    ;;(when (or (some #.(lambda (set) (find 0 (cdr set))) inherited)
    ;;	      (some #.(lambda (set) (find 0 (cdr set))) non-inherited))
    ;;(error "Cannot store permissions for OID 0."))
    (setf inherited (organize inherited))
    (setf non-inherited (organize non-inherited))
    ;;(format *loop* "~%INH: ~a~%NINH: ~a" inherited non-inherited)
    (let* ((size-ih (needed-size inherited))
	   (vector (make-array (+ (needed-size non-inherited) size-ih) :element-type '(unsigned-byte 8))))
      (with-pinned-objects (vector)
	(write-permissions (vector-sap vector) inherited)
	(write-permissions (sap+ (vector-sap vector) size-ih) non-inherited))
      vector))
  (defun prepare-membership (members);groups)
    ;(assert (every #'oddp groups))
    (setf members (sort (copy-list members) #'<))
    (let* ((size (needed-membership-bytes members)) ;; 4 byte length + needed space.
	   (vector (make-array size :element-type '(unsigned-byte 8))))
      (with-pinned-objects (vector)
	(write-members (vector-sap vector) members 0))
      vector)))

(defun acl! (target inherited non-inherited) (rz! target (prepare-permissions inherited non-inherited)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BONUS!  GROUP MEMBERSHIP! (Just because they share decode-member... and this decode from disk stupidity)

;; Its my intention that you can use a group ANYWHERE you use a user.  So to fit them together I make the 1's bit 
;; a BOOL  GROUP (if on) or USER (if off).   Then the user/group oid is shifted up 1 bit. 

;; In my group tree, I associate a list with oids. 
;; If its a group, I assocate the list of direct members.
;; If it's a user, I associate a list of all groups and ancestors of those groups that include this user 
;; (and groups that I can reach multiple ways will be in this list multiple times -- reference counting!)

;(defvar *membership-info*)
(defbtree *membership-info* 2 2)

(defun user->shifted (user-oid) (ash user-oid 1))
(defun group->shifted (group-oid) (boole boole-ior (ash group-oid 1) 1))
(defun group-p (shifted-oid) (oddp shifted-oid))
(defun user-p (shifted-oid) (evenp shifted-oid))
(defun shifted->group (shifted) (assert (oddp shifted)) (ash shifted -1))
(defun shifted->user  (shifted) (assert (evenp shifted)) (ash shifted -1))

(defun decode-members* (buffer len)
  "Like decode-members, but works on a given length in bytes"
  (if (> len 0)
      (loop for (member bytes nbuffer) = (multiple-value-list (decode-member buffer 0)) then
	                                 (multiple-value-list (decode-member nbuffer member))
	    collecting member into members
	    do (decf len bytes)
	    while (> len 0)
	    finally (return (values nbuffer members)))
      (values nil buffer)))

(defun membership@ (buffer)
  (let ((length (sap-ref-32 (origin-data) 0)))
    (reposition 4)
    (decode-members* buffer length)))
;    (multiple-value-bind (nbuffer data) 
;      (and nbuffer (release nbuffer))
;      data)))
  

(flet ((fetch-membership (root)
	 (copy-oid)
	 (let ((buffer (lookup root)))
	   (prog1 (if (found)
		      (multiple-value-bind (nbuffer data) (membership@ buffer);decode-members* buffer length)
			;(let ((length (sap-ref-32 (origin-data) 0)))
			;(reposition 4)
			;(multiple-value-bind (nbuffer data) (decode-members* buffer length)
			(and nbuffer (release nbuffer))
			data)
		      (progn (and buffer (release buffer)) nil))))))
  (defun groups (root user-oid)
    "returns all the groups (in group form) this user is part of."
    (oid (user->shifted user-oid)) (fetch-membership root))
  (defun members (root group-oid)
    "returns all the groups and users (in shifted form) that are part of this group."
    (oid (group->shifted group-oid)) (fetch-membership root)))
(flet ((set-membership (root oid members)
	 (oid oid) (copy-oid)    
	 (let ((buffer (lookup root)))
	   (cond ((and members (found)) (and buffer (release buffer)))
		 (members (and buffer (release buffer)) (setf root (make-rz root oid)))
		 ((found) (and buffer (release buffer)) (return-from set-membership (delete-rz root)))
		 (t (and buffer (release buffer)) (return-from set-membership root))))
	 (rz-start)
	 (rz! root (prepare-membership members))))
  (defun set-groups (root user-oid group-oids) (set-membership root (user->shifted user-oid) group-oids))
  (defun set-members (root group-oid members)  (set-membership root (group->shifted group-oid) members)))

(defun superset (root group-oid)
  "Find all group-oids that contain this group."
  (oid 0) (copy-oid)
  (let ((buffer (lookup root))
	(supersets nil)
	(saved-oid))
    (setf group-oid (group->shifted group-oid))
    (tagbody :start
       (unless (valid) (and buffer (release buffer)) (return-from superset supersets))
       (setf saved-oid (unoid))
       (when (group-p saved-oid)
	 (acquire-buf buffer)
	 (let ((start (origin-data))
	       (length (sap-ref-32 (origin-data) 0)))
	   (reposition 4)
	   (multiple-value-bind (nbuffer data) (decode-members* buffer length)
	     (and nbuffer (release nbuffer))
	     (when (member group-oid data) (push (shifted->group saved-oid) supersets))
	     (setf (origin-data) start))))
       (setf buffer (skip buffer))
       (go :start))))

(defun descendants (root group-oid)
  "recursive members on a group/user. Descendants that occur multiply will be have multiple mentions in the output. shifted-oid will occur once in the output."
  (labels ((rec (circ-protect shifted-oid)
	     (aif (members root (shifted->group shifted-oid))
		  (progn (assert (null (intersection circ-protect it)))
			 (cons shifted-oid
			       (nconc (remove-if #'group-p it)
				      (mapcan (papply #'rec (cons shifted-oid circ-protect)) (remove-if #'user-p it)))))
		  (list shifted-oid))))
    (sort (rec nil (group->shifted group-oid)) #'<)))

(defun ancestors (root group-oid)
  "Recursively find all ancestors of this group. Ancestors that occur multiply will have multiple mentions in the output.
group-oid will occur once in the output."
  (labels ((rec (circ-protect group-oid)
	     (aif (superset root group-oid)
		  (progn (assert (null (intersection circ-protect it)))
			 (cons group-oid (mapcan (papply #'rec (cons group-oid circ-protect)) it)))
		  (list group-oid))))
    (sort (rec nil group-oid) #'<)))

(defun enroll (root group-oid shifted-oid)
  (let ((existing (members root group-oid)))
    (if (member shifted-oid existing)
	root
	(let ((ancestors (ancestors root group-oid))
	      (descendants (if (user-p shifted-oid) (list shifted-oid) (descendants root (shifted->group shifted-oid)))))
	  (when (intersection (mapcar #'group->shifted ancestors) descendants)
	    (error "Could not enroll ~d into group ~d. It would create a circle." shifted-oid group-oid))
	  (setf root (set-members root group-oid (cons shifted-oid existing)))
	  (dolist (user (mapcar #'shifted->user (remove-if #'group-p descendants)) root)
	    (setf root (set-groups root user (nconc (groups root user) ancestors))))))))

(defun remove-many (items list)
  (loop for i in items
        do (setf list (remove i list :count 1))
        finally (return list)))

(defun expel (root group-oid shifted-oid)
  (let ((existing (members root group-oid)))
    (if (member shifted-oid existing)
	(let ((ancestors (ancestors root group-oid))
	      (descendants (if (user-p shifted-oid) (list shifted-oid) (descendants root (shifted->group shifted-oid)))))
	  (assert (null (intersection (mapcar #'group->shifted ancestors) descendants)))
	  (setf root (set-members root group-oid (remove shifted-oid existing)))
	  (dolist (user (mapcar #'shifted->user (remove-if #'group-p descendants)) root)
	    (setf root (set-groups root user (remove-many ancestors (groups root user))))))
	root)))

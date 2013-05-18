(defpackage :db.bt
  (:use :common-lisp :util.util :db.io :db.txn :db.threads)
  (:shadow split)
  (:import-from :sb-sys sap-int int-sap sap= sap>= sap<= sap+ sap- sap-ref-8 sap-ref-16 sap-ref-32)
  (:export defbtree info origin-key origin-data down-data down-key

	   creating prepare insert btree lookup valid found rubout data key data-u8s key-u8s key-u32s
	   set-origin capacity
	   leaf node leaf-capacity leaf-keybytes leaf-u32s leaf-u8s
	   datum keys packed +header-bytes+ inner-p link info))

(in-package :db.bt)

;; B+tree nodes start with a 1bit inner/leaf+31bit count then a 4byte LINK pointer. This pointer is 0 if the node is last.
;;
;; These routines have INFO associated with them, sets of key-ub32s, capacity, key-bytes, data-ub32s each 16bits. The leaf
;; info follows node info, so I toggle between them easily by +/- 8. Additionaly, each thread has a space to store
;; pointers to origin key & data, and the insert data. And room to store a down-going key, and up-going key.

(defconstant +header-bytes+ 8)

(defun sized (sap key-u32s data-u32s)
  (let* ((capacity (truncate (- +page-bytes+ +header-bytes+) (ash (+ key-u32s data-u32s) 2)))
	 (keybytes (ash (* capacity key-u32s) 2)))
    (setf (sap-ref-16 sap 0) key-u32s
	  (sap-ref-16 sap 2) capacity
	  (sap-ref-16 sap 4) keybytes
	  (sap-ref-16 sap 6) data-u32s)))
(defmacro defbtree (name key-u32s data-u32s)
  `(def-coretime-allocated-var ,name 16 #'(lambda (space) (sized space ,key-u32s 1)
					     (sized (sap+ space 8) ,key-u32s ,data-u32s)
					     space)))
					  

(defthreadlocal btree-data 
  (info        pointer)  ;; PTR to info structure that has the size of the key & data.
  (origin-key  pointer)  ;; General 'hand' PTR used over and over by btrees, holds the final result.
  (origin-data pointer) 
  (down-data   pointer)  ;; PTR to data you want inserted using btree-create.
  (down-key    memory 152)
  (up-key      memory 152)
  (up-bp       memory   4))

(defun key-u32s  () (sap-ref-16 (info) 0))
(defun key-u8s   () (ash (key-u32s) 2))
(defun capacity  () (sap-ref-16 (info) 2))
(defun keybytes  () (sap-ref-16 (info) 4))
(defun data-u32s () (sap-ref-16 (info) 6))
(defun data-u8s  () (ash (data-u32s) 2))

(defun keys (buffer) (sap+ (sap buffer) +header-bytes+))
(defun datum (buffer) (sap+ (keys buffer) (keybytes)))

(defun leaf-capacity  () (sap-ref-16 (info) 10))
(defun leaf-keybytes  () (sap-ref-16 (info) 12))
(defun leaf-u32s () (sap-ref-16 (info) 14))
(defun leaf-u8s () (ash (leaf-u32s) 2))

(defun btree (sap) (setf (info) sap))
(defun node () (setf (info) (sap+ (info) -8))) ;; set info to point 8 bytes behind.
(defun leaf () (setf (info) (sap+ (info)  8))) ;; set info to point 8 bytes ahead.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun offsets (source index)
  (let* ((key (keys source))
	 (key-start (sap+ key (* (key-u8s) index)))
	 (data (sap+ key (keybytes)))
	 (data-start (sap+ data (* (data-u8s) index))))
    (values key-start data-start)))

(defun set-origin (source index)
  (multiple-value-bind (key data) (offsets source index)
    (setf (origin-data) data) 
    (setf (origin-key) key)))

(labels ((ircopy (d s u32s repeat)
	   (unless (zerop repeat)
	     (dotimes (i u32s) (setf s (sap+ s -4) d (sap+ d -4) (sap-ref-32 d 0) (sap-ref-32 s 0)))
	     (ircopy d s u32s (1- repeat)))))
  (defun rcopy (dest index count)
    "Copy from origin to dest+index count key+data segments. Copying is done backwords by u32 word."
    ;;(format *loop* "~%rcopy: ~a ~a ~a ~a" index count (key-u32s) (data-u32s))
    (multiple-value-bind (k d) (offsets dest (+ index count))
      (ircopy d (sap+ (origin-data) (* (data-u8s) count)) (data-u32s) count)
      (ircopy k (sap+ (origin-key)  (* (key-u8s) count)) (key-u32s) count))))

(labels ((icopy (d s u32s repeat)
	   (unless (zerop repeat)
	     (dotimes (i u32s) (setf (sap-ref-32 d 0) (sap-ref-32 s 0) s (sap+ s 4) d (sap+ d 4)))
	     (icopy d s u32s (1- repeat)))))
  (defun copy (dest index count)
    (multiple-value-bind (k d) (offsets dest index)
      (icopy d (origin-data) (data-u32s) count)
      (icopy k (origin-key)  (key-u32s) count)))
  (defun write-downkey  () (icopy (origin-key)  (down-key) (key-u32s) 1))
  (defun write-downdata () (icopy (origin-data) (down-data) (data-u32s) 1))
  (defun write-upkey  ()   (icopy (origin-key)  (up-key) (key-u32s) 1))
  (defun write-bp     (b)  (setf (sap-ref-32 (origin-data) 0) b))
  (defun load-key     ()   (icopy (up-key) (origin-key) (key-u32s) 1))
  (defun swap-key     ()   (icopy (down-key) (up-key) (key-u32s) 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun inner (buffer) (boole boole-and #x80000000 (sap-ref-32 (sap buffer) 0)))
(defun inner-p (buffer) (not (zerop (inner buffer))))
(defun packed  (buffer) (boole boole-and #x7FFFFFFF (sap-ref-32 (sap buffer) 0)))
(defun set-packed (buffer n) (setf (sap-ref-32 (sap buffer) 0) (boole boole-ior (inner buffer) n)))
(defsetf packed set-packed)
(defun link     (buffer)    (sap-ref-32 (sap buffer) 4))
(defun set-link (buffer nv) (setf (sap-ref-32 (sap buffer) 4) nv))
(defsetf link set-link)
(defun prepare (buffer) (setf (sap-ref-32 (sap buffer) 0) 0 (sap-ref-32 (sap buffer) 4) 0))
(defun creating (target) (if (zerop target)
			     (let* ((space (allocate 1)) (buffer (ghost (acquire space))))
			       (prepare buffer) (release buffer) space)
			     target))

(defun key<= (a b) (dotimes (i (key-u32s) t)
		    (cond ((< (sap-ref-32 a (ash i 2)) (sap-ref-32 b (ash i 2))) (return-from key<= t))
			  ((> (sap-ref-32 a (ash i 2)) (sap-ref-32 b (ash i 2))) (return-from key<= nil)))))

(labels ((iscan<= (start end) ;; NUMBER keysap startsap endsap
	 (if (sap>= start end) end (if (key<= (down-key) start) start (iscan<= (sap+ start (key-u8s)) end)))))
  (defun scan<= (start length) (sap- (iscan<= start (sap+ start length)) start)))
(defun hunt<= (buffer length) (/ (scan<= (keys buffer) (* (key-u8s) length)) (key-u8s)))
(defun leaf<= (buffer) (hunt<= buffer (packed buffer)))
(defun node<= (buffer) (hunt<= buffer (1- (packed buffer))))

(defun embed (buffer idx)
  (set-origin buffer idx)
  (unless (and (>= (capacity) (packed buffer) idx 0)
	       (> (capacity) idx))
    (format *loop* "~%~a:Embed fucked! ~a ~a ~a ~a ~a"
	    (sb-thread:thread-name sb-thread:*current-thread*)
	    (sb-sys:sap-int (info))
	    (sector buffer)
	    (capacity) (packed buffer) idx)
    (error "~a:EMBED FUCKED ~a ~a ~a ~a ~a"
	   (sb-thread:thread-name sb-thread:*current-thread*)
	   (sb-sys:sap-int (info)) (sector buffer) (capacity) (packed buffer) idx))
  (rcopy buffer (1+ idx) (- (packed buffer) idx))
  (write-downkey)
  (write-downdata)
  (incf (packed buffer))
  buffer)

(defun expunge (buffer idx)
  (when (= idx (packed buffer))
    (contents *loop* buffer)
    (format *loop* "Can't delete this!")
    (error "Can't delete this!"))

  (set-origin buffer (1+ idx))
  (copy buffer idx (- (packed buffer) idx 1))
  (decf (packed buffer))
  buffer)

(defun l->r (lb rb count)
  (set-origin rb 0)
  (rcopy rb count (packed rb))
  (incf (packed rb) count)
  (set-origin lb (- (packed lb) count))
  (copy rb 0 count)
  (decf (packed lb) count))

(defun r->l (lb rb count)
  (set-origin rb 0)
  (copy lb (packed lb) count)
  (incf (packed lb) count)
  (set-origin rb count)
  (decf (packed rb) count)
  (copy rb 0 (packed rb)))

(flet ((move (buffer obuffer count) (if (> count 0) (r->l buffer obuffer count) (l->r buffer obuffer (- 0 count))))
       (rfill^ (buffer obuffer) (ash (+ 1 (packed buffer) (packed obuffer)) -1))
       (rfill_ (buffer obuffer) (ash (+ (packed buffer) (packed obuffer)) -1)))
  (defun _spill^ (buffer obuffer) (move buffer obuffer (- (packed obuffer) (rfill^ buffer obuffer))))
  (defun ^spill_ (buffer obuffer) (move buffer obuffer (- (packed obuffer) (rfill_ buffer obuffer)))))

(defun load-big-key (buffer) (set-origin buffer (1- (packed buffer))) (load-key))

(flet ((partner (buffer)
	 "Create another buffer, it->link = buffer->link, then buffer->link = it."
	 (let ((other (allocate 1)))
	   (assert (not (= (sector buffer) other)))
	   (let ((ob (ghost (acquire other))))
	     (setf (sb-sys:sap-ref-64 (sap ob) 0) (sb-sys:sap-ref-64 (sap buffer) 0)) ;; Copy inner+count, link
	     (setf (packed ob) 0)
	     (setf (link buffer) other)
	     ob))))
  (flet ((divide (buffer) (let ((o (partner buffer))) (l->r buffer o (ash (capacity) -1)) o)))
    (defun split (buffer idx)
      (let ((other (divide buffer))
	    (half (ash (1+ (capacity)) -1)))
	(if (>= idx half)
	    (embed other (- idx half))
	    (embed buffer idx))
	(load-big-key buffer)
	(prog1 (sector other) (release other))))))

(flet ((full? (buffer) (= (packed buffer) (capacity)))
       (1-full? (buffer) (= (packed buffer) (1- (capacity)))))
  (defun ins (buffer idx)
    (if (full? buffer)
	(let ((other (link buffer)))
	  (if (zerop other)
	      (split buffer idx)
	      (let ((ob (acquire other)))
		(if (or (full? ob) (and (1-full? ob) (= idx (capacity))))
		    (progn (release ob) (split buffer idx))
		    (progn
		      (setf ob (ghost ob))
		      (_spill^ buffer ob)
		      (if (> idx (packed buffer))
			  (embed ob (- idx (packed buffer)))
			  (embed buffer idx))
		      (load-big-key buffer) (release ob) 1)))))
	(progn (embed buffer idx) 0))))

(defun change-payload (bp)
  "Down-data now points to up-bp, which is set to the block pointer. Up-key overwrites down-key."
  (setf (sap-ref-32 (up-bp) 0) bp)
  (setf (down-data) (up-bp))
  (swap-key))

(defun block-of (buffer idx) (sap-ref-32 (datum buffer) (* (data-u8s) idx)))

(flet ((do-leaf (buffer) (leaf) (setf buffer (ghost buffer))
		(let ((idx (leaf<= buffer)))
		  (set-origin buffer idx)
		  (prog1 (ins buffer idx) (release buffer) (node))))
       (do-node (buffer)
	 (let ((idx (node<= buffer)))
	   (if (= (block-of buffer idx) (sector buffer)) (error "Circle in tree!"))
	     (let* ((upcoming (block-of buffer idx))
		    (code (insert-r (acquire upcoming))))
	       (when (= code (sector buffer)) (error "I'm about to write a circle!"))
	       (case code
		 (0 (release buffer) 0)
		 (1 (setf buffer (ghost buffer)) (set-origin buffer idx) (write-upkey) (release buffer); (bode (list buffer))
		    (if (= (1+ idx) (packed buffer)) 1 0))  ;; Spill
		 (otherwise (setf buffer (ghost buffer))
			    (set-origin buffer idx) (write-bp code)  ;; SPLIT!
			    (change-payload upcoming) (prog1 (ins buffer idx) (release buffer))))))))
  (defun insert-r (buffer) (if (inner-p buffer) (do-node buffer) (do-leaf buffer))))

(flet ((new-root (root code)
	 (format *loop* "~%New Root ~d ~d" root code)
	 (let* ((newroot (allocate 1))
		(nb (ghost (acquire newroot))))
	   (prepare nb)
	   (setf (sap-ref-32 (sap nb) 0) #x80000001) ;; Inner with count 1
	   (set-origin nb 0) (write-bp code) (change-payload root)
	   ;; (format *loop* "~%HALF key is: ~a" (sap-ref-32 (down-key) 0))
	   (embed nb 0)
	   (release nb) ;(bode (list nb))
	   newroot)))
  (defun insert (root)
    (assert (> root 15))
    (let ((code (insert-r (acquire root)))) ;; Code 1 can never happen here.
      ;;(format *loop* "(c: ~d)" code)
      (if (zerop code) root (if (= 1 code) root (new-root root code))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Lookup returns the key or the next higher one (or sap0 if no higher one). You can use (found) to ensure an exact match,
;; or valid to ensure it was some kindo of key (valid not written). 
;; Lookup does not change the value of downkey (and I don't think it changes downdata either...)

(defun valid () (not (zerop (sap-int (origin-key)))))
(defun found () (and (valid) (key<= (origin-key) (down-key))))
(defun lookup-r (buffer)
  (if (inner-p buffer)
      (let* ((idx (node<= buffer))
	     (nbuffer (acquire (block-of buffer idx))))
	(when (= (block-of buffer idx) (sector buffer)) (error "Circle in tree!"))
	(release buffer)
	(lookup-r nbuffer))
      (progn (leaf)
	     (let ((pos (leaf<= buffer)))
	       (if (< pos (packed buffer)) (set-origin buffer pos) (setf (origin-data) (int-sap 0) (origin-key) (int-sap 0)))
	       (node) buffer))))
(defun lookup (target) 
  (if (zerop target) (progn (setf (origin-key) (int-sap 0)) nil)
      (progn (assert (> target 15)) (lookup-r (acquire target)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mergeR (buffer ob) (l->r buffer ob (packed buffer)) (deallocate (sector buffer) 1))

(flet ((<=half? (buffer) (<= (packed buffer) (ash (capacity) -1)))
       (bypass (other prev-sector)
	 (unless (zerop prev-sector)
	   (let ((pbuffer (ghost (acquire prev-sector))))
	     (load-big-key pbuffer)
	     (setf (link pbuffer) other)
	     (release pbuffer)))))
  (defun del (prev-sector buffer idx)
    (let ((other (link buffer)))
      (if (or (not (<=half? buffer)) (zerop other))
	  (case (packed buffer)
	    (0 (release buffer) (error "Attempt to delete from empty buffer."))
	    (1 (deallocate (sector buffer) 1) (release buffer) (bypass other prev-sector) 2)
	    (otherwise (expunge buffer idx) (load-big-key buffer) (if (= idx (packed buffer)) 1 0)))
	  (let ((ob (ghost (acquire other))))
	    (if (<=half? ob)
		(progn (expunge buffer idx) (mergeR buffer ob) (bypass other prev-sector) (release ob) 2)
		(progn (^spill_ buffer ob) (expunge buffer idx) (load-big-key buffer) (release ob) 1))))))) ;; Spill

(defun delete-r (prev-sector buffer)
  (if (inner-p buffer)
      (let* ((idx (node<= buffer))
	     (upcoming (block-of buffer idx))
	     (prev (if (zerop idx)
		       (if (zerop prev-sector)
			   0
			   (let ((other (acquire prev-sector)))
			     (prog1 (block-of other (1- (packed other))) (release other))))
		       (block-of buffer (1- idx))))
	     (code (delete-r prev (acquire upcoming))))
	(ecase code 
	  (0 (release buffer) 0)
	  (1 (setf buffer (ghost buffer)) (set-origin buffer idx)(write-upkey)
	     (release buffer)(if (= (1+ idx) (packed buffer)) 1 0))
	  (2 (setf buffer (ghost buffer)) (del prev-sector buffer idx)))) ;; MERGE
      (progn
	(setf buffer (ghost buffer))
	(leaf)
	(set-origin buffer (leaf<= buffer))
	(or (found) (error "~a@~a:I wanted to delete ~x ~x ~x ~x but I'm deleting ~x ~x ~x ~x (prev ~a)"
			   (sb-thread:thread-name sb-thread:*current-thread*)
			   (bnum buffer)
			   (sap-ref-32 (down-key) 0) (sap-ref-32 (down-key) 4)
			   (sap-ref-32 (down-key) 8) (sap-ref-32 (down-key) 12)
			   (sap-ref-32 (origin-key) 0) (sap-ref-32 (origin-key) 4)
			   (sap-ref-32 (origin-key) 8) (sap-ref-32 (origin-key) 12) prev-sector))
	(prog1 (del prev-sector buffer (leaf<= buffer)) (release buffer) (node)))))
	     
(defun rubout (target) 
  "Delete a key from the btree."
  (assert (> target 15))
  (let ((code (delete-r 0 (acquire target))))
    ;;(format *loop* "(rc:: ~a)" code)
    (if (or (= code 0) (= code 1))
	(let ((buffer (acquire target)))
	  (if (inner-p buffer)
	      (if (= 1 (packed buffer))
		  (prog1 (if (inner-p buffer) (block-of buffer 0) 0)
		    (format *loop* "~%Losing the root: ~d (inner: ~a, remaining is: ~a)"
			    (packed buffer) (inner-p buffer) (block-of buffer 0))
		    (deallocate target 1)
		    (release buffer))
		  (progn (release buffer) target))
	      (if (= 0 (packed buffer))
		  0 (progn (release buffer) target))))
	0)))

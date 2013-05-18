(defpackage :db.txn
  (:use :common-lisp :util.util :db.io)
  (:import-from :sb-ext compare-and-swap)
  (:import-from :sb-thread thread-yield make-mutex with-mutex)
  (:import-from :sb-kernel system-area-ub32-fill)
  (:import-from :sb-sys sap= sap+ sap-ref-32 sap-ref-64)
  (:export checkpoint *free* claim vacate 
	   reset *cursor* 
	   *txn* *txn-lock* *txns* txn state dirty-p
	   prospect allocate deallocate locals
	   begin acquire release ghost withdraw reopen end        
	   *access-mode* tourist endtourist update endupdate tourists
	   replay with-writing
	   with-reading ))
	   
(in-package :db.txn)

;; Transactions are either read-only (*txn* = nil) or read-and-write (*txn* = txn structure). Tourists call 
;; (tourist) and (endtourist) before they start/end to ensure reader/writer isolation.

;; Writers (ghost) pages before updating them. This will ensure the isolate the block if they haven't already done so.
;; The writers changes are merged when they commit unless an earlier transaction has modified a block we read or wrote. 
;; In this case commit will signal 'aborted' and the transaction must be restarted.
;;
;; Database allocations (using 'allocate' and 'deallocate') are for updators only. Allocations are performed immediatly
;; in a critical zone. De-allocations are delayed until the transaction successfully commits. This behavior ensures 
;; safe operation, even in worst cases like a transaction freeing space then aborting or two transactions freeing
;; the same space.
;;
;; Journal record headers are a 4byte count of allocations and a 4byte count of updated sectors. This is followed
;; by allocation records (16 bytes each) and then sector addresses. The first page of the header is written twice. If
;; there is more data than can be fit into a single header page, the additional data spills out into following pages.
;; 1 block per sector follows the headers and (space permitting) a final 0page. The journal stops when a record with 
;; count 0 is encountered, or an invalid header (one not written twice) is found. 
;;
;; The first journal record is 'special', it preserves the freespace list by listing a claim on all outstanding allocations.
;; Replaying this record will set the freespace list to the value it had upon shut down.
;;
;; Upon committing, journal records are written for all ended toplevel (i.e. not an ended txn that follows an unended one)
;; If there is no journal space then the disk&dirtybuffers are flushed and the journal restarts. Once the journal entries
;; are written, commit spinlocks until all the readers leave and then marks the updated sectors as disk&dirty while dumping
;; old versions. Finally the commited TXNs are removed from the txn list and tourists allowed back in.

(defvar *unburden* nil) ;; A list of pages to vacate ONLY when there are no more TXNs. 
(defvar *free* (list nil (cons +user-start+ #xFFFFFFFF)))

(flet ((nearby (position)
	 (labels ((inner (prev list position)
		    (if (< position (cdar list))
			prev
			(inner list (cdr list) position))))
	   (inner *free* (cdr *free*) position))))
  (defun claim (start length &aux (end (+ start length)))
    (assert (> #x80000000 length 0))
    (let* ((before (nearby start))
	   (during (cadr before)))
      (unless (or (null during) (and (>= start (car during)) (<= end (cdr during))))
	(format *loop* "~%(or (null ~a) (and (>= ~d ~d) (<= ~d ~d)))" during start (car during) end (cdr during))
	(format *loop* "~%Start was ~d Length was: ~d" start length)
	(format *loop* "~%FREE was: ~a" *free*)
	(error "ASSERT ERROR! (or (null during) (and (>= start (car during)) (<= end (cdr during)))"))
      (assert (or (null during) (and (>= start (car during)) (<= end (cdr during)))))
		   
      (case (boole boole-ior (if (= start (car during)) 2 0) (if (= end (cdr during)) 1 0))
	(0 (rplacd (cdr before) (acons end (cdadr before) (cddr before)))
	   (setf (cdadr before) start))
	(1 (setf (cdr during) start))
	(2 (setf (car during) end))
	(3 (rplacd before (cddr before))))))
  
  (defun vacate (start length &aux (end (+ start length)))
    (assert (> #x80000000 length 0))
    (let* ((before (nearby start))
	   (after (cadr before)))
      (when (car before) (assert (<= (cdar before) start)))
      (unless (and (< start (car after)) (<= end (cdr after)))
	(format *loop* "~%Vacate ~d ~d, but free is ~a" start length *free*)
	(error "ASSERT ERROR (< start (car after))"))
      (assert (<= end (cdr after)))
      (assert (< start (car after)))
      (case (boole boole-ior (if (and (car before) (= start (cdar before))) 2 0) (if (= end (car after)) 1 0))
	(0 (rplacd before (acons start end (cdr before))))
	(1 (setf (car after) start))
	(2 (setf (cdar before) end))
	(3 (setf (cdar before) (cdr after)) (rplacd before (cddr before)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defconstant +restarted+ 0) ;; STATES
(defconstant +open+ 1)
(defconstant +closed+ 2)
(defconstant +committed+ 3)
(defconstant +withdrawn+ 4)

(defstruct (txn (:conc-name nil) (:copier nil))
  (state 1 :type (integer 0 4))  ;; 0 = restarted, 1 = open, 2 = closed, 3 = commited, 4 = withdrawn
  (observed nil)
  (allocs nil)  ;; 64 bit (32bit addr, 1bit claim/vacate, 31bit length) (maybe I should make claim-vacate the lowest bit?)
  (shadows nil)
  (poison nil)
  (locals nil))

(define-condition reset (error) ())

(defvar *txn* nil)
(defvar *txn-lock* (make-mutex :name "txn-lock"))
(defvar *txns* nil)

(defun prospect (nblocks) (car (find nblocks (cdr *free*) :key #'(lambda (range) (- (cdr range) (car range))) :test #'<=)))

(defun allocate (num-blocks)
  (assert (> #x80000000 num-blocks 0))
  (with-mutex (*txn-lock*)
    (let ((start (or (prospect num-blocks) (error "No more space on device for allocation!"))))
      (push (boole boole-ior (ash start 32) num-blocks) (allocs *txn*))
      (claim start num-blocks)
      start)))

(defun deallocate (start num-blocks)
  (assert (> #x80000000 num-blocks 0))
  (with-mutex (*txn-lock*) (push (boole boole-ior (ash start 32) (boole boole-ior #x80000000 num-blocks)) (allocs *txn*))))

(defun begin ()
  "Create a new transaction"
  (with-mutex (*txn-lock*)
    (setf *txn* (car (if (null *txns*)
			 (setf *txns* (list (make-txn)))
			 (cdr (rplacd (last *txns*) (list (make-txn)))))))))

;; POISON elements must be acquired and released. Otherwise I could acquire something, and THEN have it
;; added to my poison list, thus interferring with the release.
(defun acquire (sector)
  "Get a readable page for this sector, and lock it so the system won't reclaim it while you use it."
  (if *txn*
      (with-mutex (*txn-lock*)
	(progn (setf (observed *txn*) (adjoin sector (observed *txn*)))
	       (aif (find sector (locals *txn*) :key #'sector)
		    it
		    (aif (find sector (poison *txn*) :key #'sector)
			 (progn (assert (not (zerop (watchers it))))
				(setf (state *txn*) +restarted+) (acquire-buf it) it)
			 (cache sector))))) ;; Test with allocated-p if you think someones using sectors irresponsibly.
      (cache sector)))

(defun release (buffer)
  "Release your lock on the buffer, allowing the system to reclaim it."
  (if *txn*
      (with-mutex (*txn-lock*)
	(or (find buffer (locals *txn*))
	    (release-buf buffer)))
      (release-buf buffer)))

(defun ghost (buffer)
  "Get a writable copy of buffer."
  (assert (not (zerop (watchers buffer))))
  (with-mutex (*txn-lock*)
    (if (find buffer (locals *txn*)) 
	buffer
	(progn (setf (shadows *txn*) (cons buffer (shadows *txn*)))
	       (car (setf (locals *txn*) (cons (isolate buffer) (locals *txn*))))))))

(defun extricate (txn)
  (dolist (target (shadows txn)) (release-buf target))
  (dolist (target (poison txn))  (release-buf target))
  (dolist (target (locals txn))  (release-buf target))
  (setf (observed txn) nil (shadows txn) nil (poison txn) nil (locals txn) nil))

(flet ((undo () ;; Undo allocations
	 (dolist (alloc (nreverse (allocs *txn*)) (setf (allocs *txn*) nil))
	   (let ((addr (ldb (byte 32 32) alloc))
		 (len (ldb (byte 32 0) alloc)))
	     (when (zerop (boole boole-and len #x80000000))
	       (vacate addr len))))))
  (defun withdraw ()
    "Cancel a non-committed transaction, losing all of its updates."
    (with-mutex (*txn-lock*)
      (setf (state *txn*) +withdrawn+)
      (setf *txns* (delete *txn* *txns*))
      (extricate *txn*) (undo)))
 
  (defun reopen ()
    "Put a reset transaction back into a usable state."
    (assert (= (state *txn*) 0))
    (with-mutex (*txn-lock*)
      (setf (state *txn*) +open+)
      (extricate *txn*) (undo))))

(defun restart? () (= (state *txn*) +restarted+))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *access-mode* (the fixnum 0)) ;; Start in update mode (to replay any journal).
(defvar *cursor* +journal-start+)     ;; Pointer to where we are in the journal 
(defun tourists () (ash *access-mode* -1))
(defun tourist ()
  (assert (null *txn*))
  (do* ((current (boole boole-ior *access-mode* 1) (boole boole-ior *access-mode* 1))
	(new (+ current 2) (+ current 2)))
       ((= current (compare-and-swap (symbol-value '*access-mode*) current new)) t))
  (assert (not (zerop *access-mode*))) t)
(defun endtourist () 
  (do* ((current *access-mode* *access-mode*)
	(new (- current 2) (- current 2)))
       ((= current (compare-and-swap (symbol-value '*access-mode*) current new)) t)))
(defun update () ;; NOTE! when this returns, you hold *txn-lock*.
  (assert (not (zerop *access-mode*)))
  (do* ((current *access-mode* *access-mode*)
	(new (boole boole-and current #xFFFFFFFE) (boole boole-and current #xFFFFFFFE)))
       (nil)
    (sb-sys:without-interrupts
      (sb-sys:allow-with-interrupts (sb-thread:get-mutex *txn-lock* nil t))
      (if (= current (compare-and-swap (symbol-value '*access-mode*) current new))
	  (return-from update)
	  (sb-thread:release-mutex *txn-lock*)))))

(defun endupdate () (assert (zerop *access-mode*)) (setf *access-mode* 1))
(defun wait-on-readers () (loop until (zerop *access-mode*) do (sb-thread:thread-yield))) ; 0 0))))

(defvar *journal-fn*)
(defun journal0 ()
  (assert (= *cursor* +journal-start+))
  (funcall *journal-fn* nil (mapcar #'(lambda (s e) (boole boole-ior (ash s 32) (- e s)))
				    (cons +user-start+ (mapcar #'cdr (butlast (cdr *free*))))
				    (mapcar #'car (cdr *free*)))))

(defun checkpoint ()
  (sb-thread:with-recursive-lock (*txn-lock*) ;; If you call update to lock out visitors, you are left holding this.
    (flush)
    (setf *cursor* +journal-start+)
    (journal0)
    (sb-posix:sync)))

(labels ((commit-pages (pages count position)
	   (unless (zerop count)
	     (page-out position (car pages))
	     (commit-pages (cdr pages) (1- count) (1+ position))))
	 (extraspace?-putblank (end)
	   (when (< end +journal-end+)
	     (system-area-ub32-fill 0 (sap sysbuffer) 0 +page-words+)
	     (page-out end sysbuffer))))
  (defun journal (locals allocs)
    (let* ((nallocs (length allocs))
	   (nlocals (length locals))
	   (n32s (+ 1 1 (ash nallocs 1) nlocals))
	   (nheaders (ceiling n32s 1024)) 	   ;; Headers store store #allocs #sectors allocs... sectors... 
	   (sectors (+ 1 nheaders nlocals)))
      (unless (and (zerop nallocs) (zerop nlocals))
	(let ((end (+ *cursor* sectors)))
	  (assert (not (and (= *cursor* +journal-start+) (> end +journal-end+))))
	  (when (> end +journal-end+) (checkpoint)) ;; If no room, checkpointing will write buffers, and reset txn log.
	  (extraspace?-putblank end))
	  
	(setf (sector resbuffer) (+ 2 *cursor*))
	(let ((sap (sap sysbuffer))   	;; The following would factor SO MUCH better in forth...
	      (end (sap+ (sap resbuffer) +page-bytes+)))
	  ;; Write the COUNTS
	  (setf (sap-ref-32 sap 0) nallocs sap (sap+ sap 4)
		(sap-ref-32 sap 0) nlocals sap (sap+ sap 4))
	  
	  ;; Write the allocs... 
	  (dolist (a (reverse allocs));(allocs txn)))
	    (setf (sap-ref-64 sap 0) a sap (sap+ sap 8))
	    (when (sap= sap end)
	      (page-out (sector resbuffer) resbuffer)
	      (incf (sector resbuffer))
	      (setf sap (sap resbuffer))))

	  ;; Write the locals
	  (dolist (l locals);(locals txn))
	    (setf (sap-ref-32 sap 0) (sector l) sap (sap+ sap 4))
	    (when (sap= sap end)
	      (page-out (sector resbuffer) resbuffer)
	      (incf (sector resbuffer))
	      (setf sap (sap resbuffer))))

	  ;; Write the pages
	  (commit-pages locals nlocals (+ 1 nheaders *cursor*))

	  ;; Write the first header page
	  (page-out (1+ *cursor*) sysbuffer)
	  (page-out *cursor* sysbuffer)
	  (incf *cursor* sectors))))))

(setf *journal-fn* #'journal)

(defun inscribe (txn) (journal (locals txn) (allocs txn)))

;; I used to vacate immediately rather than *unburden*.  But then TXN A could free sector B and commit,
;; while TXN B still used sector B and then allocated a new sector (which could then be B).  This could fuck up
;; the algorithms badly.   Now I save all deallocations until there are NO outstanding txns.
(defun unburden (txn) 
  ;; Mark the freed pages for later  vacating
  (dolist (alloc (allocs txn) (setf (allocs txn) nil))
    (let ((addr (ldb (byte 32 32) alloc))
	  (len (ldb (byte 32 0) alloc)))
      (unless (zerop (boole boole-and len #x80000000))
	;;(format *loop* "~%COMMIT VACATING ~a ~a" addr (boole boole-and #x7FFFFFFF len))
	(push (cons addr (boole boole-and #x7FFFFFFF len)) *unburden*)))))

(defun perform-vacates () (dolist (unburden *unburden* (setf *unburden* nil)) (vacate (car unburden) (cdr unburden))))

(defun intersect? (list1 list2 &key (test #'eql)) (not (null (some #'(lambda (i) (member i list2 :test test)) list1))))
(defun bode (shadows)
  (dolist (txn *txns*)
    (when (and (not (zerop (state txn))) (intersect? (mapcar #'sector shadows) (observed txn)))
      (setf (state txn) 0))
    (dolist (s shadows)
      (unless (or (find (sector s) (poison txn) :key #'sector)
		  (find (sector s) (locals txn) :key #'sector))
	(acquire-buf s) (push s (poison txn))))))

(defun ordain (txn)
  (assert (zerop *access-mode*))
  (dolist (s (shadows txn)) (setf (diskversion-p s) nil))
  (dolist (l (locals txn)) (setf (diskversion-p l) t (dirty-p l) t)))

(defun commit ()
  (let ((txn (pop *txns*)))
    (inscribe txn)
    (bode (shadows txn))  ;; Propogate the changes downwards.. 
    (wait-on-readers)
    (ordain txn)    ;; Mark as disk-version
    (unburden txn)  ;; Vacate pages
    (when (null *txns*) (perform-vacates))
    (extricate txn) ;; free buffers and memory.
    (setf (state txn) +committed+)
    
    (when (and *txns* (= (state (car *txns*)) +closed+))
      (commit))))

(defun finished ()
  "End and commit txn. Caller should spinlock on this until it returns T before proceeding."
  ;;(format *loop* "~%~a:END ~a (pos ~d/~d)" *thread* (state *txn*) (position *txn* *txns*) (length *txns*))
  (sb-sys:without-interrupts
    (sb-sys:allow-with-interrupts (sb-thread:get-mutex *txn-lock* nil t))
    (case (state *txn*)
      (0 (sb-thread:release-mutex *txn-lock*) (error 'reset))
      (1 (setf (state *txn*) +closed+) (sb-thread:release-mutex *txn-lock*) nil)
      (2 (if (eq *txn* (car *txns*))
	     (progn (sb-thread:release-mutex *txn-lock*)
		    (sb-sys:with-local-interrupts (update) (commit) (endupdate))
		    (sb-thread:release-mutex *txn-lock*) t)
	     (progn (sb-thread:release-mutex *txn-lock*) nil)))
      (otherwise (sb-thread:release-mutex *txn-lock*) t))))

(defun end () (assert (not (null *txn*))) (loop until (finished) do (sb-thread:thread-yield)) (setf *txn* nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(labels ((in (sap ll max) (unless (zerop max) (in (sap+ sap 4) (cdr (rplacd ll (list (sap-ref-32 sap 0)))) (1- max))))
	 (valid-header ()
	   (and (not (zerop (sap-ref-64 (sap sysbuffer) 0)))
		(loop for a = (sap sysbuffer) then (sap+ a 4)
		   for b = (sap resbuffer) then (sap+ b 4)
		   repeat +page-words+
		   unless (= (sap-ref-32 a 0) (sap-ref-32 b 0))
		   do (return nil)
		   finally (return t)))))
  (defun replay (start)
    (page-in start sysbuffer)
    (page-in (1+ start) resbuffer)
    (when (valid-header)
      (let* ((sap (sap sysbuffer))
	     (nalloc (sap-ref-32 (sap sysbuffer) 0))
	     (sap (sap+ sap 4))
	     (nlocals (sap-ref-32 sap 0))
	     (sap (sap+ sap 4))
	     (end (sap resbuffer)))
	(format *loop* "~%Replay@~d: ~d allocs and ~d writes" start nalloc nlocals)
	(setf (sector sysbuffer) (1+ start))
	(setf (sector resbuffer) (+ start 1 (ceiling (+ 1 1 (ash nalloc 1) nlocals) +page-words+)))

	;; Redo allocs! 
	(dotimes (i nalloc)
	  (let ((val (sap-ref-32 sap 0)))  ;; LITTLE ENDIAN --- so VAL is stored first...
	    (setf sap (sap+ sap 4))
	    (let ((addr (sap-ref-32 sap 0)))
	      (setf sap (sap+ sap 4))
	      (if (zerop (boole boole-and val #x80000000))
		  (progn (format *loop* "Claim ~a ~d," addr val) (claim addr val))
		  (progn (format *loop* "Vacat ~a ~d," addr (boole boole-and val #x7FFFFFFF))
			 (vacate addr (boole boole-and val #x7FFFFFFF))))))
	  (when (sap= sap end)
	    (incf (sector sysbuffer))
	    (page-in (sector sysbuffer) sysbuffer)
	    (setf sap (sap sysbuffer))))

	;; Redo sector writes... 
	(dotimes (i nlocals)
	  (let ((sector (sap-ref-32 sap 0)))
	    (setf sap (sap+ sap 4))
	    (page-in (sector resbuffer) resbuffer)
	    (page-out sector resbuffer)
	    (incf (sector resbuffer)))
	  (when (sap= sap end)
	    (incf (sector sysbuffer))
	    (page-in (sector sysbuffer) sysbuffer)
	    (setf sap (sap sysbuffer)))))
      (replay (sector resbuffer)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro with-reading (&body body) `(progn (assert (null *txn*)) (tourist) (unwind-protect (progn ,@body) (endtourist))))
(defmacro with-writing (&body body)
  (let ((txn-restart (gensym)))
    `(progn (begin) (unwind-protect (tagbody ,txn-restart (handler-case (prog1 (progn ,@body) (end))
							    (reset () (reopen) (go ,txn-restart))))
		      (when *txn* (withdraw) (setf *txn* nil))))))


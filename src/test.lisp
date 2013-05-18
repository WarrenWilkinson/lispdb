;; Note: with-pinned-objects IS no-gcing.  There's a deadlock possibility if A enters no-gc and tries to acquire the lock.

;; Okay, so NOW I have clients... how do I test this?  I want to AVOID caching things whenever I can...
;; So, what I THINK I'll do is take TITLE + DESCRIPTION OUT of page, view, etc, and put them into the DIR-TREE.
;; So when I update a view -- I do write to the view, and to the directory.  But I DON'T need a cache this way,
;; because I just request the DIR of the page,and filter out views+links for display.  Creation date, last updater,
;; etc, will STILL be stored on the 'thing' itself. Hmmm.. actually ACL might be better in the DIR structure,
;; ditto owner and creator --- because I need that stuff for permissions.. 
;;
;; So ACL, OWNER, CREATOR, TITLE, TYPE, DESCRIPTION go into a directory tree.
;; The source alone, and maybe CREATED, LAST-UPDATED, LAST-UPDATOR, go into the thing itself?  Its better if those
;; went into DIR as well, then the TREES could be dumber.   So lets say it was a META data tree? 
;; How should it be organized?  NAME then all this other stuff?   Hmmm...  Actually it'll have to be rather generic
;; if it will store ALL the meta data. 
;; KEYOF: OWNER-OID OID TYPE --Key-Sort-- 
;;
;; One will be 'NAME' (can it have multiple?), one will be META
;; Hmmm.. I want to pack as much information as I can into this... so a good way to go would be:
;; OWNER-OID OID STRTYPE OFFSET
;; So that fits into 2*64 probably.
;; STRTYPE can be RESOURCETYPE, NAME, TITLE, DESCRIPTION, ACCESSORS (UPDATOR+CREATOR+LAST-UPDATOR), ACCESS-CONTROL etc. 
;; STRTYPE could maybe also be offsets (for where FILE data is stored). 
;; That handles pretty much ALL of my meta data intelligently. The key isn't TOO huge, and its sorted in
;; directory order, which is a BIG plus for generating listings.
;;
;; Thats a good design. Then PageSource can go into the PAGESOURCE tree, VIEWSOURCE into the viewsource tree. 
;; Views maybe versioned (I'm not decided), and eventually I'll want to have them INCLUDABLE like libraries. Documents
;; into DOCUMENTS. This design works very nicely.
;; 
;; So, my MAIN thing to do is the dir-tree, and then everything is hunky brusky. 
;; One other thing --- capabilities exist on the USERS not of the files... so storing them in dirents in probably okay.

;; OKay, now what? I have my clients, each with a directory tree... that contains cool things.  Next up, I should
;; install a PAGE tree.  The test will be many clients, each adding and updating and deleting pages to and from their
;; directory.  This means meta data as well.  I'll have a generator that produces a client name, and a page name, title,
;; and description. Each of these will be letter and count (so I can store them easily). I'll keep a record in some
;; blank pages near the start, so that the occasionaly tourist can verify everything exists. 
;; Each page can be updated (at most) 4 times.  Each thing will have a position in the junk page, and a parent.
;; when I delete the parent, I will update all guys who have it as their parent to the previous entry in the thing,
;; or themselves if there is no previous.   The tourists will also ensure they can lookup every page. 
;;
;; So first up... to create a page, I need name, title, description, oid and parent oid and contents.  Actually, I 
;; probably don't need OID... if I expect it to generate one itself and return it... I dunno.. Generating OIDS is hard...
;; I could do them like freespace, but I need one for each client... Better do it simply. multiple creates will collide.
;; So I the page to page space, then I write the entry.
;; So name title description parent oid contents create-page.
;; and oid new-content update-page. 

;; Okay, so I can store clients, directories, and pages...  Whats left?  If I could store views, and documents, then
;; I would have everything I need to start porting over data... that could be very nice.  It could be EXACTLY the
;; same test as before... just I would perhaps make all DIR things the same size, so I can fit the record into 8 bytes
;; again. Views... and documents...  Well actually. --- yes that works for views.  No not for documents. 
;; Okay then. Lets just make views then, and test it. They have SO much in common...  
;; actually.. every method is the same.
;; 
;; Documents... unfortunately are ALMOST a rez -- except their keys are different..  If I created a SUB BTREE for each
;; page, I could treat them as a REZ.  Lets stop for a moment and think.  Things have attached documents... pages do 
;; anyway. They might ALSO want attached comments or files.  ATCH files are in the directory... what about comments and
;; documents?  Are they the same? (Should you be able to write a view that accesses the comments?)  Should files
;; be allowed comments (YES).   Okay, so ANYTHNG in the directory tree can have comments.  Are views in the directory tree?
;; YES.
;;
;; But not EVERYTHING in the directory tree can have 'documents'.  Only PAGES should have documents. 
;; If I have < 16 types -- a type greater than 16 could mean page with documents at location. 
;; Hmmm...  there is no benefit to storing OID+DOCOID for documents... in fact, splitting them is better because each
;; can have its OWN set of oids. To reference outside you would have to state PARENTOID + CHILDOID.
;; 
;; However, other times I think I might want to store 'documents' on things not pages --- perhaps just loosely. 
;; So what I really want is a DATABASE object.  Page links to database (if it has one) and database links to B+tree, oids,
;; and maybe even VIEW caches..  
;;
;; Such a design would make it possible for the same view-object to apply to multiple databases...  I could use a 32
;; bit OID for documents. Such a design is trickier to handle...  The page needs to know the database object in order
;; to submit to it.  (Multiple pages could SHARE a database!)
;;
;; Documents must record their own meta information, the creating page, etc. And also run-data. The run-data is just a rez.
;; The document DIR is a flat constant thing.  VIEWS should default to using their parent page's database link... and thats
;; where they go...  but what if the database is empty or deleted?   
;; DATABASES should take an OID equal to the pages oid... if not found, then no data.   Okay, now it seems like
;; both PAGES and VIEWS store 1 additional word --- a database OID.  I could store it in DIR, but thats bad.
;; Ideally, I just store it before the REZ thing.. I could introduce an offset for this or a REZ1 varient.
;; 
;; A cursor offset variable is looking mighty fine about now. Hmmm.. having these databases locally introduces
;; features, but also adds UI problems and code complexity.  Is there a simpler way for a page to state that it
;; saves to somewhere else?   YES.  Save to the page, and have the database MERGE them --- this way the action is
;; undoable.   So now, I just save documents as normal, to the page. 
;;
;; VIEWS on the other hand are more complicated, because they can be told to use records from MULTIPLE databases. 
;; I'm okay with that. (You could click on view-sources, and add Calgary Safety, Edmonton Safety).
;;
;; This seems like a bit of trouble for views... because now they have more options for controlling what they can
;; access --- if you had a dozen views, you would have trouble. Perhaps VIEWS are like databases? So I split
;; the database object into two parts --- a document store, and a view cache.  How do you tell the view what databases
;; to access?  Besides that, I can see more and more complications ariving.. You might want to run a different computation
;; on the records from each database..  and then combine them here -- so you want to use another VIEW as an input...
;; but now permissions affect what records are showing.. suddenly things are VERY complicated... 
;;
;; Views were already complicated with their document storage & automatic updating behavior.  How can I simplify that?
;; NOT caching views greatly simplifies.  How can I use shared source code?   It seems that there are really TWO things
;; transformation code --- (functions, etc) and VIEWS... 
;; 
;; When you create a view you did both, but wouldn't it be possible to just define a library instead? 
;; The simplist is: NO view caching, NO cross-db view access, NO includes. 
;; 
;; HOW can I make all of these things easy and CONCEPTUALLY simpler!
;;
;; GOT IT! 
;; * Cross view DB access is done by adding an SOURCE 0, SOURCE 1, etc. statements... one for each page.
;;   If you omit it, it defaults to the parent page.
;;   - With each incoming tuple, it'll have a 'PAGE' variable that contains its OID, so you can switch on it.
;;   - These are an advanced feature that won't be added right away.
;; * Includes are done via 'blocks'  A set of numbered text areas.  They are just source-included.
;;   So if you have many highly similar views, you can use blocks to hold the common code. The blocks can
;;   even lay out columns for you.
;;   - These are an advanced feature (that won't be added right away).
;; * View caching as planned.
;;   - Its an advanced feature (that won't be added right away).
;;
;; Good stuff, good stuff. So now my views are just REZ again, and all submitted documents are stored with the page...
;; I just design structures to be as simple as possible and do just enough.
;; 
;; So, PAGES and the DOCUMENTS saved into them are forever unified..   I don't think I'll ever have requests to 
;; move a single document ---- but I might have requests to move the data to another page...  So, this operation
;; is better to have a DOCUMENT btree.  So PAGES have a DOCUMENT btree.  That sorta sucks, because PAGES are no longer
;; REZ this way...  An alternative is a TREE-O-TREES.  It links OIDS to document holders.  Thats probably a good
;; way to go, it helps SOURCE later. 

;; What about finding ALL of a particular users updates?  The best I could do would be have another TREE-O-TREE with
;; key oid + local-oid.    So I could associate a list of documents with the users. These documents would explain their
;; recent changes.  Not a great way to go for this feature.  This feature probably only needs like the most recent 10 actions
;; for all users.  Another feature would be email alerts when pages change.  This is better handled on my end, not with
;; documents.   So both recent-changes and alerts are best NOT done with documents.

;; SO PAGES and VIEWS are just rez things.   
;; I have a tree-o-trees, it associates PAGEOID with block-pointer of a BTREE of documents. To move all documents to a new
;; page, I can just update this stuff.  Moving a page carries its documents.  Merging documents isn't too terrible either. 
;; Within the TREE-o-Trees I actually have meta-data AND a content rez.  Documents are NOT version controlled. 

;; So, lets build my document thing...  what shall I call it?  Gazeteer is a geographical dictionary or directory...
;; 
;; The things I HAVE is a cartulary.  So the other thing would be a library, or inventory or archives.
;; So I have defined an archive table that contains links to the cartularies. The cartularies are ALL of the documents
;; the user has for a specific page.



;; Its time to TEST Client-> page, archives->cartulary->document. 
;; Basically, I should have 2 clients. And they sometimes generate pages.  Sometimes generate views. 
;; and sometimes they generate documents and attach them to pages. 
;;
;; All along the way, I also should test out modifications to any of those, and deletes of any of those. 
;; Once done, I can start porting data over to the new backend, and working on my 'fake views' and forth additions.
;; 
;; So again, to test this I need a good way to record what I've done, so the threads can share the update task...
;; I should store NA/PAGE/VIEW,LEN.  
;;
;; Two ways.. either randomly and record it to a buffer... Or determine it all ahead of time what everybody should do...
;; the record to buffer method is probably superior.
;;
;; Actually, lets store views only at certain OIDs, and pages at others... that way all I need is a byte per view
;; giving its base and length.  Secondly, only SOME page OIDs will store documents.  So I store page+len. 
;; So views + pages stored the same way, except 3 more pages follow devoted to just storing document data.
;; Document data is a 4metabytes, LENGTH ( 1 byte quadrupled) and 3 chars -- that are repeated LENGTH times.
;; I always insert documents according to the archives OID ...  But I do edit them and delete them. This
;; means at most I can have 512 documents. 
;; 

(defpackage :test
  (:use :common-lisp :util.util :db.io :db.txn :db.threads :db.bt :db.metadata :db.resources :db.dir :db.client :db.db)
  (:import-from :sb-sys sap+ sap-)
  (:shadowing-import-from :db.dir directory))

(in-package :test)

;(let ((16pages (make-array (ash +page-bytes+ 4) :element-type '(unsigned-byte 8))))
;    (dotimes (i 4096) (write-sequence 16pages *filestream*))))

(defun empty-space ()
  "creates a 256 meg empty database."
  (setf *filestream* (open (pathname "/tmp/empty.db") :direction :io :element-type '(unsigned-byte 32)
			   :if-exists :supersede
			   :if-does-not-exist :create)))


;; System-area-ubxx-fill broken on 64bit machines...
;(defun systeam-area-ub32-fill (value sap offset length) (sb-kernel:system-area-ub32-fill value sap offset length))
(defun system-area-ub32-fill (value sap offset length)
  (let ((vector (make-array length :element-type '(unsigned-byte 32) :initial-element value)))
    (sb-sys:with-pinned-objects (vector)
      (sb-kernel:system-area-ub8-copy (sb-sys:vector-sap vector) 0 sap offset (* length 4)))))

;; db.io
;; this test creates  24 threads which randomly cache, release or isolate pages using db.io routines. individualy, they
;; all follow good protocol.  thus, when its done, there should be no leftover watchers on anything (except sysbuffer).
(flet ((db.io-bitch (c)
	 ;(sleep (random 0.01))
	 (if (zerop (random 1)) (sb-thread:thread-yield))
	 (do ((holds nil)
	      (i 0 (1+ i)))
	     ((>= i 400) (dolist (h holds) (release-buf h)))
	   (let ((n (random 4096)))
	     (sb-sys:without-interrupts 
	       (when (zerop (mod i 10)) (terpri *loop*))
	       (format *loop* "(~a:~a)" c i))
	     (setf holds (handler-case (aif (find n holds :key #'sector)
					    (if (zerop (random 2))
						(progn (release-buf it) (delete it holds))
						(prog1 (nsubstitute (db.io:isolate it) it holds :count 1) 
						  (release it)))
					    (cons (cache n) holds))
			   (nobuffers () (dolist (h holds) (release-buf h)) nil)))))))
  (defun db.io-test ()
    (do ((i 0 (1+ i))
	 (threads nil))
	((>= i 24) (dolist (th threads (format *loop* "~%done: ~d locks inappropriately held."
					       (count 0 (cddr (db.io:debug-watchers)) :test #'<)))
		     (sb-thread:join-thread th)))
      (push (sb-thread:make-thread (papply #'db.io-bitch i) :name (format nil "bitch-~d" i)) threads))))


;; db.txn 
;; this test works by maintaining a freespace bitmap of allocs and deallocs.  at all times i make sure that
;; the freespace list has the same data as the bitmap.
(defun db.txn-alloctest ()
  (let ((bitmap (make-array 4096 :initial-element nil)))
    (setf db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff)))
    (flet ((run (start) (let* ((set-p (not (svref bitmap start)))
			       (end (or (position set-p bitmap :start start) 4096))
			       (runlen (1+ (random (- end start)))))
			  (format *loop* "~%~:[vacate~; claim~] ~a ~a" set-p (+ +user-start+ start) runlen)
			  (if set-p (db.txn:claim (+ +user-start+ start) runlen)
			      (db.txn:vacate (+ +user-start+ start) runlen))
			  (loop for i upfrom start repeat runlen do (setf (svref bitmap i) set-p))))
	   (check ()
	     (do ((start (position nil bitmap))
		  (entry (cdr db.txn:*free*) (cdr entry)))
		 ((null entry))
	       (let ((clearto (or (position t bitmap :start start) 4096)))
		 (assert (= (+ +user-start+ start) (caar entry)))
		 (assert (= (+ +user-start+ clearto) (min (cdar entry) (+ +user-start+ 4096))))
		 (setf start (or (position nil bitmap :start clearto) 4096))))))
      (dotimes (i 1000) (run (random 4096)) (check))
      ;; vacate everything
      (loop for nextset = (position t bitmap)
	    while nextset
	    do (run nextset) (check))
      (assert (equalp db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff)))))))

(defun !zero()
  (loop for buffer across db.io:+buffers+
        do (setf (diskversion-p buffer) nil (dirty-p buffer) nil (sector buffer) 0)
        do (system-area-ub32-fill 0 (sap buffer) 0 +page-words+)))

;; this test works by spawning several transactions that try to set byte i to i, where i is their transaction number, 
;; on sixteen pages. they start operating on page i%16+start, and progress to greater page numbers. transaction 0 doesn't do
;; anything, which prevents any changes from being committed to the disk.  when we are done, the contents of the bytes
;; of the pages are dumped, and there should be a clear pattern (txn 0 has all zeros, txn 1 has 1 in 1s column, 2 has
;; former plus 2's in 2's column, etc)
(flet ((checkpages (n)
	 (dotimes (i 16 t)
	   (if (zerop i)
	       (format *loop* "~%~:[--~;~2d~]: " *txn* n)
	       (format *loop* "~%    "))
	   (format *loop* "~2d " i)
	   (let* ((b (acquire (+ +user-start+ i)))
		  (sap (sap b)))
	     (dotimes (q (1+ n))
	       (format *loop* "~x" (sb-sys:sap-ref-8 sap q))
	       (or (= q (sb-sys:sap-ref-8 sap q)) (progn (release b) (return-from checkpages nil))))
	     (dotimes (q (- 16 n 1))
	       (format *loop* "~x" (sb-sys:sap-ref-8 sap (+ q n 1)))
	       (or (= 0 (sb-sys:sap-ref-8 sap (+ q n 1))) (progn (release b) (return-from checkpages nil))))
	     (release b))))
       (db.txn-bitch (n txn)
	 (let ((*txn* txn))
	   (declare (special *txn*))
	   ;;(sleep (random 0.01))
	   (if (zerop (random 1)) (sb-thread:thread-yield))
	   (tagbody :start
	      (format *loop* "~%starting ~a" n)
	      (handler-case (dotimes (i 16 (end))
			      (let* ((i (mod (+ i n) 16))
				     (buffer (db.txn:ghost (db.txn:acquire (+ +user-start+ i)))))
				(sb-sys:without-interrupts 
				  (when (zerop i) (terpri *loop*))
				  (format *loop* "(~d: ~d)" n i))
				(sb-kernel:system-area-ub8-fill 0 (sap buffer) n (- +page-bytes+ n))
				(setf (sb-sys:sap-ref-8 (sap buffer) n) n)
				(release buffer)))
		(reset () (reopen) (go :start)))))))
  (defun db.txn-test ()
    (!zero)
    (empty-space)
    (checkpages 0)
    (format *loop* "~%Spawn threads...")
    (let* ((txns (loop repeat 15 collecting (progn (begin) *txn*)))
	   (threads (loop for i upfrom 1
		          for txn in txns
			  collecting (sb-thread:make-thread (papply #'db.txn-bitch i txn)))))
      (dolist (th threads) (sb-thread:join-thread th))
      (or (progn (setf *txn* nil) (checkpages 15)) (progn (format *loop* "... error in reader txn.")
							 (error "Error in db.txn-test")))
      (format *loop* "~%done..."))))

;; journaling
;; multiple threads repeatedly hammer a range of sectors with transactions, reads, withdraws, etc. afterwards, a single
;; reader checksums all of those sectors, and a cheat zeros them out. the journal is then replayed and the sectors
;; checksummed again. they should match the earlier checksums.

(flet ((txn-bitch (n txn ins)
	 (let ((*txn* txn))
	   (declare (special *txn*))
	   ;;(sleep 0.01);(random 3.0))
	   (if (zerop (random 1)) (sb-thread:thread-yield))
	   (labels ((inner (ins)
		      ;;(sleep (random 1.0))
		      (if (null ins)
			  (end)
			  (destructuring-bind (sector value start end &rest more-ins) ins
			    (sb-sys:without-interrupts 
			      (when (zerop (mod sector 4)) (terpri *loop*))
			      (format *loop* "(~2,'0d@~d: ~2,'0x [~4,'0d ~4,'0d])" n sector value start end))
			    (let ((b (db.txn:ghost (db.txn:acquire sector))))
			      (if (< end start)
				  (progn (sb-kernel:system-area-ub8-fill value (sap b) 0     end)
					 (sb-kernel:system-area-ub8-fill value (sap b) start (- +page-bytes+ start)))
				  (sb-kernel:system-area-ub8-fill value (sap b) start (- end start)))
			      (db.txn:release b))
			    (inner more-ins)))))
	     (tagbody :start (format *loop* "~%!starting ~a!" n)
		(handler-case (inner ins)
		  (reset () (reopen) (go :start)))))))
       (make-ins ()
	 (loop repeat (random 10)
	    nconc (list (+ +user-start+ (random 16)) (1+ (random 255)) (random +page-bytes+) (random +page-bytes+))))
       (checksum () 
	 (with-reading 
	   (loop for sector upfrom +user-start+ repeat 16 
	         as checksum = (let ((b (acquire sector)))
				 (loop repeat +page-words+
				       for sap = (sap b) then (sb-sys:sap+ sap 4) 
				       with total = 0
				       do (setf total (mod (+ (sb-sys:sap-ref-32 sap 0) total) (1- (expt 2 32))))
				       finally (progn (release b) (return total))))
	         do (format t "~%~d: ~8,'0x" sector checksum)
	         collecting checksum))))
  (defun db.journal-test ()
    (!zero)
    (empty-space)
    (setf db.txn:*cursor* db.io:+journal-start+)
    (do ((i 0 (1+ i))
	 (threads nil))
	((>= i 16) (dolist (th threads) (sb-thread:join-thread th)))
      (push (sb-thread:make-thread (papply #'txn-bitch i (begin) (make-ins))
				   :name (format nil "bitch-~d" i)) threads))
    (format *loop* "~%wrote data... (~d inapproprately held locks)" (count 0 (cddr (debug-watchers)) :test #'<))
    (setf *txn* nil)
    (let ((checksums (checksum)))
      (!zero)
      (format t "~%journal replay!")
      (db.txn:replay db.io:+journal-start+)
      (unless (every #'= checksums (checksum))
	(error "checksums do not match! replay fails!")))))


;; Allocation Journaling
;; This test tests that allocations are also journaled properly.
;; There are 4 counters. Each counter has an index node (starting at +user-space+), that stores a block address.
;; That block has the current count. Increment is copy-on-write, and then the index block is updated. 
;; I choose multiple counters so that not all TXNs will be in conflict. 

;; Each bitch sleeps a random time, then begins, it attempts to  random counter and attempts to update it. 

(flet ((bitch (n counter);n txn &aux (counter (random 4)))
	 (declare (ignore n))
	 (let ((*txn* nil))
	   (declare (special *txn*))
	   (if (zerop (random 1)) (sb-thread:thread-yield))
	   ;;(db.txn:begin)
	   (with-writing 
	      ;;(format *loop* "~%Thread ~d updating counter ~d." n counter)
	     (let* ((space (db.txn:allocate 1))
		    (new-block (db.txn:ghost (db.txn:acquire space)))
		    (index (db.txn:ghost (db.txn:acquire (+ +user-start+ counter))))
		    (old (sb-sys:sap-ref-32 (sap index) 0))
		    (oldbuf (acquire old)))
	       (setf (sb-sys:sap-ref-32 (sap new-block) 0) (1+ (sb-sys:sap-ref-32 (sap oldbuf) 0)))
			      ;;(format *loop* "~%Counter was at: ~a" (sb-sys:sap-ref-32 (sap index) 0))
			      ;;(format *loop* "~%Make counter point to : ~a" space)
	       (setf (sb-sys:sap-ref-32 (sap index) 0) space)
	       (deallocate old 1)
	       (release oldbuf)
	       (release new-block)
	       (release index))))))
  (defun db.journal-alloc-test (&aux (threads nil))
    ;; All I gotta do is create 128 threads, 32 for each counter.  That way at the end there should only be
    (format *loop* "~%Journal-alloc-test...")
    (setf db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff)))
    (setf db.txn:*cursor* +journal-start+)
    (!zero)
    (empty-space)
    ;; Initially, the first 8 blocks are allocated. The first 4 point to the next 4, the next 4 have zeros.
    (with-writing 
      (allocate 8)
      (dotimes (i 4)
	(let ((blk (ghost (acquire (+ +user-start+ i)))))
	  (setf (sb-sys:sap-ref-32 (sap blk) 0) (+ +user-start+ 4 i))
	  (release blk)))
      (dotimes (i 4)
	(let ((blk (ghost (acquire (+ +user-start+ 4 i)))))
	  (setf (sb-sys:sap-ref-32 (sap blk) 0) 0)
	  (release blk))))

    ;; 4 allocated blocks, and the counts should all be 32... The replay should trigger the same.
    (dotimes(i 32)(push (sb-thread:make-thread (papply #'bitch (+ i 00) 0) :name(format nil "~d" (+ i 00))) threads))
    (dotimes(i 32)(push (sb-thread:make-thread (papply #'bitch (+ i 32) 1) :name(format nil "~d" (+ i 32))) threads))
    (dotimes(i 32)(push (sb-thread:make-thread (papply #'bitch (+ i 64) 2) :name(format nil "~d" (+ i 64))) threads))
    (dotimes(i 32)(push (sb-thread:make-thread (papply #'bitch (+ i 96) 3) :name(format nil "~d" (+ i 96))) threads))
    (dolist (th threads) (sb-thread:join-thread th))

    ;;(setf *txn* nil)
    (format t "~%Done!")
    (flet ((vals-of (addr) (let ((blk (acquire addr))) (prog1 (sb-sys:sap-ref-32 (sap blk) 0) (release blk)))))
      (let* ((counters (mapcar #'vals-of (iota 4 +user-start+)))
	     (values (mapcar #'vals-of counters))
	     (free (cons nil (mapcar #'(lambda (a) (cons (car a) (cdr a))) (cdr db.txn:*free*)))))
	(format t "~%Okay, counters are at: ~d~%And are: ~d" counters values)

	(!zero)
	(setf *free* (list nil (cons +user-start+ #xFFFFFFFF)))
	(db.txn:replay db.io:+journal-start+)
	(let* ((rcounters (mapcar #'vals-of (iota 4 +user-start+)))
	       (rvalues (mapcar #'vals-of counters)))
	  (format t "~%Replayed counters are at: ~d~%And are: ~d" rcounters rvalues)
	  (unless (and (equalp rcounters counters) (equalp rvalues values))
	    (error "Replayed writes do not match!"))
	  (unless (equalp free *free*)
	    (error "Replayed allocations do not match! (got ~a, expected ~a)" *free* free)))))))

;; Btree Operations
;; These tests test basic b+tree operations (digests are tested seperately later). The test works by keeping a list
;; of added numbers, and then randomly choosing numbers and removing them (if inserted) or adding them otherwise.
;; After each operation, a seek of every existant key is done and a scan of all the leaves to ensure
;; they are in order and no extra numbers are present.  Each KEY holds key-squared-mod-32bit as its data.

(defbtree *test-info* 36 220) ;; Really big data, so I can get more levels... ;(malloc 16))
;(db.bt:custom *test-info* 36 220)  

;; HACK, set the *test-info* to only think it can FIT 5 BPS ber node. (nodes now fit 5, leaves fit 3)
(setf (sb-sys:sap-ref-16 *test-info* 2) 5)

(defconstant +overlap+ 50) ;; Used by multi-test.. I write 0-63, then 50-50+63 ... etc, so I get some overlap.

(defthreadlocal test-data 
  (scratch   memory 1364)) ;; AT LEAST 1364 bytes of space. Used as a temp space.


(labels ((first-leaf (root)
;;	   (format *loop* "~%First Leaf...~d" root)
	   (let ((buffer (acquire root)))
	     (if (db.bt:inner-p buffer)
		 (let ((next (sb-sys:sap-ref-32 (db.bt:datum buffer) 0)))
		   (when (= next root) (error "Inner node links to itself at idx 0!"))
		   (release buffer)
		   (first-leaf next))
		 (progn (release buffer) root))))
	 (hibits (v) (do ((c 0 (1+ c)))
			 ((zerop v) c)
		       (setf v (boole boole-and v (1- v)))))
	 (sq (number) (boole boole-and #xFFFFFFFF (* number number)))
	 (checktree (root leaf added)
	   (format *loop* "~%Checktree ~d: " leaf)
	   (when (zerop root) (assert (null added)) (format *loop* "~%0tree check passed.") (return-from checktree nil))
	   (when (zerop leaf) (error "Zero leaf!"))
	   
	   (let* ((buffer (acquire leaf))
		  (c (db.bt:packed buffer)))
	     (do* ((added added (cdr added)) 
		   (c c (1- c))) 
		  ((zerop c))
	       (when (null added)
		 (error "~%B+tree says I have one more item that I have a record for!"))
	       (system-area-ub32-fill 0 (db.bt:down-key) 0 36)
	       (setf (sb-sys:sap-ref-32 (db.bt:down-key) 0) (car added))
	       (let* ((buffer (db.bt:lookup root))
		      (ks (origin-key))
		      (ds (origin-data)))
		 ;;(format *loop* "~%Lookup ~a got ~a ~a" (car added) (sb-sys:sap-ref-32 ks 0) (sb-sys:sap-ref-32 ds 0))
		 (unless (found)
		   (let ((val (sb-sys:sap-ref-32 ks 0)))
		     (contents *loop* buffer)
		     (format *loop* "~%missed ~a, got ~a ~a." (car added) val (sb-sys:sap-ref-32 ks 4))
		     (format *loop* "~d" (mapcar #'(lambda (i) (sb-sys:sap-ref-32 ks i)) (util.util:iota (db.bt:key-u32s)
												    (* 36 4) 4)))
		     (let ((next (sb-sys:sap-ref-32 ks 4)))
		       (release buffer)
		       (error "Lookup missed ~a, got ~a ~a." (car added) val next))))
		 (unless (and (= (car added) (sb-sys:sap-ref-32 ks 0))
			      (let ((n (sq (car added))))
				(every #'(lambda (i) (= n (sb-sys:sap-ref-32 ds i))) (util.util:iota 220 0 4))))
		   (format *loop* "Lookup missed ~d+~d (got ~d+~d,~d)."
			   (car added) (sq (car added))
			   (sb-sys:sap-ref-32 ks 0)
			   (sb-sys:sap-ref-32 ds 0)
			   (sb-sys:sap-ref-32 ds 4))
		   (release buffer)
		   (error  "Lookup missed ~d+~d." (car added) (sq (car added))))
		 (release buffer)))

	     (multiple-value-bind (ks ds) (db.cursor:leaf-start buffer)
	       (dotimes (i c)
		 (unless (and added (= (car added) (sb-sys:sap-ref-32 ks 0))
			      (every #'(lambda (i) (zerop (sb-sys:sap-ref-32 ks i))) (cdr (util.util:iota (db.bt:key-u32s) 0 4)))
			      (let ((n (sq (car added))))
				(every #'(lambda (i) (= n (sb-sys:sap-ref-32 ds i))) (util.util:iota 220 0 4))))
		   (format *loop* "~d" (mapcar #'(lambda (i) (sb-sys:sap-ref-32 ks i)) (util.util:iota (db.bt:key-u32s) 0 4)))
		   (format *loop* "~d" (mapcar #'(lambda (i) (sb-sys:sap-ref-32 ds i)) (util.util:iota 220 0 4)))
		   (format *loop* "Missing ~d+~d (got ~d+~d,~d)." (and added (car added)) (and added (sq (car added)))
			   (sb-sys:sap-ref-32 ks 0)
			   (sb-sys:sap-ref-32 ds 0)
			   (sb-sys:sap-ref-32 ds 4))
		   (error "Missing ~d+~d (got ~d+~d,~d)." (and added (car added)) (and added (sq (car added)))
			  (sb-sys:sap-ref-32 ks 0)
			  (sb-sys:sap-ref-32 ds 0)
			  (sb-sys:sap-ref-32 ds 4)))
		 (setf added (cdr added)
		       ks (sb-sys:sap+ ks (db.bt:key-u8s))
		       ds (sb-sys:sap+ ds (db.bt:leaf-u8s)))))
	     (if added
		 (checktree root (prog1 (db.bt:link buffer) (release buffer)) added)
		 (release buffer)))))
  (defun db.btree-test-nodups (max)
    (setf db.txn:*free* (list nil (cons (1+ db.io:+user-start+) #xffffffff)))
    (with-writing 
      (let ((added (list nil)) ;; kept sorted!
	    (root +user-start+))
	(btree *test-info*)
	(let ((buffer (ghost (acquire root))))
	  (db.bt:prepare buffer))
	(format *loop* "~%B+tree initialized.")
	(dotimes (i max)
	  (when (zerop (mod i 100)) (format *loop* "~%Progress: ~d" i))
	  (let* ((number (random 7000))
		 (sap (scratch))
		 (pos (position number (cdr added) :test #'<=))
		 (place (if pos (nthcdr pos added) (last added))))
	    (system-area-ub32-fill 0 (db.bt:down-key) 0 (db.bt:key-u32s))
	    (setf (sb-sys:sap-ref-32 (db.bt:down-key) 0) number)
	    (system-area-ub32-fill (sq number) sap 0 220)
	    (setf (db.bt:down-data) sap)
	    (setf root (if (eql (cadr place) number)
			   (progn (rplacd place (cddr place)) (db.bt:rubout root))
			   (progn (rplacd place (cons number (cdr place))) (db.bt:insert (creating root)))))))
	(checktree root (first-leaf root) (cdr added))
	
	;; Ensure that SOME new blocks have been allocated! (comment this out if inserts < 340)
	(assert (not (equalp db.txn:*free* (list nil (cons (1+ db.io:+user-start+) #xffffffff)))))
	
	(format *loop* "~%Clearing entries...")
	(do ((len (length (cdr added)) (1- len)))
	    ((zerop len))
	  (let* ((pos (random len))
		 (list (nthcdr pos added))
		 (val (car (cdr list))))
	    ;;(format *loop* "~%D: ~d" val)
	    (rplacd list (cddr list))
	    (setf (sb-sys:sap-ref-32 (db.bt:down-key) 0) val)
	    (setf root (db.bt:rubout root))
	    (if (zerop root)
		(format *loop* "~%Tree is gone, checking skipped ~a." list)
		(checktree root (first-leaf root) (cdr added)))))
	(assert (zerop root)))) ;; Tree was deleted... 
    (assert (equalp db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff))))) ;; Space reclaimed.


;; BTREE MULTIKEY test!
;; Build a tree out of 0-63 added DOZENS and DOZENS of times. 

;; I have 64 unique keys, each can be added 16 times. 
;; Thus an array of 64 16bits.  Then I generate randoms 64 and 16... 
;; (random 1024)    top 4 bits are the bit index, low 6 are the array-index
;; 

  (defun db.btree-test-dups (max)
    (setf db.txn:*free* (list nil (cons (1+ db.io:+user-start+) #xffffffff)))
    (with-writing 
      (let ((inserts (make-array 64 :element-type '(unsigned-byte 16)))
	    (added (list nil)) ;; kept sorted!
	    (root +user-start+))
	(btree *test-info*)
	(let ((buffer (ghost (acquire root))))
	  (db.bt:prepare buffer))
	(format *loop* "~%B+tree initialized.")
	(dotimes (i max)
	  (let* ((n   (random 1024))
		 (idx (ldb (byte 6 0) n))
		 (f-idx (ldb (byte 4 6) n))
		 (f-bits (ash 1 f-idx))
		 (sap (scratch))
		 (pos (position idx (cdr added) :test #'<=)); (1- (length added)))
		 (place (if pos (nthcdr pos added) (last added))))
	    
	    (when (zerop (mod i 100)) (format *loop* "~%Progress: ~d" i))
	    (system-area-ub32-fill 0 (db.bt:down-key) 0 (db.bt:key-u32s))
	    (setf (sb-sys:sap-ref-32 (db.bt:down-key) 0) idx)
	    (system-area-ub32-fill (sq idx) sap 0 220)
	    (setf (db.bt:down-data) sap)
	    (if (zerop (boole boole-and f-bits (aref inserts idx)))
		(progn
		  (setf (aref inserts idx) (boole boole-ior f-bits (aref inserts idx)))
		  (rplacd place (cons idx (cdr place)))
		  (setf root (db.bt:insert (creating root))))
		;;(format *loop* "~%INSERT ~a ~a" idx (hibits (aref inserts idx))))
		(progn
		  ;;(format *loop* "~%DELETE ~a (~a)" idx (hibits (aref inserts idx)))
		  (rplacd place (cddr place))
		  (setf root (db.bt:rubout root))
		  (setf (aref inserts idx) (boole boole-and (- #xFFFF f-bits) (aref inserts idx))))))
	  (checktree root (first-leaf root) (cdr added)))
	
	;; Ensure that SOME new blocks have been allocated! (comment this out if inserts < 340)
	(assert (not (equalp db.txn:*free* (list nil (cons (1+ db.io:+user-start+) #xffffffff)))))
	
	(format *loop* "~%Clearing entries...")
	(do ((len (length (cdr added)) (1- len)))
	    ((zerop len))
	  (let* ((pos (random len))
		 (list (nthcdr pos added))
		 (val (car (cdr list))))
	    ;;(format *loop* "~%D: ~d" val)
	    (rplacd list (cddr list))
	    (setf (sb-sys:sap-ref-32 (db.bt:down-key) 0) val)
	    (setf root (db.bt:rubout root))
	    (if (zerop root)
		(format *loop* "~%Tree is gone, checking skipped ~a." list)
		(checktree root (first-leaf root) (cdr added)))))
	
	(assert (zerop root)))) ;; Tree was deleted... 
    (assert (equalp db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff))))) ;; Space reclaimed.


;; What does this test do?  How can I simply test this?  First inserts 1-X/n, second inserts X/n - 2x/n, etc, etc.
;; hmmm... Periodically, a READER comes in a checks the tree, and then spawns a TXN which deletes any existant
;; y*X/n thing.  At the end, I should have 0-X with all multiples of X/N removed. 

;; Actually, I need to store my LIST on-disk... thats sucky. I have 4096 bytes, 1024 ints.
;; I could just read them all.. anything NON-zero means that value is present. 
;; So my checktree could go on that... Thats going to SERIALIZE transactions... actually, lets give each
;; TXN its OWN buffer.. 

  (labels ((get-root () (let ((buffer (acquire +user-start+)))
			  (prog1 (sb-sys:sap-ref-32 (sap buffer) 0) (release buffer))))
	   (set-root (n) (let ((buffer (ghost (acquire +user-start+))))
			   (setf (sb-sys:sap-ref-32 (sap buffer) 0) n)
			   (release buffer)))
	   (set-present (n offset yes-no)
	     (let ((buffer (ghost (acquire n))))
	       (setf (sb-sys:sap-ref-8 (sap buffer) offset) (if yes-no 1 0))
	       (release buffer)))
	   (present (n &aux (buffer (acquire n)))
	     ;;(format *loop* "(^~a:~a:~a^)" *thread* n (bnum buffer))
	     (loop for i upfrom 0 repeat +page-bytes+
		for sap = (sap buffer) then (sb-sys:sap+ sap 1)
		nconc (if (zerop (sb-sys:sap-ref-8 sap 0)) nil (list (+ i (* +overlap+ n))))
		finally (release buffer)))
	   (clear (n &aux (buffer (ghost (acquire n))))
	     (system-area-ub32-fill 0 (sap buffer) 0 +page-words+)
	     (release buffer))
	   (all-clear ()
	     (clear 0) (clear 1) (clear 2) (clear 3)
	     (clear 4) (clear 5) (clear 6) (clear 7)
	     (clear 8) (clear 9) (clear 10) (clear 11)
	     (clear 12) (clear 13) (clear 14) (clear 15))
	   (all-present ()
	     (sort (nconc (present 0) (present 1) (present 2) (present 3)
			  (present 4) (present 5) (present 6) (present 7)
			  (present 8) (present 9) (present 10) (present 11)
			  (present 12) (present 13) (present 14) (present 15)) #'<))
	   (do-delete (txn-n c n)
	     ;;(format *loop* "Doing delete...")
	     (let ((*txn* nil))
	       (declare (special *txn*))
	       (with-writing
		 (btree *test-info*)
		 (system-area-ub32-fill 0 (db.bt:down-key) 0 (db.bt:key-u32s))
		 (setf (sb-sys:sap-ref-32 (db.bt:down-key) 0) n)
		 (set-present txn-n c nil)
		 (let ((root (get-root)))
		   (let ((new-root (db.bt:rubout root)))
		     (unless (= new-root root) (set-root new-root)))))))
	   (do-insert (txn-n c n)
	     (let ((*txn* nil))
	       (declare (special *txn*))
	       (with-writing 
		 (btree *test-info*)
		 (let ((sap (scratch)))
		   (system-area-ub32-fill 0 (db.bt:down-key) 0 (db.bt:key-u32s))
		   (setf (sb-sys:sap-ref-32 (db.bt:down-key) 0) n)
		   (system-area-ub32-fill (sq n) sap 0 220)
		   (setf (db.bt:down-data) sap))
		 ;; GET THE ROOT, its in a page!
		 (set-present txn-n c t)
		 (let ((root (get-root)))
		   (let ((new-root (db.bt:insert (creating root))))
		     (unless (= new-root root) (set-root new-root)))))))
	   (bitch (n c insert)
	     (let ((*thread* n)
		   (start (* n +overlap+)))
	       (declare (special *thread*))
	       (dotimes (i c)
		 ;; (format *loop* "~%~a:INSERT ~d" n (+ i start))
		 (if insert
		     (do-insert n i (+ i start))
		     (do-delete n i (+ i start)))
		 (when (zerop (mod (+ n i) 10)) (terpri *loop*))
		 (format *loop* "[~a:D ~d]" n (+ i start)))))
	   (read-bitch (n threads)
	     (let ((*txn* nil)
		   (*thread* n))
	       (declare (special *txn* *thread*))
	       (btree *test-info*)
	       (loop while (some #'sb-thread:thread-alive-p threads)
		     do (sleep (random 2.0))
		     do (with-reading 
			  (let ((root (get-root))
				(added (all-present)))
			    (format *loop* "~%~a:TOURIST ~a ~a!" n root (length added))
			    (when added (checktree root (first-leaf root) added))))))))
    ;; READ BITCH continues to run until all the other TXNs are ended...
    (defun db.btree-test-multiuser (N)
      (let ((threads nil))
	(setf db.txn:*free* (list nil (cons (1+ db.io:+user-start+) #xffffffff)))
	(btree *test-info*)
	(with-writing 
	  (set-root 0)
	  (all-clear))

	(dotimes(i 16) (push (sb-thread:make-thread (papply #'bitch i (truncate N 16) t)
						    :name (format nil "~d" i)) threads))
	(push (sb-thread:make-thread (papply #'read-bitch 17 threads) :name "rbitch1") threads)
	(push (sb-thread:make-thread (papply #'read-bitch 18 threads) :name "rbitch2") threads)
	(push (sb-thread:make-thread (papply #'read-bitch 19 threads) :name "rbitch3") threads)
	(push (sb-thread:make-thread (papply #'read-bitch 20 threads) :name "rbitch4") threads)
	(dolist (th (cddddr threads)) (sb-thread:join-thread th))
	
	(sb-thread:join-thread (first threads))
	(sb-thread:join-thread (second threads))
	(sb-thread:join-thread (third threads))
	(sb-thread:join-thread (fourth threads))
	(format *loop* "~%Done Writing...")
	
	;; Now delete data!
	(setf threads nil)
	(dotimes (i 16) (push (sb-thread:make-thread (papply #'bitch i (truncate N 16) nil)
						     :name (format nil "~d" i)) threads))
	(push (sb-thread:make-thread (papply #'read-bitch 17 threads) :name "rbitch1") threads)
	(push (sb-thread:make-thread (papply #'read-bitch 18 threads) :name "rbitch2") threads)
	(push (sb-thread:make-thread (papply #'read-bitch 19 threads) :name "rbitch3") threads)
	(push (sb-thread:make-thread (papply #'read-bitch 20 threads) :name "rbitch4") threads)
	(dolist (th (cddddr threads)) (sb-thread:join-thread th))

	(sb-thread:join-thread (first threads))
	(sb-thread:join-thread (second threads))
	(sb-thread:join-thread (third threads))
	(sb-thread:join-thread (fourth threads))
	(format *loop* "~%Done deleting!")
	(assert (zerop (get-root)))
	(assert (equalp db.txn:*free* (list nil (cons (1+ db.io:+user-start+) #xffffffff))))
	)))

  (labels ((get-root () (let ((buffer (acquire +user-start+)))
			  (prog1 (sb-sys:sap-ref-32 (sap buffer) 0) (release buffer))))
	   (set-root (n) (let ((buffer (ghost (acquire +user-start+))))
			   (setf (sb-sys:sap-ref-32 (sap buffer) 0) n)
			   (release buffer)))
	   (do-insert (start length target)
	     (format *loop* "~%~a:Create@~a ~a ~a" *thread* target start length)
	     (dotimes (i length target)
	       (let ((key (boole boole-ior (ash start 16) i)))
		 (setf (down-data) (scratch))
		 (system-area-ub32-fill (sq key) (scratch) 0 (leaf-u32s))
		 (system-area-ub32-fill 0 (db.bt:down-key) 0 (db.bt:key-u32s))
		 (setf (sb-sys:sap-ref-32 (db.bt:down-key) 0) key)
		 (setf target (insert (creating target))))))
	   (do-delete (start length target)
	     (format *loop* "~%~a:Delete@~a ~a ~a" *thread* target start length)
	     (dotimes (i length target)
	       (system-area-ub32-fill 0 (db.bt:down-key) 0 (db.bt:key-u32s))
	       (setf (sb-sys:sap-ref-32 (db.bt:down-key) 0) (boole boole-ior (ash start 16) i))
	       (setf target (db.bt:rubout target))))
	   (map-at (place)
	     (let ((map (acquire (+ 2 (boole boole-and #b111 place))))
		   (offset (ash place -3)))
	       (prog1 (sb-sys:sap-ref-8 (sap map) offset) (release map))))
	   (set-map-at (place nv)
	     (let ((map (ghost (acquire (+ 2 (boole boole-and #b111 place)))))
		   (offset (ash place -3)))
	       (prog1 (setf (sb-sys:sap-ref-8 (sap map) offset) nv) (release map))))
	   (choose-range ()
	     (let ((length (+ (random 8) (random 9))) ;; 0-15, Approx normal distribution, centered around 8.
		   (start (random 128))) ;; Could be upto 65536, but I want collisions.
	       (assert (<= 0 length 15))
	       (values start length)))
	   (all-present ()
	     (let ((buffers (loop for i from 0 to 7 collecting (acquire (+ 2 i)))))
	       (let ((present (loop repeat 128
				    for i upfrom 0
				    as buffer = (elt buffers (boole boole-and #b111 i))
				    as count = (sb-sys:sap-ref-8 (sap buffer) (ash i -3))
				    nconc (iota count (ash i 16)))))
		 (mapcar #'release buffers)
		 present)))
	   (rbitch (txn-n threads)
	     (let ((*txn* nil)
		   (*thread* txn-n))
	       (declare (special *txn* *thread*))
	       (btree *test-info*)
	       (loop while (some #'sb-thread:thread-alive-p threads)
		     do (sleep (random 8.0))
		     do (with-reading 
			  (let ((root (get-root))
				(added (all-present)))
			    (format *loop* "~%~a:TOURIST ~a ~a!" txn-n root (length added))
			    (when added (checktree root (first-leaf root) added)))))))
	   (bitch (txn-n) 
	     (let ((*thread* txn-n)
		   (*txn* nil))
	       (declare (special *thread* *txn*))
	       (btree *test-info*)
	       (dotimes (i 64)
		 (with-writing 
		   (btree *test-info*)
		   (multiple-value-bind (start length) (choose-range)
		     (let* ((originally (get-root))
			    (target originally))
		       (setf target (do-delete start (map-at start) target))
		       (setf target (do-insert start length target))
		       (set-map-at start length)
		       (unless (= originally target) (set-root target)))))))))
    (defun db.btree-test-sequential ()
      (let ((threads nil))
	(setf db.txn:*free* (list nil (cons (1+ db.io:+user-start+) #xffffffff)))
	(with-writing 
	  (set-root 0)
	  (dotimes (i 8)
	    (let ((map (ghost (acquire (+ i 2)))))
	      (sb-kernel:system-area-ub8-fill 0 (sap map) 0 +page-bytes+)
	      (release map))))
	
	(btree *test-info*)
	(dotimes(i 16) (push (sb-thread:make-thread (papply #'bitch i) :name (format nil "~d" i)) threads))
	(push (sb-thread:make-thread (papply #'rbitch 17 threads) :name "rbitch1") threads)
	(push (sb-thread:make-thread (papply #'rbitch 18 threads) :name "rbitch2") threads)
	(push (sb-thread:make-thread (papply #'rbitch 19 threads) :name "rbitch3") threads)
	(push (sb-thread:make-thread (papply #'rbitch 20 threads) :name "rbitch4") threads)
	(dolist (th (cddddr threads)) (sb-thread:join-thread th))
	
	(sb-thread:join-thread (first threads))
	(sb-thread:join-thread (second threads))
	(sb-thread:join-thread (third threads))
	(sb-thread:join-thread (fourth threads))
	
	(format *loop* "~%Done Writing...")
	
	;; Now delete data!
;      (setf threads nil)
;      (dotimes (i 16) (push (sb-thread:make-thread (papply #'bitch i (truncate N 16) nil)
;						   :name (format nil "~d" i)) threads))
;      (push (sb-thread:make-thread (papply #'read-bitch 17 threads) :name "rbitch1") threads)
;      (push (sb-thread:make-thread (papply #'read-bitch 18 threads) :name "rbitch2") threads)
;      (push (sb-thread:make-thread (papply #'read-bitch 19 threads) :name "rbitch3") threads)
;      (push (sb-thread:make-thread (papply #'read-bitch 20 threads) :name "rbitch4") threads)
;      (dolist (th (cddddr threads)) (sb-thread:join-thread th))
;      
;      (sb-thread:join-thread (first threads))
;      (sb-thread:join-thread (second threads))
;      (sb-thread:join-thread (third threads))
;      (sb-thread:join-thread (fourth threads))
;      (format *loop* "~%Done deleting!")
;      (assert (zerop (get-root)))
 ;     (assert (equalp db.txn:*free* (list nil (cons (1+ db.io:+user-start+) #xffffffff))))
      ))))


  
;; CLIENT TESTS

;; Whats a good test? Subscribing half a dozen entries, and then looking them up... 
;; Yeah, a list of ten names. I just start inserting them, (moving from TO-INS to DID-INS, and then check
;; em out).  Each time I check it out, I incf the client type. 
(labels ((bitch (tn added add-list)
	   (if (null (cdr add-list))
	       (format *loop* "~%~a:Done!" tn)
	       (let* ((n (random (1- (length add-list))))
		      (place (nthcdr n add-list))
		      (val (cadr place)))
		 (if (zerop (random 1)) (sb-thread:thread-yield))
		 (with-writing (apply #'db.client:subscribe val))
		 (rplacd place (cddr place))
		 (push val added)
		 ;;(format *loop* "~%Added ~a" val)
		 (with-reading
		 ;;(format *loop* "~%~a:I should check ~a" tn (length added))
		   (dolist (a added)
		     (destructuring-bind (hostname company caste) a
		       (with-client hostname
		       ;;(format *loop* "~%~a: NAME: ~s CASTE: ~a" hostname (db.client:name) (db.client:caste))
			 (assert (string-equal company (db.client:company)))
			 (assert (= caste (db.client:caste)))))))
		 (bitch tn added add-list)))))
  (defun db.client-test-multiuser ()
    (let ((threads nil))
      (with-writing
	(let ((b (ghost (acquire db.client:+super-block+))))
	  (system-area-ub32-fill 0 (sap b) 0 +page-words+)
	  (release b)))
      (setf db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff)))
      (flet ((gowith (n add-list)
	       (push (sb-thread:make-thread 
		      (lambda () (let ((*thread* n)
				  (*txn* nil))
			      (declare (special *thread* *txn*))
			      (bitch n nil (cons nil (copy-list add-list))))) :name (format nil "~d" n)) threads)))
	(gowith 0 '(("WARREN" "Warren's Company" 0)
		    ("GREG"   "Greg's Company" 0)
		    ("TOM"    "Tom's Company" 0)
		    ("PEG"    "Peg's Company" 0)
		    ("ALEX"   "Alex's Company" 0)
		    
		    ("VINNIE" "Vinnie's Company" 0)
		    ("ALANNA" "Alanna's Company" 0)
		    ("BRIAN" "Brian's Company" 0)
		    ("CHRIS" "Chris' Company" 0)
		    ("DOM" "Dom's Company" 0)
		    
		    ("JESSE" "Jesse's Company" 0)
		    ("MARK" "Mark's Company" 0)
		    ("KRYSTAL" "Krystal's Company" 0)
		    ("TYSON"   "Tyson's Company" 0)
		    ("MIKE"    "Mike's Company" 0)))
	(gowith 1 '(("BM"     "Black & McDonald" 0)
		    ("TW"     "Tom Wilkinson Second site" 0)
		    ("INS"    "I&N&S Industry" 0)
		    ("HMV"    "Heavy Metal Vehicles" 0)
		    ("GGN"    "Geographic Gieger Network" 0)))
	(gowith 2 '(("A"      "Aaaaa" 0)
		    ("B"      "Baaaa" 0)
		    ("C"      "Caaaa" 0)
		    ("D"      "Daaaa" 0)
		    ("E"      "Eaaaa" 0)
		    ("F"      "Faaaa" 0)
		    ("G"      "Gaaaa" 0)
		    ("H"      "Haaaa" 0)
		    ("I"      "Iaaaa" 0)
		    ("J"      "Jaaaa" 0)
		    ("K"      "Kaaaa" 0)
		    ("L"      "Laaaa" 0)
		    ("M"      "Maaaa" 0)))
	(gowith 3 '(("A1"      "Aaaaa" 0)
		    ("A2"      "Baaaa" 0)
		    ("A3"      "Caaaa" 0)
		    ("A4"      "Daaaa" 0)
		    ("A5"      "Eaaaa" 0)
		    ("A6"      "Aaaaa" 0)
		    ("A7"      "Baaaa" 0)
		    ("A8"      "Caaaa" 0)
		    ("A9"      "Daaaa" 0)
		    ("B1"      "Eaaaa" 0)))
	(mapcar #'sb-thread:join-thread threads)
	(format *loop* "~%ALLDONE!")
	))))

;; DIRECTORY TESTS

;; This test  creates many many directory entries with random sized contents. Then it repeats resizing everything,
;; and then it goes again and deletes everything.  

;(defun update! (updater) (setf (updated) (2010now) (updater) updater))

(labels ((verify (name title desc parent oid updater)
	   (with-reading
	     (with-client "WARREN"
	       (let ((cursorb (progn (pair oid parent) (db.dir:entry-key) (lookup (db.client:directory))))
		     (had-error nil))
		 (unless (found)
		   ;(setf (sb-sys:sap-ref-64 (parent+oid) 0) (sb-sys:sap-ref-64 (origin-key) 0)
		   ;(sb-sys:sap-ref-64 (parent+oid) 8) (sb-sys:sap-ref-64 (origin-key) 8))
		   (multiple-value-bind (gotp goto) (unpair) 
		     (format *loop* "~%Missing ~a ~a (got ~a ~a: ~a ~a)." oid parent
			     (sb-sys:sap-ref-64 (origin-key) 0)
			     (sb-sys:sap-ref-64 (origin-key) 8) gotp goto))
		   (error "~%Missing ~a ~a (got ~a ~a)." oid parent
			  (sb-sys:sap-ref-64 (origin-key) 0)
			  (sb-sys:sap-ref-64 (origin-key) 8)))
		 (let ((metadata (db.metadata:read-metadata)))
		   (unless (and (eq (db.metadata:owner metadata) parent)    (eq (db.metadata:creator metadata) oid)
				(eq (db.metadata:updater metadata) updater) (eq (db.metadata:metatype metadata) oid))
		   (format *loop* "~%Key: ~a ~a" (sb-sys:sap-ref-64 (origin-key) 0)
			   (sb-sys:sap-ref-64 (origin-key) 8))
		   (format *loop* "~%Enc: ~a ~a" (sb-sys:sap-ref-64 (down-key) 0) (sb-sys:sap-ref-64 (down-key) 8))
		   (multiple-value-bind (parent oid) (unpair)
		     (format *loop* "~%KEY parent: ~a oid: ~a" parent oid))
		 
;		   (format *loop* "~%BUFFER: ~a, AT: ~a, KEY: ~a, DATA: ~a~%(datum: ~a, size: ~a, expt: ~a ~a)"
;			   cursorb (at) (origin-key) (origin-data) (datum cursorb) (data-u8s)
;			   (sb-sys:sap+ (sb-sys:sap+ (sap cursorb) 8) (* (key-u8s) (at)))
;			   (sb-sys:sap+ (datum cursorb) (* (data-u8s) (at))))
	       
		   (format *loop* "~%OWNER: ~a, CREATOR: ~a, UPDATER: ~a, METATYPE: ~a"
			   (db.metadata:owner metadata) (db.metadata:creator metadata)
			   (db.metadata:updater metadata) (db.metadata:metatype metadata))
		   (format *loop* "%~%EXPT: ~a ~a ~a ~a" parent oid updater oid)
		   (setf had-error t)))
		 (setf cursorb (db.cursor:next cursorb))
		 (multiple-value-bind (string cb) (db.dir:name@ cursorb)
		   (setf cursorb cb)
		   (unless (string-equal name string)
		     (format *loop* "~%NAME: ~a~%EXPT: ~a" string name)
		     (setf had-error t)))
		 (multiple-value-bind (string cb) (db.dir:title@ cursorb)
		   (setf cursorb cb)
		   (unless (string-equal title string)
		     (format *loop* "~%TITLE: ~a~% EXPT: ~a" string name)
		     (setf had-error t)))
		 (multiple-value-bind (string cb) (db.dir:description@ cursorb)
		   (setf cursorb cb)
		   (unless (string-equal desc string)
		     (format *loop* "~%DESC: ~a~%EXPT: ~a" string desc)
		     (setf had-error t)))
		 (when had-error (error "There was some kind of verify error."))
		 (when cursorb (release cursorb))))))
	 (ridmeof (tn parent oid)
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (if (zerop (random 1)) (sb-thread:thread-yield))
	     (format *loop* "~%Remove ~d --> ~d" parent oid)
	     (with-writing
	       (with-client "WARREN"
		 (pair oid parent)
		 (with-directory-tree (dir)
		   (db.dir:entry-key)
		   (let ((buffer (lookup dir))) (unless (found) (error "Missing DIR for ~a ~a" parent oid)) (release buffer))
		   (setf dir (db.dir:delete-entry dir)))))))
	 (modi (tn parent oid)
	   "Changes the size and contents of everything."
	   (let ((*txn* nil)
		 (*thread* tn)
		 (nname  (make-string (* 8 oid) :initial-element (code-char (+ 48 (mod (+ oid 6)  10)))))
		 (ntitle (make-string (mod (* 2 (+ 30 oid)) 60) :initial-element (code-char (+ 48 (mod (+ oid 7) 10)))))
		 (ndesc  (make-string (* 1 oid) :initial-element (code-char (+ 48 (mod (+ oid 8) 10))))))
	     (declare (special *txn* *thread*))
	     (if (zerop (random 1)) (sb-thread:thread-yield))
	     (format *loop* "~%Modify ~d --> ~d" parent oid)
	     (with-writing
	       (with-client "WARREN"
		 (pair oid parent)
		 (db.dir:entry-key)
		 (with-directory-tree (dir)
		   (let ((buffer (lookup dir)))
		     (unless (found) (error "Missing meta data for ~a ~a" parent oid))
		     (setf buffer (db.cursor:ghost-cursor buffer))
		     
		     ;; This is an unusual opp...
		     ;;(update! 9999) ;; Set the updater to 9999
		     (setf (meta-raw +updater+) 9999)

		     (release buffer))
		   (setf dir (db.dir:name! dir nname)
			 dir (db.dir:title! dir ntitle)
			 dir (db.dir:description! dir ndesc)))))
	     (verify nname ntitle ndesc parent oid 9999)))
	 (bitch (tn parent oid)
	   (let ((*txn* nil)
		 (*thread* tn)
		 (name  (make-string oid :initial-element (code-char (+ 48 (mod oid 10)))))
		 (title (make-string (mod (* oid 2) 60) :initial-element (code-char (+ 48 (mod (+ oid 1) 10)))))
		 (desc  (make-string (* 8 oid) :initial-element (code-char (+ 48 (mod (+ oid 2) 10))))))
	     (declare (special *txn* *thread*))
	     (if (zerop (random 1)) (sb-thread:thread-yield))
	     (format *loop* "~%Insert ~d --> ~d" parent oid)
	     (with-writing
	       (with-client "WARREN"
		 (with-directory-tree (dir)
		   ;; CREATER, METATYPE = = OID, OWNER = PARENT
		   (setf dir (db.dir:make-entry dir oid parent (db.metadata:make-metadata oid 0 oid parent 0 oid))
			 ;;parent oid oid)
			 dir (db.dir:name! dir name)
			 dir (db.dir:title! dir title)
			 dir (db.dir:description! dir desc)))))
	     (verify name title desc parent oid oid))))
  (defun db.dir-test-multiuser ()
    ;; Insert as a binary tree... each thread just inserting one of the next level. 
    ;; string sizes id, id*2, id*8.  Then I'll go through and change them slighly. Then I'll go through
    ;; and delete them all. 
    ;; This test is REALLY slow because of all the thread creation... 
    (let ((*txn* nil)
	  (*thread* 0))
      (declare (special *txn* *thread*))
      (setf db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff)))
      (with-writing
	(let ((b (ghost (acquire db.client:+super-block+))))
	  (system-area-ub32-fill 0 (sap b) 0 +page-words+)
	  (release b))
	(db.client:subscribe "WARREN" "Warren" 0)))

    (let ((offset 0)
	  (starting-free (cons nil (mapcar #'(lambda (a) (cons (car a) (cdr a))) (cdr db.txn:*free*))))
	  (threads nil))
      (flet ((ins (tn parent oid)
	       (push (sb-thread:make-thread (papply #'bitch tn parent oid)  :name (format nil "~d" tn)) threads))
	     ;(ins (tn parent oid) (bitch tn parent oid))
	     (futz (tn parent oid)
	       (push (sb-thread:make-thread (papply #'modi tn parent oid) :name (format nil "~d" tn)) threads))
	     (ridof (tn parent oid)
	       (push (sb-thread:make-thread (papply #'ridmeof tn parent oid) :name (format nil "~d" tn)) threads)))
	;(ridof (tn parent oid) (ridmeof tn parent oid)))

	(dotimes (height 8)
	  (if (zerop height)
	      (ins 0 0 0)
	      (let ((parent-offset (1- (expt 2 (1- height)))))
		(dotimes (breadth (expt 2 height))
		  (when (zerop (mod breadth 32))
		    (format *loop* "~%Waiting for threads...")
		    (mapcar #'sb-thread:join-thread threads)
		    (setf threads nil))
		  (let ((parent (+ parent-offset (truncate breadth 2)))
			(n (+ breadth offset)))
		    (ins (mod breadth 32) parent n)))))
	  (incf offset (expt 2 height)))

	(format *loop* "~%Done Inserting! Wait for threads...")
	(mapcar #'sb-thread:join-thread threads)

	(setf offset 0)
	(dotimes (height 8)
	  (if (zerop height)
	      (futz 0 0 0)
	      (let ((parent-offset (1- (expt 2 (1- height)))))
		(dotimes (breadth (expt 2 height))
		  (when (zerop (mod breadth 32))
		    (format *loop* "~%Waiting for threads...")
		    (mapcar #'sb-thread:join-thread threads)
		    (setf threads nil))
		  (let ((parent (+ parent-offset (truncate breadth 2)))
			(n (+ breadth offset)))
		    (futz (mod breadth 32) parent n)))))
	  (incf offset (expt 2 height)))

	(format *loop* "~%Done Updating! Wait for threads...")
	(mapcar #'sb-thread:join-thread threads)
	
	(setf offset 0)
	(dotimes (height 8)
	  (if (zerop height)
	      (ridof 0 0 0)
	      (let ((parent-offset (1- (expt 2 (1- height)))))
		(dotimes (breadth (expt 2 height))
		  (when (zerop (mod breadth 32))
		    (format *loop* "~%Waiting for threads...")
		    (mapcar #'sb-thread:join-thread threads)
		    (setf threads nil))
		  (let ((parent (+ parent-offset (truncate breadth 2)))
			(n (+ breadth offset)))
		    (ridof (mod breadth 32) parent n)))))
	  (incf offset (expt 2 height)))
	)

      (format *loop* "~%Done, await threads...")
      (mapcar #'sb-thread:join-thread threads)
      
      (with-reading (with-client "WARREN" (or (zerop (db.client:directory)) (error "Non zerop dir!"))))
      ;(format *loop* "~%At the end, freespace is: ~a~%(was: ~a)" db.txn:*free* starting-free)
      (assert (equalp db.txn:*free* starting-free))
      )))

;; Page tests

;; This test creates directory entries for pages.  It records that its done this in some junk pages, so that
;; it can be random operations. It works on 4 clients simaltaniosuly.

;; Each junk page stores   PARENT, BASELETTER, PAGEbytes, Namebytes, Titlebytes, Descbytes. 
;; Parent is 8bit, 8bit, 16bit, 8bit, 8bit, 16bit) (so 8 bytes).  An OID can be up to 128.


(defun choose-client () (random 3))   ;; Must return values from 0-2, or test-acl has an infinite loop.
(defun client-map (client) (1+ client))
(defun client-name (n) (ecase n (0 "FIRST") (1 "SECOND") (2 "THIRD")))
(defun data-map (client offset) (assert (< offset 3)) (+ 4 (* client 3) offset))
(defun ascii-char (n) (code-char (+ 65 (mod n 58))))
(defun find-a-parent (sap start)
  (loop repeat 128
        for pos = start then (boole boole-and (1+ pos) #x7F) ;; Limit to 128
        do (unless (zerop (sb-sys:sap-ref-64 sap (ash pos 3))) (return-from find-a-parent (1+ pos)))
        finally (return 0)))
(defun make-data (mbuffer oid parent)
  ;; I must update the map @ oid to hold the size of the strings and characters I'm returning.
  (let ((base-char (random 58))
	(page-data (random 1200))
	(name-data (random 32))
	(title-data (random 64))
	(desc-data (random 150))
	(sap (sb-sys:sap+ (sap mbuffer) (ash (1- oid) 3))))
    ;;(format *loop* "~%VS: ~a ~a ~a ~a ~a" base-char page-data name-data title-data desc-data)
    (setf (sb-sys:sap-ref-8 sap 0) parent
	  (sb-sys:sap-ref-8 sap 1) base-char
	  (sb-sys:sap-ref-16 sap 2) page-data
	  (sb-sys:sap-ref-8 sap 4) name-data
	  (sb-sys:sap-ref-8 sap 5) title-data
	  (sb-sys:sap-ref-16 sap 6) desc-data)
    (values (make-string page-data :initial-element (ascii-char base-char))
	    (make-string name-data :initial-element (ascii-char (+ 1 base-char)))
	    (make-string title-data :initial-element (ascii-char (+ 2 base-char)))
	    (make-string desc-data :initial-element (ascii-char (+ 3 base-char))))))

(defun assert-dir (oid parent etype nexpt texpt dexpt)
  (pair oid parent) (db.dir:entry-key)
  (let ((buffer (lookup (db.client:directory))))
    (unless (found)
      (multiple-value-bind (p o) (unpair)
	(format *loop* "~%Didn't find directory ~a->~a, found ~a->~a" parent oid p o)
	(error "Couldn't find directory ~a->~a... found ~a->~a" parent oid p o)))
    (let ((md (db.metadata:read-metadata)))
      (unless (= (db.metadata:metatype md) etype)
	(error "directory ~a->~a has bad metatype ~a, expected ~a." parent oid (db.metadata:metatype md) etype))
      (unless (= (db.metadata:owner md) oid)   (error "dir ~a->~a has bad owner: ~a" parent oid (db.metadata:owner md)))
      (unless (= (db.metadata:creator md) oid) (error "dir ~a->~a has bad creator: ~a" parent oid (db.metadata:creator md)))
      (unless (= (db.metadata:updater md) oid) (error "dir ~a->~a has bad updater: ~a" parent oid (db.metadata:updater md))))
    (format *loop* "~%Meta data checks out...")
    (setf buffer (db.cursor:next buffer))
    (multiple-value-bind (nstr buffer) (db.dir:name@ buffer)
      (multiple-value-bind (tstr buffer) (db.dir:title@ buffer)
	(multiple-value-bind (dstr buffer) (db.dir:description@ buffer)
	  (unless (string-equal nstr nexpt) (error "dir ~a->~a bad name: ~a~%expt: ~a" parent oid nstr nexpt))
	  (unless (string-equal tstr texpt) (error "dir ~a->~a bad title: ~a~%expt: ~a"parent oid tstr texpt))
	  (unless (string-equal dstr dexpt) (error "dir ~a->~a bad desc: ~a~%expt: ~a" parent oid dstr dexpt))
	  (when buffer (release buffer)))))))

(defun assert-page (oid pexpt)
  (oid oid) (db.resources:rz-start)
  (let ((buffer (lookup (db.client:page))))
    (unless (found)
      (format *loop* "~%Didn't find page ~a." oid)
      (error "Couldn't find page ~a" oid))
    (multiple-value-bind (pstr buffer) (db.resources:sexp@ buffer)
      (unless (string-equal pstr pexpt) (error "~a bad page: ~a~%expt: ~a" oid pstr pexpt))
      (when buffer (release buffer)))))

(defun assert-view (oid vexpt)
  (oid oid) (db.resources:rz-start)
  (let ((buffer (lookup (db.client:content))))
    (unless (found)
      (format *loop* "~%Didn't find view ~a." oid)
      (error "Couldn't find view ~a." oid))
    (multiple-value-bind (vstr buffer) (db.resources:sexp@ buffer)
      (unless (string-equal vstr vexpt) (error "~a bad view: ~a~%expt: ~a" oid vstr vexpt))
      (when buffer (release buffer)))))

(defun assert-doc (oid dexpt)
  (oid oid) (db.resources:rz-start)
  (let ((buffer (lookup (db.archives:cartulary))))
    (unless (found)
      (format *loop* "~%Didn't find document ~a." oid)
      (error "Couldn't find document ~a." oid))
    (multiple-value-bind (metadata dstr buffer) (db.resources:unpack-document buffer)
      (assert (= +document+ (metatype metadata))) ;; I actually store length... but I add the type back when I unpack
      (assert (= (created metadata) 0))
      (assert (= (creator metadata) oid))
      (assert (= (owner metadata) oid))
      (assert (= (updated metadata) 0))
      (assert (= (updater metadata) oid))
      (unless (string-equal dstr dexpt) (error "~a bad doc: ~a~%expt: ~a" oid dstr dexpt))
      (when buffer (release buffer)))))

(labels ((rbitch (tn threads) 
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (loop while (some #'sb-thread:thread-alive-p threads)
		   do (sleep (random 2.0))
		;;do (if (zerop (random 1)) (sb-thread:thread-yield))
;		   do (sb-thread:thread-yield)
		   do (with-reading
;			(format *loop* "~%~a:VERIFY..." *thread*)
			(let* ((client (choose-client))
			       (map (acquire (client-map client))))
			  (with-client (client-name client)
			    ;; How can I test this one easily? I should just iterate and test each one... 
			    ;; has its parent.. 
			    ;;(format *loop* "~%~a:Verify..." *thread*)
			    (loop repeat 128
			          for start = 0 then (1+ start)
			          unless (zerop (sb-sys:sap-ref-64 (sap map) (ash start 3)))
			          do (let* ((sap (sb-sys:sap+ (sap map) (ash start 3)))
					    (parent (sb-sys:sap-ref-8  sap 0))
					    (base   (sb-sys:sap-ref-8  sap 1))
					    (page   (sb-sys:sap-ref-16 sap 2))
					    (name   (sb-sys:sap-ref-8  sap 4))
					    (title  (sb-sys:sap-ref-8  sap 5))
					    (desc   (sb-sys:sap-ref-16 sap 6))
					    (oid (1+ start)))
;					    (error-p nil))
				       (let ((nexpt (make-string name :initial-element (ascii-char (+ 1 base))))
					     (texpt (make-string title :initial-element (ascii-char (+ 2 base))))
					     (dexpt (make-string desc  :initial-element (ascii-char (+ 3 base)))))
					 (assert-dir oid parent 0 nexpt texpt dexpt))
				       (let ((pexpt (make-string page :initial-element (ascii-char base))))
					 (assert-page oid pexpt)))))
			  (release map))))))
	 (do-operation (mbuffer oid &aux (parent (sb-sys:sap-ref-8 (sap mbuffer) (ash (1- oid) 3))))
;;	   (format *loop* "~%MOD ~a->~a" parent oid)
	   (multiple-value-bind (page-data name title desc) (make-data mbuffer oid parent)
	     ;(format *loop* "~%OP ~a (p: ~a) (~a ~a ~a ~a)" (1+ oid) parent (length name) (length title) (length desc)
	     ;(length page-data))
	     (with-directory-tree (dir)
	       (pair oid parent)
	       (setf dir (db.dir:name! dir name))
	       (setf dir (db.dir:title! dir title))
	       (setf dir (db.dir:description! dir desc)))
	     (with-page-tree (page) (oid oid) (setf page (db.resources:sexp! page page-data)))))
	 (do-delete (mbuffer oid &aux (parent (sb-sys:sap-ref-8 (sap mbuffer) (ash (1- oid) 3))))
	   (let ((toupdate (loop repeat 128
			         for i upfrom 0
			         when (= oid (sb-sys:sap-ref-8 (sap mbuffer) (ash i 3)))
			         do (setf (sb-sys:sap-ref-8 (sap mbuffer) (ash i 3)) parent)
			         and collect (1+ i))))
	     (format *loop* "~%DEL ~a->~a (~a)" parent oid toupdate)
	     (setf (sb-sys:sap-ref-64 (sap mbuffer) (ash (1- oid) 3)) 0)
	     (with-directory-tree (dir)
	       (pair oid parent)
	       (setf dir (db.dir:delete-entry dir))
	       (dolist (upd toupdate)
		 (pair upd oid)
		 (format *loop* "~%MV: ~a-->~a  ~a-->~a" oid upd parent upd)
		 (setf dir (db.dir:move dir parent))))
	     (with-page-tree (page) (oid oid) (setf page (db.resources:delete-rz page)))))
	 (do-create (mbuffer oid parent)
	   (multiple-value-bind (page-data name title desc) (make-data mbuffer oid parent)
	     (with-directory-tree (dir)
	       (setf dir (db.dir:make-entry dir oid parent (make-metadata 0 0 oid oid 0 oid))) ;; oid parent oid oid 0))
	       (setf dir (db.dir:name! dir name))
	       (setf dir (db.dir:title! dir title))
	       (setf dir (db.dir:description! dir desc)))
	     (with-page-tree (page)
	       (setf page (db.resources:make-rz page oid))
	       (setf page (db.resources:sexp! page page-data)))))
	 (delbitch (tn)
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (tagbody :again
;		(format *loop* "~%dbitch start over...")
;		(with-writing 
		(begin)
		:start	       
		(handler-case (progn (let* ((client (choose-client))
					    (mbuffer (ghost (acquire (client-map client))))
					    (oid (find-a-parent (sap mbuffer) (random 128))))
				       (with-client (client-name client)
					 (unless (zerop oid) (do-delete mbuffer oid))
					 (release mbuffer)))
				     (end))
		  (reset () (reopen) (go :start)))
		(tourist)
		(dotimes (i 3)
		  (let ((mbuffer (acquire (client-map i))))
		    (unless (zerop (find-a-parent (sap mbuffer) 0)) (release mbuffer) (endtourist) (go :again))
		    (release mbuffer)))
		(endtourist))))
	 (bitch (tn)
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (dotimes (i 24)
	       ;; How do I decide the operation? 
	       ;; PICK a client. 
	       ;; PICK an OID? 
	       ;; Does it exist? NO: Create. (Pick something that exists to be my parent)  Yes, Randomly update or delete. 
	       ;; IF I delete, find all children of me, and update them to be my parent.
	       (with-writing
		 (let* ((client (choose-client))
			(mbuffer (ghost (acquire (client-map client))))
			(place (random 128)))
		   (with-client (client-name client)
		     (if (zerop (sb-sys:sap-ref-64 (sap mbuffer) (ash place 3)))
			 (do-create mbuffer (1+ place) (find-a-parent (sap mbuffer) place))
			 (if (zerop (random 6))
			     (do-delete mbuffer (1+ place))
			     (do-operation mbuffer (1+ place)))))
		   (release mbuffer)))))))
  (defun db.resources-test-multiuser ()
    (setf db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff)))
    (let ((*txn* nil)
	  (*thread* 0))
      (declare (special *txn* *thread*))
      (with-writing
	(let ((b (ghost (acquire db.client:+super-block+))))
	  (system-area-ub32-fill 0 (sap b) 0 +page-words+)
	  (release b))
	(dotimes (i 4)
	  (let ((b (ghost (acquire (client-map i)))))
	    (system-area-ub32-fill 0 (sap b) 0 +page-words+)
	    (release b)))
	(db.client:subscribe "FIRST" "Client 0" 0)
	(db.client:subscribe "SECOND" "Client 1" 0)
	(db.client:subscribe "THIRD" "Client 2" 0)))

    (let ((orig-free (cons nil (mapcar #'(lambda (a) (cons (car a) (cdr a))) (cdr db.txn:*free*))))
	  (wthreads)
	  (rthreads))
      (dotimes (i 28) (push (sb-thread:make-thread (papply #'bitch i) :name (format nil "~d" i)) wthreads))
      (dotimes (i 2)  (push (sb-thread:make-thread (papply #'rbitch (+ i 28) wthreads) :name (format nil "R~d" i)) rthreads))
      (mapcar #'sb-thread:join-thread wthreads)
      (mapcar #'sb-thread:join-thread rthreads)
      (format *loop* "~%Done operations...")

      (setf wthreads nil rthreads nil)
      (dotimes (i 28) (push (sb-thread:make-thread (papply #'delbitch i) :name (format nil "D~d" i)) wthreads))
      (dotimes (i 4) (push (sb-thread:make-thread (papply #'rbitch (+ i 28) wthreads) :name (format nil "R~d" i)) rthreads))
      (mapcar #'sb-thread:join-thread wthreads)
      (mapcar #'sb-thread:join-thread rthreads)
      (format *loop* "~%done deletions, FS: ~a" db.txn:*free*)
      (assert (equalp db.txn:*free* orig-free))

      (let ((*txn* nil)
	    (*thread* 0))
	(declare (special *txn* *thread*))
	(with-reading
	  (dotimes (i 3)
	    (with-client (client-name i)
	      (assert (zerop (db.client:directory)))
	      (assert (zerop (db.client:page))))))))))

;; Everything test

;; This test creates directory entries for pages and then documents for certain pages via random operations.
;; It works on 2 clients simaltaniosuly.

(labels ((rbitch (tn threads) 
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (loop while (some #'sb-thread:thread-alive-p threads)
		   do (sleep (random 2.0))
		;;do (if (zerop (random 1)) (sb-thread:thread-yield))
;		   do (sb-thread:thread-yield)
		   do (with-reading
			(let ((client (choose-client)))
			  (let ((map (acquire (client-map client))))
			    (with-client (client-name client)
			      (loop repeat 128
				    for start = 0 then (1+ start)
				    unless (zerop (sb-sys:sap-ref-64 (sap map) (ash start 3)))
				    do (let* ((sap (sb-sys:sap+ (sap map) (ash start 3)))
					      (parent (sb-sys:sap-ref-8  sap 0))
					      (base   (sb-sys:sap-ref-8  sap 1))
					      (page   (sb-sys:sap-ref-16 sap 2))
					      (name   (sb-sys:sap-ref-8  sap 4))
					      (title  (sb-sys:sap-ref-8  sap 5))
					      (desc   (sb-sys:sap-ref-16 sap 6))
					      (oid (1+ start)))
					 ;;(format *loop* "~%VERIFY ~a->~a" parent oid)
					 (let ((nexpt (make-string name :initial-element (ascii-char (+ 1 base))))
					       (texpt (make-string title :initial-element (ascii-char (+ 2 base))))
					       (dexpt (make-string desc  :initial-element (ascii-char (+ 3 base)))))
					   (assert-dir oid parent (if (< oid 64) 0 1) nexpt texpt dexpt))
					 (let ((expt (make-string page :initial-element (ascii-char base))))
					   (if (< oid 64) (assert-page oid expt) (assert-view oid expt))))))
			    (release map))
			  (dotimes (i 3)
			    (let ((dmap (acquire (data-map client i))))
			      (db.archives:open-archives (1+ i))
			      (loop repeat +page-bytes+
				    for doid = 0 then (1+ doid)
				    unless (zerop (sb-sys:sap-ref-8 (sap dmap) doid))
				    do (let* ((len (sb-sys:sap-ref-8 (sap dmap) doid))
					      (expt (make-string len :initial-element (ascii-char (+ doid len)))))
					 (assert-doc doid expt)))
			      (db.archives:close-archives)
			      (release dmap))))))))
	 (delete-dir (oid mbuffer)
	   (let* ((parent (sb-sys:sap-ref-8 (sap mbuffer) (ash (1- oid) 3)))
		  (toupdate (loop repeat 128
			       for i upfrom 0
			       when (= oid (sb-sys:sap-ref-8 (sap mbuffer) (ash i 3)))
			       do (setf (sb-sys:sap-ref-8 (sap mbuffer) (ash i 3)) parent)
			       and collect (1+ i))))
	     (format *loop* "~%DEL ~a->~a (~a)" parent oid toupdate)
	     (setf (sb-sys:sap-ref-64 (sap mbuffer) (ash (1- oid) 3)) 0)
	     (with-directory-tree (dir)
	       ;(let* ((originally (db.client:directory))
	       ;(target originally))
	       (pair oid parent)
	       (setf dir (db.dir:delete-entry dir))
	       (dolist (upd toupdate)
		 (format *loop* "~%MV: ~a-->~a  ~a-->~a" oid upd parent upd)
		 (pair upd oid)
		 (setf dir (db.dir:move dir parent))))))
	 (del-page (oid mbuffer)
	   (format *loop* "~%Delete page ~a" oid)
	   (delete-dir oid mbuffer)
	   (with-page-tree (page)
	     (format *loop* " PageData(@~d)" page)
	     (oid oid) (setf page (db.resources:delete-rz page))))
	 (update-page (oid mbuffer &aux (parent (sb-sys:sap-ref-8 (sap mbuffer) (ash (1- oid) 3))))
	   (multiple-value-bind (pd nd td dd) (make-data mbuffer oid parent)
	     (format *loop* "~%Update Page ~a->~a." parent oid)
     	     (let* ((originally (db.client:directory))
		    (target originally))
	       (pair oid parent)
	       (setf target (db.dir:name! target nd))
	       (setf target (db.dir:title! target td))
	       (setf target (db.dir:description! target dd))
	       (unless (= originally target)
		 (db.client:ghost-client)
		 (setf (db.client:directory) target)))
	     (let* ((originally (db.client:page))
		    (target originally))
	       (oid oid)
	       (setf target (db.resources:sexp! target pd))
	       (unless (= originally target)
		 (db.client:ghost-client)
		 (setf (db.client:page) target)))))
	 (create-page (oid mbuffer)
	   (let ((parent (find-a-parent (sap mbuffer) oid)))
	     (multiple-value-bind (pd nd td dd) (make-data mbuffer oid parent)
	       (with-directory-tree (dir)
		 ;(let* ((originally (db.client:directory))
		 (format *loop* "~%CreatePage(@~d): ~a->~a." dir parent oid)
		 (setf dir (db.dir:make-entry dir oid parent (make-metadata 0 0 oid oid 0 oid))) ;; oid parent oid oid 0))
		 (setf dir (db.dir:name! dir nd))
		 (setf dir (db.dir:title! dir td))
		 (setf dir (db.dir:description! dir dd)))
;		 (unless (= originally target)
;		   (db.client:ghost-client)
;		   (setf (db.client:directory) target)))
	       ;;(format *loop* "~%Write: ~a~[~;(~a)~]" (length page-data) (length page-data) (elt page-data 0))
	       (with-page-tree (page)
		 ;(let* ((originally (db.client:page))
		 ;(target originally))
		 (format *loop* " PageData(@~d)" page)
		 (setf page (db.resources:make-rz page oid))
		 (setf page (db.resources:sexp! page pd))))))
;		 (unless (= originally target)
;		   (db.client:ghost-client)
;		   (setf (db.client:page) target))))))
	 (del-view (oid mbuffer)
	   (format *loop* "~%Delete view ~a" oid)
	   (delete-dir oid mbuffer)
	   (with-content-tree (view)
	     (format *loop* " ViewData(@~d)" view)
	     (oid oid) (setf view (db.resources:delete-rz view))))
	     ;(let* ((originally (db.client:view))

;	     (unless (= originally target)
;	       (db.client:ghost-client)
;	       (setf (db.client:view) target))))
	 (update-view (oid mbuffer &aux (parent (sb-sys:sap-ref-8 (sap mbuffer) (ash (1- oid) 3))))
	   (multiple-value-bind (vd nd td dd) (make-data mbuffer oid parent)
	     (format *loop* "~%Update View ~a ~a" oid (length vd))
     	     (let* ((originally (db.client:directory))
		    (target originally))
	       (pair oid parent)
	       (setf target (db.dir:name! target nd))
	       (setf target (db.dir:title! target td))
	       (setf target (db.dir:description! target dd))
	       (unless (= originally target)
		 (db.client:ghost-client)
		 (setf (db.client:directory) target)))
	     (let* ((originally (db.client:content))
		    (target originally))
	       (oid oid)
	       (setf target (db.resources:sexp! target vd))
	       (unless (= originally target)
		 (db.client:ghost-client)
		 (setf (db.client:content) target)))))
	 (create-view (oid mbuffer)
	   (let ((parent (find-a-parent (sap mbuffer) oid)))
	     (multiple-value-bind (vd nd td dd) (make-data mbuffer oid parent)
	       (with-directory-tree (dir)
		 ;(let* ((originally (db.client:directory))
		 (format *loop* "~%CreateView(@~d): ~a->~a." dir parent oid)
		 (setf dir (db.dir:make-entry dir oid parent  (make-metadata 1 0 oid oid 0 oid))) ;; oid parent oid oid 1))
		 (setf dir (db.dir:name! dir nd))
		 (setf dir (db.dir:title! dir td))
		 (setf dir (db.dir:description! dir dd)))
;		 (unless (= originally target)
;		   (db.client:ghost-client)
;		   (setf (db.client:directory) target)))
	       (with-content-tree (view)
;	       (let* ((originally (db.client:view))
;		      (target originally))
		 (format *loop* " ViewData(@~d)" view)
		 (setf view (db.resources:make-rz view oid))
		 (setf view (db.resources:sexp! view vd))))))
;		 (unless (= originally target)
;		   (db.client:ghost-client)
;		   (setf (db.client:view) target))))))
	 (doc-add (dbuffer doid)
	   (let ((length (+ 2 (random 253)))) ;; it'll have two quotes... 
	     (setf (sb-sys:sap-ref-8 (sap dbuffer) doid) length)
	     (let* ((originally (db.archives:cartulary))
		    (target originally))
	       (format *loop* " ~a ADD document ~a (~a)" target doid length)
	       ;;(error "No sexp40 anymore, nor a make-document!")
	       (setf target (make-rz target doid))
	       (setf target (rz! target (pack-document
					 (db.metadata:make-metadata +document+ 0 doid doid 0 doid)
					 (make-string length :initial-element (ascii-char (+ length doid))))))
	       (unless (= originally target)
		 (setf (db.archives:cartulary) target)))))
	 (doc-mod (dbuffer doid)
	   (let ((length (+ 2 (random 253)))) ;; it'll have two quotes... 
	     (setf (sb-sys:sap-ref-8 (sap dbuffer) doid) length)
	     (let* ((originally (db.archives:cartulary))
		    (target originally))
	       (format *loop* " ~a MOD document ~a (~a)" target doid length)
	       (oid doid)
	       (setf target (rz! target (pack-document
					 (db.metadata:make-metadata +document+ 0 doid doid 0 doid)
					 (make-string length :initial-element (ascii-char (+ length doid))))))
	       ;;(setf target (sexp40! target (make-string length :initial-element (ascii-char (+ length doid)))))
	       ;;(error "No sexp40 anymore!")
	       (unless (= originally target)
		 (setf (db.archives:cartulary) target)))))
	 (doc-del (dbuffer doid)
	   (setf (sb-sys:sap-ref-8 (sap dbuffer) doid) 0)
	   (let* ((originally (db.archives:cartulary))
		  (target originally))
	     (format *loop* " ~a DEL document ~a" target doid)
	     (oid doid) (setf target (delete-rz target))
	     (unless (= originally target)
	       (setf (db.archives:cartulary) target))))
	 (do-doc-op (client oid)
;	   (format *loop* "doc-op-skipped.") (return-from do-doc-op nil)
	   (let ((dbuffer (ghost (acquire (data-map client (1- oid))))))
	     ;; What goes into data? 8bit lengths? 
	     (format *loop* "~%Cartulary ~a" oid)
	     (db.archives:open-archives oid)
	     (db.archives:ghost-archives)
	     ;; (format *loop* " doid: ~a," (db.archives:next-doid))
	     (if (and (< 0 (db.archives:next-doid) +page-bytes+) (zerop (random 2)))
		 (let ((doid (random (db.archives:next-doid))))
		   (if (zerop (sb-sys:sap-ref-8 (sap dbuffer) doid))
		       (doc-add dbuffer doid)
		       (if (zerop (random 3))
			   (doc-del dbuffer doid)
			   (doc-mod dbuffer doid))))
		 (doc-add dbuffer (db.archives:gen-doid)))
	     (format *loop* " next doid is ~a." (db.archives:next-doid))
	     (db.archives:write-archives)
	     ;;(format *loop* "~%Done close...")
	     (release dbuffer)))
	 (do-doc (client n)
	   (let ((oid (1+ n))
		 (mbuffer (acquire (client-map  client))))
	     (if (zerop (sb-sys:sap-ref-64 (sap mbuffer) (ash (1- oid) 3)))
		 (progn (setf mbuffer (ghost mbuffer)) (prog1 (create-page oid mbuffer) (release mbuffer)))
		 (progn (release mbuffer) (do-doc-op client oid)))))
	 (do-data (client)
	   (let* ((n (random 128))
		  (oid (1+ n))
		  (data-p (< oid 4))
		  (mbuffer (ghost (acquire (client-map  client))))
		  (blank-p (zerop (sb-sys:sap-ref-64 (sap mbuffer) (ash n 3))))
		  (view-p (not (< oid 64)))
		  (delete-p (and (not blank-p) (zerop (random 6)))))
	     (cond ((and view-p delete-p) (del-view oid mbuffer))
		   ((and view-p blank-p) (create-view oid mbuffer))
		   (view-p (update-view oid mbuffer))
		   ((and (not data-p) delete-p) (del-page oid mbuffer))
		   (blank-p (create-page oid mbuffer))
		   (t (update-page oid mbuffer)))
	     (release mbuffer)))
	 (bitch (tn)
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (dotimes (i 32)
	       ;; How do I choose an operation.. 
	       ;; Pick a client...
	       ;; Choose data or page/view. (3/6) 
	       ;; if <3, insert data into that page, or create the page.
	       ;; else pick a random OID, some OIDS are views. Do a create/update or delete.
	       (with-writing
		 (let ((client (choose-client))
		       (r (random 6)))
		   (format *loop* "~%BITCH~d(~d of 32) task ~d" *thread* i r)
		   (with-client (client-name client)
		     (if (< r 3)
			 (do-doc client r)
			 (do-data client))))))))
	 (delbitch (tn)
  	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (tagbody :again
;		(format *loop* "~%dbitch start over...")
;		(with-writing 
		(begin)
		:start	       
		(handler-case (progn (let* ((client (choose-client))
					    (mbuffer (ghost (acquire (client-map client))))
					    (oid (find-a-parent (sap mbuffer) (if (zerop (random 4)) 0 (random 128)))))
				       (with-client (client-name client)
					 (cond ((zerop oid) nil)
					       ((< oid 4)
						(format *loop* "~%del page ~a:" oid)
						(let* ((dbuffer (acquire (data-map client (1- oid))))
						       (doid (loop for i from 0 to (1- +page-bytes+)
								when (not (zerop (sb-sys:sap-ref-8 (sap dbuffer) i)))
								do (return i)
								finally (return nil))))
						  (format *loop* " (delete doid ~a)" doid)
						  (if doid
						      (progn (db.archives:open-archives oid)
							     (db.archives:ghost-archives)
							     (setf dbuffer (ghost dbuffer))
							     (doc-del dbuffer doid)
							     (db.archives:write-archives))
						      (del-page oid mbuffer))
						  (release dbuffer)))
					       ((< oid 64) (del-page oid mbuffer))
					       (t          (del-view oid mbuffer)))
					 (release mbuffer)))
				     (end))
		  (reset () (reopen) (go :start)))
		(tourist)
		(dotimes (i 3)
		  (let ((mbuffer (acquire (client-map i))))
		    (unless (zerop (find-a-parent (sap mbuffer) 0)) (release mbuffer) (endtourist) (go :again))
		    (release mbuffer)))
		(endtourist)))))
  (defun test-all-multiuser ()
    (setf db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff)))
    (let ((*txn* nil)
	  (*thread* 0))
      (declare (special *txn* *thread*))
      (with-writing
;      (begin)
	(let ((b (ghost (acquire db.client:+super-block+))))
	  (system-area-ub32-fill 0 (sap b) 0 +page-words+)
	  (release b))
	(dotimes (i 3)
	  (let ((b (ghost (acquire (client-map i)))))
	    (system-area-ub32-fill 0 (sap b) 0 +page-words+)
	    (release b))
	  (dotimes (j 3)
	    (let ((b (ghost (acquire (data-map i j)))))
	      (system-area-ub32-fill 0 (sap b) 0 +page-words+)
	      (release b))))
	(db.client:subscribe "FIRST" "Client 0" 0)
	(db.client:subscribe "SECOND" "Client 1" 0)
	(db.client:subscribe "THIRD" "Client 2" 0))
;      (end)
      
      (let ((orig-free (cons nil (mapcar #'(lambda (a) (cons (car a) (cdr a))) (cdr db.txn:*free*))))
	    (wthreads)
	    (rthreads))
	(dotimes (i 28) (push (sb-thread:make-thread (papply #'bitch i) :name (format nil "~d" i)) wthreads))
	(dotimes (i 4)  (push (sb-thread:make-thread (papply #'rbitch (+ i 28) wthreads)
						     :name (format nil "R~d" i)) rthreads))
	(mapcar #'sb-thread:join-thread wthreads)
	(mapcar #'sb-thread:join-thread rthreads)
	(format *loop* "~%Done operations...")
	(setf wthreads nil rthreads nil)

	(dotimes (i 28) (push (sb-thread:make-thread (papply #'delbitch i) :name (format nil "D~d" i)) wthreads))
	(dotimes (i 4) (push (sb-thread:make-thread (papply #'rbitch (+ i 28) wthreads)
						    :name (format nil "R~d" i)) rthreads))
	
	(mapcar #'sb-thread:join-thread wthreads)
	(mapcar #'sb-thread:join-thread rthreads)

	(format *loop* "~%done deletions, FS: ~a" db.txn:*free*)
	(assert (equalp db.txn:*free* orig-free))

	(let ((*txn* nil)
	      (*thread* 0))
	  (declare (special *txn* *thread*))
	  (with-reading 
;	  (tourist)
	    (dotimes (i 3)
	      ;; Oops, use with-client
	      (with-client (client-name i)
		;(let ((c (db.client:affiliate (client-name 0))))
		(assert (zerop (db.client:directory)))
		(assert (zerop (db.client:page)))
		(assert (zerop (db.client:archives)))))))))))
;		(release c))))))))))
;	  (endtourist))
;	))))


;; How should I test dir changes? I'll need a MAP of what the directories tree, and I must seek around and update it
;; Insert, Delete, Move are the commands I use...  I'll just store parents, and I can build a graph from that... 
;; I'll use 14 pages or so, 16 bits. 

;; For this test, I'll only let parent be SMALLER than OID --- that will keep cycles out AND prevent moving item 0. 
;; In the real application, I'll have to check to ensure cycles are not created. 

(labels ((all-present (client)
	   (loop repeat 3
	         for bnum upfrom 0 
	         for real-pos upfrom 0 by 1024
		 nconc (let ((buffer (acquire (data-map client bnum))))
			 (prog1 (loop repeat 1024
				      for i upfrom 0
				      unless (zerop (sb-sys:sap-ref-32 (sap buffer) (ash i 2)))
				      collect (cons (+ i real-pos) (sb-sys:sap-ref-32 (sap buffer) (ash i 2))))
			   (release buffer)))))
	 (choose-oid () (+ 1 1 (random (+ 1022 1024 1024)))) ;; No ops to OID 0 or OID 1.
	 (nearest-possible-parent (client oid)
	   (assert (> oid 0))
	   (if (= oid 1)
	       0
	       (multiple-value-bind (j relative) (truncate oid 1024)
		 (let ((buffer (acquire (data-map client j))))
		   (loop for i from (1- relative) downto 0
		         unless (zerop (sb-sys:sap-ref-32 (sap buffer) (ash i 2)))
			 do (progn (release buffer) (return (+ (* 1024 j) i)))
		         finally (progn (release buffer)
					(return (if (zerop j) 1 (nearest-possible-parent client (1- (* j 1024)))))))))))
	 (build-path (client)
	   ;; Actually, Just chase them down until I find a parent = 1. No present list needed.
	   ;; Use the present list to build a path for the reader to verify.
	   ;(let ((oid (nearest-possible-parent client (choose-oid))))
	   (let ((start-oid (nearest-possible-parent client (choose-oid))))
	     (loop for oid = start-oid then (data-set client oid)
		   when (zerop oid) do (error "What the fuck: ~a: ~a" start-oid path)
		   until (= oid 1)
		   collect oid into path
	           finally (return-from build-path path))))
	 (data-set (client oid)
	   (multiple-value-bind (j relative) (truncate oid 1024)
	     (let ((buffer (acquire (data-map client j))))
	       (prog1 (sb-sys:sap-ref-32 (sap buffer) (ash relative 2))
		 (release buffer)))))
	 (set-data-set (client oid parent)
	   (multiple-value-bind (j relative) (truncate oid 1024)
	     (let ((buffer (ghost (acquire (data-map client j)))))
	       (setf (sb-sys:sap-ref-32 (sap buffer) (ash relative 2)) parent)
	       (release buffer))))
	 (test-owner (oid) oid)
	 (test-creator (oid) oid)
	 (test-name (oid) (format nil "~4,'0x" oid))
	 (test-title (oid) (format nil "~4,'0x|title|~4,'0x" oid oid))
	 (test-description (oid) (format nil "~4,'0x|desc|~4,'0x" oid oid))
	 (install (client oid)
	   (let ((parent (nearest-possible-parent client (max 1 (random oid)))))
	     ;;(format *loop* "~%~a:INSTALL: ~a-->~a" *thread* parent oid)
	     (set-data-set client oid parent)
	     (with-directory-tree (dir)
	       (setf dir (db.dir:make-entry dir oid parent (make-metadata 1 0 (test-owner oid) (test-creator oid)
									  0 (test-creator oid))))
	       ;;oid parent (test-owner oid) (test-creator oid) 0))
	       (setf dir (db.dir:name! dir (test-name oid)))
	       (setf dir (db.dir:title! dir (test-title oid)))
	       (setf dir (db.dir:description! dir (test-description oid))))))
	 (mymove (client oid)
	   ;; Moving this guy is easy... 
	   (let ((old (data-set client oid))
		 (new (max 1 (nearest-possible-parent client (max 1 (random oid))))))
	     (unless (= old new)
	       (set-data-set client oid new)
	       (format *loop* "~%~a:MOVE: ~a->~a to ~a->~a" *thread* old oid new oid)
	       (with-directory-tree (dir)
		 (pair oid old)
		 (setf dir (move dir new))))))
	 (myremove (client oid)
	   ;; What about everything that has THIS as a parent! 
	   (declare (ignore client)) (format *loop* "~%~a:REMOVE: ~a (not implemented)." *thread* oid))
	 (bitch (tn)
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (dotimes (i 32)
	       ;; How do I choose an operation.. 
	       ;; Pick a client...
	       ;; Pick an OID... if exists, move or delete, otherwise create.
	       (with-writing
		 (let ((client (choose-client))
		       (oid (choose-oid)))
		   (with-client (client-name client)
		     (if (zerop (data-set client oid))
			 (install client oid)
			 (if (zerop (random 4))
			     (myremove client oid)
			     (mymove client oid)))))))))
	 (rbitch (tn threads) 
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (loop while (some #'sb-thread:thread-alive-p threads)
		   do (sleep (random 2.0))
		;;do (if (zerop (random 1)) (sb-thread:thread-yield))
;		   do (sb-thread:thread-yield)
		   do (with-reading
			(let* ((client (choose-client))
			       (path (build-path client))
			       (store nil) (data nil) (key nil))
			  ;; I should test that this is exactly the same as just SEEKING it...
			  (format *loop* "~%Do path: ~a" path)
			  (with-client (client-name client)
			    (if (null path)
				(pair 1 0)
				(pair (car path) (or (cadr path) 1))) (copy-pair)
			    (setf store (lookup (db.client:directory))
				  data (origin-data)
				  key (origin-key))
			    (assert (found))
			    (format *loop* "--lookup good!")
			    (dotimes (i 4)
			      (let ((buffer
				     (case i
				       (0 (format *loop* "~%ABS dir...")
					  (chdir (make-pathname :directory
								(cons :absolute (nreverse (mapcar #'test-name path))))))
				       (1 (format *loop* "~%PAR dir...")
					  (loop with buffer = (topdir)
					        for p = (reverse path) then (nthcdr amount p)
					        as amount = (min (length p) (random 3))
					        as dir = (cons :relative (mapcar #'test-name (subseq p 0 amount)))
					        do (progn (release buffer)
							  ;;(format *loop* "~%PAR: ~a" dir)
							  (setf buffer (chdir (make-pathname :directory dir))))
					        until (null p)
					        finally (return buffer)))
				       (2 (format *loop* "~%ABS-BAD dir...")
					  (block nil
					    (handler-case 
						(chdir (make-pathname
							:directory (nconc (list :absolute)
								    (nreverse (mapcar #'test-name path))
								    (make-list (1+ (random 3)) :initial-element "BAD"))))
					      (badpath () (return (restore-dir))))
					    (error "No error signaled...")))
				       (3 (format *loop* "~%PAR-BAD dir...")
					  (loop with buffer = (topdir)
					        for p = (reverse (list* "BAD" "BAD" "BAD" "BAD" path))
					              then (nthcdr amount p)
					     as amount = (min (length p) (random 3))
					     as dir = (cons :relative (mapcar #'test-name (subseq p 0 amount)))
					     do (progn (release buffer)
						       ;;(format *loop* "~%BPAR: ~a" dir)
						       (handler-case (setf buffer (chdir (make-pathname :directory dir)))
							 (badpath () (return (restore-dir)))))
					     until (null p)
					     finally (error "NO error triggered!"))))))
				(assert (eq store buffer))
				(assert (sb-sys:sap= data (origin-data)))
				(assert (sb-sys:sap= key (origin-key)))
				(format *loop* " --- ~a sucess!" i)
				(release buffer)))
			    (release store))))))))
  (defun test-chdir ()
    ;; Create 30 writers who do thousands of dir operations.
    ;; create 2  readers who examines the present data and does dozens of tests.
    (setf db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff)))
    (let ((*txn* nil)
	  (*thread* 0))
      (declare (special *txn* *thread*))
      (with-writing
	(let ((b (ghost (acquire db.client:+super-block+))))
	  (system-area-ub32-fill 0 (sap b) 0 +page-words+)
	  (release b))
	(dotimes (i 3)
	  (let ((b (ghost (acquire (1+ i)))))
	    (system-area-ub32-fill 0 (sap b) 0 +page-words+)
	    (release b))
	  (dotimes (j 3)
	    (let ((b (ghost (acquire (data-map i j)))))
	      (system-area-ub32-fill 0 (sap b) 0 +page-words+)
	      (release b))))
	(db.client:subscribe "FIRST" "Client 0" 0)
	(db.client:subscribe "SECOND" "Client 1" 0)
	(db.client:subscribe "THIRD" "Client 2" 0)

	(dotimes (i 3)
	  (with-client (client-name i)
	    (let ((oid 1) (parent 0))
	      (set-data-set i oid parent)
	      (with-directory-tree (dir)
		(setf dir (db.dir:make-entry dir oid parent (make-metadata 1 0 (test-owner oid) (test-creator oid)
									   0 (test-creator oid))))
		;; parent (test-owner oid) (test-creator oid) 0))
		(setf dir (db.dir:name! dir (test-name oid)))
		(setf dir (db.dir:title! dir (test-title oid)))
		(setf dir (db.dir:description! dir (test-description oid)))))))))
	    

    (let ((orig-free (cons nil (mapcar #'(lambda (a) (cons (car a) (cdr a))) (cdr db.txn:*free*))))
	  (wthreads)
	  (rthreads))
      (declare (ignore orig-free))
      (dotimes (i 28) (push (sb-thread:make-thread (papply #'bitch i) :name (format nil "~d" i)) wthreads))
      (dotimes (i 4) (push (sb-thread:make-thread (papply #'rbitch (+ i 28) wthreads)
						  :name (format nil "R~d" i)) rthreads))
      (mapcar #'sb-thread:join-thread wthreads)
      (mapcar #'sb-thread:join-thread rthreads)
      (format *loop* "~%Done operations...")
      (setf wthreads nil rthreads nil)
      
      nil)))


;; TEST Security

;; This test works by creating 3 clients with different resource trees.  Then writers randomly update the ACL lists
;; at random, while readers continually check that every page has proper permissions for all persons.

;; The security model is just repeating sets of role 2 byte pairs for OIDS 0 .. 7 representing non-inheritables and
;; inheritables.  Value of 0 means NOTHING, value of 1 means custom. Everything else is as normal. The number of the
;; set corresponds to the page oid. 4096 sets of 16 bytes =  256 entries.For simplicity, each client will have a BINARY tree.

;;         1
;;   10       11
;; 100 101  110 111
;;  
;; Moving left is like shift. Descending right is like shift + 1...  Moving up is like shifting other way..

;; Also, in the tree I store user oids as (expt oid 2) (this increases the likelyhood I'll have to use extended notation)


(labels ((create-binary-page (oid)
	   (with-directory-tree (dir)
	     ;;(format *loop* "~%CreatePage(@~d): ~a->~a." dir (ash oid -1) oid)
	     ;;(setf dir (db.dir:make-entry dir oid (ash oid -1) (mod oid 8) (mod oid 8) 0))
	     (setf dir (db.dir:make-entry dir oid (ash oid -1) (make-metadata 1 0 (mod oid 8) (mod oid 8) 0 (mod oid 8))))
	     (setf dir (db.dir:name! dir (princ-to-string oid)))
	     (setf dir (db.dir:title! dir "Page"))
	     (setf dir (db.dir:description! dir "Description"))))
	 (create-fulltree () (loop for i from 1 to 63 do (create-binary-page i)))
	 (resolve-role (page-oid recorded-acl)
	   (ecase recorded-acl
	     (2 (make-array 5 :element-type '(unsigned-byte 32) 
			      :initial-element (* page-oid #x01010101)))
	     (3 db.acl:+role-administrator+)
	     (4 db.acl:+role-designer+)
	     (5 db.acl:+role-editor+)
	     (6 db.acl:+role-user+)
	     (7 db.acl:+role-submitter+)
	     (8 db.acl:+role-reader+)	     
	     (9 db.acl:+role-no-access+)))
	 (proper-level (client page-oid user)
	   (let ((b (acquire (1+ client))))
	     (loop for place = page-oid then (ash place -1)
		   for inherit         = (sb-sys:sap-ref-8 (sap b) (+ (* place 64) (* 2 user)))
		   for non-inherit     = (sb-sys:sap-ref-8 (sap b) (+ (* place 64) (* 2 user) 1))
		   until (= place 0)
		   ;;do (format *loop* "~%Scan: ~a (page: ~a)" place page-oid)
		   do (if (and (= place page-oid) (not (zerop non-inherit)))
			  (progn (release b) (return-from proper-level (resolve-role place non-inherit)))
			  (when (not (zerop inherit))
			    (release b) (return-from proper-level (resolve-role place inherit)))))
	     (release b)
	     db.acl:+role-no-access+))
	 (del-security (client)
	   (let* ((b (ghost (acquire (1+ client))))
		  (oid (loop for sap = (sap b) then (sb-sys:sap+ sap 8)
			     repeat 512
			     when (not (zerop (sb-sys:sap-ref-64 sap 0)))
			     do (return (truncate (sb-sys:sap- sap (sap b)) 64)))))
	     (prog1 (when oid
		      (format *loop* "~%DEL ~d/~d" client oid)
		      (setf (sb-sys:sap-ref-64 (sap b) (* oid 64)) 0
			    (sb-sys:sap-ref-64 (sap b) (+ (* oid 64) 8)) 0
			    (sb-sys:sap-ref-64 (sap b) (+ (* oid 64) 16)) 0
			    (sb-sys:sap-ref-64 (sap b) (+ (* oid 64) 24)) 0
			    (sb-sys:sap-ref-64 (sap b) (+ (* oid 64) 32)) 0
			    (sb-sys:sap-ref-64 (sap b) (+ (* oid 64) 40)) 0
			    (sb-sys:sap-ref-64 (sap b) (+ (* oid 64) 48)) 0
			    (sb-sys:sap-ref-64 (sap b) (+ (* oid 64) 56)) 0)
		      (with-acl-tree (acl) (oid oid) (setf acl (db.resources:delete-rz acl)))
		      t)
	       (release b))))
	 (mod-security (client oid)
	   (let* ((b (ghost (acquire (1+ client))))
		  (security (loop for user from 0 to 31
			          for i-security = (+ 2 (random 8))
				  for n-security = (+ 2 (random 8))
			          do (setf (sb-sys:sap-ref-8 (sap b) (+ (* oid 64) (* 2 user)))   i-security)
			          do (setf (sb-sys:sap-ref-8 (sap b) (+ (* oid 64) (* 2 user) 1)) n-security)
			          collect (list (expt user 2) i-security n-security))))
	     (release b)
	     (flet ((irole (class) (mapcar #'car (remove class security :key #'second :test-not #'=)))
		    (nrole (class) (mapcar #'car (remove class security :key #'third  :test-not #'=))))
	       (let ((i (list (if (and (null (irole 2)) (zerop (random 10)))
				  (list db.acl:+role-no-access+)
				  (list* (make-array 5 :element-type '(unsigned-byte 32) 
						     :initial-element (* oid #x01010101))
					 (irole 2)))
			      (list* db.acl:+role-administrator+ (irole db.acl:+administrator+))
			      (list* db.acl:+role-designer+      (irole db.acl:+designer+))
			      (list* db.acl:+role-editor+        (irole db.acl:+editor+))
			      (list* db.acl:+role-user+          (irole db.acl:+user+))
			      (list* db.acl:+role-submitter+     (irole db.acl:+submitter+))
			      (list* db.acl:+role-reader+        (irole db.acl:+reader+))
			      (list* db.acl:+role-no-access+     (irole db.acl:+no-access+))))
		     (n (list (list* (make-array 5 :element-type '(unsigned-byte 32) 
						:initial-element (* oid #x01010101))
				     (nrole 2))
			      (list* db.acl:+role-administrator+ (nrole db.acl:+administrator+))
			      (list* db.acl:+role-designer+      (nrole db.acl:+designer+))
			      (list* db.acl:+role-editor+        (nrole db.acl:+editor+))
			      (list* db.acl:+role-user+          (nrole db.acl:+user+))
			      (list* db.acl:+role-submitter+     (nrole db.acl:+submitter+))
			      (list* db.acl:+role-reader+        (nrole db.acl:+reader+))
			      (list* db.acl:+role-no-access+     (nrole db.acl:+no-access+)))))
		 (with-acl-tree (acl) 
		   ;; Install the appropriate ACL for oids 0->7 with these security permissions....
		   (setf acl (db.db:set-acl acl oid i n)))))))
;		   (copy-oid) 
;		   (let ((buffer (lookup acl)))
;		     (unless (found) (setf acl (db.resources:make-acl acl oid)))
;		     (and buffer (release buffer)))
;		   (oid oid) (rz-start)
;		   (setf acl (db.acl:acl! acl i n)))))))
	 (bitch (tn)
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (dotimes (i 64 (format *loop* "~%Writer Thread ~d finished." tn))
	       ;; How do I choose an operation.. 
	       ;; Pick a client... Pick an OID... customize the security level.
	       (with-writing
		 (let ((client (choose-client))
		       (oid (1+ (random 63)))) 
		   (with-client (client-name client)
		     ;;(format *loop* "~%ADD ~d/~d" client oid)
		     (mod-security client oid)))))))
	 (delbitch (tn)
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (loop with done = 0  ;; IF CHOOSE-CLIENT is hardcoded, this will infinite loop! 
		   for client = (choose-client)
		   do (with-writing (with-client (client-name client)
				      (or (del-security client)
					  (setf done (boole boole-ior done (ash 1 client))))))
		   until (= done 7))))
	 (rbitch (tn threads)
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (loop while (some #'sb-thread:thread-alive-p threads)
		   do (sleep (random 40.0))
		;;do (if (zerop (random 1)) (sb-thread:thread-yield))
;		   do (sb-thread:thread-yield)
		   do (format *loop* "~%Reader ~d..." tn)
		   do (with-reading
			(let* ((client (choose-client)))
			  (with-client (client-name client)
			    ;; Verify security.
			    (loop for oid from 1 to 63
				  for path = (loop for o = oid then (ash o -1)
						   while (> o 1)
						   collecting (princ-to-string o) into names
						   finally (return (make-pathname :directory
								    (cons :absolute (nreverse names)))))
				  do (format *loop* "~d, " oid)
				  do (loop for user from 0 to 31
					   for expected = (proper-level client oid user)
					   for real = (db.db:role (list (expt user 2)) path)
					   unless (equalp expected real)
					   do (error "WRONG ACL (~d ~d ~d) expected: ~a got ~a."
						     client oid user expected real))))))))))
  (defun test-acl ()
    ;; Create 30 writers who do thousands of acl operations.
    ;; create 2  readers who examines the present data and does dozens of tests.
    (setf db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff)))
    (let ((*txn* nil)
	  (*thread* 0))
      (declare (special *txn* *thread*))
      (with-writing
	(let ((b (ghost (acquire db.client:+super-block+))))
	  (system-area-ub32-fill 0 (sap b) 0 +page-words+)
	  (release b))
	(dotimes (i 3)
	  (let ((b (ghost (acquire (1+ i)))))   ;; PAGES 1, 2, 3 are the security maps for clients 1,2,3.
	    (system-area-ub32-fill 0 (sap b) 0 +page-words+)
	    (release b)))
	(db.client:subscribe "FIRST" "Client 0" 0)
	(db.client:subscribe "SECOND" "Client 1" 0)
	(db.client:subscribe "THIRD" "Client 2" 0)
	(dotimes (i 3) (with-client (client-name i) (format *loop* "~%Populating client ~d" i) (create-fulltree)))))

    (let ((orig-free (cons nil (mapcar #'(lambda (a) (cons (car a) (cdr a))) (cdr db.txn:*free*))))
	  (wthreads)
	  (rthreads))
      
      ;;(format *loop* "READY TO RUN TESTS...")
      
      ;; BASIC operation --- Writers continually ADD, MODIFY and DELETE ACL permissions.  Readers continually
      ;; stop, check every page/user combination to ensure the value returned by ACL computations matches the 
      ;; value returned by the test-model. 


      (dotimes (i 28) (push (sb-thread:make-thread (papply #'bitch i) :name (format nil "~d" i)) wthreads))
      (dotimes (i 4) (push (sb-thread:make-thread (papply #'rbitch (+ i 28) wthreads)
						  :name (format nil "R~d" i)) rthreads))
      (mapcar #'sb-thread:join-thread wthreads)
      (mapcar #'sb-thread:join-thread rthreads)
      (format *loop* "~%Done insertions...")
      (setf wthreads nil rthreads nil)

      (dotimes (i 28) (push (sb-thread:make-thread (papply #'delbitch i) :name (format nil "~d" i)) wthreads))
      (dotimes (i 4) (push (sb-thread:make-thread (papply #'rbitch (+ i 28) wthreads)
						  :name (format nil "R~d" i)) rthreads))
      (mapcar #'sb-thread:join-thread wthreads)
      (mapcar #'sb-thread:join-thread rthreads)
      (format *loop* "~%Done deletions...")
      (setf wthreads nil rthreads nil)

      (format *loop* "~%FREE: ~s~%ORIG: ~s" *free* orig-free)
      (let ((*txn* nil)
	    (*thread* 0))
	(declare (special *txn* *thread*))
	(with-reading (dotimes (i 3) (with-client (client-name i)
				       (format *loop* "~%CLIENT~d ACL: ~d" i (db.client:acl))
				       (assert (zerop (db.client:acl)))))))
      (assert (equalp db.txn:*free* orig-free)))))



;; TEST Membership

;; Two tests. The first just test fetching and retrieval algorithms.  That members and members* works, that setting
;; and tearing down permissions work.
;;
;;   a) Put security into a known state. a basic tree of groups (upto 1-(2),(3)-(45),(67)) and a user's are bitpatterns.
;;	 * Create a basic binary tree of groups. 1 being the top, 2&3 being its children, etc up to 7. 
;;	 * Create 127 users from 1 to #b1111111 that are members of these groups. 
;;   b) Test that this security matches the expected state.

;; The second test tests the actual modification algorithms  When I add a group to another group, all members down
;; the line end up with correct references. 

(defun group-members (grpn)
  (sort (if (<= grpn 3)
	    (list* (db.acl:group->shifted (ash grpn 1))
		   (db.acl:group->shifted (1+ (ash grpn 1)))
		   (mapcar #'db.acl:user->shifted 
			   (remove-if #'(lambda (n) (zerop (boole boole-and n (ash 1 (1- grpn))))) (iota 127 1))))
	    (mapcar #'db.acl:user->shifted 
		    (remove-if #'(lambda (n) (zerop (boole boole-and n (ash 1 (1- grpn))))) (iota 127 1)))) #'<))
(defun user-groups (user &aux groups)
  (unless (zerop (boole boole-and user #b0000001)) (push 1 groups))
  (unless (zerop (boole boole-and user #b0000010)) (push 2 groups))
  (unless (zerop (boole boole-and user #b0000100)) (push 3 groups))
  (unless (zerop (boole boole-and user #b0001000)) (push 4 groups))
  (unless (zerop (boole boole-and user #b0010000)) (push 5 groups))
  (unless (zerop (boole boole-and user #b0100000)) (push 6 groups))
  (unless (zerop (boole boole-and user #b1000000)) (push 7 groups))
  (nreverse groups))

(defconstant +test-group-members+ (coerce (mapcar #'group-members (iota 7 1)) 'vector))
(defconstant +test-user-groups+   (coerce (mapcar #'user-groups   (iota 127 1)) 'vector))

(labels ((expt-group-members (group) (elt +test-group-members+ (1- group)))
	 (expt-user-groups   (user) (elt +test-user-groups+    (1- user)))
	 (expt-superset      (group) (if (= group 1) nil (list (ash group -1))))
	 (expt-ancestors (group) 
	   (nreverse (loop for grp = group then (ash grp -1)
			   while (> grp 0)
			   collecting grp)))
	 (expt-descendants (group)
	   (sort
	    (cons (db.acl:group->shifted group)
	    (mapcan (util.util:compose #'copy-list #'expt-group-members)
		    (ncase group
		      (1 '(1 2 3 4 5 6 7))
		      (2 '(2 4 5))
		      (3 '(3 6 7))
		      (4 '(4))
		      (5 '(5))
		      (6 '(6))
		      (7 '(7))))) #'<))
	 (initialize-data ()
	   (with-membership-tree (root)
	     (loop for group from 1 to 7
		   do (setf root (db.acl:set-members root group (expt-group-members group))))
	     (loop for user from 1 to 127
	 	   do (setf root (db.acl:set-groups root user (expt-user-groups user))))))
	 (descendant-test (&aux (group (1+ (random 7))))
	   (format *loop* " descendants of ~d" group)
	   (or (equalp (expt-descendants group) 
		       (with-membership-tree (mm) (db.acl:descendants mm group)))
	       (error "descendants failed, expected ~s got ~s" 
		      (expt-descendants group)
		      (with-membership-tree (mm) (db.acl:descendants mm group)))))
	 (ancestor-test (&aux (group (1+ (random 7))))
	   (format *loop* " ancestors of ~d" group)
	   (or (equalp (expt-ancestors group) (with-membership-tree (mm) (db.acl:ancestors mm group)))
	       (error "ancestors failed, expected ~s got ~s" 
		      (expt-ancestors group)
		      (with-membership-tree (mm) (db.acl:ancestors mm group)))))
	 (superset-test (&aux (group (1+ (random 7))))
	   (format *loop* " supersets of ~d" group)
	   (or (equalp (expt-superset group) (with-membership-tree (mm) (db.acl:superset mm group)))
	       (error "superset failed, expected ~s got ~s" 
		      (expt-superset group)
		      (with-membership-tree (mm) (db.acl:superset mm group)))))
	 (groups-test (&aux (user (1+ (random 127))))
	   (format *loop* " groups of ~d" user)
	   (or (equalp (expt-user-groups user) (with-membership-tree (mm) (db.acl:groups mm user)))
	       (error "Groups failed, expected ~s got ~s" 
		      (expt-user-groups user)
		      (with-membership-tree (mm) (db.acl:groups mm user)))))
	 (members-test (&aux (group (1+ (random 7))))
	   (format *loop* " members of ~d" group)
	   (or (equalp (expt-group-members group) (with-membership-tree (mm) (db.acl:members mm group)))
	       (error "Members failed, expected ~s got ~s" 
		      (expt-group-members group)
		      (with-membership-tree (mm) (db.acl:members mm group)))))
	 (rbitch (tn)
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     ;; Now... what shall I do as a test? 
	     ;; I want to test descendants and members, as well as just fetching. 
	     ;; So 1,0 = descendants. 3,2 = ancestors. 5 = user fetch, 6 = group fetch.
	     (loop repeat 32
		   do (sleep (random 0.1))
		   do (format *loop* "~%Reader ~d..." tn)
		   do (with-reading
			(with-client (client-name 0)
			  (ncase (random 7)
			    (0 (descendant-test))
			    (1 (descendant-test))
			    (2 (superset-test))
			    (3 (ancestor-test))
			    (4 (ancestor-test))
			    (5 (groups-test))
			    (6 (members-test)))))))))
  (defun test-membership-primitives ()
    ;; Create 32 readers who just sanity check that my group access routines work on a reasonably complicated data set.
    (setf db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff)))
    (let ((*txn* nil)
	  (*thread* 0))
      (declare (special *txn* *thread*))
      (with-writing
	(let ((b (ghost (acquire db.client:+super-block+))))
	  (system-area-ub32-fill 0 (sap b) 0 +page-words+)
	  (release b))
	(db.client:subscribe "FIRST" "Client 0" 0)
	(with-client (client-name 0)
	  (format *loop* "~%Initializing Data...")
	  (initialize-data))))
    ;;(error "Now how do I test this? I suppose 32 readers that just have their way with it...")
    (let ((orig-free (cons nil (mapcar #'(lambda (a) (cons (car a) (cdr a))) (cdr db.txn:*free*))))
	  (rthreads))
      
      (dotimes (i 32) (push (sb-thread:make-thread (papply #'rbitch i) :name (format nil "R~d" i)) rthreads))
      (mapcar #'sb-thread:join-thread rthreads)
      (setf rthreads nil)

      ;;(format *loop* "~%FREE: ~s~%ORIG: ~s" *free* orig-free)
      (assert (equalp db.txn:*free* orig-free)))))


;; Basic procedure: 3 clients.   Each client starts off with No groups and No users. 
;;  
;; WRITER
;;   1) Pick a Number from 1 to 255
;;      Numbers >= 64 are users
;;   2) If User, Test groups.
;;   3) If Group do one of:
;;      a) Modify by selecting some number > us and adding it to this group.
;;      b) Modify by selecting some existing member and removing them from membership.
;;      b) Trigger a circle error by finding a parent of us and attempting to add to this group.
;;      c) Test ancestors
;;      d) Test descendants
;;      e) Test members. 
;;
;;  READER
;;    --- I don't know.  How about iterate the groups and users and test everything? 
;;
;;  DELETER
;;     removing members from groups. When they empty they should auto-delete. Everything should collapse to 0.
;;
;; What sort of binary structure do I need... Just 64 bytes per group --- I'll have 64 groups.  Each byte is
;; a member.  Numbers >= 64 are users.  


;; -------These test-xxx forms should become EXPT-xxx.  Then I can do the test inline.
(labels ((random-shifted (greater-than)
	   (let ((r (+ greater-than (random (+ (- 64 greater-than) 126)))))
	     (if (< r 64)
		 (db.acl:group->shifted (1+ r))
		 (db.acl:user->shifted (- r 64)))))
	 (holders-of (client shifted)
	   (assert (< shifted 255))
	   (let* ((b (acquire (1+ client)))
		  (sap (sap b)))
	     (loop for i from 0 upto (1- (if (db.acl:user-p shifted) 4096 (* 64 (db.acl:shifted->group shifted))))
		   when (= (sb-sys:sap-ref-8 sap i) shifted)
		   collect (1+ (truncate i 64))
		   finally (release b))))
	 (children-of (client group)
	   (let* ((b (acquire (1+ client)))
		  (sap (sap b)))
	     (sort (remove 255 (loop for i from (+ 0 (* 64 (1- group))) to (1- (* 64 group))
				  collecting (sb-sys:sap-ref-8 sap i)
				  finally (release b))) #'<)))
	 (rec-holders (client shifted)
	   (labels ((rec (shifted)
		      ;(format *loop* "~%HOLDERS (~:[user~;group~])~d: ~a" (db.acl:group-p shifted) (ash shifted -1)
		      ;     (holders-of client shifted))
		      (aif (holders-of client shifted)
			   (cons shifted (mapcan (compose #'rec #'db.acl:group->shifted) it))
			   (list shifted))))
	     (sort (rec shifted) #'<)))
	 (expt-ancestors (client group) (mapcar #'db.acl:shifted->group (rec-holders client (db.acl:group->shifted group))))
	 (expt-groups (client oid)
	   (mapcar #'db.acl:shifted->group
		   (remove (db.acl:user->shifted oid) (rec-holders client (db.acl:user->shifted oid)))))
	 (expt-descendants (client group)
	   (labels ((rec (shifted &aux (children (children-of client (db.acl:shifted->group shifted))))
		      ;(format *loop* "~%CHILDREN (~:[user~;group~])~d: ~a" (db.acl:group-p shifted) (ash shifted -1)
		      ;     children)
		      (cons shifted (nconc (remove-if #'db.acl:group-p children)
					   (mapcan #'rec (remove-if #'db.acl:user-p children))))))
	     (sort (rec (db.acl:group->shifted group)) #'<)))
	 (toggle-membership (client group-oid slot)
	   (let* ((b (ghost (acquire (1+ client))))
		  (oid (sb-sys:sap-ref-8 (sap b) (+ (* (1- group-oid) 64) slot))))
	     (format *loop* "~%~:[EXPEL~;ENROLL~]~d: ~d" (= 255 oid) client group-oid)
	     (with-membership-tree (root)
	       (if (= 255 oid)
		   (let ((next (random-shifted group-oid)))
		     (format *loop* " --> ~d" next)
		     (assert (or (db.acl:user-p next) (< group-oid (db.acl:shifted->group next))))
		     (setf root (db.acl:enroll root group-oid next))
		     (unless (some #'(lambda (i) (= (sb-sys:sap-ref-8 (sap b) (+ (* (1- group-oid) 64) i)) next)) (iota 64 0))
		       ;; If already a member, don't actually change anything.
		       (setf (sb-sys:sap-ref-8 (sap b) (+ (* (1- group-oid) 64) slot)) next)))
		   (progn 
		     (setf (sb-sys:sap-ref-8 (sap b) (+ (* (1- group-oid) 64) slot)) 255)
		     (setf root (db.acl:expel root group-oid oid)))))
	     (release b)))
	 (attempt-circle (client group-oid)
	   (awhen (holders-of client (db.acl:group->shifted group-oid))
	     (format *loop* "~%ATTEMPT CIRCLE: ~d --> (one of) ~a" group-oid it)
	     (handler-bind ((error #'(lambda (e) (declare (ignore e)) (return-from attempt-circle))))
	       (with-membership-tree (root)
		 (setf root (db.acl:enroll root group-oid (db.acl:group->shifted (elt it (random (length it))))))))
	     (error "Attempt circle did not produce an error!")))
	 (bitch (tn)
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (dotimes (i 64 (format *loop* "~%Writer Thread ~d finished." tn))
;	       (error "Bitch not implemented."))))
	       (with-writing
		 (let ((client (choose-client))
		       (op (random (+ 64 16 16 16 16))))
		   (with-client (client-name client)
		     (cond ((< op 64) (toggle-membership  client (1+ (random 63)) op))
			   ((< op 72) (attempt-circle     client (1+ (random 63))))
			   ((< op 80) (attempt-circle     client (1+ (random 63))))
			   ((< op 96) (let* ((group (1+ (random 63)))
					     (expt (expt-ancestors client group))
					     (real (with-membership-tree (root) (db.acl:ancestors root group))))
					(format *loop* "~%Ancestors of group ~d" group)
					(or (equalp expt real)
					    (error "(~d g~d) Wrong ancestors, expected ~s got ~s." client group expt real))))
			   ((< op 112) (let* ((group (1+ (random 63)))
					      (expt (expt-descendants client group))
					      (real (with-membership-tree (root) (db.acl:descendants root group))))
					 (format *loop* "~%Descendants of group ~d" group)
					 (or (equalp expt real)
					     (error "(~d g~d) Wrong descendants, expected ~s got ~s."
						    client group expt real))))
			   (t (let* ((r (random 126))
				     (expt (expt-groups client r))
				     (real (with-membership-tree (root) (db.acl:groups root r))))
				(format *loop* "~%Groups of user ~d" r)
				(or (equalp expt real)
				    (error "(~d u~d) Wrong groups, expected ~s got ~s." client r expt real)))))))))))

	 (del-membership (client)
	   (let* ((b (acquire (1+ client))))
	     (loop for i from 0 upto (1- 4096)
		when (not (= 255 (sb-sys:sap-ref-8 (sap b) i)))
		do (multiple-value-bind (1-group slot) (truncate i 64)
		     (toggle-membership client (1+ 1-group) slot)
		     (release b)
		     (return-from del-membership t)))
	     (release b) nil))
	 (delbitch (tn)
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (loop with done = 0  ;; IF CHOOSE-CLIENT is hardcoded, this will infinite loop! 
		   for client = (choose-client)
		   do (with-writing
			(with-client (client-name client)
			  (or (del-membership client)
			      (setf done (boole boole-ior done (ash 1 client))))))
		   until (= done 7))))
	 (rbitch (tn threads)
	   (let ((*txn* nil)
		 (*thread* tn))
	     (declare (special *txn* *thread*))
	     (loop while (some #'sb-thread:thread-alive-p threads)
		   do (sleep (random 16.0))
		   do (format *loop* "~%Reader ~d..." tn)
;		   do (error "rbitch not implemented.")))))
		   do (with-reading
			(let* ((client (choose-client)))
			  (with-client (client-name client)
;			    ;; Verify security.
			    (with-membership-tree (root) 
			    (loop for group from 1 to 63
				  for expected = (children-of client group)
				  for real = (db.acl:members root group)
				  do (or (equalp expected real)
					 (error "Bad children (~d, ~d): got ~a expected ~a" client group real expected))
				  do (when (zerop (random 16))
				       (let ((expected (expt-ancestors client group))
					     (real (db.acl:ancestors root group)))
					 (or (equalp expected real)
					     (error "Bad ancestors (~d, ~d): got ~a expected ~a"
						    client group real expected))))
				  do (when (zerop (random 16))
				       (let ((expected (expt-descendants client group))
					     (real (db.acl:descendants root group)))
					 (or (equalp expected real)
					     (error "Bad descendants (~d, ~d): got ~a expected ~a"
						    client group real expected)))))
			    (loop for user from 0 to 126
			          do (when (zerop (random 64))
				       (let ((expected (expt-groups client user))
					     (real (db.acl:groups root user)))
					 (or (equalp expected real)
					     (error "Bad groups (~d, (user)~d): got ~a expected ~a"
						    client user real expected)))))))))))))
  (defun test-membership-thoroughly ()
    (setf db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff)))
    (let ((*txn* nil)
	  (*thread* 0))
      (declare (special *txn* *thread*))
      (with-writing
	(let ((b (ghost (acquire db.client:+super-block+))))
	  (system-area-ub32-fill 0 (sap b) 0 +page-words+)
	  (release b))
	(dotimes (i 3)
	  (let ((b (ghost (acquire (1+ i)))))   ;; PAGES 1, 2, 3 are the security maps for clients 1,2,3.
	    (system-area-ub32-fill #xFFFFFFFF (sap b) 0 +page-words+)
	    (release b)))
	(db.client:subscribe "FIRST" "Client 0" 0)
	(db.client:subscribe "SECOND" "Client 1" 0)
	(db.client:subscribe "THIRD" "Client 2" 0)))
    ;(dotimes (i 3) (with-client (client-name i) (format *loop* "~%Populating client ~d" i) (create-fulltree)))))

    (let ((orig-free (cons nil (mapcar #'(lambda (a) (cons (car a) (cdr a))) (cdr db.txn:*free*))))
	  (wthreads)
	  (rthreads))

      (dotimes (i 28) (push (sb-thread:make-thread (papply #'bitch i) :name (format nil "~d" i)) wthreads))
      (dotimes (i 4) (push (sb-thread:make-thread (papply #'rbitch (+ i 28) wthreads)
						  :name (format nil "R~d" i)) rthreads))
      (mapcar #'sb-thread:join-thread wthreads)
      (mapcar #'sb-thread:join-thread rthreads)
      (format *loop* "~%Done Operations...")
      (setf wthreads nil rthreads nil)

      (dotimes (i 28) (push (sb-thread:make-thread (papply #'delbitch i) :name (format nil "~d" i)) wthreads))
      (dotimes (i 4) (push (sb-thread:make-thread (papply #'rbitch (+ i 28) wthreads)
						  :name (format nil "R~d" i)) rthreads))
      (mapcar #'sb-thread:join-thread wthreads)
      (mapcar #'sb-thread:join-thread rthreads)
      (format *loop* "~%Done deletions...")
      (setf wthreads nil rthreads nil)

      (format *loop* "~%FREE: ~s~%ORIG: ~s" *free* orig-free)
      (let ((*txn* nil)
	    (*thread* 0))
	(declare (special *txn* *thread*))
	(with-reading (dotimes (i 3) (with-client (client-name i)
				       (format *loop* "~%CLIENT~d MBSHIP: ~d" i (db.client:membership))
				       (assert (zerop (db.client:membership)))))))
      (assert (equalp db.txn:*free* orig-free)))))
    
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(db.db:initialize)
(empty-space) 
(endupdate)
(format *loop* "~%System initialized.")
;; this is almost the startup stuff.  i should journal replay first though.
;; the block journal has a zero count when its done.. the good alloc journal has data, the bad one starts with
;; a 0 8byte word.  (after writing a new alloc journal, the last thing i do is write the first 2 8byte words:
;; free max @ 0,free max @ max. specifying that everything is in fact free.


;; Okay, my change seems to have fixed it mostly... I got a lookup missed after 215 iterations... (a reader 
;; error) unfortunately the error message is lost.
;; Ran 14 more times and thne I got this:
;Unhandled memory fault at #x8C3019.
;   [Condition of type SB-SYS:MEMORY-FAULT-ERROR]
;
;Restarts:
; 0: [TERMINATE-THREAD] Terminate this thread (#<THREAD "rbitch2" RUNNING {11E06479}>)
;
;Backtrace:
;  0: (SB-SYS:MEMORY-FAULT-ERROR)
;  1: ("foreign function: #x806462B")
;  2: ("foreign function: #x8064960")
;  3: ((LAMBDA (DB.IO::B)) #<error printing object>)
;  4: (SB-KERNEL:%FIND-POSITION-IF #<CLOSURE (LAMBDA (DB.IO::B)) {143E3BFD}> #(#S(DB.IO:BUFFER :WATCHERS 1 :DISKVERSION-P NIL :DIRTY-P NIL :SECTOR 0 :SAP #.(SB-SYS:INT-SAP #XB55D0008) :BNUM 0) #S(DB.IO:BUFFER :WATCHERS 1 :DISKVERSION-P NIL :DIRTY-P NIL :SECTOR 8413 :SAP #.(SB-SYS:INT-SAP #XB55D1008) :BNUM 1) #S(DB.IO:BUFFER :WATCHERS 1 :DISKVERSION-P T :DIRTY-P T :SECTOR 20274 :SAP #.(SB-SYS:INT-SAP #XB55D2008) :BNUM 2) #S(DB.IO:BUFFER :WATCHERS 0 :DISKVERSION-P NIL :DIRTY-P NIL :SECTOR 20092 :SAP #.(SB-SYS:INT-SAP #XB55D3008) :BNUM 3) #S(DB.IO:BUFFER :WATCHERS 0 :DISKVERSION-P T :DIRTY-P T :SECTOR 19641 :SAP #.(SB-SYS:INT-SAP #XB55D4008) :BNUM 4) #S(DB.IO:BUFFER :WATCHERS 0 :DISKVERSION-P NIL :DIRTY-P NIL :SECTOR 20139 :SAP #.(SB-SYS:INT-SAP #XB55D5008) :BNUM 5) ...) NIL 0 NIL #<FUNCTION IDENTITY>)
;  5: (DB.IO:FIND-SECTOR 19095)
;  6: (DB.IO:CACHE 19095)
;  7: (DB.TXN:ACQUIRE 19095)

;; It seems all my current errors are just in the readers thread....  This test probably isn't a great one to run
;; to find this problem...  Because visitors are relatively few...   Is it better to have 1 visitor more frequent?
;; or several less frequent? 
;;
;; 112x then lisp died...

;; All tests now work, test-everything is still untried.

;; Test resources has been running 105 times. Still going, I'm gonna let it rn overnight.... 360 tests completed,
;; and still going. Strange no LISP error... I'm thinking that maybe its a real lisp problem, that is mostly
;; present test-sequential or earlier... something strange... anyway I can continue implementing now.


;; Well, congradulations to me.  I have a working database. 
;; Now what?  I have two options:
;; A) PORT data over... 
;; B) Port code over... 
;;
;; If I port data over... I have no way of using it...
;; If I port code over... I have to write the initialization stuff earlier... which I probably need... lets do that.


;; (run-at-time 0 13 #'slime-repl-clear-buffer)

;; Did 662 ... no problems... seems okay I guess... 

;  (setf db.txn:*cursor* db.io:+journal-start+)
;  (format *loop* "~%db.expanse-test...")
;  (db.expanse-test)
  ;(format *loop* "~%Done...")
;(dotimes (i 80)
;  (setf db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff)))
;  (setf db.txn:*cursor* db.io:+journal-start+)
;  (format *loop* "~%db.expanse-grow-test-many...")
;  (db.expanse-grow-test-many)
;  (format *loop* "~%Done ~d" i))

;;(setf db.txn:*free* (list nil (cons db.io:+user-start+ #xffffffff)))
;;(db.expanse-grow-test 3072 1385)

;; Does pre-allocating this one work? 
;GROW-TEST 2047
;GROWING AT: 1765
;writing saved: 22732


;; Works great, now I can figure out the recursive permission reader and finish this test...
;; (one thing I might want to do is fix custom to depend upon the USER AND Page-oid so I'll sometimes test multiple
;; different customs.  Maybe its not worth it... just get it working. 
;;
;; Okay, I can pretty much successfully write and shit... but for some reason, I'm either writing crap, or reading crap.
;; I get these HUGE INHERIT guys back that use strange OIDS (not even in sorted order? wtf!) 
;; So nearly there... except for shitty reading and writing... I think the problem is that I PROBABLY don't skip the first
;; 4 bytes... OOPs! that would probably do it......  Still have the problem. 
;; 
;; It seems the permissions I generate are correct..  The permissions I claim I'm writing are different than the ones 
;; I'm reading..
;;
;; Close... but sometimes BOTH the expected and the fetched values are wrong... god damn. 
;;
;; Seems to work... now unleash all the other users... then the other clients (remember to make done = 0, not 6 again).
;; Then unleash other threads.

;; Did 70 successfully, and then I went to bed... it seems like it works. 

;; Okay... supersets... shouldn't be too hard to compute...


;; Okay, time to fix these tests --- go with 1 client, and limit the number of threads.. figure out some of the problems
;; I'm having... make it so it doesn't error at end so I can bulk test until I have a problem.
;;
;; Seems to work... do lots of tests, and then 
;;   a) Add Reader/Deleter...
;;   b) Add multiple users.

;; OKAY!   Reading Works... so does deleting EXCEPT for the fact that I think I'm writing NILs when I SHOULD be
;; deleting entries entirely. 

;; Okay... there is a bug when I did my delete..  It seemed to be right around the LAST one... 
;; Can probably duplicate without readers (and that would be faster...)  
;;  * Does it happen without readers? 
;;  * Does it happen with 4 writers?  --- Not so much.
;;  * Does it happen with single writer? 

;; Wow, got a random bad-groups... I can't track it yet... one of the readers picked it up.

;; Seem to have fixed it... toggle wasn't setf-ing root during expels.
;; Okay, all threads --- next add multiple clients.


;(dotimes (i 1)
;  (setf db.txn:*cursor* db.io:+journal-start+)
;  (format *loop* "~%test-all-multiuser...")
;  (test-all-multiuser))
;(error "Okay...")

;(dotimes (i 1)
;  (setf db.txn:*cursor* db.io:+journal-start+)
;  (test-membership-thoroughly)
;  (format *loop* "~%DONE ~d" i))
;
;(error "Tested Membership Thoroughly")
;
;(dotimes (i 1)
;  (setf db.txn:*cursor* db.io:+journal-start+)
;  (test-membership-primitives)
;  (format *loop* "~%DONE ~d" i))
;
;(error "Tested Membership Primitives")
;
;(dotimes (i 1)
;  (setf db.txn:*cursor* db.io:+journal-start+)
;  (test-acl)
;  (format *loop* "~%DONE ~d" i))
;;
;(error "All the ACL and GROUP stuff passed!")

;; These tests tend to lock up the machine when run under emacs. 
;; I ran it in raw SBCL outside of X, and was able to run it 20 times, no problems.

(with-open-file (out "/home/warren/testoutput.log" :direction :output :if-does-not-exist :create :if-exists :supersede)
  (dotimes (x 1)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%db.io-test...")
    (finish-output out)
    (db.io-test)
    
    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%db.txn-alloctest...")
    (finish-output out)
    (db.txn-alloctest)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%db.txn-test...")
    (finish-output out)
    (db.txn-test)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%db.journal-test...")
    (finish-output out)
    (db.journal-test)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%db.journal-alloc-test...")
    (finish-output out)
    (db.journal-alloc-test)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%db.btree-test-nodups...")
    (finish-output out)
    (db.btree-test-nodups 600)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%db.btree-test-dups...")
    (finish-output out)
    (db.btree-test-dups 600)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%db.btree-test-multiuser...")
    (finish-output out)
    (db.btree-test-multiuser 1024)

;;   Capable of producing an error after long running times. Always in the READER thread.. I should try just 2 readers
;;   and more frequent running.
    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%db.btree-test-sequential...")
    (finish-output out)
    (db.btree-test-sequential)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%db.client-test-multiuser...")
    (finish-output out)
    (db.client-test-multiuser)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%db.dir-test-multiuser...")
    (finish-output out)
    (db.dir-test-multiuser)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%db.resources-test-multiuser...")
    (finish-output out)
    (db.resources-test-multiuser)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%test-all-multiuser...")
    (finish-output out)
    (test-all-multiuser)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%test-chdir...")
    (finish-output out)
    (test-chdir)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%test-acl...")
    (finish-output out)
    (test-acl)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%test-membership-primitives...")
    (finish-output out)
    (test-membership-primitives)

    (setf db.txn:*cursor* db.io:+journal-start+)
    (format out "~%test-membership-thoroughly...")
    (finish-output out)
    (test-membership-thoroughly)
    
    (format out "~%DONE TESTS ~a..." (1+ x))
    (finish-output out)
    
    (format *loop* "~%TESTS DONE! ~a" (1+ x))))

;(dotimes(i 4)(setf db.txn:*cursor* db.io:+journal-start+)(db.resources-test-multiuser)(format *loop* "~%DONE ~a" i))
;(dotimes(i 4)(setf db.txn:*cursor* db.io:+journal-start+)(test-all-multiuser)(format *loop* "~%DONE ~a" i))

;; What about having a *tests* variable--- and I just adjoin them onto that?  Many functions  I could locally bind
;; *tests* to make local tests (as-test-set name). That gives me hierarchy, I could even make it multiply depthed.
;; Tests are just functions, they would use some special ast-error ast-warn ast-values ast commands to check things.
;;
;; To be perfectly honest though... I doesn't add anything but pretty pictures. 

;; FACTOR THE TESTS!
;; I do things like (single oid) (rz-start) (let ((originally (???)) (target originally) (rz-delete40 originally)) ...)
;; enough that I should combine them... I could return zero upon change --- or better was and IS... or
;; actually, just take WAS earlier and work awy... either way, single rl rz-delete40 could be combined...
;; 

;; Okay, its time to refactor clean up the tests and the API a little bit based on how the tests use it... 
;; This won't be especially easy. I think on the originally, target thing I can use some macros...
;; with-directory,   with-page,  with-cartulary, with-client

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; todo: have my web editor include an md5 checksum token of existing conflict. them when i commit, i can check against this
;; and easily detect mid-air collisions and warn the user they are overwriting somebody elses changes. 

;; todo: Use the journal reset mechanism as a backup mechanism --- another client just logs in, steals the latest
;; journal records and applies them. When the journal is reset, I record the time between updates... then once a client
;; uses the journal to backup, I tell them come back in 1/2 that time (to always be good).


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; okay, what next? btrees or transaction logs?   i think the txn log should come as part of the databases file format..
;; maybe?  i dunno... how big should the transaction log be? 
;; 
;; i think that the transaction log operates entirely independent of the database. therefore it should be defined
;; as part of the db/txn structure. so it should be the next file, or between txn and io. 

;; theres a problem!  if i commit and alloc a bunch of things --- the allocs happen after the writes --- if i crash,
;; the txn is logged, but the allocs may not be yet!   if i do things the other way, i could lose space (less of a
;; problem).

;; i need --- as part of the txn going through, that the allocations were written.... therefore, they should be arguments
;; to end. a list of allocations, and a list of deallocations...  the txn should txn protect the alloc page...
;; 
;; this is a real big pain in the ass!  another way might be to allocate, and undo upon restart...
;; 
;; a better way might be to mix allocations in with txns (txn journal entries have allocation entries after their
;; sector entries. this way, only txns can allocate space, but its easy to make a 1job txn that allocates space then commits
;; and then the txn-less streamer just writes to it.
;;
;; then my peek method by default also skips things that are about to be written... i still have a alloc journal, but
;; its rewritten when the txn log fills up.
;;
;; thats a much better design!


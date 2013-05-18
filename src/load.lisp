(declaim (optimize (speed 0) (compilation-speed 0) (space 0) (debug 3)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require :sb-md5)
  (require :sb-posix)
  (require :sb-bsd-sockets)
  (require 'asdf)
  (asdf:operate 'asdf:load-op :swank)
  (asdf:operate 'asdf:load-op :local-time))
(setf local-time:*default-timezone* local-time:+utc-zone+)
(setf *random-state* (make-random-state t))

(in-package :sb-impl)
(export '(toplevel-repl sharp-r) :sb-impl)
(in-package :cl-user)

(defconstant +version+ 3.2)

;; 3.2
;;  X General DB clean up
;;    X Simplified db-threads.  The data structures are now in the files that use them, rather than globally in thread.lisp
;;    X Simplify declaration of INFO's so they are in the files that use them. 
;;    X Switched to a base36 encoded client names. 
;;    X Got rid of get-parent, set-parent. 
;;  X DB Re-imaging.
;;    X Change DB type number constants ---> Document is now 0, Page is now 1, View is now 2, Chart is now 3.
;;    X Simplify security.  I have 1 set of bits for documents, and 1 set for resource items. 
;;    X Let charts and views share the same resource tree. (Later I'll merge pages in as well...)
;;      X Rename view tree to be resource-tree 
;;      X implement chart saving and loading stuff... as much as the DB needs to know.
;;  * Added Charting
;;      X Chart edit/preview/update cycle.
;;      X Charts display.
;;      X Update 'list of views' to also include the list of charts beneath each view.
;;      X Add the list of views to the charts side panel.
;;      * Implement the chart legend. 
;;  * Implement BBS demo and signup thing and subdomain.

(format t "~%Loading UTIL...")
(load "src/util/util")
(load "src/util/tree")
(load "src/util/tagged")  ;; Could clean this, but haven't. 
(load "src/util/blowfish")
(load "src/util/encrypt")

(format t "~%Loading DB...")
(load "src/db/io")       ;; (eventually add a search tree/hashtable to sector lookups to speed that operation)
(load "src/db/txn")
(load "src/db/threads")   ;; Database thread-local variables.
(load "src/db/bt")        ;; btrees of fixed sizes.
(load "src/db/cursor")  
(load "src/db/metadata")
(load "src/db/resources")
(load "src/db/dir")       ;; Directories are complex, user make-entry & delete-entry
(load "src/db/acl")       ;; ACL lists and GROUPS have their own complicated algorithms.
                          ;; (also, stop decoding-on-the-fly, and split this into ACL and GROUPS.)
(load "src/db/client")    ;; Client b+tree. For subscribing and affiliating.
(load "src/db/archives")  
(load "src/db/db")        ;; I should look at my package exports... maybe I can export less.
;(load "src/test")        
;(error "TESTS DONE")

#|
(format t "~%Loading DBDMP...")
(load "src/dbdmp/dbdmp")   ;; HERE I AM... 
                           ;;  x Change DB constant numbers.
                           ;;  x Run tests again...
                           ;;  x Change security settings. 
                           ;;  x Run tests again...
                           ;;  x Copy dump data over here.
                           ;;  x Fix importer to work nicely.
                           ;;  x Import --- test it out! 
                           ;;    -- Seems good... lets add chart saving.

(format t "~%Loading FORTH...")
(load "src/forth/base")
(load "src/forth/closure")
(load "src/forth/error")
(load "src/forth/nondetermism")
(load "src/forth/document")
(load "src/forth/style")
(load "src/forth/forth")

(format t "~%Loading Flow...")
(load "src/flow/flow")


(format t "~%Loading VIEWS...")
;; Views are a bit more complex than they need to be BECAUSE I've made the assumption that I won't load the view when 
;; loading from a cache (even though caches don't exist yet).  It goes
;; VIEW-FORTH --> Wrappers --> View Obj --> Accessors --> view.  If I made the assumption I always have the view, and cleaned
;; up some of the FORTH, I might be able to have VIEW-FORTH --> VIEW. (Load the data into object, and the FORTH CODE into
;; control>)
(load "src/view/compiler")
(load "src/view/parser")    ;; TODO: move this out of view into forth because both view and chart use it.
(load "src/view/view.lisp") ;; Currently Reads db.archives package in addition to db.db. needs doc knowledge.

(load "src/chart/afm.lisp")
(load "src/chart/base.lisp")
(load "src/chart/svg.lisp") ;; NOTE: If I decide to render PNG, CL-GD might handle the bitmap parts.
(load "src/chart/compiler.lisp")
(load "src/chart/chart.lisp")   ;; Okay, my next step is to make some sort of CHART object, that contains the basics.

;; I need cchart -- compile chart package. It has some similarities to view compiler... I might be able to generalize
;; and reuse parts of it.  The parser is also something I can definitely re-use.

;; Its a precompile step... Basically, I collect the word... AND I replace the users... Well shit.  I could just
;; collect the word, and do a string replace.  Actually, I can't do that... because it is already tokenized.
;; Basically, I want to change the word -- correct the number.   But I only want to change the main words name
;; IFF it is a define word...   Wow....
;; Actually... if I don't use the view compiler, (and I don't think I have any number things) that is probably best 
;; of all. 
;; Okay, so generalize tha parser (maybe put it into Forth).  Write a new chart compiler. And a chart object/engine
;; that knows how to evaluate user code in order to liason with the chart interface...  How small can this be? 


(format t "~%Loading HTTP...")
(load "src/http/modlisp") ;; loose.
(load "src/http/request")
(load "src/http/session")

;; WEB is a mess. I should probably split it into web-common, a variety of web-areas, and then web to tie it back
;; together.
(format t "~%Loading Web...")
(load "src/web/html")
(load "src/web/misc")
(load "src/web/uivar")    ;; Bring this earlier. It describes the workflow. 1 list/struct put earlier could be better.
(load "src/web/fragment") 
(load "src/web/view")     ;; VIEWS and PAGES both carry a path with them... this is probably unnecessary because I have
(load "src/web/chart")
(load "src/web/designer") ;; resource-path> in *ui-vars*.
(load "src/web/parser")
(load "src/web/render")
(load "src/web/page")
(load "src/web/csv")  ;; This should probably go into a 'WIZARDS' folder.
(load "src/web/secure")
(load "src/web/aux/marketing")
(load "src/web/web")

(format util.util:*loop* "~%Source loaded.")

;; Okay, great, I got the CHART editor loaded... 
;; But as part of this, I should load the view up --- and use what I know about it to build default chart code. 
;; I can probably get a preview/update cycle working without having to futz with the database too greatly.

(defvar *stop-state* 0) ;; 0 means not stopped.

(defun stop ()
  (unless (zerop (compare-and-swap (symbol-value '*stop-state*) 0 1))
    (format util.util:*loop* "~%Couldn't stop, state was: ~d" *stop-state*)
    (return-from stop nil))
  (setf web.web:*maintenance-mode* t) ;; We won't let anybody else in (no new TXNs)
  (loop until (null db.txn:*txns*))  ;; Wait until all the writers are done.
  (db.txn:update) 
  (loop until (zerop db.txn:*access-mode*) do (assert (null db.txn:*txns*)) do (sb-thread:thread-yield))
  (db.db:checkpoint)
  (setf *stop-state* 2)
  (format util.util:*loop* "~%System stopped...")
  t)

(defun resume ()
  (unless (= 2 (compare-and-swap (symbol-value '*stop-state*) 2 3))
    (format util.util:*loop* "~%Couldn't resume, state was: ~d" *stop-state*)
    (return-from resume nil))
  
  (db.txn:endupdate)  ;; Visitors can came back now too...
  (sb-thread:release-mutex db.txn:*txn-lock*)
  (setf web.web:*maintenance-mode* nil)
  (setf *stop-state* 0)
  (format util.util:*loop* "~%System resumed...")
  t)

(defun signal-success () (ignore-errors (delete-file (pathname "success.tmp"))))
(defun signal-failure () (ignore-errors (delete-file (pathname "failure.tmp"))))

(flet ((startup (&optional headless-p initialize-p)
	 (when headless-p
	   (format t "~%Starting SWANK...")
	   (setf swank:*use-dedicated-output-stream* nil)
	   (swank:create-server :port 4005 :dont-close t :coding-system "utf-8-unix" :style :spawn))
	 ;; I can't make any sense of how local-time handles timezones,
	 ;; this turns them off so I can do it myself.
	 (setf local-time:*default-timezone* local-time:+utc-zone+)
	 
	 ;;(setf cl-who:*prologue* "<!DOCTYPE html>")
	 (db.db:initialize)
	 (setf db.io:*filestream* (open (make-pathname :name "db" :type "bin" :directory '(:relative))
					:direction :io :element-type '(unsigned-byte 32)
					:if-exists (if initialize-p :error :overwrite)
					:if-does-not-exist (if initialize-p :create :error)))
	 (cond ((eq initialize-p t)
		(format t "~%Initializing Database...")
		(db.txn:endupdate)
		(db.db:with-writing 
		  (db.db:make-client "FORMLIS" "FormLis" 0)
		  (db.db:make-client "SANDBOX" "Sandbox" 0))
		(db.db:checkpoint))
	       ((null initialize-p)
		(format t "~%Replaying Database...")
		(db.txn:replay db.io:+journal-start+)
		(db.db:checkpoint)
		(db.txn:endupdate))
	       (t (format t "~%Database created but left unpopulated.")
		  (db.txn:endupdate)))
	 
	 (format t "~%Database Ready...")
	 (web.web:start)
	 (format t "~%Web Server Running...")
	 (sb-sys:enable-interrupt sb-unix:SIGHUP #'(lambda (signal code context)
						     (declare (ignore signal code context))
						     (format util.util:*loop* "~%SIGHUP received.")
						     (let ((success nil))
						       (unwind-protect (and (stop) (setf success t))
							 (if success (signal-success) (signal-failure))))))

	 (sb-sys:enable-interrupt sb-unix:SIGUSR1 #'(lambda (signal code context)
						      (declare (ignore signal code context))
						      (format util.util:*loop* "~%SIGUSR1 received.")
						      (let ((success nil))
							(unwind-protect (and (resume) (setf success t))
							  (if success (signal-success) (signal-failure))))))
	 (format t "~%Unix IPC Installed...")))
  (defun start-server () (util.util:restore-core-allocs) (startup t nil) (sb-impl:toplevel-repl nil))
  (defun start-local (&optional initialize-p) (startup nil initialize-p)))

(defun make-image () (sb-ext:save-lisp-and-die "formlis.core" :executable nil :toplevel #'start-server))

;;(db.db:checkpoint) ;; Save to disk... use shutdown when the system is going down (it prevents writing after)

;; Alberta Health <-- Vinnies friend has a dad who works here...


;(let ((db.db:*thread* 1))
;  (declare (special *thread*))
;  (dolist (client (db.db:clients))
;    (db.db:with-client client
;      (db.db:with-writing 
;	(db.db:with-view-tree (view)
;	  (db.db:oid db.db:+user-alphabetical-view+)
;	  (db.db:copy-oid) 
;	  (setf view (db.db:sexp! view "COLUMN0 \"Username\" ; 
;COLUMN1 \"Enabled\" ;
;VALUE0 [/Enabled] if bold else grey ink then [/Username] ;
;VALUE1 [/Enabled] if \"Yes\" else \"No\" then ; 
;
;SORT0 ascending ;")))))))


;(flet ((fix-permissions (permissions)
;	 (util.util:aif (find (car permissions) (list db.acl:+role-administrator+
;						      db.acl:+role-designer+
;						      db.acl:+role-editor+
;						      db.acl:+role-user+
;						      db.acl:+role-submitter+
;						      db.acl:+role-reader+
;						      db.acl:+role-no-access+) :test #'equalp)
;			(progn (format t "~%Fixed ~s --> ~s" permissions (db.db:role-number util.util:it)) 
;			       (cons util.util:it (cdr permissions)))
;			(if (equalp (car permissions) #(0 0 2147519628 2147519624 2147536012))
;			    (progn (format t "~%Fixed ~s --> user" permissions)
;				   (cons db.acl:+role-user+ (cdr permissions)))
;			    permissions))))
;  (let ((db.db:*thread* 1))
;    (declare (special *thread*))
;    (let ((clients (db.db:with-reading (db.db:clients))))
;      (dolist (client clients)
;	(db.db:with-writing
;	  (db.db:with-client client
;	    (format t "~%CLIENT: ~s" client)
;	    (loop for oid from 0 to (1- (db.client:next-oid))
;	       for (inherit . ninherit) = (db.db:permissions oid)
;	       when (or inherit ninherit) 
;	       do (format t "~%Permissions ~a: ~s ~s" oid inherit ninherit)
;	       and do (db.db:with-acl-tree (acl)
;			(setf acl (db.db:set-acl acl oid (mapcar #'fix-permissions inherit)
;						 (mapcar #'fix-permissions ninherit)))))))))))

;	        
;	  
;	  (db.db:!!!dump-documents 256))))



;; DUMP ALL DATA FROM A PAGE SO I CAN RE-IMPORT...
;(let ((db.db:*thread* 1))
;  (declare (special *thread*))
;  (db.db:with-writing 
;    (db.db:with-client "BGE"
;      (db.db:!!!dump-documents 256))))
    
;(let ((db.db:*thread* 1))
;  (db.db:with-writing 
;    ;; caste is usually 1...
;    (db.db:make-client "AUTOPRO" "Autopro" 1)
;    (db.db:with-client "AUTOPRO"
;      (db.db:with-acl-tree (acl)
;	(setf acl (db.db:set-acl acl db.db:+top-page+ (list (list db.db:+role-designer+ #b01)) nil))))))

;(dolist (client (db.db:clients))
;  (db.db:with-writing 
;    (db.db:with-client client
;      (db.db:make-group-page)
;      (db.db:make-import-page))))

;(dolist (client (db.db:clients))
;  (db.db:with-writing 
;    (db.db:with-client client
;      (db.db:with-acl-tree (acl)
;	(setf acl (db.db:set-acl acl db.db:+top-page+ (list (list db.db:+role-designer+ #b01)) nil))))))
;(db.db:with-writing 
;  (db.db:with-client "FORMLIS"
;    (db.db:with-acl-tree (acl)
;      (setf acl (db.db:set-acl acl db.db:+top-page+ (list (list db.db:+role-designer+ #b01)
;							  (list db.db:+role-reader+ #b00)) nil))))
;  (db.db:with-client "SANDBOX"
;    (db.db:with-acl-tree (acl)
;      (setf acl (db.db:set-acl acl db.db:+top-page+ (list (list db.db:+role-designer+ #b01 #b00)) nil)))))

;; BIG TODO:  X Direct output HTTP
;;              - Long term I could have my own HTTP server... that would make configuration and deployment easier.
;;                But is tricky to interface with OS.  For now, I'll just drive down a level by sending things directly
;;                rather than holding onto them.
;; 
;;                I'll still be single threaded for now... mod_lisp threading isn't so wonderful anyway.
;;
;;                Its complicated, because it is BACKWARDS... I define the interface here, rather than waiting until
;;                later... I need request objects, sure, but... y'know... I shouldn't have these indirections.
;;
;;                New flow is --- I recieve a request.  Read the headers, restore the session, dispatch to handle.
;;                                close the request...
;;
;;                I have this request object... do I need it?  Should I have a per-thread thing to populate? 
;;                I think the object is a good idea. Just not sure how I'll build it... I guess start with an
;;                empty request -- and then start reading headers and populating it.
;;                Then try to restore the a session. 
;;
;;                THREAD 0 isn't used by HTTP... so I can always get a TXN from CLI.  Threads 1-15 do.
;;            * Redo the permission errors, they talk to much.
;;            * Properly multithreaded web system
;;            * Use parent-path for breadcrumbs
;;            X Cookie detection.
;;            X URL rewriters for cookieless sessions
;;            X Add a host-subdomain and host-scheme command to http, rebuild client to work with that.
;;            X Single session per bot type --- DO NOT PRINT DELETE/DESIGN/HISTORY/etc LINKS (or better, make DELETE links
;;              send a DELETE, not post if that is possible).  Do not make BOTS have to request a session.
;;              (seperate session per bot type, and a catchall session for other bots).
;;            X Manually minify CSS
;;            X Convert to a Limited Palette PNG sprite sheet, and PNGCRUSH it, and OPTIPNG it.
;;              USE: OPTIPNG, PNGOUT, advpng, deflopt.   That should make it as tiny as possible. 
;;              Try also, PNGNQ which does lossy.
;;            X gzip compress the stylesheet and have that as an option. 
;;             ;; Send content-encoding: gzip.
;;            X Keyword options that ARE numbers should be saved as numbers. 
;;            X Add last-modified dates to ALL page rendering (leave views without this for now).
;;            * Look into my postbacks and errors --- see if there is a way I could rewrite so that after correcting an error
;;              the post can resume.  This would be useful for things like having to log-in-again and not losing the,
;;              updates.
;;            * Eventually, people will want their own DOMAIN names... can I support that easily? 
;;            X Access control
;;            * Errors should LOG themselves.
;;            * Category offsets, so they can be in the middle of the data rather than at the front.
;;            * Grouping something at the end doesn't group properly --- it groups the next column instead!
;;            * Do something with errors so they don't abort view construction... they should just collect the errors...
;;              -- preview should show them, display should ignore them --- They need to be continuable...
;;            * Add e-tags to pages in the form: loginid-langid-lastmodified-lastsecuritymodified-lastdocmodified.
;;            * Add support for if-modified-since and if-none-match.
;;            * Site maps, Search Indexes and usage statistics (including recently modified). Also RSS feeds of recent
;;              changes.  I assume these will all depend on the same basic framework --- walk the entire site periodically
;;              and compile a completely new set of information.
;;            * Page History.
;;            * More robust time handling in the face of time zones...  Currently I am using CALGARY time and pretending
;;              It is GMT.   This is wrong, but systematically wrong, so I can correct it when I implement proper time 
;;              handling.
;;            * Allow multiple SHOW and HIDE statements.  They should be OR-ED together... HIDE maybe anded... 
;;            X I want a COLLAPSE word so I can default certain columns to collapsed.
;;            * I want a FORMAT word (maybe?) so users can control the output formatting of columns.
;;            ? double login problem --- probably due to sessions AND loging in at the same time.
;;            X I'll probably want a DATA IMPORTER...  So I can take a CSV file --- and display it.
;;              They can select the import names (and I should display the fields ON the page in some way so they can
;;              select where they should go).   AND give them the option to 'X' off rows they don't want, or make 
;;              changes some changes. 
;;            * Change it so that DOWNLOADING view as CSV doesn't depend on a continuation.
;;            X Implement security pages.
;;            * Re-arrange all of my web stuff.... there is a better organization than everything in the dog pile.
;;              Its a bit of a pain because it's all a big switch statement --- but their might be a way to represent it
;;              as 'web primitives' and 'web processes'.
;;            * Clean up the directory a bit. Delete all those stale text files.
;;            X Reorder permissions  --- NO-ACCESS, CUSTOM, ADMIN ...  No-access has highest precedence, then custom, then
;;              admin. (WON'T DO).
;;            X Fix that bug where if I have no newlines at the end of an included subform or table or something
;;              I get an error.
;;            * TEST making multiple user accounts with the exact same name.  It shouldn't be allowed -- but Dad
;;              pulled it off.
;;            X Start thinking about CHARTS -- and the tables that will power them.  I'll probably need a couple of
;;              days of solid 'thinking time'.
;;            * Update manual to have some security interface notes.
;;            * Finish writing the manual FAQ
;;            * Finish adding groups. 
;;            * give DATES the ability to default to today/now +/- months, hours, weeks.  This is probably more useful
;;              than specifying an absolute date.

;; Recently...
;;  * Switched from application/x-www-form-urlencoded to multipart/form-data.  This is required for the future file/upload
;;    fields.
;;  * Security controls now work.  Groups not implemented yet... but it is still pretty good.

;; BUSINESS TODO:
;;   * Grow my sales pipe at the bottom (how can I get more evangelists --,   How can I get people to want to talk about
;;     my product more.
;;   * People are probably confused about what they are getting. 
;;   * Too few people know about it... I should try to make that PR sheet thing now... research tools to do so.
;;   X Fixed CSV export.... not so eager to use quotes.  No leading space because it caused excel to treat as unquoted.
;;   ? With repeating rows --- sometimes triggering them causes the views to stop displaying... 
;;     (can't reproduce -- I think it might have been a cache issue...)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Okay, now I need to add interfaces for GROUPS and SECURITY...  this isn't as trivial as I'd like.
;; I think I'll do GROUPS first. 

;; But I seem to resist starting this... why is that?  That is sometimes a subconciously sign
;; that I haven't addressed something...
;; 
;; Perhaps the difficulty in a group list?  Perhaps I have no roadmap for getting their? 
;; Whats the simplist thing...
;;
;; The singlist thing is making my two column thing that is functionally tied up with something...
;; I gotta have control throughout (i can't just hand it a list argument). 
;;
;; Then my next step would be to iterate the users while processing this, in order to show them...
;; Hmm... but I have to write to two places... that could be harder..   Maybe I should precompute my
;; lists before building this.... it would probably be easier... I'll just put them into boxes, and then
;; render.  It's not exactly the same as lists. 
;; 
;;
;; Forget about the right hand side for now.


;; OKAY!!! OKAY!!! I see the difficulty.  There is no way to cleanly hook this feature in. 
;; Groups are pages right?  BUT how can I Let people create a new group?  Also my WEB stuff is
;; getting kind of ornerous... maybe I should clean first?  A version 3.1 that gets a thorough cleanup...
;;
;; X In DB, switch to using raw RL rather than those wrapper routines. 
;; * Try to clean up the UI-VAR stuff... and maybe have a macro that totally sets all the other macros up.
;;   (as-writer-for as-reader-for)
;; X Some of the web stuff is a bit wonkey... maybe there is a way I can better setup those headers? 
;; * Can I make web:serve straight table driven?  (no but I can and did simplify)
;; X Rename acquire + release in db.io to something else so I don't have to shadow them later.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Okay... how can I make my program better today?  
;;
;; I DO need to import stuff --- and this WILL become my importer later... what makes sense?
;; I'll be expected to import files, text, CSV, databases, etc... 
;; 
;; How can I make this work without a lot of fuss....
;; I really think I SHOULD be able to hook up my VIEW engine to your CSV..  THEN You could use my report engine
;; to create a view..
;;
;; THEN, if the view had save buttons you could just select the things you wanted saved and click SAVE. 
;; ... Thats interesting.  -- Is it a good way to go?   Or is the added VIEW complexity worse than just having
;; an import wizard? 
;;
;; A wizard is probably better. In anycase --- the wizard SHOULD display the information it got... 
;; which (at first) will just be 1 table of data. I want you to be able to TOGGLE COLUMNS off.  So to say:
;; NO, DON'T IMPORT. 
;;
;; What will the backend of this be?  Pretty easy I think --- if all I have are tuples -- and a target page.,
;; I can wack that in in ~10loc.   The bigger question is where is the meta data coming from?  CREATED/UPDATED
;; are easy... CREATER/UPDATER are easy... what about OWNER?  Its probably easiest to just say the uploader is the
;; owner. 
;;
;; WHERE will this wizard be?  --- IT could be when you attach it to a page?  It could be in SYSTEM 
;; --- data import ---
;;
;; This operation is going to be pretty rare I think.  It should install the data. --- What about creating a view...
;; probably not.   So I think system/import --- this wizard tries to do intelligent things based on what you've
;; given it. 





;; Hmm... I should probably work on the self-signup and demo thing maybe?  
;; It might be better to focus on THAT then it would be to blog...
;;
;;  * Maybe I can take my CSV importer and let people try it out --
;;    (SEE if FormLis can handle your data).
;;  * What would be REALLY fast about my product that might generate some interest...
;;    (or be useful!). 
;;  * What sort of useful apps could I build in FormLis that would be good...
;;    --- Maybe the pbWikis  'add supported wikis' model is worth looking at.
;;  * Maybe handling self-signup is a good way to go... 
;;  * Hmmm... Rubber spoon yes. 
;;  * IF I have a free account --- what am I locking down? 
;;    (file uploads?, branding? limited users?)
;;  * I think any SINGLE free app should be restricted to a page a form and many views.
;;  * Or maybe just a free app --- but you don't get access to the designer tools. 
;;  * 
;;  * Something that is REALLY easy... 
;;    REALLY REALLY EASY to spread... 
;;  * 
;;  * OKAY ---- One thing I could do is make the self signup work --- and then at the last
;;    stage have a 'invite your office' stage -- where maybe I pilfer their email and produce
;;    a list...   Or at least giving them an invite link to mass mail. 
;;  * If I can give an incentive...  a referal program. 
;;
;;  * I should add 'share links' on my main formlis page... maybe. 
;;
;; -- Demo builder --
;;
;; I think I should make a demo builder that uses a CSV import. 
;;
;; Then I create the page, upload their data, and build some views. 
;;
;; I think I would need something for them to do afterwards...  Perhaps
;; customizing their main page. 

;; http://andrewchenblog.com/2008/06/29/25-reasons-users-stop-using-your-product-an-analysis-of-customer-lifecycle/
;;
;; Can I make this fun for people?  Views give instant feedback... hmmm.. 
;;
;; Hmmmm... maybe I should do a free model that means you get 3 designers, and each can have 6 users.
;; The designers each get a page and a view... and can permit 6 users to use it.
;;
;; Yeah --- the freebie is 2 designers, 8 users, and two forms each with 3 views. 
;;
;; My goal should be to get these two freebie designers up to speed on what my tools can do... 
;; So I want them integrated with the forum or something... 
;;
;; Okay, freebie means 8 users (max). And three pages. Home, Page1 & Page2. 
;; 
;; No... waste of time... I should instead focus on getting them signed up for the full featured 3 month trail.
;; To this end, I should have THIS stream lined, and then link them to tutorials on building a form, importing CSV data
;; and producing reports.
;;
;; I think I should look at my stats again. 
;; 

;;
;;
;;
;; 
;; 
;;
;;

;;
;; ANGEL LIST
;; Chris Arsenault  --- Calgary
;; http://angel.co/kevin_swan  -- edmonton
;; http://angel.co/kradage     -- vancouver
;; http://angel.co/dannyrobinson -- vancouver
;; http://angel.co/brendan       -- vancouver

;; 
;; W Media Ventures, a Vancouver-based venture capital firm.






;; Alright... I have some problems. 
;; My open-cache function expects me to be pointing at a view... 
;; Alright, fixed by adding open-view-direct and open-view-indirect. 
;; Now my problem is it is called object> and I think only view uses this.  So I should call it view>
;; Okay... now I have my environment basically set up...  So now what? 
;; Do I walk the data?  Or do I run my forth program on it? 
;;
;; The latter I guess. So... time to work on that.

;; I think I need saving.. that would make it a lot easier to develop now. 
;; * Postphone to Friday so I have weekend to do the data porting. 

;; Okay, a chart of daily taken   vs daily average  for several people. 
;; Nope, not together. Daily total per person in each category. 
;; Can I put my X-EMPH on my averages --- yes... but hmm..   I should be able to compute the average to display
;; in the bars. 

;;

;;PLOT bar chart ;
;;ON "/*/" leaf-totals ;
;;LIMIT 1 ;
;;POINTS-PER-ROW 6 ;
;;
;;X p# 0 case "Protein" ; then 
;;     1 case "Grains" ; then
;;     2 case "Vegetables" ; then
;;     3 case "Fruits" ;  then 
;;     4 case "Milk"   ;  then
;;     5 case "Fats"   ;  then ;

;;Y 5 p# + column ; 


;; The above is giving me pretty good results... EXCEPT for one issue.  I'm not defining 1 plot with many values... 
;; I'm actually defining many many plots. 
;;
;; So either I change the meaning of REPEAT to be what I need... or I change the plot to scale bars after ignoring all
;; the nils. 
;;
;; I think changing REPEAT might be the better solution (in this case). Because then I only have a TOM label, 
;; rather than a TOM-Protein, TOM-Grain, etc. 

;; This also handles the color thing. Is there a time when I would want the OTHER (current) behavior? 
;; Perhaps if a single element listed several data points.   If I don't call it REPEAT though, what do I call it?
;; 
;; Basically, this is when you are defining your plot using data in several columns, rather than across rows. 
;; 
;; I think I want REPEAT --- but it isn't the tool I want to use here.  
;; Repeats should be TOTAL DIFFERENT colors --- so that handles the color thing.

;; Maybe DATA-POINTS-PER-ROW ... ;; ACROSS 
;; or DENSITY? 
;; POINTS-PER-ROW  --- seems okay.. 
;; ... year, points-per-row.

|#

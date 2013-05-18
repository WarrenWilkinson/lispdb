(defpackage :util.util
  (:use :common-lisp)
  (:import-from :sb-alien extern-alien alien-funcall unsigned function)
  (:import-from :sb-sys system-area-pointer)
  (:export *output* *loop*
	   ensure-list papply iota malloc parsenum split compose
	   it aand aif awhen acond
	   nposition nfind ncase group compile-time-value scan
	   2010now 2010normalize 2010base
	   base64 base64-pad unbase64 url-base64 url-unbase64 split 
	   pathname-directory+name pathname+file pathname+dir pathname-last pathname-up

	   def-coretime-allocated-var
	   restore-core-allocs))


(in-package :util.util)

(defvar *output* *standard-output*)
(defvar *loop* *standard-output*)

(defun ensure-list (a) (if (listp a) a (list a)))
(defun papply (fn &rest values) #'(lambda (&rest more-args) (apply fn (append values more-args))))
(defun iota (count &optional (start 0) (step 1)) (loop repeat count for i from start by step collect i))
(defun malloc (bytes) 
  (let ((mem (alien-funcall (extern-alien "malloc" (function system-area-pointer unsigned)) bytes)))
    (sb-kernel:system-area-ub8-fill 0 mem 0 bytes) 
    mem))
(defun parsenum (string &optional (start 0))
  (and (stringp string) (multiple-value-bind (number used) (parse-integer string :junk-allowed t :start start)
			  (when (= (length string) used) number))))
(defun split (element sequence &key (start 0) (end (length sequence)))
  (when (< start end)
    (loop for i = start then (1+ j)
          as j = (position element sequence :start i :end end)
          collect (subseq sequence i (or j end))
          while j)))
(defun compose (&rest fns)
    (if fns
	(let ((fn1 (car (last fns)))
	      (fns (butlast fns)))
	  #'(lambda (&rest args)
	      (reduce #'funcall fns :from-end t :initial-value (apply fn1 args))))
	#'identity))

(defmacro aif (test-form then-form &optional else-form) `(let ((it ,test-form)) (if it ,then-form ,else-form)))
(defmacro awhen (test-form &body body) `(aif ,test-form (progn ,@body)))
(defmacro aand (&rest args)
  (cond ((null args) t)
	((null (cdr args)) (car args))
	(t `(aif ,(car args) (aand ,@(cdr args))))))
(defmacro acond (&rest clauses)
  (if (null clauses) nil
      (let ((cl1 (car clauses))
	    (sym (gensym)))
	`(let ((,sym ,(car cl1)))
	   (if ,sym (let ((it ,sym)) (declare (ignorable it)) ,@(cdr cl1))
	       (acond ,@(cdr clauses)))))))

(defun nposition (item sequence n &key (test #'eq) (key #'identity))
  (if (zerop n) 
      (position item sequence :test test :key key)
      (awhen (nposition item sequence (1- n) :test test :key key)
	(position item sequence :start (1+ it) :test test :key key))))
(defun nfind (item sequence n &key (test #'eq) (key #'identity))
  (awhen (nposition item sequence n :test test :key key) (elt sequence it)))

(defmacro ncase (expr &rest clauses)
  ;; A special case that evalutes the clause argument (so I can use constants) and only does integer comparisons.
  (let ((gs (gensym)))
    `(let ((,gs ,expr))
       (declare (ignorable ,gs))
       (cond ,@(mapcar #'(lambda (clause) `((= ,gs ,(car clause)) ,@(cdr clause))) (butlast clauses))
	     ,@(let ((last (car (last clauses))))
		    (if (or (eq 'otherwise (car last)) (eq t (car last)))
			`((t ,@(cdr last)))
			`(((= ,gs ,(car last)) ,@(cdr last))
			  (t (error ,(format nil "Expected one of ~a but got ~~a"
					     (eval (cons 'list (mapcar #'car clauses)))) ,gs)))))))))

(defun group (source n)
  (if (zerop n)
      (error "Zero Length")
      (labels ((rec (source acc)
		 (let ((rest (nthcdr n source)))
		   (if (consp rest)
		       (rec rest (cons (subseq source 0 n) acc))
		       (nreverse (cons source acc))))))
	(when source (rec source nil)))))

(defmacro compile-time-value (expr) (eval expr))

(defun scan (fn sequence &rest args &key key from-end (start 0) end initial-value)
  (declare (ignore key start end))
  (nreverse (apply #'reduce 
		   (if from-end 
		       #'(lambda (val acc &aux (acc (ensure-list acc))) (cons (funcall fn val (car acc)) acc))
		       #'(lambda (acc val &aux (acc (ensure-list acc))) (cons (funcall fn (car acc) val) acc)))
		   sequence
		   (if (member :initial-value args) (list* :initial-value (list initial-value) args) args))))

(let ((t2010 (encode-universal-time 0 0 0 1 1 2010)))
  (defun 2010now () (- (get-universal-time) t2010))
  (defun 2010normalize (time) (+ time t2010))
  (defun 2010base (time) (assert (>= time t2010)) (- time t2010)))

(defconstant +base64+ (make-array 64 :initial-contents
 (list #\A #\B #\C #\D #\E #\F #\G #\H 
       #\I #\J #\K #\L #\M #\N #\O #\P 
       #\Q #\R #\S #\T #\U #\V #\W #\X
       #\Y #\Z #\a #\b #\c #\d #\e #\f 
       #\g #\h #\i #\j #\k #\l #\m #\n
       #\o #\p #\q #\r #\s #\t #\u #\v 
       #\w #\x #\y #\z #\0 #\1 #\2 #\3  
       #\4 #\5 #\6 #\7 #\8 #\9 #\+ #\/))) 
(defconstant +unbase64+ (make-array 80 :initial-contents
  (list 62 ;; + (ascii 43)
	nil nil nil ;; 2 unused chars, and #\-
	63 ;; / 
	52 53 54 55 56 57 58 59 60 61 ;; ascii digits
	nil nil nil nil nil nil nil ;; 7 unused characters
	0  1  2  3  4  5  6  7
	8  9  10 11 12 13 14 15
	16 17 18 19 20 21 22 23
	24 25                       ;; Ascii upper case
	nil nil nil nil nil nil ;; 6 usused characters
	26 27 28 29 30 31 32 33 
	34 35 36 37 38 39 40 41
	42 43 44 45 46 47 48 49
	50 51)))                ;; Remaining ascii lower case
   
(defun base64 (seq &aux (max (length seq)))
  ;; For every 3 bytes, we can get 4 base64 chars.
  (loop for i from 0 to (1- max) by 3
        as num = (boole boole-ior (boole boole-ior (ash (elt seq i) 16) (if (< (+ i 1) max) (ash (elt seq (+ i 1)) 8) 0))
			          (if (< (+ i 2) max) (elt seq (+ i 2)) 0))
        collect (svref +base64+ (ldb (byte 6 18) num))
        collect (svref +base64+ (ldb (byte 6 12) num))
        when (< (+ i 1) max)
        collect (svref +base64+ (ldb (byte 6 6) num))
        when (< (+ i 2) max)
        collect (svref +base64+ (ldb (byte 6 0) num))))

(defun base64-pad (seq)
  ;; 3 input bytes = 4 output bytes.  = means the last 3-inputs was just 1 byte, == means it was 2 bytes. 
  (concatenate 'string (base64 seq) (ncase (mod (length seq) 3) (0 "") (1 "=") (2 "=="))))

(defun unbase64 (seq &aux (max (length seq)))
  (flet ((unbase64 (char) (elt +unbase64+ (- (char-code char) 43))))
  ;; Every 4 base64 chars gives us 3 bytes
  (loop for i from 0 to (1- max) by 4
        as num = (boole boole-ior
		  (boole boole-ior
		   (boole boole-ior (ash (unbase64 (elt seq i)) 18)
			  (if (< (+ i 1) max) (ash (unbase64 (elt seq (+ i 1))) 12) 0))
		   (if (< (+ i 2) max) (ash (unbase64 (elt seq (+ i 2))) 6) 0))
		  (if (< (+ i 3) max) (unbase64 (elt seq (+ i 3))) 0))
        collect (ldb (byte 8 16) num)
        when (< (+ i 2) max)
        collect (ldb (byte 8 8) num)
        when (< (+ i 3) max)
        collect (ldb (byte 8 0) num))))

(defun url-base64 (seq) (nsubstitute #\/ #\_ (nsubstitute #\- #\+ (base64 seq))))
(defun url-unbase64 (seq) (unbase64 (nsubstitute #\_ #\/ (substitute #\+ #\- seq))))

(defun pathname-directory+name  (path)
  (let ((name (if (pathname-type path)
		  (list (concatenate 'string (pathname-name path) "." (pathname-type path)))
		  (when (pathname-name path)
		    (list (pathname-name path))))))
    (case (car (pathname-directory path))
      (:relative (append (pathname-directory path) name))
      (:absolute (append (pathname-directory path) name))
      (otherwise (append '(:relative ".") name)))))
(defun pathname-last (path) (or (car (last (cdr (pathname-directory+name path))))  (error "Pathname-last of ~a" path)))
(defun pathname+file (path file) (make-pathname :directory (pathname-directory+name path) :name file))
(defun pathname+dir  (path dir) (make-pathname :directory (append (pathname-directory+name path) (list dir))))
(defun pathname-up (path)
  (assert (not (equal #P"/" path)))
  (make-pathname :directory (if (or (pathname-name path) (pathname-type path))
				(pathname-directory path)
				(if (null (cdr (pathname-directory path)))
				    (cdr (pathname-directory path))
				    (butlast (pathname-directory path))))))


(defvar *core-allocs* nil)
(defmacro def-coretime-allocated-var (name size initialization-fn)
  `(progn (defvar ,name (load-time-value (funcall ,initialization-fn (malloc ,size))))
	  (push #'(lambda () (setf ,name (funcall ,initialization-fn (malloc ,size)))) *core-allocs*)))

(defun restore-core-allocs () (dolist (fn *core-allocs* (setf *core-allocs* nil)) (funcall fn)))


(defpackage :db.threads
  (:use :common-lisp :util.util)
  (:import-from :sb-sys sap-int int-sap sap+ sap-ref-32 sap-ref-64)
  (:export +numthreads+ *thread* defthreadlocal pointer integer memory))

(in-package :db.threads)

(defconstant +numthreads+ 32)
(defvar *thread* 0)

#-:X86-64 (defconstant +sap-size+ 4)
#+:X86-64 (defconstant +sap-size+ 8)

;; This macro defines a psuedo structure that is *thread* local and  heap-allocated (and therefore non-relocatable).
(defmacro defthreadlocal (name &rest variables)
  (let ((size (reduce #'+ variables :key #'(lambda (v) (case (second v) (pointer +sap-size+) (otherwise (third v))))))
	(star-name (intern (concatenate 'string "*" (symbol-name name) "*"))))
    (labels ((compile-pointer (offset name &aux (sname (intern (format nil "SET-~a" name))))
	       #-:X86-64
	       `((defun ,name () (int-sap (sap-ref-32 ,star-name (+ ,offset (* *thread* ,size)))))
		 (defun ,sname (sap) (setf (sap-ref-32 ,star-name (+ ,offset (* *thread* ,size))) (sap-int sap)))
		 (defsetf ,name ,sname))
	       #+:X86-64
	       `((defun ,name () (int-sap (sap-ref-64 ,star-name (+ ,offset (* *thread* ,size)))))
		 (defun ,sname (sap) (setf (sap-ref-64 ,star-name (+ ,offset (* *thread* ,size))) (sap-int sap)))
		 (defsetf ,name ,sname)))
	     (compile-integer (offset name part-size)
	       (ecase part-size
		 (32 `((defmacro ,name () '(sap-ref-32 ,star-name (+ ,offset (* *thread* ,size))))))
		 (64 `((defmacro ,name () '(sap-ref-64 ,star-name (+ ,offset (* *thread* ,size))))))))
	     (compile-memory (offset name)
	       `((defmacro ,name () '(sap+ ,star-name (+ ,offset (* *thread* ,size)))))))
      `(progn 
	 (def-coretime-allocated-var ,star-name (* ,size +numthreads+) #'identity); load-time-value (malloc (* ,size +numthreads+))))
	 ;(eval-when (eval load);:execute :load-toplevel)
	 ;(progn
	 ;  (setf ,star-name (malloc (* ,size +numthreads+)))
	 ;  (sb-kernel:system-area-ub8-fill 0 ,star-name 0 (* ,size +numthreads+)))
	 ,@(loop with offset = 0
	         for (name type part-size) in variables
	         append (ecase type
			  (pointer (compile-pointer offset name))
			  (integer (compile-integer offset name part-size))
			  (memory (compile-memory offset name)))
	         do (incf offset (case type (pointer +sap-size+) (otherwise part-size))))))))

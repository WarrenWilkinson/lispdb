(defpackage :util.tree
  (:use :common-lisp :util.util)
  (:export term fork term-p fork-p name payload tree-find ntree-cut tree-append))

(in-package :util.tree)

;; A very simple tree.  Note a payload of NIL means the node is both terminator and fork.

(defun term (name value) (assert (atom value)) (cons name value))
(defun fork (name leaves) (assert (listp leaves)) (cons name leaves))
(defun name (n) (car n))    (defun set-name (n v) (setf (car n) v))    (defsetf name set-name)
(defun payload (n) (cdr n)) (defun set-payload (n v) (setf (cdr n) v)) (defsetf payload set-payload)
(declaim (inline name payload))

(defun term-p (node) (and (consp node) (atom (payload node))))
(defun fork-p (node) (and (consp node) (listp (payload node))))

(defun tree-find (root path &key (test #'string-equal))
  "Finds the subtree at path."
  (tagbody :start
     (unless path (return-from tree-find (values root t)))
     (unless (fork-p root) (return-from tree-find (values root nil)))
     (setf root (nfind (caar path) (payload root) (cdar path) :test test :key #'name)
	   path (cdr path))
     (go :start)))

(defun ntree-cut (root path &key (test #'string-equal))
  "(Functionally) Removes the subtree at path, returns the new root."
  (prog1 root ;(setf root (copy-list root))
    (tagbody :start
       (awhen (and path (fork-p root) (nposition (caar path) (payload root) (cdar path) :test test :key #'name))
	 (setf path (cdr path))
	 (if (null path)
	     (let ((place (nthcdr it root)))              (rplacd place (cddr place)))
	     (setf root (elt root (1+ it))))
	 (go :start)))))

(defun tree-append (branch root path &key (test #'string-equal))
  (prog1 (setf root (copy-list root))
    (tagbody :start
       (if (null path)
	   (rplacd (last root) (list branch))
	   (aif (and (fork-p root) (nposition (caar path) (payload root) (cdar path) :test test :key #'name))
		(progn (setf path (cdr path))
		       (let ((copy (copy-list (elt root (1+ it))))) (setf (elt root (1+ it)) copy root copy))
		       (go :start))
		(error "Could not append to tree @ ~s, the path doesn't exist." path))))))



	   
	   
     
;;;; cl-matchstick.lisp

(in-package #:cl-matchstick)

;;; "cl-matchstick" goes here. Hacks and glory await!

(define-condition fail-match (error) ())
(defun fail-match ()
  (error 'fail-match))

(defun sym-not-kwd-p (x)
  (and (symbolp x)
       (not (keywordp x))))

;; The whole idea is to successfully compare with symbols in all packages
;; If we don't do like that, we occasionally cause name-conflicts when exporting these symbols
;; (for example, with ESRAP-LIQUID)
(defun codewalk-pattern-at-the-car (pattern)
  (flet ((default ()
	   `(progn ,(codewalk-pattern pattern)
		   expr)))
    (let ((it (car pattern)))
      (macrolet ((v (x)
		   `(string= ,(string x) it)))
	(if (sym-not-kwd-p it)
	    (cond ((v or) (with-gensyms (g!-outer)
			    `(block ,g!-outer
			       ,@(mapcar (lambda (x)
					   `(handler-case ,(codewalk-pattern x t)
					      (fail-match () nil)
					      (:no-error (&rest rest)
						(declare (ignore rest))
						(return-from ,g!-outer expr)))) ; short-circuit
					 (cdr pattern))
			       (fail-match))))
		  ((v len) `(progn (if (not (and (listp expr))
					    (equal ,(second pattern) (length expr)))
				       (fail-match))
				   ,(codewalk-pattern (third pattern) t)
				   expr))
		  ((v not) `(handler-case ,(codewalk-pattern (second pattern) t)
			      (fail-match () expr)
			      (:no-error () (fail-match))))
		  ((v quote) `(if (equal ',(second pattern) expr)
				  expr
				  (fail-match)))
		  (t (default)))
	    (default))))))

(defun codewalk-list-subpattern (pattern)
  (with-gensyms (g!-res)
    (flet ((default ()
	     `(progn (rebind-expr-next)
		     ,(codewalk-pattern pattern t))))
      (if (consp pattern)
	  (let ((it (car pattern)))
	    (macrolet ((v (x)
			 `(string= ,(string x) it)))
	      (cond ((v collect-until) (codewalk-pattern `(not ,(second pattern)) t))
		    ((v collect-while) `(let ((,g!-res nil))
					  (iter (while t)
						(handler-case (rebind-expr-next)
						  (fail-match () (terminate)))
						(push (handler-case ,(codewalk-pattern (second pattern) t)
							(fail-match () (rebind-expr-prev) (terminate)))
						      ,g!-res))
					  (nreverse ,g!-res)))
		    ((v maybe) `(progn (handler-case (rebind-expr-next)
					 (fail-match () nil))
				       (handler-case ,(codewalk-pattern (second pattern) t)
					 (fail-match () (rebind-expr-prev)))))
		    ((v cdr) `(progn (rebind-expr-cdr)
				 ,(codewalk-pattern (second pattern) t)))
		    ((v car) `(progn (rebind-expr-car)
				 ,(codewalk-pattern (second pattern) t)))
		    ((v cap) (progn (setf (gethash (second  pattern) *vars*) t)
				`(setf (recap ,(second pattern))
				       ,(codewalk-list-subpattern (third pattern)))))
		    (t (default)))))
	  (default)))))

(defvar *vars*)

(defun codewalk-atomic-pattern (pattern)
  (cond ((keywordp pattern) `(if (not (eq ,pattern expr))
				 (fail-match)))
	((stringp pattern) `(if (or (not (stringp expr))
				    (not (string= ,pattern expr)))
				(fail-match)))
	((symbolp pattern) (if (not (string= "_" (string pattern)))
			       (let ((lst (split "_" (string pattern) :limit 2)))
				 (if (equal 2 (length lst))
				     (if (string= "" (cadr lst))
					 (error "Empty predicate name after underscore: ~a" (string pattern))
					 `(progn (when (not (,(intern (cadr lst)) expr))
						   (fail-match))
						 ,(if (not (string= "" (car lst)))
						      (progn (setf (gethash (intern (car lst)) *vars*) t)
							     `(setf (recap ,(intern (car lst))) expr))
						      'expr)))
				     (progn (setf (gethash pattern *vars*) t)
					    `(setf (recap ,pattern) expr))))
			       'expr))
	(t (error "Don't know how to codewalk this atomic pattern"))))

(defvar cap nil)

(defmacro capture-layer (&body body)
  `(let ((cap (cons (make-hash-table :test #'eq) cap)))
     ,@body))

(defmacro recap (var)
  `(lookup ',var cap))

(defun lookup (symbol env)
  (if (null env)
      (symbol-value symbol) ; here we glue to CLs variables
      (multiple-value-bind (val got) (gethash symbol (car env))
	(if got
	    val
	    (lookup symbol (cdr env))))))

(define-setf-expander recap (sym)
  (if (not (symbolp sym))
      (error "Recap can be only of (unevaluated) symbol."))
  (with-gensyms (g!-new-val)
    (values nil
	    nil
	    `(,g!-new-val)
	    `(setf (gethash ',sym (car cap)) ,g!-new-val)
	    `(recap ,sym))))

(defun codewalk-cons-pattern (pattern)
  (with-gensyms (g!-expr-iter)
    `(progn (when (not (consp expr))
	      (fail-match))
	    (let ((,g!-expr-iter (mk-expr-iter expr)))
	      (macrolet ((rebind-expr-car () `(setf expr (funcall ,',g!-expr-iter :car)))
			 (rebind-expr-cdr () `(setf expr (funcall ,',g!-expr-iter :cdr)))
			 (rebind-expr-next () `(setf expr (funcall ,',g!-expr-iter :next)))
			 (rebind-expr-prev () `(setf expr (funcall ,',g!-expr-iter :prev))))
		,@(mapcar (lambda (x)
			    `(let (expr)
			       ,(codewalk-list-subpattern x)))
			  pattern)
		,@(if (or (atom (car (last pattern)))
			  (not (eq 'cdr (car (car (last pattern))))))
		      `((if (not (end-of-iter-p ,g!-expr-iter))
			    (fail-match)))))
	      expr))))

(defun end-of-iter-p (iter)
  (funcall iter :end-p))

(defun codewalk-pattern (pattern &optional at-the-car)
  (if (atom pattern)
      (codewalk-atomic-pattern pattern)
      (if at-the-car
	  (codewalk-pattern-at-the-car pattern)
	  (codewalk-cons-pattern pattern))))

(defun %codewalk-pattern (pattern)
  (let ((*vars* (make-hash-table :test #'eq)))
    (values (codewalk-pattern pattern t) *vars*)))

(defun mk-expr-iter (thing)
  (let ((inner-thing thing)
	prev-cur-thing
	cur-thing
	(index -1))
    (lambda (cmd)
      ;; (format t "inner : ~a, prev-cur-thing : ~a, cur-thing : ~a, index : ~a~%"
      ;; 	      inner-thing prev-cur-thing cur-thing index)
      (ecase cmd
	(:next (if (equal -1 index)
		   (if (not (consp inner-thing))
		       (fail-match))
		   (if (or (not (consp cur-thing))
			   (not (consp (cdr cur-thing))))
		       (fail-match)))
	       (if (equal -1 index)
		   (setf index 0
			 cur-thing inner-thing)
		   (setf index (1+ index)
			 prev-cur-thing cur-thing
			 cur-thing (cdr cur-thing)))
	       (car cur-thing))
	(:prev (if (equal -1 index)
		   (error "Something went horribly wrong -- requesting prev when index -1"))
	       (if (eq :prev-used prev-cur-thing)
		   (error "Something went horribly wrong -- requesting two prevs in a row"))
	       (setf index (1- index)
		     cur-thing prev-cur-thing
		     prev-cur-thing :prev-used)
	       (car cur-thing))
	(:car (if (equal -1 index)
		  (when (or (not (consp inner-thing))
			    (cdr inner-thing))
		    (fail-match))
		  (when (or (not (consp cur-thing))
			    (cdr cur-thing))
		    (fail-match)))
	      (if (equal -1 index)
		  (setf index 0
			cur-thing (car inner-thing))
		  (setf index (1+ index)
			prev-cur-thing cur-thing
			cur-thing (car cur-thing))))
	(:cdr (if (equal -1 index)
		  (setf index 0
			cur-thing (cdr inner-thing))
		  (setf index (1+ index)
			prev-cur-thing cur-thing
			cur-thing (cdr cur-thing))))
	(:end-p (if (equal -1 index)
		    (not (cdr inner-thing))
		    (not (cdr cur-thing))))))))

		  
(defmacro with-match (pattern thing &body body)
  (once-only (thing)
    (multiple-value-bind (code vars) (%codewalk-pattern pattern)
      `(capture-layer
	(let ((expr ,thing))
	  (declare (ignorable expr))
	  ,code
	  (symbol-macrolet ,(iter (for (key nil) in-hashtable vars)
				  (collect `(,key (recap ,key))))
	    ,@body))))))

(defmacro when-match (pattern thing &body body)
  (once-only (thing)
    (with-gensyms (g!-x)
      (multiple-value-bind (code vars) (%codewalk-pattern pattern)
	`(capture-layer 
	  (let ((expr ,thing))
	    (declare (ignorable expr))
	    (handler-case ,code
	      (fail-match () nil)
	      (:no-error (&rest ,g!-x)
		(declare (ignore ,g!-x))
		(symbol-macrolet ,(iter (for (key nil) in-hashtable vars)
					(collect `(,key (recap ,key))))
		  ,@body)))))))))

(defmacro ecase-match (thing &rest specs)
  (once-only (thing)
    (with-gensyms (g!-x g!-outer)
      `(block ,g!-outer
	 ,@(mapcar (lambda (spec)
		     (destructuring-bind (pattern . body) spec
		       (multiple-value-bind (code vars) (%codewalk-pattern pattern)
			 `(capture-layer
			   (let ((expr ,thing))
			     (declare (ignorable expr))
			     (handler-case ,code
			       (fail-match () nil)
			       (:no-error (&rest ,g!-x)
				 (declare (ignore ,g!-x))
				 (symbol-macrolet ,(iter (for (key nil) in-hashtable vars)
							 (collect `(,key (recap ,key))))
				   (return-from ,g!-outer (progn ,@body))))))))))
		   specs)
	 (fail-match)))))
      
  
(defmacro match-p (pattern thing)
  `(when-match ,pattern ,thing t))

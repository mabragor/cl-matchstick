;;;; package.lisp

(defpackage #:cl-matchstick
  (:use #:cl #:iterate)
  (:shadowing-import-from #:alexandria #:with-gensyms #:once-only)
  (:shadowing-import-from #:cl-ppcre #:split)
  (:export #:with-match #:when-match #:ecase-match #:fail-match
	   #:match-p
	   #:or #:len #:not #:collect-until #:collect-while #:maybe #:cdr #:car #:cap))


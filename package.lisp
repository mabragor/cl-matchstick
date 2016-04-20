;;;; package.lisp

(defpackage #:cl-matchstick
  (:use #:cl #:iterate)
  (:shadowing-import-from #:alexandria #:with-gensyms #:once-only)
  (:export #:with-match #:when-match #:case-match #:fail-match))


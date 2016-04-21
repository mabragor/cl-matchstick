;;;; cl-matchstick.asd

(asdf:defsystem #:cl-matchstick
  :description  "Conveniently and consicely parse s-exps."
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :serial t
  :depends-on (#:alexandria #:iterate #:cl-ppcre)
  :components ((:file "package")
               (:file "cl-matchstick")))

(defsystem :cl-matchstick-tests
  :description "Tests for CL-MATCHSTICK."
  :licence "MIT"
  :depends-on (#:cl-matchstick #:alexandria #:fiveam)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-matchstick))))
  (load-system :cl-matchstick-tests)
  (funcall (intern "RUN-TESTS" :cl-matchstick-tests)))

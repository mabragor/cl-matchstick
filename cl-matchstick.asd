;;;; cl-matchstick.asd

(asdf:defsystem #:cl-matchstick
  :description  "Conveniently and consicely parse s-exps."
  :author "Alexandr Popolitov <popolit@gmail.com>"
  :license "MIT"
  :serial t
  :components ((:file "package")
               (:file "cl-matchstick")))

(defsystem :cl-vhdl-tests
  :description "Tests for CL-VHDL."
  :licence "MIT"
  :depends-on (:cl-vhdl :fiveam :cl-interpol :optima :fare-quasiquote-optima)
  :components ((:file "tests")))

(defmethod perform ((op test-op) (sys (eql (find-system :cl-vhdl))))
  (load-system :cl-vhdl-tests)
  (funcall (intern "RUN-TESTS" :cl-vhdl-tests)))

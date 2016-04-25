
(in-package :cl-user)

(defpackage :cl-matchstick-tests
  (:use :alexandria :cl :cl-matchstick :fiveam)
  (:shadowing-import-from :fiveam :fail)
  (:export #:run-tests))

(in-package :cl-matchstick-tests)

(def-suite matchstick)
(in-suite matchstick)

(defun run-tests ()
  (let ((results (run 'matchstick)))
    (fiveam:explain! results)
    (unless (fiveam:results-status results)
      (error "Tests failed."))))

(test trivial-patterns
  (is (equal :ok (with-match _ '(1 2 3) :ok)))
  (is (equal :ok (with-match _ 1 :ok)))
  (is (equal :ok (with-match _listp '(1 2 3) :ok)))
  (is (equal :ok (with-match _atom '1 :ok)))
  (signals (fail-match) (with-match _atom '(1 2 3) :ok))
  (signals (fail-match) (with-match _listp '1 :ok))
  (is (equal 1 (with-match x_integerp '1 x)))
  (is (equal 1 (with-match x '1 x))))


(test simple-patterns
  (is (equal '(3 2 1) (with-match (a b c) '(1 2 3) (list c b a))))
  (signals (fail-match) (with-match (a b) '(1 . 2) (list a b)))
  (is (equal '(1 2) (with-match (a (cdr b)) '(1 . 2) (list a b))))
  (is (equal '(1 nil) (with-match (a (cdr b)) '(1) (list a b))))
  (signals (fail-match) (with-match (a (cdr b)) '1 (list a b)))
  (is (equal '(1 (2 3) 3) (with-match (a (cdr b) c) '(1 2 3) (list a b c))))
  )

(test match-keywords
  (is (equal 3 (with-match (:a a) '(:a 3) a)))
  (signals (fail-match) (with-match (:a a) '(:b 3) a)))

(test match-p
  (is (equal t (match-p (:a a) '(:a 3))))
  (is (equal nil (match-p (:a a) '(:b 3)))))

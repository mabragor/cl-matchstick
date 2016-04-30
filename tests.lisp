
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

(defun ecase-match-fun (x)
  (ecase-match x (:a 1) (:b 2)))
  
(test ecase-match
  (is (equal 1 (ecase-match-fun :a)))
  (is (equal 2 (ecase-match-fun :b)))
  (signals (fail-match) (ecase-match-fun :c)))

(defun or-fun (x)
  (with-match ((or :to :downto) x y) x
    (list x :smth y)))

(test or 
  (is (equal '(1 :smth 2) (or-fun '(:to 1 2))))
  (is (equal '(1 :smth 2) (or-fun '(:downto 1 2))))
  (signals (fail-match) (or-fun '(:caboom! 1 2))))

(defun cap-fun (x)
  (with-match ((cap dir (or :to :downto)) x y) x
    (list x dir y)))

(test cap
  (is (equal '(1 :to 2) (cap-fun '(:to 1 2))))
  (is (equal '(1 :downto 2) (cap-fun '(:downto 1 2))))
  (signals (fail-match) (cap-fun '(:caboom! 1 2))))

(test quote
  (is (equal 1 (ecase-match 1 ('1 1))))
  (signals (fail-match) (ecase-match 2 ('1 1))))

(test nested-matches
  (is (equal '((2 3) 1 (2 3))
	     (with-match (x y) '(1 (2 3))
	       (list (with-match (x y) y
		       (list x y))
		     x y)))))

(defun cap-maybe-fun (x)
  (with-match ((cap on (maybe (:on _)))
	       (cap until (maybe (:until _)))) x
    (list on until)))

(test maybe
  (is (equal '(nil (:until 2)) (cap-maybe-fun '((:until 2)))))
  (is (equal '((:on 1) nil) (cap-maybe-fun '((:on 1)))))
  (is (equal '((:on 1) (:until 2)) (cap-maybe-fun '((:on 1) (:until 2)))))
  (is (equal '(nil nil) (cap-maybe-fun nil))))
  

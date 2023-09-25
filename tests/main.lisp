(defpackage kamera/tests/main
  (:use :cl
        :kamera
        :rove))
(in-package :kamera/tests/main)

#+nil
(asdf:test-system :kamera)

(deftest unification
  (testing "Unify equal lists"
    (ok (equal
          (unify '(human "Jeff") '(human "Jeff"))
          '(human "Jeff"))))
  (testing "Unify with variables"
    (let ((x (make-variable 'x))
          (y (make-variable 'y)))
      (unify (cons x 10) (cons 20 y))
      (ok (equal (?- y) 10))
      (ok (equal (?- x) 20)))))

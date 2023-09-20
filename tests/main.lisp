(defpackage kamera/tests/main
  (:use :cl
        :kamera
        :rove))
(in-package :kamera/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :kamera)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

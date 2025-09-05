(defpackage music-suite/tests/main
  (:use :cl
        :music-suite
        :rove))
(in-package :music-suite/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :music-suite)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

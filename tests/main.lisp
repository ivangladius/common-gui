(defpackage hello-world/tests/main
  (:use :cl
        :hello-world
        :rove))
(in-package :hello-world/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :hello-world)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))

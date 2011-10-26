(ns incremental.test.heap
  (:use [incremental.test.immediates :only [compiled=]])
  (:use [clojure.test]))

(deftest pairs
  (is (compiled= (car (cons 1 2)) "1"))
  (is (compiled= (cdr (cons 1 2)) "2"))
  (is (compiled= (cons \a \b) "(a . b)")))

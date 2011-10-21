(ns incremental.test.locals
  (:use [incremental.test.immediates :only [compiled=]])
  (:use [clojure.test]))

(deftest locals
  (is (compiled= (let ((a 0)) a) "0"))
  (is (compiled= (let ((a 2) (b 1)) (fx+ a b)) "3")))

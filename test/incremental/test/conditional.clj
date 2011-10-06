(ns incremental.test.conditional
  (:use [incremental.test.immediates :only [compiled=]])
  (:use [clojure.test]))

(deftest if
  (is (compiled= (if (fx= 1 1) 1 2) "1"))
  (is (compiled= (if (fx= 1 2) 1 2) "2")))

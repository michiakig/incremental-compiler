(ns incremental.test.primitives
  (:use [incremental.test.immediates :only [compiled=]])
  (:use [clojure.test]))

(deftest unary-primitives
  (is (compiled= (fxadd1 1) "2")))

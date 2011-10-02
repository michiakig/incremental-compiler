(ns incremental.test.primitives
  (:use [incremental.test.immediates :only [compiled=]])
  (:use [clojure.test]))

(deftest unary-primitives
  (is (compiled= (fxadd1 1) "2"))
  (is (compiled= (fxsub1 1) "0"))
  (is (compiled= (char->fixnum \a) "97"))
  (is (compiled= (fixnum->char 97) "a"))
  (is (compiled= (fixnum? 1) "#t"))
  (is (compiled= (fixnum? \a) "#f"))
  (is (compiled= (fxzero? 0) "#t"))
  (is (compiled= (fxzero? 1) "#f"))
  (is (compiled= (char? \a) "#t"))
  (is (compiled= (char? 0) "#f")))

(deftest binary-primitives
  (is (compiled= (fx+ 1 2) "3")))

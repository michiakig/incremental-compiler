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
  (is (compiled= (fx+ 1 2) "3"))
  (is (compiled= (fx- 2 1) "1"))
  (is (compiled= (fx+ (fx- (fx- 30 3) 3) (fx- 6 5)) "25"))
  
  (is (compiled= (fx= 0 0) "#t"))
  (is (compiled= (fx= 1 0) "#f"))
  
  (is (compiled= (fx< 0 1) "#t"))
  (is (compiled= (fx< 1 0) "#f"))
  (is (compiled= (fx< 1 2) "#t"))
  (is (compiled= (fx< 2 1) "#f"))
  (is (compiled= (fx< 1 10000) "#t"))
  )

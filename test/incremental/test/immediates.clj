(ns incremental.test.immediates
  (:use [incremental.compiler :only [compile-and-run]])
  (:use [clojure.test]))

(defmacro compiled= [program result]
  `(= (compile-and-run ~program) ~result))

(deftest integers
 (is (compiled= 42 "42"))
 (is (compiled= 0 "0"))
 (is (compiled= 1 "1"))
 (is (compiled= -1 "-1"))
 (is (compiled= 10 "10"))
 (is (compiled= -10 "-10"))
 (is (compiled= 2736 "2736"))
 (is (compiled= -2736 "-2736"))
 (is (compiled= 536870911 "536870911"))
 (is (compiled= -536870912 "-536870912")))

(deftest booleans
  (is (compiled= true "#t"))
  (is (compiled= false "#f")))

(deftest empty-list
  (is (compiled= '() "()")))

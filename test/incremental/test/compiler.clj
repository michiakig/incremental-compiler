(ns incremental.test.compiler
  (:use [incremental.compiler])
  (:use [clojure.test]))

(deftest integer-test
  (is (= "42" (compile-and-run 42))))

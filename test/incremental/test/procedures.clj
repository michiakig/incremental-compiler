(ns incremental.test.procedures
  (:use [incremental.test.immediates :only [compiled=]])
  (:use [clojure.test]))

(deftest letrec
  (is (compiled= (letrec ((double (lambda (x) (fx+ x x)))) (app double 2))
                 "4"))
  (is (compiled= (letrec ((double (lambda (x) (fx+ x x)))
                          (triple (lambda (x) (fx+ x (fx+ x x)))))
                         (app double (app triple 3)))
                 "18"))
  (is (compiled= (letrec ((sum (lambda (x)
                                        (if (fx= 0 x)
                                          0
                                          (fx+ x (app sum (fx- x 1)))))))
                         (app sum 100))
                 "5050")))

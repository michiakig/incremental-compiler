(ns incremental.compiler
  (:use [clojure.java.io :only [writer output-stream file reader]]
        [clojure.pprint :only [cl-format]]
        [incremental.utils])
  (:import [java.io BufferedReader InputStreamReader]))

(defn emit
  [fmt & rest]
  (apply cl-format true
         (if (= \newline (last fmt))
           fmt
           (str fmt \newline)) rest))

(defn compile-program
  [x]
  (when-not (integer? x) (throw (IllegalArgumentException.)))
  (emit "    .text")
  (emit "    .globl scheme_entry")
  (emit "    .type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit "    movl $~a, %eax" x)
  (emit "    ret"))

(defn compile-and-run
  [x]
  (binding [*out* (writer (output-stream (file "out.s")))]
    (compile-program x)
    (.flush *out*))
  (shell "gcc src/rt.c out.s")
  (let [result (shell "./a.out")]
    (shell "rm out.s a.out")
    (first result)))

;; (println (compile-and-run 42))

(ns incremental.compiler
  (:use [clojure.java.io :only [writer output-stream file reader]]
        [clojure.pprint :only [cl-format]]
        [incremental.utils])
  (:import [java.io BufferedReader InputStreamReader]))

(defn emit
  "output fmt using cl-format, appending a newline if the string
  doesn't already end with one"
  [fmt & rest]
  (apply cl-format true
         (if (= \newline (last fmt))
           fmt
           (str fmt \newline)) rest))

(def fixnum-shift 2)
(def word-size 4) ; bytes
(def fixnum-bits (- (* word-size 8) fixnum-shift))
(def fixnum-lower (- (Math/pow 2 (- fixnum-bits 1))))
(def fixnum-upper (- (Math/pow 2 (- fixnum-bits 1)) 1))
(defn fixnum? [x]
  (and (integer? x) (<= fixnum-lower x fixnum-upper)))

(def bool_false 0x2f)
(def bool_true 0x6f)
(defn boolean? [x] (= (class x) java.lang.Boolean))

(def empty-list 0x3f)
(defn empty-list? [x] (and (list? x) (empty? x)))

(defn immediate? [x]
  (or (fixnum? x) (boolean? x) (empty-list? x)
      ; ...
      ))

(defn immediate-rep
  [x]
  (cond (integer? x) (bit-shift-left x fixnum-shift)
        (boolean? x) (if x bool_true bool_false)
        (empty-list? x) empty-list
        ; ...
        ))

(defn compile-program
  "compile source program x and emit assembly for it"
  [x]
  (when-not (immediate? x) (throw (IllegalArgumentException.
                                   "only supports constants")))
  (emit "    .text")
  (emit "    .globl scheme_entry")
  (emit "    .type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit "    movl $~a, %eax" (immediate-rep x))
  (emit "    ret"))

(defn compile-and-run
  "compile the program x, assemble it with gcc along with the C
  runtime, run it, cleanup generated filed, and return the first line
  of the output, "
  [x]
  (binding [*out* (writer (output-stream (file "out.s")))]
    (compile-program x)
    (.flush *out*))
  (shell "gcc src/rt.c out.s")
  (let [result (shell "./a.out")]
    (shell "rm out.s a.out")
    (first result)))

;; (println (compile-and-run 42))

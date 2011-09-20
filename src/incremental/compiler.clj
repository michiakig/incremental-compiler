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

(def char-shift 8)
(def char-tag 0x0f)

(defn immediate? [x]
  (or (fixnum? x) (boolean? x) (empty-list? x) (char? x)
      ; ...
      ))

(defn immediate-rep
  [x]
  (cond (integer? x) (bit-shift-left x fixnum-shift)
        (boolean? x) (if x bool_true bool_false)
        (empty-list? x) empty-list
        (char? x) (bit-or (bit-shift-left (int x) char-shift)
                          char-tag)
        ; ...
        ))

;; original Scheme code uses putprop/getprop and the property list on
;; symbols, instead in Clojure we use an explicit map
(def primitives (atom {}))
(defmacro defprimitive
  "adds the named primitive to the map of primitives, including a
  function for emitting the code for the primitive"
  [[name & args] & body]
  `(dosync
    (swap! primitives assoc
           '~name
           {:is-prim true
            :arg-count ~(count args)
            :emitter (fn ~(vec args)
                       ~@body)})))
(defn primitive? [x]
  (and (symbol? x) (not (nil? (@primitives x)))))
(defn primitive-emitter
  "look up the function for emitting this primitive's code"
  [x]
  (:emitter (@primitives x)))
(defn primcall? [expr]
  (and (list? expr) (not (empty? expr)) (primitive? (first expr))))
(defn emit-primcall [expr]
  (let [prim (first expr)
        args (rest expr)]
    ;; (check-primcall-args prim args)
    (apply (primitive-emitter prim) args)))

(defn emit-immediate [x]
  (emit "    movl $~a, %eax" (immediate-rep x)))

(defn emit-expr [expr]
  (cond (immediate? expr) (emit-immediate expr)
        (primcall? expr) (emit-primcall expr)
        :else (throw (IllegalArgumentException.
                      (str "unsupported expression: "expr)))))

(defprimitive (fxadd1 arg)
  (emit-expr arg)
  (emit "    addl $~s, %eax" (immediate-rep 1)))

(defn compile-program
  "compile source program x by emitting boilerplate code and calling
  emit-expr"
  [x]
  (emit "    .text")
  (emit "    .globl scheme_entry")
  (emit "    .type scheme_entry, @function")
  (emit "scheme_entry:")
  (emit-expr x)
  (emit "    ret"))

(defn compile-and-run
  "compile the program x, assemble it with gcc along with the C
  runtime, run it, cleanup (delete) generated files, and return the
  first line of the output"
  [x]
  (binding [*out* (writer (output-stream (file "out.s")))]
    (compile-program x)
    (.flush *out*))
  (shell "gcc src/rt.c out.s")
  (let [result (shell "./a.out")]
    (shell "rm out.s a.out")
    (first result)))

;; (println (compile-and-run 42))

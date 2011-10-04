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
(def fixnum-mask 3)
(def fixnum-tag 0)
(def word-size 4) ; bytes
(def fixnum-bits (- (* word-size 8) fixnum-shift))
(def fixnum-lower (- (Math/pow 2 (- fixnum-bits 1))))
(def fixnum-upper (- (Math/pow 2 (- fixnum-bits 1)) 1))
(defn fixnum? [x]
  (and (integer? x) (<= fixnum-lower x fixnum-upper)))

(def bool_false 0x2f)
(def bool_true 0x6f)
(def bool-bit 6)
(defn boolean? [x] (= (class x) java.lang.Boolean))

(def empty-list 0x3f)
(defn empty-list? [x] (and (list? x) (empty? x)))

(def char-shift 8)
(def char-tag 0x0f)
(def char-mask 0xff)

(defn immediate? [x]
  (or (fixnum? x) (boolean? x) (empty-list? x) (char? x)))

(defn immediate-rep
  "return the immediate representation of the value x. The value x is
  in our language being compiled, and the returned result is in the
  compilation target language"
  [x]
  (cond (integer? x) (bit-shift-left x fixnum-shift)
        (boolean? x) (if x bool_true bool_false)
        (empty-list? x) empty-list
        (char? x) (bit-or (bit-shift-left (int x) char-shift)
                          char-tag)
        ; ...
        ))

;; original Scheme code uses putprop/getprop and the property list on
;; symbols, instead in Clojure we use an explicit map (in an atom)
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

(defn emit-primcall [si expr]
  (let [prim (first expr)
        args (rest expr)]
    ;; (check-primcall-args prim args)
    (apply (primitive-emitter prim) si args)))

(defn emit-immediate [si x]
  (emit "    movl $~a, %eax" (immediate-rep x)))

(defn emit-expr [si expr]
  (cond (immediate? expr) (emit-immediate si expr)
        (primcall? expr) (emit-primcall si expr)
        :else (throw (IllegalArgumentException.
                      (str "unsupported expression: "expr)))))

;; some primitives
(defprimitive (fxadd1 si arg)
  (emit-expr si arg)
  (emit "    addl $~s, %eax" (immediate-rep 1)))
(defprimitive (fxsub1 si arg)
  (emit-expr si arg)
  (emit "    subl $~s, %eax" (immediate-rep 1)))
(defprimitive (char->fixnum si arg)
  (emit-expr si arg)
  (emit "    shrl $~a, %eax" (- char-shift fixnum-shift)))
(defprimitive (fixnum->char si arg)
  (emit-expr si arg)
  (emit "    shll $~a, %eax" (- char-shift fixnum-shift))
  (emit "    orl $~a, %eax" char-tag))

(defn emit-predicate-suffix []
  (emit "    sete %al")
  (emit "    movzbl %al, %eax")
  (emit "    sal $~s, %al" bool-bit)
  (emit "    or $~s, %al" bool_false))

(defmacro deftypep
  "defines a type predicate primitive"
  [name mask tag]
  `(defprimitive [~name si# arg#]
     (emit-expr si# arg#)
     (emit "    and $~s, %al" ~mask)
     (emit "    cmp $~s, %al" ~tag)
     (emit-predicate-suffix)))

(deftypep fixnum? fixnum-mask fixnum-tag)
(deftypep char? char-mask char-tag)

(defprimitive (fxzero? si arg)
  (emit-expr si arg)
  (emit "    shrl $~a, %eax" fixnum-shift) ; eax hold value of arg
  (emit "    cmp $~s, %eax" 0) ; compare eax to 0
  (emit-predicate-suffix))

(defprimitive (fx+ si arg1 arg2)
  (emit-expr si arg1)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr (- si word-size) arg2)
  (emit "    addl ~a(%esp), %eax" si))

(defprimitive (fx- si arg1 arg2)
  (emit-expr si arg2)
  (emit "    movl %eax, ~a(%esp)" si)
  (emit-expr (- si word-size) arg1)
  (emit "    subl ~a(%esp), %eax" si)) ; subtract arg2 from arg1,
                                       ; leave result in eax
(defn emit-function-header [name]
  (emit "    .text")
  (emit "    .globl ~a" name)
  (emit "    .type ~a, @function" name)
  (emit "~a:" name))

(defn compile-program
  "compile source program x by emitting boilerplate code and calling
  emit-expr"
  [x]
  (emit-function-header "L_scheme_entry")
  (emit-expr (- word-size) x)
  (emit "    ret")
  (emit-function-header "scheme_entry")
  (emit "    movl %esp, %ecx")
  (emit "    movl 4(%esp), %esp")
  (emit "    call L_scheme_entry")
  (emit "    movl %ecx, %esp")
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


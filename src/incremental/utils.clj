(ns incremental.utils
  (:use [clojure.java.io :only [reader]]))

(defn shell
  [cmd]
  (let [proc (.. (Runtime/getRuntime) (exec cmd))]
    (.waitFor proc)
    (line-seq (reader (.getInputStream proc)))))

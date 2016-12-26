(ns brave7 
  (:use [clojure.repl :only [doc]]))

(defmacro infix
  [infixed]
  (list (second infixed)
        (first infixed)
        (last infixed)))


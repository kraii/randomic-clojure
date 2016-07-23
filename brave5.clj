(ns brave5 
  (:use [clojure.repl :only [doc]]))

(defn comp2
  [fn2 fn1]
  (fn [& args] (fn2 (apply fn1 args))))

(defn assoc-in2
  [m [k & ks] v]
  (if (empty? ks)
    (assoc m k v)
    (assoc m k (assoc-in2 (k m) ks v))))

(defn update-in2
  [m [k & ks] v]
  (if (empty? ks)
    (update m k v)
    (update m k (update-in2 (k m) ks v))))

  
; here there be nonsense for me repl
(def socd (assoc-in {} [:bill :bob :brain] "laser"))
(def socd2 (assoc-in2 {} [:bill :bob :brain] "laser"))
(def mappy {:bill "toast"})
(def foo {:user {:bar "baz"}})
(def food (assoc-in foo [:user :id] "some-id"))
(def foody (assoc-in2 foo [:user :id] "some-id"))
(def batman {:batman {:name "brucie" :equipment [:utility-belt :batarang]}})
(def upgraded-batman (update-in batman [:batman :equipment] conj :batmobile))
(def upgraded-batman2 (update-in batman [:batman :equipment] conj :batmobile))

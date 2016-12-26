(ns zeros.core
  (:require [clojure.test :refer :all]))

(defn zeros [n]
  (loop [total 0
         current n]
    (if (< 1 (/ current 5))
      total
      (recur (+ total (quot current 5)) (quot current 5)))))

(deftest Testing...
  (is (= (zeros 0) 0) "Zero has 0 trailing zeros")
  (is (= (zeros 6) 1))
  (is (= (zeros 30) 7)))

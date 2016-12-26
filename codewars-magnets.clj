(ns doubles
  (:require [clojure.test :refer :all]))

;https://www.codewars.com/kata/56c04261c3fcf33f2d000534/train/clojure

(defn- pow [base pow]
  (Math/pow base pow))

(defn- force-at
  [k n]
  (/ 1 (* k (pow (inc n) (* 2 k)))))

(defn- force-row [row k]
  (reduce + (map (fn [n] (force-at k n)) row)))

(defn- build-matrix
  [maxk maxn]
  (map (fn [_] (range 1.0 (inc (double maxn)))) (range 1.0 (inc (double maxk)))))

(defn double-boxes
  [maxk maxn]
  (let [matrix (build-matrix maxk maxn)]
    (reduce + (map-indexed (fn [i row] (force-row row (inc i))) matrix))))

(defn assertFuzzyEquals [act exp]
  (let [inrange (<= (Math/abs (- act exp)) 1e-6)]
    (if (= inrange false)
      (println "abs(actual - expected) must be <= 1e-6. Expected was " exp " but got: " act))
    (is (= inrange true)))) ; lel

(deftest a-test1
  (testing "double-boxes"
        (assertFuzzyEquals (double-boxes 1, 10), 0.5580321939764581)
        (assertFuzzyEquals (double-boxes 10, 1000), 0.6921486500921933)
        (assertFuzzyEquals (double-boxes 10, 10000), 0.6930471674194457)
        (assertFuzzyEquals (double-boxes 20, 10000), 0.6930471955575918)
)) 

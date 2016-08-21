(ns dir-reduc.core
  (:require [clojure.test :refer :all]))

(defn- pointless?
  [x y]
  (or (= (set [x y]) #{"NORTH" "SOUTH"})
      (= (set [x y]) #{"EAST" "WEST"})))

(defn- filter-pointless
  [arr]
  (loop [unfiltered arr
         filtered []]
    (cond
      (empty? unfiltered) filtered
      (pointless? (first unfiltered) (second unfiltered)) (recur (drop 2 unfiltered) filtered) 
      :else (recur (rest unfiltered) (conj filtered (first unfiltered))))))

(defn dirReduc
  [arr]
  (if (empty? arr)
    nil
    (let [filtered (filter-pointless arr)]
      (if (= arr filtered)
        arr
        (recur filtered)))))

(deftest test-1
  (testing "Test 1"
    (def ur ["NORTH", "SOUTH", "SOUTH", "EAST", "WEST", "NORTH", "WEST"])
    (def vr ["WEST"])
    (is (= (dirReduc ur) vr)))) 

(deftest test-2
  (testing "Test 2"
    (let [non-reducable ["NORTH" "WEST" "SOUTH" "EAST"]] 
      (is (= (dirReduc non-reducable) non-reducable)))))

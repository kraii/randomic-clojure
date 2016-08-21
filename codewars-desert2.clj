(ns dir-reduc2.core
  (:require [clojure.test :refer :all]))

(defn- pointless?
  [x y]
  (or (= #{x y} #{"NORTH" "SOUTH"})
      (= #{x y} #{"EAST" "WEST"})))

(defn- dirReduc
  [arr]
  (let [result
        (reduce #(if (pointless? (peek %1) %2)
                   (pop %1)
                   (conj %1 %2))
                [] arr)]
    (if (empty? result) nil result)))

(deftest test-1
  (testing "Test 1"
    (def ur ["NORTH", "SOUTH", "SOUTH", "EAST", "WEST", "NORTH", "WEST"])
    (def vr ["WEST"])
    (is (= (dirReduc ur) vr)))) 

(deftest test-2
  (testing "Test 2"
    (let [non-reducable ["NORTH" "WEST" "SOUTH" "EAST"]] 
      (is (= (dirReduc non-reducable) non-reducable)))))

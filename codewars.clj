(ns lel-tests
  (:require [clojure.test :refer :all]))

(defn factory [x]
  (fn [y] (map (partial * x) y)))

(defn race [v1 v2 g]
  (let [t (/ g (- v2 v1))]
    [(int (Math/floor t))]))

(defn nb-months 
  [start-price-old start-price-new saving-per-month percent-loss-per-month]
  (letfn [(nb-months-inner [months savings current-old-price current-new-price current-loss]
            (let [
                  cost (- current-new-price current-old-price)
                  decimal-loss (/ current-loss 100)]
              (if (>= savings cost)
                [months (Math/round (Math/abs (double (- savings cost))))]
                (nb-months-inner
                 (+ 1 months)
                 (+ savings saving-per-month)
                 (- current-old-price (* current-old-price decimal-loss))
                 (- current-new-price (* current-new-price decimal-loss))
                 (if (= 0 (mod months 2)) (+ 0.5 current-loss) current-loss)))))]
    (nb-months-inner 0 0 start-price-old start-price-new percent-loss-per-month))
)

(defn persistence [n]
  (loop [x n
         mp 0]
  (let [digits (map #(Character/digit % 10) (str x))]
    (if (<= (count digits) 1) 
      mp
      (recur (reduce * digits) (+ 1 mp))))))

(defn sum-range [n1 n2]
  (- 
   (/ (* n2 (+ n2 1)) 2) 
   (/ (* n1 (- n1 1)) 2)))

(defn finance [n]
  (loop [days-in-week n
         first-saving 0
         total 0]
    (if (< days-in-week 0)
      total
      (recur (- days-in-week 1)
             (+ first-saving 2)
             (+ total (sum-range first-saving (+ first-saving days-in-week)))))))

(defn digital-root [n]
  (let [digits (map #(Character/digit % 10) (str n))
        root (reduce + digits)]
    (if (< root 10)
      root
      (digital-root root))))

(defn nextin [names n]
  {:pre [(pos? n)]}
  (if names
    (nth names (mod n (* 2 (count names))))
    nil))

(defn who_is_next [names n]
  {:pre [(pos? n)]}
  (if names
    (loop [queue (transient (vec names))
           x 1]
      (let [first (get queue x)]
          (if (>= x n)
            first
            (recur (conj! (conj! queue first) first)
                   (inc x)))))
    nil))

(defn who-next [names n]
  {:pre [(pos? n) (<= n 1000000000)]}
  (letfn [(find-pow [n size]
            (loop [pow 0
                   x 0]
              (let [pow2 (int (Math/pow 2 pow))
                    row-size (* size pow2)
                    total (+ x row-size)]
                (if (> total n) 
                  [pow2 x]
                  (recur (inc pow) total)))))]
    (let [size (count names)
          n0 (dec n)
          [cycle x] (find-pow n0 size)]
      (get names (int (/ (mod (- n0 x) (* cycle size)) cycle))))))

(defn find-even-index [arr]
  {:pre [(< (count arr) 1000) (not-empty arr) (vector? arr)]}
  (loop [i 0]    
    (cond
     (>= i (count arr)) -1
     (= (reduce + (subvec arr 0 i)) (reduce + (subvec arr (inc i)))) i
     :else (recur (inc i)))))

(defn prime? [n]
      (.isProbablePrime (BigInteger/valueOf n) 5))

(defn primes [start end]
  "Returns a lazy sequence of primes between start and end inclusive"
  (filter prime? (range start (inc end))))

(defn gap [g m n]
  (loop [ps (primes m n)]
    (let [pair (take 2 ps)]
      (cond
       (< (count pair) 2) nil
       (= g (- (second pair) (first pair)))  pair
       :else (recur (next ps))))))

(defn gcd [x y]
  (loop [a x
         b y]
    (if (zero? b)
      a
      (recur b (mod a b)))))

(defn lcm [x y]
  (/ (* x y) (gcd x y)))

(defn convert-fracts [lst]
  (map (fn [pair]
         (let [x (first pair)
               y (second pair)
               l (lcm x y)]  
               [(* l x) (* l y)]))
       lst))

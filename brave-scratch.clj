(ns brave-scratch)

; chaptur 3
; 2
(defn add-100 [n] (+ 100 n))
; 3
(defn dec-maker [n] (fn [x] (- x n)))
; 4
(defn mapset [func coll] (set (map func coll)))
; 5
(def alien-body-parts [{:name "head" :size 3}
                       {:name "left-eye" :size 1}
                       {:name "left-ear" :size 1}
                       {:name "mouth" :size 1}
                       {:name "nose" :size 1}
                       {:name "neck" :size 2}
                       {:name "left-shoulder" :size 3}
                       {:name "left-upper-arm" :size 3}
                       {:name "chest" :size 10}
                       {:name "back" :size 10}
                       {:name "left-forearm" :size 3}
                       {:name "abdomen" :size 6}
                       {:name "left-kidney" :size 1}
                       {:name "left-hand" :size 2}
                       {:name "left-knee" :size 2}
                       {:name "left-thigh" :size 4}
                       {:name "left-lower-leg" :size 3}
                       {:name "left-achilles" :size 1}
                       {:name "left-foot" :size 2}])

(defn matching-parts
  [part]
  [(matching-part part "mid-left")
   (matching-part part "mid")
   (matching-part part "mid-right")
   (matching-part part "right")]
  )

(defn matching-part
  [part orientation]
  {:name (clojure.string/replace (:name part) #"^left-" (str orientation "-"))
   :size (:size part)})

(defn alienize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part]
            (into final-body-parts (set (concat [part] (matching-parts part)))))
          []
          asym-body-parts))

; 6

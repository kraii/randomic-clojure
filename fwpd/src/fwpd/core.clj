(ns fwpd.core
  (:require [clojure.string :as str]))

(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  [string]
  (map #(str/split % #",") (str/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                   {}
                   (map vector vamp-keys unmapped-row)))
                 rows))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(def suspects (mapify (parse (slurp filename))))

;#2.
(defn append
  [suspects name glitter-index]
  (conj suspects {:name name :glitter-index glitter-index}))

;#3
(defn validate
  ([suspect] (validate suspect {:name string? :glitter-index integer?}))
  ([suspect validations]
   (every? #((% validations) (% suspect)) [:name :glitter-index])))

;#4
(defn to-csv
  [suspects]
  (letfn [(conv [suspect] [(:name suspect) (:glitter-index suspect)])]
    (str/join "\n" (map (partial str/join ",") (map conv suspects)))))


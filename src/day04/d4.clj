(ns day04.d4
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def ^:private srcpath "src/day04/")

(defrecord ^:private Assignment [left right])

(defn format-range [set]
  (let [plot (map #(if (contains? set %) % ".") (range 0 10))]
    (format "%s  %s-%s\n" (string/join plot) (apply min set) (apply max set))))

(defn print-assignment [assignment]
  (str (format-range (:left assignment))
       (format-range (:right assignment))))

(defn format-assignments [assignments] (map print-assignment assignments))

(defn parse-range-set [range-str]
  (let [range-str-pair (string/split range-str #"-")
        range-pair (map #(Integer/parseInt %) range-str-pair)
        range (range (first range-pair) (inc (second range-pair)))]
    (into #{} range)))

(defn parse-assignment [line]
  (let [range-str-pair (string/split line #",")
        range-set-pair (map parse-range-set range-str-pair)]
    (Assignment. (first range-set-pair) (second range-set-pair))))

(defn load-assignments [filename]
  (->> filename
       (str srcpath)
       (slurp)
       (string/split-lines)
       (map parse-assignment)))

(defn is-fully-contained [assignment]
  (let [l (:left assignment)
        r (:right assignment)]
    (or (set/subset? l r) (set/subset? r l))))

(defn num-overlaps [assignments]
  (count (filter is-fully-contained assignments)))

(defn part1 [filename]
  (as-> filename it
    (load-assignments it)
    (num-overlaps it)))

(defn part2 [filename] filename)

(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

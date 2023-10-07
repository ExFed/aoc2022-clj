(ns day04.d4
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def ^:private srcpath "src/day04/")

(defrecord ^:private Assignment [left right])

(defn parse-range-set [range-str]
  (let [range-str-pair (string/split range-str #"-")
        range-pair (map #(Integer/parseInt %) range-str-pair)
        range (apply range range-pair)]
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

(defn is-overlap [assignment]
  (seq (set/intersection (:left assignment) (:right assignment))))

(defn num-overlaps [assignments]
  (count (filter is-overlap assignments)))

(defn part1 [filename]
  (as-> filename it
    (load-assignments it)
    (num-overlaps it)))

(defn part2 [filename] filename)

(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

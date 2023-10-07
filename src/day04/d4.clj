(ns day04.d4
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(def ^:private srcpath "src/day04/")

(defrecord ^:private Assignment [left right])

(defn format-range [min max set]
  (let [plot (map #(if (contains? set %) "=" ".") (range min max))]
    (format "%s  %s-%s\n" (string/join plot) (apply clojure.core/min set) (apply clojure.core/max set))))

(defn format-assignment [min max assignment]
  (str (format-range min max (:left assignment))
       (format-range min max (:right assignment))))

(defn format-assignments [assignments]
  (let [min (reduce min (into [] cat (map :left assignments)))
        max (reduce max (into [] cat (map :right assignments)))]
    (map (partial format-assignment min max) assignments)))

(defn print-assignments [assignments]
  (doseq [s (format-assignments assignments)]
    (println s))
  assignments)

(defn parse-range-set [range-str]
  (let [range-str-pair (string/split range-str #"-")
        range-pair (map #(Integer/parseInt %) range-str-pair)
        range (range (first range-pair) (inc (second range-pair)))]
    (into #{} range)))

(defn parse-assignment [line]
  (let [range-str-pair (string/split line #",")
        range-set-pair (map parse-range-set range-str-pair)]
    (Assignment. (first range-set-pair) (second range-set-pair))))

(defn- load-assignments-0 [filename]
  (->> filename
       (str srcpath)
       (slurp)
       (string/split-lines)
       (map parse-assignment)))

(def load-assignments (memoize load-assignments-0))

(defn is-either-fully-contained [assignment]
  (let [l (:left assignment)
        r (:right assignment)]
    (or (set/subset? l r) (set/subset? r l))))

(defn is-overlapping [assignment]
  (let [l (:left assignment)
        r (:right assignment)]
    (seq (set/intersection l r))))

(defn num-either-fully-contained [assignments]
  (count (filter is-either-fully-contained assignments)))

(defn num-overlapping [assignments]
  (count (filter is-overlapping assignments)))

(defn part1 [filename]
  (as-> filename it
    (load-assignments it)
    (num-either-fully-contained it)))

(defn part2 [filename]
  (as-> filename it
    (load-assignments it)
    (num-overlapping it)))

(defn -main [& args]
  (if (first args) (print-assignments (load-assignments "input")) '())
  {'part1 (part1 "input")
   'part2 (part2 "input")})

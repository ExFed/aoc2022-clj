(ns day03.d3
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn load-rucks [filename]
  (->> filename
       (str "src/day03/")
       (slurp)
       (string/split-lines)))

(defn split-ruck-compartments [ruck]
  (let [m (/ (count ruck) 2)]
    [(subs ruck 0 m) (subs ruck m)]))

(defn find-errors [ruck]
  (let [compartments (split-ruck-compartments ruck)
        csa (set (char-array (first compartments)))
        csb (set (char-array (second compartments)))]
    (first (set/intersection csa csb))))

(def priority-of (let [chs-lower (range (int \a) (inc (int \z)))
                       chs-upper (range (int \A) (inc (int \Z)))
                       chs (concat chs-lower chs-upper)]
                   (zipmap (map char chs) (map inc (range)))))

(defn part1 [filename]
  (as-> filename it
    (load-rucks it)
    (map #(-> %
              (find-errors)
              (priority-of)) it)
    (reduce + it)))

(defn find-common [triple]
  (let [a (nth triple 0)
        b (nth triple 1)
        c (nth triple 2)]
    (first (set/intersection (set a) (set b) (set c)))))

(defn part2 [filename]
  (as-> filename it
    (load-rucks it)
    (partition 3 it)
    (map #(-> %
              (find-common)
              (priority-of)) it)
    (reduce + it)))

(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

(ns day03.d3
  (:require [clojure.string :as string]
            [clojure.set :as set]))

(defn load-rucks [filename]
  (->> filename
       (str "src/day03/")
       (slurp)
       (string/split-lines)
       (map (fn [s]
              (let [m (/ (count s) 2)
                    r [(subs s 0 m) (subs s m)]]
                (map char-array r))))))

(defn find-errors [ruck]
  (set/intersection (set (first ruck)) (set (last ruck))))


(def priority-of (let [chs-lower (range (int \a) (inc (int \z)))
                       chs-upper (range (int \A) (inc (int \Z)))
                       chs (concat chs-lower chs-upper)]
                   (zipmap (map char chs) (map inc (range)))))

(defn part1 [filename]
  (as-> filename it
    (load-rucks it)
    (map #(-> %
              (find-errors)
              (first)
              (priority-of)) it)
    (reduce + it)))

(defn part2 [filename])

(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

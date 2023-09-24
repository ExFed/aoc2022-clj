(ns day01.d1
  (:require [clojure.java.io :as io]
            [clojure.string :as string]))

;; https://adventofcode.com/2022/day/1

(defn- indexof-max [v]
  (reduce
   (fn [i, j]
     (if (>= (nth v i) (nth v j)) i j))
   0
   (range (.size v))))

(defn- tokenize-elves [strs]
  (let [parts (partition-by string/blank? strs)]
    (filterv #(not (string/blank? (first %))) parts)))

(defn- parse-elf [elf-strs] (mapv #(Integer/parseInt %) elf-strs))

(defn- parse-elves [strs] (mapv #(parse-elf %) (tokenize-elves strs)))

(defn- largest-sum-of-calories [lines]
  (let [elves (parse-elves lines)
        elves-calories (mapv #(reduce + %) elves)
        max-elf (indexof-max elves-calories)]
    (nth elves-calories max-elf)))

(defn- part1 [filename]
  (with-open [rdr (io/reader (str "src/day01/" filename))]
    (let [lines (line-seq rdr)]
      (largest-sum-of-calories lines))))

(defn -main [& args]
  {"Part 1" (part1 (or (and (seq args) (first args)) "input"))})

(ns day01.d1
  (:require [clojure.string :as string]))

;; https://adventofcode.com/2022/day/1

(defn parse-elf [lines] (mapv #(Integer/parseInt %) lines))

(defn load-elves [filepath]
  (mapv #(parse-elf (string/split-lines %)) (string/split (slurp filepath) #"\n\n")))

(defn load-elves-calories [filename]
  (mapv #(reduce + %) (load-elves (str "src/day01/" filename))))

(def load-elves-calories-memo (memoize load-elves-calories))

(defn part1 [filename]
  (let [elves-calories (load-elves-calories-memo filename)]
    (first (sort > elves-calories))))

(defn part2 [filename]
  (let [elves-calories (load-elves-calories-memo filename)]
    (transduce (take 3) + (sort > elves-calories))))

(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

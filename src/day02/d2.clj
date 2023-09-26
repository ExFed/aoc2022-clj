(ns day02.d2
  (:require [clojure.set :as set]
            [clojure.string :as string]))

(def rps {::rock ::scissors
          ::paper ::rock
          ::scissors ::paper})

(def rps-inv (set/map-invert rps))

(def rps-codes-p1 {"A" ::rock
                   "B" ::paper
                   "C" ::scissors
                   "X" ::rock
                   "Y" ::paper
                   "Z" ::scissors})

(def rps-codes-p2 {"A" ::rock
                   "B" ::paper
                   "C" ::scissors
                   "X" ::lose
                   "Y" ::draw
                   "Z" ::win})

(defn get-play [away wld]
  (case wld
    ::win (rps-inv away)
    ::lose (rps away)
    ::draw away))

(def rps-scores {::rock 1 ::paper 2 ::scissors 3})

(def win-scores {::win 6 ::draw 3 ::lose 0})

(defn get-result [away home]
  (cond
    (= (rps home) away) ::win
    (= home (rps away)) ::lose
    :else ::draw))

(defn score [away home]
  (+
   (win-scores (get-result away home))
   (rps-scores home)))

(defn load-guide [filename]
  (->> filename
       (str "src/day02/")
       (slurp)
       (string/split-lines)
       (map #(.split % " "))))

(defn part1 [filename]
  (->> filename
       (load-guide)
       (map #(map rps-codes-p1 %))
       (map #(apply score %))
       (reduce +)))

(defn part2 [filename]
  (->> filename
       (load-guide)
       (map #(map rps-codes-p2 %))
       (map #(let [[away wld] %]
               (score away (get-play away wld))))
       (reduce +)))


(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

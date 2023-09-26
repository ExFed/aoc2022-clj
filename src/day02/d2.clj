(ns day02.d2
  (:require [clojure.string :as string]))

(def rps {::rock ::scissors
          ::paper ::rock
          ::scissors ::paper})

(def rps-codes {"A" ::rock
                "B" ::paper
                "C" ::scissors
                "X" ::rock
                "Y" ::paper
                "Z" ::scissors})

(def rps-scores {::rock 1 ::paper 2 ::scissors 3})

(def win-scores {::home 6 ::tie 3 ::away 0})

(defn get-winner [away home]
  (cond
    (= (rps home) away) ::home
    (= home (rps away)) ::away
    :else ::tie))

(defn score [away home]
  (+
   (win-scores (get-winner away home))
   (rps-scores home)))

(defn load-guide [filename]
  (->> filename
       (str "src/day02/")
       (slurp)
       (string/split-lines)
       (map #(as-> % it
               (.split it " ")
               (map rps-codes it)))))

(defn part1 [filename]
  (->> filename
       (load-guide)
       (map #(apply score %))
       (reduce +)))

(defn -main [& args]
  {'part1 (part1 "input")})

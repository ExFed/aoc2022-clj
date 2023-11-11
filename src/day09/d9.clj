(ns day09.d9
  (:require [clojure.string :as string]
            [util :refer [+v, -v]]))

(defn parse-motions [filename]
  (let [data (util/load-data 9 filename)
        lines (string/split-lines data)
        pairs (map #(string/split % #" ") lines)]
    (map #(vec [(first %) (Integer/parseInt (second %))]) pairs)))

(def init-coord [0 0])
(def init-rope {::head init-coord ::tail init-coord})

(defn print-rope [h w {:keys [::head ::tail]}]
  (loop [x 0 y (dec h)]
    (cond
      (= head [x y]) (do (print "H") (recur (inc x) y))
      (= tail [x y]) (do (print "T") (recur (inc x) y))
      (= init-coord [x y]) (do (print "s") (recur (inc x) y))
      (>= x w) (do (println) (recur 0 (dec y)))
      (>= y 0) (do (print ".") (recur (inc x) y)))))

(defn print-steps [h w motion-steps]
  (doseq [[motion steps] motion-steps]
    (println "==" (first motion) (second motion) "==")
    (println)
    (doseq [step steps]
      (print-rope h w step)
      (println))))

(defn parse-direction [dir]
  (condp = dir
    "U" [0 +1]
    "D" [0 -1]
    "R" [+1 0]
    "L" [-1 0]
    (throw (IllegalArgumentException. (str "unexpected direction: " dir)))))

(defn move-coord [coord dir] (+v coord (parse-direction dir)))

(defn move-tail [prev-rope {:keys [::head ::tail] :as rope}]
  (let [[x-dist y-dist] (-v head tail)]
    (cond
      (or (<= 2 (Math/abs x-dist))
          (<= 2 (Math/abs y-dist))) (assoc rope ::tail (::head prev-rope))
      :else rope)))

(defn move-rope [rope [dir num]]
  (loop [{:keys [::head ::tail] :as prev-rope} rope
         ropes []
         num num]
    (if (> num 0)
      (let [next-rope {::head (move-coord head dir) ::tail tail}
            next-rope (move-tail prev-rope next-rope)]
        (recur next-rope (conj ropes next-rope) (dec num)))
      ropes)))

(defn interpret-motions [filename]
  (loop [motions (parse-motions filename)
         rope init-rope
         motion-steps [['(Initial State) [rope]]]]
    (let [motion (first motions)
          ropes (move-rope rope motion)
          motions (rest motions)
          motion-steps (conj motion-steps [motion ropes])]
      (if (empty? motions)
        motion-steps
        (recur motions (peek ropes) motion-steps)))))

(defn count-unique-tail-positions [motion-steps]
  (let [tails (set (mapcat #(map ::tail (second %)) motion-steps))]
    (count tails)))

(defn part1 [filename]
  (let [motion-steps (interpret-motions filename)]
    (count-unique-tail-positions motion-steps)))


(defn part2 [filename])

(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

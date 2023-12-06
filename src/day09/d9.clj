(ns day09.d9
  (:require [clojure.string :as string]
            [util :refer [+v -v maxv minv clamp]]))

(defn parse-motions [filename]
  (let [data (util/load-data 9 filename)
        lines (string/split-lines data)
        pairs (map #(string/split % #" ") lines)]
    (map #(vec [(first %) (Integer/parseInt (second %))]) pairs)))

(def init-knot [0 0])
(defn init-rope [len] (vec (repeat len init-knot)))

(defn print-rope [[left bottom right top] [head tail]]
  (loop [x left y top]
    (cond
      (= head [x y]) (do (print "H") (recur (inc x) y))
      (= tail [x y]) (do (print "T") (recur (inc x) y))
      (= init-knot [x y]) (do (print "s") (recur (inc x) y))
      (> x right) (do (println) (recur 0 (dec y)))
      (>= y bottom) (do (print ".") (recur (inc x) y)))))

(defn get-bounds [motion-steps]
  (let [heads (mapcat #(map first (second %)) motion-steps)]
    (reduce #(into (minv (subvec %1 0 2) %2)
                   (maxv (subvec %1 2 4) %2))
            [0 0 0 0]
            heads)))

(defn print-steps [motion-steps]
  (let [bounds (get-bounds motion-steps)]
    (doseq [[motion steps] motion-steps]
      (println "==" (first motion) (second motion) "==")
      (println)
      (doseq [step steps]
        (print-rope bounds step)
        (println)))))

(defn parse-direction [dir]
  (condp = dir
    "U" [0 +1]
    "D" [0 -1]
    "R" [+1 0]
    "L" [-1 0]
    (throw (IllegalArgumentException. (str "unexpected direction: " dir)))))

(defn move-coord [coord dir] (+v coord (parse-direction dir)))

(defn move-knots
  ([dir rope] (move-knots dir (first rope) (next rope)))
  ([dir head tails]
   (loop [heads [(move-coord head dir)]
          mid (first tails)
          tails (next tails)]
     (let [[x-dist y-dist] (-v (peek heads) mid)
           [dx dy] (if (or (> (Math/abs x-dist) 1) (> (Math/abs y-dist) 1))
                     [(util/clamp -1 1 x-dist) (util/clamp -1 1 y-dist)]
                     [0 0])
           next-mid (+v mid [dx dy])
           next-heads (conj heads next-mid)]
       (if (empty? tails)
         next-heads
         (recur next-heads (first tails) (next tails)))))))

(defn move-rope [rope [dir num]]
  (loop [ropes [rope]
         num num]
    (if (> num 0)
      (let [rope (move-knots dir (peek ropes))]
        (recur (conj ropes rope) (dec num)))
      ropes)))

(defn interpret-motions [filename rope-len]
  (loop [motions (parse-motions filename)
         rope (init-rope rope-len)
         motion-steps [['(Initial State) [rope]]]]
    (let [motion (first motions)
          ropes (move-rope rope motion)
          motions (rest motions)
          motion-steps (conj motion-steps [motion ropes])]
      (if (empty? motions)
        motion-steps
        (recur motions (peek ropes) motion-steps)))))

(defn count-unique-tail-positions [motion-steps]
  (let [tails (mapcat #(map last (second %)) motion-steps)
        unique-tails (set tails)]
    (count unique-tails)))

(defn part1 [filename]
  (let [motion-steps (interpret-motions filename 2)]
    (count-unique-tail-positions motion-steps)))

(defn part2 [filename]
  (let [motion-steps (interpret-motions filename 10)]
    (count-unique-tail-positions motion-steps)))

(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

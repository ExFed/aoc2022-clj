(ns day08.d8
  (:require [clojure.string :as string]))

(def srcpath "src/day08/")

(defn load-data [filename] (slurp (str srcpath filename)))

(defn map-grid [f grid]
  (map (fn [row-cells] (map #(f %) row-cells)) grid))

(defn parse-grid [filename]
  (let [text (load-data filename)
        grid (map #(string/split % #"") (string/split-lines text))]
    (map-grid #(Integer/parseUnsignedInt %) grid)))

(defn format-grid
  [grid] (string/join "\n" (map (partial string/join "\t") grid)))

(defn grid-height [grid [row-index col-index]]
  (let [row-cells (nth grid row-index [])]
    (nth row-cells col-index nil)))

(defn adv [[row col] dir]
  (condp = dir
    :north [(dec row) col]
    :south [(inc row) col]
    :east [row (inc col)]
    :west [row (dec col)]
    (throw (IllegalArgumentException. (str "unsupported direction: " dir)))))

(defn grid-coords-next [grid [row col]]
  (cond
    (grid-height grid [row (inc col)]) [row (inc col)]
    (grid-height grid [(inc row) 0]) [(inc row) 0]
    :else nil))

(defn grid-coords [grid]
  (take-while identity (iterate (partial grid-coords-next grid) [0 0])))

(defn visible-from-edge?
  ([grid coord]
   (or
    (visible-from-edge? grid coord :north)
    (visible-from-edge? grid coord :east)
    (visible-from-edge? grid coord :west)
    (visible-from-edge? grid coord :south)))
  ([grid coord dir]
   (let [h0 (grid-height grid coord)]
     (loop [coord (adv coord dir)]
       (let [h1 (grid-height grid coord)]
         (cond
           (nil? h1) true
           (<= h0 h1) false
           :else (recur (adv coord dir))))))))

(defn part1 [filename]
  (let [grid (parse-grid filename)
        cells-visible (map #(visible-from-edge? grid %) (grid-coords grid))]
    (count (filter identity cells-visible))))

(defn trees-visible [grid coord dir]
  (let [h0 (grid-height grid coord)]
    (loop [coord (adv coord dir) n 0]
      (let [h1 (grid-height grid coord)]
        (cond
          (nil? h1) n ;; see the edge (no tree!)
          (>= h1 h0) (inc n) ;; see a taller tree
          :else (recur (adv coord dir) (inc n)) ;; see a shorter tree (keep looking)
          )))))

(defn scenic-score [grid coord]
  (*
   (trees-visible grid coord :north)
   (trees-visible grid coord :east)
   (trees-visible grid coord :west)
   (trees-visible grid coord :south)))

(defn part2 [filename]
  (let [grid (parse-grid filename)
        coords (grid-coords grid)
        scenic-scores (map (partial scenic-score grid) coords)]
    (apply max scenic-scores)))

(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

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

(defn grid-cell [grid [row-index col-index]]
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
    (grid-cell grid [row (inc col)]) [row (inc col)]
    (grid-cell grid [(inc row) 0]) [(inc row) 0]
    :else nil))

(defn grid-coords [grid]
  (take-while identity (iterate (partial grid-coords-next grid) [0 0])))

(defn hidden-from-edge?
  ([grid coord]
   (and
    (hidden-from-edge? grid coord :north)
    (hidden-from-edge? grid coord :east)
    (hidden-from-edge? grid coord :west)
    (hidden-from-edge? grid coord :south)))
  ([grid coord dir]
   (let [cell0 (grid-cell grid coord)
         coord1 (adv coord dir)]
     (loop [coord1 coord1]
       (let [cell1 (grid-cell grid coord1)]
         (cond
           (nil? cell1) false
           (<= cell0 cell1) true
           :else (recur (adv coord1 dir))))))))

(defn visible-from-edge? [grid coord] (not (hidden-from-edge? grid coord)))

(defn part1 [filename]
  (let [grid (parse-grid filename)
        cells-visible (map (partial visible-from-edge? grid) (grid-coords grid))]
    (count (filter identity cells-visible)))
)

(defn part2 [filename])


(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

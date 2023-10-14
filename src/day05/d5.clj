(ns day05.d5
  (:require [clojure.string :as string]))

(def srcpath "src/day05/")

(defn take-crates-from-stack
  ([stack] (take-crates-from-stack stack 1))
  ([stack num-crates] [(take num-crates stack) (drop num-crates stack)]))
(defn put-crates-into-stack [stack crates] (concat crates stack))
(defn format-crate [crate] (if crate (str "[" crate "]") "   "))

(defn get-crate
  [supplies sn cn]
  (let [;; one-indexing
        sn (dec sn)
        stack (nth supplies sn)
        ;; 0-> base of stack
        kcats (reverse stack)]
    (nth kcats cn nil)))

(defn take-crates
  ([supplies from] (take-crates supplies from 1))
  ([supplies from num-crates]
   (let [from (dec from) ;; one-indexing
         stack (nth supplies from)
         [crates stack] (take-crates-from-stack stack num-crates)
         crate-and-supplies [crates (assoc supplies from stack)]]
     crate-and-supplies)))

(defn put-crates [crates-and-supplies to]
  (let [crates (first crates-and-supplies)
        supplies (second crates-and-supplies)
        to (dec to) ;; one-indexing
        stack (nth supplies to)
        stack (put-crates-into-stack stack crates)
        supplies (assoc supplies to stack)]
    supplies))

(defn move-crates
  ([supplies op] (move-crates supplies (:from op) (:to op) (:move op)))
  ([supplies from to] (move-crates supplies from to 1))
  ([supplies from to num-crates]
   (-> supplies
       (take-crates from num-crates)
       (put-crates to))))

(defn format-supplies [supplies]
  (let [width (count supplies)
        height (reduce #(max %1 (count %2)) 0 supplies)
        col-range (range 1 (inc width))
        row-range (range height)
        numrow-cells (map #(str " " % " ") col-range)
        cell-mapper (fn [v h] (format-crate (get-crate supplies h v)))
        row-mapper (fn [v] (map (partial cell-mapper v) col-range))
        rows (into [numrow-cells] (map row-mapper row-range))]
    (string/join "\n" (map #(string/join " " %) (reverse rows)))))

(defn index-of
  ([pred coll] (index-of pred coll 0))
  ([pred coll start-at]
   (let [coll (drop start-at coll)
         index (count (take-while #(not (pred %)) coll))]
     (if
      (< index (count coll))
       (+ start-at index)
       nil))))

(defn index-of-all [pred coll]
  (loop [start-at 0
         indexes []]
    (let [index (index-of pred coll start-at)]
      (if index
        (recur (inc index) (conj indexes index))
        indexes))))

(defn parse-supply-stack [lines index]
  (->> lines
       (map #(nth % index nil))
       (filter #(and % (not= \space %)))
       (map #(symbol (str %)))))

(defn parse-supplies [lines]
  (let [label-line (last lines)
        stack-lines (drop-last lines)
        column-indexes (index-of-all #(not= \space %) label-line)]
    (mapv #(parse-supply-stack stack-lines %) column-indexes)))

(defn parse-operation [line]
  (->> line
       (re-matches #"move ([0-9]+) from ([0-9]+) to ([0-9]+)")
       (rest)
       (map #(Integer/parseUnsignedInt %))
       (zipmap [:move :from :to])))

(defn parse-procedure [lines] (map parse-operation lines))

(defn parse-supplies-and-procedure [text]
  (let [lines (string/split-lines text)
        i (index-of empty? lines)
        slines (take i lines)
        plines (drop (inc i) lines)]
    [(parse-supplies slines) (parse-procedure plines)]))

(defn load-supplies-and-procedure [filename]
  (->> filename
       (str srcpath)
       (slurp)
       (parse-supplies-and-procedure)))

(defn execute-operation
  ([supplies op] (execute-operation supplies op false))
  ([supplies op batch-move]
   (if-not (< 0 (:move op))
     supplies
     (let [moves-remaining (if batch-move (:move op) 1)
           supplies (move-crates supplies (:from op) (:to op) moves-remaining)
           next-op (assoc op :move (- (:move op) moves-remaining))]
       (recur supplies next-op batch-move)))))

(defn execute-procedure
  ([supplies procedure] (execute-procedure supplies procedure false))
  ([supplies procedure batch-move]
   (loop [supplies supplies
          next-op (first procedure)
          procedure (rest procedure)]
     (if-not next-op
       supplies
       (recur (execute-operation supplies next-op batch-move) (first procedure) (rest procedure))))))

(defn get-top-crates [supplies]
  (string/join (map first supplies)))

(defn part1 [filename]
  (let [[supplies procedure] (load-supplies-and-procedure filename)
        supplies (execute-procedure supplies procedure)]
    (get-top-crates supplies)))

(defn part2 [filename]
  (let [[supplies procedure] (load-supplies-and-procedure filename)
        supplies (execute-procedure supplies procedure true)]
    (get-top-crates supplies)))


(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

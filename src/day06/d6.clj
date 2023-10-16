(ns day06.d6)

(defn append-circular [buffer element]
  (conj (subvec buffer 1) element))

(defn is-start-marker [buffer]
  (and (every? #(not (nil? %)) buffer)
       (apply distinct? buffer)))

(defn make-buffer
  ([length] (make-buffer length nil))
  ([length value] (reduce (fn [a _] (conj a value)) [] (range length))))

(defn indexof-marker
  ([marker-length input-str] (indexof-marker marker-length input-str 0))
  ([marker-length input-str start-index]
   (loop [input input-str index start-index buffer (make-buffer marker-length)]
     (cond
       (empty? input) nil
       (is-start-marker buffer) index
       :else (recur (subs input 1)
                    (inc index)
                    (append-circular buffer (first input)))))))

(def srcpath "src/day06/")
(defn load-signal-stream [filename] (slurp (str srcpath filename)))

(defn part1 [filename]
  (->> filename
       (load-signal-stream)
       (indexof-marker 4)))

(defn part2 [filename]
    (->> filename
        (load-signal-stream)
        (indexof-marker 14)))

(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

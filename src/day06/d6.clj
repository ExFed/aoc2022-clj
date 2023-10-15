(ns day06.d6)

(def buffer-size 4)
(defn append-circular
  ([buffer element]
   (if (< (count buffer) buffer-size)
     (conj buffer element)
     (recur (subvec buffer 1) element))))

(defn is-start-marker [buffer]
  (and (= buffer-size (count buffer))
       (apply distinct? buffer)))

(defn indexof-start-packet
  ([input-str] (indexof-start-packet input-str 0))
  ([input-str start-index]
   (loop [input input-str index start-index buffer []]
     (if (is-start-marker buffer)
       index
       (recur (subs input 1) (inc index) (append-circular buffer (first input)))))))

(def srcpath "src/day06/")
(defn load-signal-stream [filename] (slurp (str srcpath filename)))

(defn part1 [filename]
  (indexof-start-packet (load-signal-stream filename)))

(defn part2 [filename] '())

(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

(ns day07.d7
  (:require [clojure.string :as string]))

(defrecord Context [cwd pathsizes])
(def init-context (Context. [] (sorted-map-by compare)))

(defn put-size [^Context context [size filename]]
  (let [path (conj (:cwd context) filename)]
    (update context :pathsizes #(into % {path size}))))

(defn output-line? [s] (not (string/starts-with? s "$")))

(defn parse-file-size [line]
  (let [[size-str filename] (string/split line #" " 2)
        size (Integer/parseUnsignedInt size-str)]
    [size filename]))

(defn cd [command-line]
  (let [[_$ _cmd name] (string/split command-line #" " 3)]
    (fn [^Context context buffer-lines]
      [(update context
               :cwd
               #(cond
                  (= name "/") []
                  (= name "..") (pop %)
                  :else (conj % name)))
       buffer-lines])))

(defn ls [^Context context buffer-lines]
  (let [ls-lines (take-while output-line? buffer-lines)
        size-lines (filter #(not (string/starts-with? % "dir")) ls-lines)
        sizes (map parse-file-size size-lines)
        context (reduce #(put-size %1 %2) context sizes)
        buffer-lines (drop-while output-line? buffer-lines)]
    [context buffer-lines]))

(defn dispatch-command [command-line]
  (cond
    (string/starts-with? command-line "$ ls") ls
    (string/starts-with? command-line "$ cd") (cd command-line)
    :else (throw (IllegalArgumentException. (str "cannot dispatch command: " command-line)))))

(defn interpret [^Context context buffer-lines]
  (let [next-line (first buffer-lines)
        buffer-lines (rest buffer-lines)
        command (dispatch-command next-line)
        [context buffer-lines] (command context buffer-lines)]
    (if (empty? buffer-lines)
      (:pathsizes context)
      (recur context buffer-lines))))

(defn assoc-sizetree [pathsizes]
  (reduce #(assoc-in %1 (first %2) (second %2)) (sorted-map) pathsizes))

(defn sum-dir-sizes [pathsizes]
  (let [sizetree (assoc-sizetree pathsizes)
        dirpaths (distinct (map pop (filter seq (keys pathsizes))))
        dirtrees (map #(get-in sizetree %) dirpaths)]
    (map #(reduce + (filter integer? (tree-seq map? vals %))) dirtrees)))

(def srcpath "src/day07/")
(defn load-data [filename] (slurp (str srcpath filename)))

(defn part1 [filename]
  (->> filename
       (load-data)
       (string/split-lines)
       (interpret init-context)
       (sum-dir-sizes)
       (filter (partial >= 100000))
       (reduce +)))

(defn part2 [filename] '(TODO))

(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

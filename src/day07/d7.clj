(ns day07.d7
  (:require [day07.shell :as shell]
            util))

(def srcpath "src/day07/")

(defn load-data [filename] (slurp (str srcpath filename)))

(defn load-filesystem [filename] (shell/parse (load-data filename)))

(defn path-prefix? [pre path]
  (cond
    (empty? pre) true
    (empty? path) false
    (not= (first pre) (first path)) false
    :else (recur (rest pre) (rest path))))

(defn dir? [entry] (instance? day07.shell.DirEntry entry))

(defn file? [entry] (instance? day07.shell.FileEntry entry))

(defn du [fs path]
  (let [files (filter file? fs)
        files-in-dir (filter #(path-prefix? path (:path %)) files)]
    (reduce + (map :size files-in-dir))))

(defn part1 [filename]
  (let [fs (load-filesystem filename)
        dir-paths (map :path (filter dir? fs))
        dir-dus (map (partial du fs) dir-paths)
        small-dir-dus (filter (partial >= 100000) dir-dus)]
    (reduce + small-dir-dus)))

(def total-disk 70000000)
(def min-unused 30000000)
(def max-usable (- total-disk min-unused))

(defn part2 [filename]
  (let [fs (load-filesystem filename)
        root-du (du fs [])
        min-to-free (- root-du max-usable)
        dirs (filter dir? fs)
        dir-dus (map #(du fs (:path %)) dirs)
        candidate-dus (filter (partial < min-to-free) dir-dus)]
    (first (sort candidate-dus))))


(defn -main [& args]
  {'part1 (part1 "input")
   'part2 (part2 "input")})

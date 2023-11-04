(ns day07.part1
  (:require [day07.shell :as shell]))

(def srcpath "src/day07/")
(defn load-data [filename] (slurp (str srcpath filename)))

(defn path-prefix? [pre path]
  (cond
    (empty? pre) true
    (empty? path) false
    (not= (first pre) (first path)) false
    :else (recur (rest pre) (rest path))))

(defn du [fs path]
  (if (empty? fs)
    0
    (let [fs (filter (partial instance? day07.shell.FileEntry) fs)
          fs (filter #(path-prefix? path (:path %)) fs)]
      (reduce + (map :size fs)))))

(defn part1 [filename]
  (let [data (load-data filename)
        fs (shell/parse data)
        dirs (partial instance? day07.shell.DirEntry)
        dir-paths (map :path (filter dirs fs))
        dir-dus (map (partial du fs) dir-paths)
        small-dir-dus (filter (partial >= 100000) dir-dus)]
    (reduce + small-dir-dus)))

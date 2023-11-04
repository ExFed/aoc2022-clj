(ns day07.shell
  (:require [clojure.string :as string]
            util))


(defrecord FileEntry [path size])

(defrecord DirEntry [path])

(defrecord ^:private FSContext [cwd entries])

(def root-dir (DirEntry. []))

(defn- fs-insert [^FSContext context entry]
  (update context :entries conj entry))

(defn- fs-insert-dir [^FSContext context name]
  (let [path (conj (:cwd context) name)]
    (fs-insert context (DirEntry. path))))

(defn- fs-insert-file [^FSContext context name size]
  (let [path (conj (:cwd context) name)]
    (fs-insert context (FileEntry. path size))))

(defn- fs-insert-ls-line [^FSContext context line]
  (let [[dir|size name] (string/split line #" " 2)]
    (if (= "dir" dir|size)
      (fs-insert-dir context name)
      (fs-insert-file context name (Integer/parseUnsignedInt dir|size)))))

(defn- output-line? [s] (not (string/starts-with? s "$")))

(defn- interpret-ls [^FSContext context shell-lines]
  (let [ls-lines (take-while output-line? shell-lines)
        shell-lines (drop-while output-line? shell-lines)
        context (reduce #(fs-insert-ls-line %1 %2) context ls-lines)]
    [context shell-lines]))

(defn- update-cwd [cd-arg cwd]
  (cond
    (= cd-arg "/") []
    (= cd-arg "..") (pop cwd)
    :else (conj cwd cd-arg)))

(defn- interpret-cd [^FSContext context arg]
  (update context :cwd (partial update-cwd arg)))

(defn- interpret-command-line [^FSContext context shell-lines lnum]
  (let [command-line (first shell-lines)
        buffer-lines (rest shell-lines)
        [$ cmd arg] (string/split command-line #" " 3)]
    (cond
      (not $) (throw (IllegalStateException. (str "Expected '$' on line " lnum)))
      (= "ls" cmd) (interpret-ls context buffer-lines)
      (= "cd" cmd) [(interpret-cd context arg) buffer-lines]
      :else (throw (IllegalStateException. (str "Unexpected command '" cmd "' on line " lnum))))))

(defn- parse-shell-lines [shell-lines]
  (loop [lnum 0
         [context buffer-lines] [(FSContext. [] [root-dir]) shell-lines]]
    (if (empty? buffer-lines)
      (sort-by :path util/lex-comp (:entries context))
      (recur (inc lnum) (interpret-command-line context buffer-lines lnum)))))

(defn parse [text]
  (parse-shell-lines (string/split-lines text)))

(ns util
  (:require [clojure.string :as string]))

(defmacro linas->
  "Similar to clojure.core/as->; returns a vector of intermediate values."
  [expr name & forms]
  (let [linn `lin#]
    `(let [~name ~expr
           ~linn [~name]
           ~@(interleave (repeat name) (butlast forms) (repeat linn) (repeat `(conj ~linn ~name)))]
       ~(if (empty? forms)
          [name]
          `(conj ~linn ~(last forms))))))

(defn linfn
  "Creates a function that tracks intermediate values."
  ([] '())
  ([f] (fn [x] (cons (f x) nil)))
  ([f & fs] (fn [x] (let [y (f x)]
                      (lazy-seq (cons y ((apply linfn fs) y)))))))

;; (defn linfn
;;   "Creates a function that tracks intermediate values."
;;   (fn [x] (loop [ys [x] fs fs]
;;             (if (empty? fs)
;;               ys
;;               (let [f (first fs)
;;                     x (last ys)
;;                     y (f x)]
;;                 (recur (conj ys y) (rest fs)))))))

(defn lex-comp
  [xs ys]
  (cond
    (and (empty? xs) (empty? ys)) 0
    (empty? xs) -1
    (empty? ys) 1
    :else (let [x (first xs)
                y (first ys)
                c (compare x y)]
            (if (= 0 c)
              (recur (rest xs) (rest ys))
              c))))

(defn format-maptree
  ([tree] (format-maptree tree 2))
  ([tree indent] (str "/" (format-maptree tree indent 0)))
  ([tree indent level]
   (if (map? tree)
     (let [spaces (string/join (repeat (* indent level) " "))
           sublevel (inc level)
           subtrees (map #(str "- " (first %) (format-maptree (second %) indent sublevel)) tree)]
       (string/join (map (partial str "\n" spaces) subtrees)))
     (str " " tree))))


;; ((linfn #(* 9 %) #(- % 12)) 6)
;; (take 3 ((linfn (repeat (partial * 2))) 1))
(take 2 ((linfn (partial * 9) #(- % 12) #(str "hello " % " world") #(do (println "capitalizing...") (clojure.string/capitalize %))) 6))

(ns main
  (:require [day01.d1 :as d1]
            [day02.d2 :as d2]
            [day03.d3 :as d3]
            [day04.d4 :as d4]
            [day05.d5 :as d5]
            [day06.d6 :as d6]
            [day07.d7 :as d7]))

(def solutions [d1/-main,
                d2/-main,
                d3/-main,
                d4/-main,
                d5/-main,
                d6/-main
                d7/-main])

(defn exec-day [day-num args]
  (when (> day-num (count solutions))
    (throw (IllegalArgumentException.
            (str "Day " day-num " not found ... did you add it to the solutions?"))))
  (let [solution (nth solutions (dec day-num))]
    (println (str "Day " day-num " answer:"))
    (println (apply solution args))))

(defn -main [& args]
  (if (seq args)
    ;; run the day and its args
    (let [day-num (first args)
          day-args (rest args)]
      (exec-day (Integer/parseInt day-num) day-args))

    ;; no args, run all solutions
    (doseq [day (map inc (range (.size solutions)))] (exec-day day []))))

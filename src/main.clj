(ns main
  (:require [day01.d1 :as d1]
            [day02.d2 :as d2]))

(def solutions [d1/-main, d2/-main])

(defn exec-day [day-num args]
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

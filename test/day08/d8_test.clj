(ns day08.d8-test
  (:require
   [clojure.test :refer [deftest is]]
   [day08.d8 :as subject]))

(deftest part1-test
  (is (= 21
         (subject/part1 "test")))
  (is (= 1794
         (subject/part1 "input"))))

(deftest part2-test
  (is (= 8
         (subject/part2 "test")))
;;   (is (= 0
;;          (subject/part2 "input")))
  )

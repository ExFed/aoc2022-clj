(ns day09.d9-test
  (:require
   [clojure.test :refer [deftest is]]
   [day09.d9 :as subject]))

(deftest part1-test
  (is (= 13
         (subject/part1 "test")))
  (is (= 5735
         (subject/part1 "input"))))

(deftest move-tails-test
  (is (= [[1 0] [0 0]]
         (subject/move-knots "R" [[0 0] [0 0]])))
  (is (= [[2 0] [1 0]]
         (subject/move-knots "R" [[1 0] [0 0]])))
  (is (= [[1 1] [0 0]]
         (subject/move-knots "U" [[1 0] [0 0]])))
  (is (= [[1 2] [1 1]]
         (subject/move-knots "U" [[1 1] [0 0]])))
  (is (= [[2 1] [1 1]]
         (subject/move-knots "R" [[1 1] [0 0]]))))

(deftest part2-test
  (is (= 1
         (subject/part2 "test")))
  (is (= 2478
         (subject/part2 "input"))))

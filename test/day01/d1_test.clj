(ns day01.d1-test
  (:require
   [clojure.test :refer [deftest is]]
   [day01.d1 :as subject]))

(deftest part1-test
  (is (= 24000 (subject/part1 "test")))
  (is (= 71506 (subject/part1 "input")))
  (is (= 45000 (subject/part2 "test")))
  (is (= 209603 (subject/part2 "input"))))

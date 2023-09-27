(ns day03.d3-test
  (:require
   [clojure.test :refer [deftest is]]
   [day03.d3 :as subject]))

(deftest day03.d3-test
  (is (= 1 (subject/priority-of \a)))
  (is (= 26 (subject/priority-of \z)))
  (is (= 27 (subject/priority-of \A)))
  (is (= 52 (subject/priority-of \Z)))
  (is (= 157 (subject/part1 "test")))
  (is (= 7691 (subject/part1 "input")))
  (is (= 70 (subject/part2 "test")))
  (is (= 2508 (subject/part2 "input"))))

(ns day02.d2-test
  (:require
   [clojure.test :refer [deftest is]]
   [day02.d2 :as subject]))

(deftest day02.d2-test
  (is (= 15 (subject/part1 "test")))
  (is (= 10310 (subject/part1 "input")))
  (is (= 12 (subject/part2 "test")))
  (is (= 14859 (subject/part2 "input"))))

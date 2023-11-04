(ns day07.d7-test
  (:require
   [clojure.test :refer [deftest is]]
   [day07.d7 :as subject]))

(deftest part1-test
  (is (= 95437
         (subject/part1 "test")))
  (is (= 1642503
         (subject/part1 "input"))))

(deftest part2-test
  (is (= 24933642
         (subject/part2 "test"))))

(ns day04.d4-test
  (:require
   [clojure.test :refer [deftest is]]
   [day04.d4 :as subject]))

(deftest is-overlap-test
  (is (= 2 (subject/part1 "test"))))

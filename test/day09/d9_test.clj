(ns day09.d9-test
  (:require
   [clojure.test :refer [deftest is]]
   [day09.d9 :as subject]))

(deftest part1-test
  (is (= 13
         (subject/part1 "test")))
  (is (= 5735
         (subject/part1 "input"))))

(ns day08.d8-test
  (:require
   [clojure.test :refer [deftest is]]
   [day08.d8 :as subject]))

(deftest part1-test
  (is (= 21
         (subject/part1 "test")))
  (is (= 1794
         (subject/part1 "input"))))

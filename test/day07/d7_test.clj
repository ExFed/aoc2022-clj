(ns day07.d7-test
  (:require
   [clojure.test :refer [deftest is]]
   [day07.d7 :as subject]))

(deftest defn-test
  (is (= 95437
         (subject/part1 "test"))))

(ns day05.d5-test
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is]]
            [day05.d5 :as subject]))

(defn lines [& lines] (string/join "\n" lines))

(def test-supplies ['(N Z) '(D C M) '(P)])

(deftest format-supplies-test
  (is (= (lines "    [D]    "
                "[N] [C]    "
                "[Z] [M] [P]"
                " 1   2   3 ")
         (as-> test-supplies it
           (subject/format-supplies it)))))

(deftest move-crates-test
  (is (= (lines "[D]        "
                "[N] [C]    "
                "[Z] [M] [P]"
                " 1   2   3 ")
         (as-> test-supplies it
           (subject/move-crate it 2 1)
           (subject/format-supplies it)))))

(deftest index-of-all-test
  (is (= [1 4 7]
         (subject/index-of-all #(not= \space %) " a  b  c "))))

(deftest part1-test
  (is (= "CMZ"
         (subject/part1 "test"))))

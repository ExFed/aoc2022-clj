(ns util-test
  (:require
   [clojure.test :refer [deftest is]]
   [util :as subject]))

(deftest maxv-test
  (is (= [1]
         (subject/maxv [-1] [0] [1])))
  (is (= [1 2 3 4]
         (subject/maxv [1 0 0 -1] [0 2 -2 0] [0 -3 3 0] [-4 0 0 4]))))

(deftest minv-test
  (is (= [-1]
         (subject/minv [-1] [0] [1])))
  (is (= [-4 -3 -2 -1]
         (subject/minv [1 0 0 -1] [0 2 -2 0] [0 -3 3 0] [-4 0 0 4]))))

(ns day06.d6-test
  (:require
   [clojure.test :refer [deftest is are]]
   [day06.d6 :as subject]))

(deftest indexof-start-packet-test
  (are [expect input] (= expect (subject/indexof-start-packet input))
    7 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    5 "bvwbjplbgvbhsrlpgdmjqwftvncz"
    6 "nppdvjthqldpwncqszvftbrmjlhg"
    10 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    11 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))

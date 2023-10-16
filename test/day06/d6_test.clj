(ns day06.d6-test
  (:require
   [clojure.test :refer [deftest is are]]
   [day06.d6 :as subject]))

(deftest indexof-start-packet-test
  (are [expect input] (= expect (subject/indexof-marker 4 input))
    7  "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    5  "bvwbjplbgvbhsrlpgdmjqwftvncz"
    6  "nppdvjthqldpwncqszvftbrmjlhg"
    10 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    11 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))

(deftest indexof-start-message-test
  (are [expect input] (= expect (subject/indexof-marker 14 input))
    19 "mjqjpqmgbljsphdztnvjfqwrcgsmlb"
    23 "bvwbjplbgvbhsrlpgdmjqwftvncz"
    23 "nppdvjthqldpwncqszvftbrmjlhg"
    29 "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
    26 "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"))

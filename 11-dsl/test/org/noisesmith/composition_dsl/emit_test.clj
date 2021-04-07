(ns org.noisesmith.composition-dsl.emit-test
  (:require [clojure.test :refer [deftest is are]]
            [org.noisesmith.composition-dsl.emit :as sut]))

(deftest event-emit-test
  (are [external internal input]
       (= external
          (sut/to-sco internal)
          (->> input
               (reduce sut/event-emit sut/empty-state)
               (sut/to-sco)))

       '[[f 1 0 4 -2 0 0 1 1]
         [f 2 0 4 -2 0 1 1 2]
         [f 3 0 4 -2 0 2 2 3]
         [i 1 0 3 1 2 3]]
       {:table-count 3
        :notes [{:i 1
                 :start 0
                 :duration 3
                 :table-ids [1 2 3]}]
        :tables [{:f 1 :breakpoints [0 0 1 1]}
                 {:f 2 :breakpoints [0 1 1 2]}
                 {:f 3 :breakpoints [0 2 2 3]}]}
       [{:i 1
         :start 0
         :duration 3
         :params {:key/order [:foo :bar :baz]
                  :foo [[0 0] [1 1]]
                  :bar [[0 1] [1 2]]
                  :baz [[0 2] [2 3]]}}]

       '[[f 1 0 4 -2 0 0 1 1]
         [f 2 0 4 -2 0 1 1 2]
         [f 3 0 4 -2 0 2 1 3]
         [f 4 0 6 -2 0 0 1 1 2 4]
         [f 5 0 4 -2 0 1 1 0]
         [f 6 0 6 -2 0 0 1 1 2 0]
         [i 1 0 1 1 2 3]
         [i 1 1 1 4 5 6]]
       {:table-count 6
        :notes [{:i 1
                 :start 0
                 :duration 1
                 :table-ids [1 2 3]}
                {:i 1
                 :start 1
                 :duration 1
                 :table-ids [4 5 6]}]
        :tables [{:f 1 :breakpoints [0 0 1 1]}
                 {:f 2 :breakpoints [0 1 1 2]}
                 {:f 3 :breakpoints [0 2 1 3]}
                 {:f 4 :breakpoints [0 0 1 1 2 4]}
                 {:f 5 :breakpoints [0 1 1 0]}
                 {:f 6 :breakpoints [0 0 1 1 2 0]}]}
       [{:i 1
         :start 0
         :duration 1
         :params {:key/order [:foo :bar :baz]
                  :foo [[0 0] [1 1]]
                  :bar [[0 1] [1 2]]
                  :baz [[0 2] [1 3]]}}
        {:i 1
         :start 1
         :duration 1
         :params {:key/order [:foo :bar :baz]
                  :foo [[0 0] [1 1] [2 4]]
                  :bar [[0 1] [1 0]]
                  :baz [[0 0] [1 1] [2 0]]}}]))

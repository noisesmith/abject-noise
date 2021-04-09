(ns org.noisesmith.composition-dsl.generate-test
  (:require [clojure.test :refer [deftest is are]]
            [org.noisesmith.composition-dsl.generate :as sut]))

(deftest event-collect-test
  (are [x y] (= x
                (->> y
                     (reduce sut/event-collect sut/empty-score)
                     (:instruments)))
       []
       []

       [{:i 1 :start 0 :duration 1 :params {:key/order nil}}]
       [{:e :start :t 0 :i 1}
        {:e :end :t 1}]

       [{:i 1 :start 0 :duration 1 :params {:key/order nil}}]
       [{:e :start :t 0 :i 1}
        {:e :stop :i 1 :t 1}]

       [{:i 1 :start 0 :duration 2 :params {:key/order nil}}
        {:i 2 :start 1 :duration 1 :params {:key/order nil}}]
       [{:e :start :t 0 :i 1}
        {:e :start :t 1 :i 2}
        {:e :end :t 2}]

       ;; f 1 0 2 3 0 2 4
       ;; i 1 0 3 1
       [{:i 1
         :start 0
         :duration 3
         :params {:key/order [:foo]
                  :foo [[0 0] [1 2] [2 4]]}}]
       [{:e :start :t 0 :i 1 :order [:foo]}
        {:e :parameter :i 1 :t 0 :p :foo :v 0}
        {:e :parameter :i 1 :t 1 :p :foo :v 2}
        {:e :parameter :i 1 :t 2 :p :foo :v 4}
        {:e :stop :i 1 :t 3}]))

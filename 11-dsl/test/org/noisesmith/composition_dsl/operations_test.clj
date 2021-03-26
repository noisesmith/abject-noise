(ns org.noisesmith.composition-dsl.operations-test
  (:require [clojure.test :as test :refer [deftest is are]]
            [org.noisesmith.composition-dsl.operations :as sut]))

(deftest input-dispatch-test
  (are [x y]
       (= x
          (->> y
               (reduce sut/input-dispatch sut/empty-composition)
               (:events)))
       [{:e :start :i 1 :t 0 :params [:foo]}
        {:e :parameter :i 1 :p :foo :t 0 :v 42}
        {:e :parameter :i 1 :p :foo :t 1 :v 12}
        {:e :parameter :i 1 :p :foo :t 2 :v 42}
        {:e :stop :i 1 :t 2}
        {:e :start :i 1 :t 3}
        {:e :end :t 4}]
       [[:i 1 :foo]
        [:p :foo 42]
        [:t 1]
        [:p :foo 12]
        [:t 1]
        [:p :foo 42]
        [:d 1]
        [:t 1]
        [:i 1]
        [:t 1]
        [:e]]))

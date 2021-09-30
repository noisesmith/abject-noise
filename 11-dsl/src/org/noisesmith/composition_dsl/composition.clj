(ns org.noisesmith.composition-dsl.composition
  (:require [org.noisesmith.composition-dsl.emit :as emit]
            [org.noisesmith.composition-dsl.operations :as op]
            [org.noisesmith.composition-dsl.generate :as gen]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO - make a DSL targetting the features in the operations namespace ;;;;;;;
;; TODO - use the tables namespace                                       ;;;;;;;
;; the redef below should be equivalent to this...                       ;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
#_
(def score
  [{:i 1
    :start 0
    :duration 1
    ;; TODO - want better table representation...
    :params {:key/order [:amp :hz]
             :amp [[0 0.1 1 1 1 1 1 1 0.1 0]]
             :hz [[1000 1000]]}}])

(def score
  [[:i 1 :amp :hz]
   [:p :amp 0]
   [:p :hz 1000]
   [:t 0.1]
   [:p :amp 1]
   [:t 0.8]
   [:p :amp 1]
   [:t 0.1]
   [:p :amp 0]
   [:p :hz 1000]
   [:e]])

(defn spy
  [prefix data]
  (binding [*out* *err*]
    (println prefix (pr-str data)))
  data)

(defn -main
  [& _]
  (->> score
       (spy "original input")
       (reduce op/input-dispatch op/empty-composition)
       (spy "from op")
       (:events)
       (reduce gen/event-collect gen/empty-score)
       (spy "from gen")
       (:instruments)
       (reduce emit/event-emit emit/empty-state)
       (spy "from emit")
       (emit/to-sco)
       (run! (fn [v]
               (apply println v)))))

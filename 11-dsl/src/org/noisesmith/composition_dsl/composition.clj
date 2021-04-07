(ns org.noisesmith.composition-dsl.composition
  (:require [org.noisesmith.composition-dsl.emit :as emit]))

;; TODO - make a DSL targetting the features in the operations namespace
;; TODO - use the tables namespace
(def score
  [{:i 1
    :start 0
    :duration 1
    ;; TODO - want better table representation...
    :params {:key/order [:amp :hz]
             :amp [[0 0.1 1 1 1 1 1 1 0.1 0]]
             :hz [[1000 1000]]}}])

(defn -main
  [& _]
  (->> score
       (reduce emit/event-emit emit/empty-state)
       (emit/to-sco)
       (run! (fn [v]
               (apply println v)))))

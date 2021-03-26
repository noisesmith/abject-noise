(ns org.noisesmith.composition-dsl
  (:require [org.noisesmith.composition-dsl.operations :as op]
            [org.noisesmith.composition-dsl.generate  :as gen]))

;;; a dsl for "musical" composition

;;; the primary interest is describing changes
;;; of numeric parameters over time

;;; DSL processing
(defn input->events
  [input]
  (let [composition op/empty-composition]
    (->> input
         (reduce op/input-dispatch composition)
         (:events))))

(defn events->generated
  [events]
  (let [score gen/empty-score]
    (->> events
         (reduce gen/event-collect score)
         (:instruments))))

(ns noisesmith.csound)

;; convert a score's tables from event times to instrument times
;; convert tables from instrument times to event times

;; make a simple way to represent the tables and realise them

;; TODO - mixup of additive and divisive manipulation of sequences
;; TODO - use the art of fighting games (concepts like openings, blocks, combos, zoning)
;; to create something that should not be enjoyable but somehow is

(def empty-table
  {:duration 2
   :breakpoints [[0 1] [1 1]]})

(defn total-duration
  "for a given set of breakpoints, finds the cumulative total value"
  [breakpoints]
  (->> []))

(defn table->data
  "for a given table data structure, creates the array of double that should
  be used by csound"
  [{:keys [duration breakpoints]} sr]
  (let [size (* duration
                sr)
        steps (total-duration breakpoints)
        step-size (/ size
                     steps)]
    (for [step-flagstone (range steps)
          step (range step-size)
          :let [ ]]
      ...)))

(defn -main
  []
  (println "e"))

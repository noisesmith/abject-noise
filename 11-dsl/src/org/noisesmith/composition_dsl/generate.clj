(ns org.noisesmith.composition-dsl.generate)

(def empty-score
  {:instruments []})

(defmulti event-collect
  (fn [categorized {:keys [e]}]
    e))

(defmethod event-collect :start
  [categorized {:keys [i t]}]
  (-> categorized
      (update :instruments conj {:i i :start t})
      (update :instruments (partial sort-by
                                    (juxt :start (fnil :duration 0))))
      (update :instruments vec)))

(defn find-last-index
  [f coll]
  (loop [i (dec (count coll))]
    (cond (< i 0)
          nil
          (f (nth coll i))
          i
          :else
          (recur (dec i)))))

(defn update-last-instance
  [instruments i f]
  (if-let [index (find-last-index #(= (:i %) i) instruments)]
    (update instruments index f)
    (do (println "no index for" i "in" instruments)
        instruments)))

(defn end-instrument
  [stop-time]
  (fn set-duration
    [{:keys [start] :as ins}]
    (if (:duration ins)
      ins
      (let [duration (- stop-time
                        start)]
        (assoc ins :duration duration)))))

(defmethod event-collect :stop
  [{:keys [instruments] :as categorized} {:keys [i t]}]
  (-> categorized
      (update :instruments
              update-last-instance i (end-instrument t))))

(defmethod event-collect :parameter
  [{:keys [instruments] :as categorized} {:keys [i p v t]}]
  (let [param-change (fn [running-instrument]
                       (update-in running-instrument [:params p]
                                  (fnil conj [])
                                  [t v]))]
    (-> categorized
        (update :instruments update-last-instance i param-change))))

(defmethod event-collect :end
  [categorized {:keys [t]}]
  (-> categorized
      (update :instruments (partial mapv (end-instrument t)))))

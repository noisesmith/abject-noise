(ns org.noisesmith.composition-dsl.emit)

(def empty-state
  {:table-count 0
   :notes []
   :tables []})

(defn prepare-tables
  [{:keys [table-count] :as emitted} params]
  (into {}
        (map (fn [i p]
               (let [table-id (+ i (inc table-count))
                     bps (val p)]
                 [(key p)
                  {:f table-id
                   :breakpoints (into [] cat bps)}]))
             (range)
             (dissoc params :key/order))))

(defn event-emit
  [emitted {:keys [params] :as event}]
  (let [tables (prepare-tables emitted params)
        event-params (mapv (fn [id]
                             (get-in tables [id :f]))
                          (:key/order params))
        note (-> event
                 (assoc :table-ids event-params)
                 (dissoc :params))]
    (-> emitted
        (update :notes conj note)
        (update :tables into (vals tables))
        (update :table-count + (count tables)))))

(defn ftable
  [{:keys [f breakpoints]}]
  (let [size (/ (count breakpoints)
                1)]
    (into ['f f 0 size -2]
          breakpoints)))

(defn note
  [{:keys [i start duration table-ids]}]
  (into ['i i start duration]
        table-ids))

(defn to-sco
  [{:keys [tables notes]}]
  (-> []
      (into (map ftable) tables)
      (into (map note) notes)))

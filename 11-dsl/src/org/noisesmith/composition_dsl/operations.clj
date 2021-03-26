(ns org.noisesmith.composition-dsl.operations)

(def empty-composition
  {:t 0
   :i nil
   :orchestra {}
   :events []})

;; each input will be a vector, where the
;; first token will describe the kind of event

;;; input DSL
;; the default is setting the time
;; for following events

;;; this simultaneously updates the
;;; event series of a composition, and the
;;; state (based on these events)
(defmulti  input-dispatch
  (fn [composition [d]]
    d))

(defmethod input-dispatch :default
  [composition [_ t]]
  (update composition :t + t))

(defn ensure-present
  "if the instrument isn't active in the composition,
  an event is emitted to create it"
  [{:keys [t] :as composition} instrument params]
  (if (get-in composition [:orchestra instrument])
    composition
    (-> composition
        (update :events
                conj {:e :start
                      :i instrument
                      :params params
                      :t t})
        (update :orchestra assoc instrument {:start t}))))

(defmethod input-dispatch :i
  [composition [_ which & params]]
  (-> composition
      (ensure-present which params)
      (assoc :i which)))

(defmethod input-dispatch :p
  [{:keys [t i] :as composition} [_ parameter value]]
  (-> composition
      (update :events
              conj {:e :parameter
                    :i i
                    :p parameter
                    :v value
                    :t t})
      (assoc-in [:orchestra i :params parameter] value)))

(defmethod input-dispatch :d
  [{:keys [t] :as composition} [_ which]]
  (-> composition
      (update :events
              conj {:e :stop
                    :i which
                    :t t})
      (update :orchestra dissoc which)))

(defmethod input-dispatch :e
  [{:keys [t] :as composition} [_]]
  (-> composition
      (update :events conj {:e :end
                            :t t})
      (reduced)))

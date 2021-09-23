(ns noisesmith.compo
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [noisesmith.generative :as gen]))

(defn loadbp
  [f]
  (load-file (str "breakpoints/" f)))

(def step0
  (-> (gen/->table (loadbp 'step0))
      (gen/scale (* 0.003))))

(def step1
  (-> (gen/->table (loadbp 'step1))
      (gen/scale (* 0.003))))

(def source-time
  (-> (gen/->table (loadbp 'source-time))))

(def grain-duration
  (-> (gen/->table (loadbp 'grain-duration))))

(defn granulatable
  [start total-time]
  (take-while #(< (:in %)
                  total-time)
              (rest
               (iterate (fn [{:keys [in]}]
                          (let [min-dt (gen/interpolate step0 in)
                                max-dt (gen/interpolate step1 in)
                                delta-in (gen/curve min-dt max-dt)
                                in' (+ in delta-in)
                                st (gen/interpolate source-time in)
                                source-t (gen/curve st (+ st 1))
                                dur (gen/interpolate grain-duration in)
                                duration (gen/curve dur (* dur 5))
                                vl (gen/curve -30 0)
                                vr (gen/curve -30 0)]
                            {:in in'
                             :duration duration
                             :data {:t source-t
                                    :vl vl
                                    :vr vr
                                    :fade-in (/ duration 5.0)
                                    :fade-out (/ duration 5.0)
                                    :src 1}}))
                        {:in 0}))))

(defn evdata->scodata
  [{:keys [in duration data :as ev]}]
  (let [{:keys [t vl vr fade-in fade-out src]} data]
    ["i1" in duration vl vr t fade-in fade-out src]))

(defn event->sco
  [row-data]
  (some->> row-data
           (evdata->scodata)
           (string/join " ")))


(defn check-timeout
  [{:keys [timestamp total-time] :as step}]
  (<= timestamp total-time))


(def table-headers
  ["f 1 0 0 1 \"src.ardour/Untitled-2021-04-27-09-30-00/export/session.wav\" 0 4 1"
   "f 2 0 0 1 \"src.ardour/Untitled-2021-04-27-09-30-00/export/session.wav\" 0 4 2"])

(defn gran-score
  []
  (into table-headers
        (cons ""
              (map event->sco
                   (granulatable 0 60)))))

(defn -main
  [& args]
  (run! println (gran-score)))

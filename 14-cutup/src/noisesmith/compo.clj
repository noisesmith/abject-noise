(ns noisesmith.compo
  (:require [clojure.java.io :as io]
            [clojure.string :as string]
            [noisesmith.generative :as gen]))

;; TODO - separate into helpers and the specific score
;;        make more scores that use this score's output as their input

(defn loadbp
  ([f offset scale]
   (-> (load-file (str "breakpoints/" f))
       (gen/->table)
       (gen/scale scale)
       (gen/offset offset)))
  ([f scale]
   (loadbp f 0 scale))
  ([f]
   (loadbp f 0 1)))

(defn until-done
  [f init total-time]
  (take-while #(< (:in %)
                  total-time)
              (rest
               (iterate f
                        (assoc init :in 0)))))

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


(defn ->table-headers
  [fname]
  [(str "f 1 0 0 1 \"" fname "\" 0 4 1")
   (str "f 2 0 0 1 \"" fname "\" 0 4 2")])


(defn mk-tables
  [table-specs]
  (into {}
        (map (fn [[bind args]]
               [bind (apply loadbp
                            (str (:prefix table-specs) "/" (first args))
                            (rest args))]))
        (dissoc table-specs :prefix)))

(defn granulatable
  [start total-time table-specs]
  (let [tables (mk-tables table-specs)
        {:keys [step0 step1 source-time grain-duration]} tables
        {:keys [volume-left volume-right fade-slope]} tables]
    (until-done
     (fn [{:keys [in]}]
       (let [min-dt (gen/interpolate step0 in)
             max-dt (gen/interpolate step1 in)
             delta-in (gen/curve min-dt max-dt)
             in' (+ in delta-in)
             st (gen/interpolate source-time in)
             source-t (gen/curve st (+ st 1))
             dur (gen/interpolate grain-duration in)
             duration (gen/curve dur (* dur 5))
             vl (gen/interpolate volume-left in)
             vl (gen/curve (- vl 30) vl)
             vr (gen/interpolate volume-right in)
             vr (gen/curve (- vr 30) vr)
             slope (gen/interpolate fade-slope in)]
         {:in in'
          :duration duration
          :data {:t source-t
                 :vl vl
                 :vr vr
                 :fade-in (* duration slope)
                 :fade-out (* duration slope)
                 :src (rand-nth [1 2])}}))
     {}
     total-time)))

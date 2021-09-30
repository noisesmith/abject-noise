(ns noisesmith.source-1
  (:require [noisesmith.compo :as compo]))


(defn gran-score
  []
  (let [source-file "src.ardour/Untitled-2021-04-27-09-30-00/export/session.wav"]
    (into (compo/->table-headers source-file)
          (cons ""
                (map compo/event->sco
                     (compo/granulatable 0 60 '{:prefix source-1
                                                :step0 [step0 0.003]
                                                :step1 [step1 0.003]
                                                :source-time [source-time]
                                                :grain-duration [grain-duration]
                                                :volume-left [volume-left]
                                                :volume-right [volume-right]
                                                :fade-slope [slope]}))))))

(defn -main
  [& args]
  (run! println (gran-score)))

(ns org.noisesmith.graph-loader
  (:require [clojure.xml :as xml]
            [clojure.java.io :as io])
  (:import (javax.xml.parsers SAXParserFactory)))

;; nb: this graph forms what is known as a stochasic grammar
;; look at what might be needed to make a swarm grammar?


(defn non-external-parser
  [input y]
  (let [instance (SAXParserFactory/newInstance)
        load-external? "http://apache.org/xml/features/nonvalidating/load-external-dtd"
        _ (.setFeature instance load-external? false)
        parser (.newSAXParser instance)]
    (.parse parser input y)))

(defn load-graph
  [data-source]
  (let [graph-source (io/input-stream data-source)]
    (xml/parse graph-source non-external-parser)))

(defn build-adj
  [adj {:keys [tag attrs content] :as entity}]
  ;; (println "processing" (pr-str entity))
  (case tag
    ;; currently ignoring :data elements
    :data adj
    :node (update adj (:id attrs) (fnil identity #{}))
    :edge (update adj (:source attrs) (fnil conj #{}) (:target attrs))))

(defn adj-list
  "loads up a graph from the xml/parse output into a proper adjacency list"
  [data-source]
  (let [document (load-graph data-source)
        _ (assert (= (:tag document) :graphml))
        body (:content document)
        graphs (filter (comp #{:graph} :tag) body)
        entities (mapcat :content graphs)]
    (reduce build-adj
            {}
            entities)))

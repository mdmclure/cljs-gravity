(ns gravity.tools
  (:require
   [gravity.macros :refer-macros [log warn err]]
   [clairvoyant.core :as trace :include-macros true]))

(defn- get-args
  "Return the first arg or all the list as a js-obj"
  [coll]
  (if (= (count coll) 1)
    (clj->js (first coll))
   	(clj->js coll)))

(defn map-map
  "Maps a binary function over the key value pairs which
  takes the key and value as input and outputs a new value"
  [f m]
  (reduce (fn [altered-map [k v]]
            (assoc altered-map k (f k v))) {} m))

(defn vector-diff-js
  "takes two javascript vectors and an identity function and gives back a map containing :only1, a javascript vector of the elements in the first vector that aren't in the second (according to the identity function), and :only2, the converse."
  [v1 v2 idfn]
  (let [v1 (or v1 #js [])
        v2 (or v2 #js [])
        keys1 (set (map #(aget % idfn) v1))
        keys2 (set (map #(aget % idfn) v2))]
    {:only1 (clj->js (filterv #(not (contains? keys2 (aget % idfn))) v1))
     :only2 (clj->js (filterv #(not (contains? keys1 (aget % idfn))) v2))}))

 (trace/trace-forms
  (defn trace-var [v]
    v))

(defn map-to-jsarray [m & [encoder]]
  (let [array (array)]
    (doseq [[k v] m]
      (.push array k)
      (.push array (if encoder (apply encoder v) v)))
    array))
 
(defn jsarray-to-map [array & [decoder]]
  (let [size (.-length array)]
    (loop [i 0
           map {}]
      (let [k (aget array i)
            v (aget array (inc i))]
        (if (< i (dec size))
          (recur (+ i 2)
                 (assoc map k (if decoder (apply decoder v) v)))
          map)))))

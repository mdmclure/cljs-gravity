(ns gravity.tools
  (:require
   [clairvoyant.core :as trace :include-macros true]))

(defn- get-args
  "Return the first arg or all the list as a js-obj"
  [coll]
  (if (= (count coll) 1)
    (clj->js (first coll))
   	(clj->js coll)))


(defn log
  "Log in the console"
  [& args]
  (.log js/console (get-args args))
	)

(defn warn
  "Warn in the console"
  [& args]
  (.warn js/console (get-args args)))

(defn err
	"Error in the console"
	[& args]
	(.error js/console (get-args args)))

(defn map-map [f m]
  "Maps a binary function over the key value pairs which
  takes the key and value as input and outputs a new value"
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

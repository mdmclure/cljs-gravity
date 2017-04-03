;; (when-not (undefined? js/self.importScripts)
;; 	(.importScripts js/self "../libs/d3.js" "../libs/d3.layout.force3d.js"))

;;---------------------------------

(ns gravity.force.worker
  (:refer-clojure :exclude [str force])
  (:import [goog.object])
  (:require [clojure.set :as set]
            [clairvoyant.core :as trace :include-macros true]))

(defn answer
  "Post a message back"
  ([message]
   	(.postMessage js/self (clj->js message)))
  ([message data]
  	(.postMessage js/self (clj->js message) (clj->js data))))




(defn- get-args
  "Return the first arg or all the list as a js-obj"
  [coll]
  (if (= (count coll) 1)
    (clj->js (first coll))
   	(clj->js coll)))

(defn log
  "Log in the console"
  [args]
  (.log js/console "[force.worker/log]: " (get-args args)))

(defn warn
  "Warn in the console"
  [args]
  (.warn js/console "[force.worker/warn]: " (get-args args)))

(defn str
  [& args]
  (let [arr (clj->js args)]
    (.join arr "")))

(defn eval
  [value]
  (js/eval value))


(def force (atom nil))
(def parameters (atom nil))
(def id-index-map (atom nil))

(defn id->index [id]
  (get @id-index-map id))

(defn index->id [index]
  (get (set/map-invert @id-index-map) index))

(defn id->force-node [id]
  (aget (.nodes @force)
        (id->index id)))

;; --------------------------------



(defn tick
  "Tick function for the force layout.
  MODIFIED: For each node, we also insert the id into
  the typed array being sent back to the "
  [_]
  (let [nodes (.nodes @force)
        size (.-length nodes)]
    ;;(log [(first nodes)])
    (when (> size 0)
      (let [arr (new js/Float32Array (* size 4))
            buffer (.-buffer arr)]
        (loop [i 0]
          (let [j (* i 4)
                node (aget nodes i)]
            (aset arr j (index->id i))
            (aset arr (+ j 1) (.-x node))
            (aset arr (+ j 2) (.-y node))
            (if (js/isNaN (.-z node))
              (aset arr (+ j 3) 0)
              (aset arr (+ j 3) (.-z node))))
          (when (< i (dec size))
            (recur (inc i))))
        (answer {:type "nodes-positions" :data arr} [buffer])))))

(defn end
  "End function for the force layout"
  [_]
  (answer {:type "end"}))








(defn init
  [params]
  (let [params (js->clj params :keywordize-keys true)]
    (reset! parameters params)
    (answer {:type :ready})
    nil))

(defn make-force
  []
  (let [params @parameters
        force-instance (-> (.force3d js/d3.layout)
                           (.size (clj->js (:size params)))
                           (.linkStrength (:linkStrength params))
                           (.friction (:friction params))
                           (.linkDistance (:linkDistance params))
                           (.charge (:charge params))
                           (.gravity (:gravity params))
                           (.theta (:theta params))
                           (.alpha (:alpha params))
                           )]
    (.on force-instance "tick" tick)
    (.on force-instance "end" end)
    force-instance))


(defn- make-force-nodes
  "Add or remove the correct amount of nodes and keep their positions.  MODIFIED: Build the mapping from id->index for links"
  [nodes]
  (let [size (.-length nodes)
        force-nodes (array)]
    (when (> size 0)
      (reset! id-index-map)
      (loop [i 0]
        (let [node (aget nodes i)
              id (.-id node)]
          (swap! id-index-map assoc id i)
          (if (.-position node)
            (.push force-nodes (.-position node))
            (.push force-nodes {})))
        (when (< i (dec size))
          (recur (inc i)))))
    force-nodes))

 
(defn- make-force-links [links]
  (let [size (.-length links)
        force-links (array)]
    (when (> size 0)
      (loop [i 0]
        (let [link (aget links i)
              source-id (.-source link)
              target-id (.-target link)
              sim (aget link "similarity")
              source-index (id->index source-id)
              target-index (id->index target-id)]
          (.push force-links (clj->js {:source source-index :target target-index :similarity sim})))
        (when (< i (dec size))
          (recur (inc i)))))
    force-links))


(defn start
  "start the force"
  []
  (when-not (nil? @force)
    (.start @force)))

(defn stop
  "Stop the force"
  []
  (when-not (nil? @force)
    (.stop @force)))

(defn resume
  "Resume the force"
  []
  (when-not (nil? @force)
    (.resume @force)))

(defn set-nodes
  "Set the nodes list.  nodes should be a js array"
  [nodes]
  (stop)
  (let [new-force (make-force)
        force-nodes (make-force-nodes nodes)]
    (.nodes new-force force-nodes)
    (reset! force new-force)
    (start)))


(defn set-links
  "Set the links list"
  [links]
  (stop)
  (when-not (nil? @force)
    (let [force-links (make-force-links links)]
      (.links @force force-links)
      (start))))


(defn precompute
  "Force the layout to precompute"
  [steps]
  (if (or (< steps 0) (nil? steps))
    (do
      (.log js/console "Precomputing layout with default value. Argument given was <0. Expected unsigned integer, Given:" steps )
      (precompute 50))
    (do
      (let [start (.now js/Date)]
        (.on @force "tick" nil)
        (dotimes [i steps]
          (.tick @force))
        (.on @force "tick" tick)
        (log (str "Pre-computed in " (/ (- (.now js/Date) start) 1000) "ms.")))
      )))



(defn set-position
  "Set a node's position"
  [data]
  (let [id (-> data .-id)
        position (-> data .-position)
        node (id->force-node id)
        alpha (.alpha @force)]

    (stop)

    ;;(when-not (> alpha 0)
      ;;(.alpha @force 0.01))

    (set! (.-x node) (.-x position))
    (set! (.-y node) (.-y position))
    (set! (.-z node) (.-z position))


    (set! (.-px node) (.-x position))
    (set! (.-py node) (.-y position))
    (set! (.-pz node) (.-z position))

    ;;(set! (.-fixed node) false)

    (tick nil)
    ))

(defn pin
  "pin a node by id"
  [data]
  (let [id (-> data .-id)
        node (id->force-node id)]
    (set! (.-fixed node) true)))

(defn unpin
  "unpin a node by id"
  [data]
  (let [id (-> data .-id)
        node (id->force-node id)]
    (set! (.-fixed node) false)))




(defn dispatcher
  "Dispatch a message to the corresponding action (route)."
  [event]

  (let [message (.-data event)
        type (.-type message)
        data (.-data message)]
    (case type
      "init"  (init data)
      "start" (start)
      "stop"  (stop)
      "resume" (resume)
      "tick" (tick nil)
      "set-nodes" (set-nodes data)
      "set-links" (set-links data)
      "precompute" (precompute data)

      "set-position" (set-position data)
      "pin" (pin data)
      "unpin" (unpin data)

      ;set params
      "size" (swap! parameters assoc :size (js->clj data))
      "linkStrength" (swap! parameters assoc :linkStrength (eval data))
      "friction" (swap! parameters assoc :friction data)
      "linkDistance" 
      (swap! parameters assoc :linkDistance (eval data))
      "charge" (swap! parameters assoc :charge (eval data))
      "gravity" (swap! parameters assoc :gravity data)
      "theta" (swap! parameters assoc :theta data)
      "alpha" (swap! parameters assoc :alpha data)

      (warn (str "Unable to dispatch '" type "'")))))


;; :size [1 1]
;; :linkStrength 1
;; :friction 0.9
;; :linkDistance 20
;; :charge -30
;; :gravity 0.1
;; :theta 0.8
;; :alpha 0.1



(defn ^:export create
  "Main entry point"
  []
  (.addEventListener js/self "message" dispatcher))

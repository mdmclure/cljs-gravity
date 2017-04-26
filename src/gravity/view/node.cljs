(ns gravity.view.node
  (:require
   ;[clairvoyant.core :as trace :include-macros true]
   [gravity.macros :refer-macros [log warn err]]
   [clairvoyant.core :as trace :include-macros true]))



(defn- get-color   ;; TODO
  "Give a color for a given node"
  [group classifier]
  (let [key group]
    (new js/THREE.Color (classifier key))))

(def get-unique-color
  (memoize get-color))



(defn get-rand-pos
  "Give a random position between -extent and +extent"
  [extent]
  (-> (* 2 extent)
      (rand)
      (- extent)))

(defn generate-cube-geometry
  "Generate a generic geometry"
  []
  (new js/THREE.BoxGeometry 1 1 1))

(defn generate-geometry
  "Generate a generic geometry"
  []
  (new js/THREE.SphereGeometry 1 10 10))

(def get-unique-cube-geometry
  (memoize generate-cube-geometry))

(def get-unique-geometry
  (memoize generate-geometry))

(defn generate-material
  "Generate a generic material"
  [color-key]
  (new js/THREE.MeshLambertMaterial (clj->js {:color (new js/THREE.Color color-key)
                                              :visible true
                                              :wireframe false})))


(def get-unique-material
  (memoize generate-material))


(defn generate-collider
  "create and return a new node mesh used for collisions"
  [node classifier]
  (let [geometry (get-unique-geometry)
        material (get-unique-material (get-unique-color (.-group node) classifier))
        sphere (new js/THREE.Mesh geometry material)]
    (.set (.-scale sphere) 0.3 0.3 0.3)
    sphere))

(defn create
  "Return a cloned node with a random position and a collider object"
  [node classifier]
  (let [ext 100
        node (.clone js/goog.object node)
        collider (generate-collider node classifier)
        position (new js/THREE.Vector3 (get-rand-pos ext)
                      (get-rand-pos ext) (get-rand-pos ext))]
    (if (not (.-id node))
      (err "Node must have id slot! " node)
      (do
        (set! (.-position node) position)
        (set! (.-mesh node) collider)
        (set! (.-castShadow collider) true)
        (set! (.-node collider) node)
        (set! (.-selected node) false)))
    node))



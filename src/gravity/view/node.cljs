(ns gravity.view.node
  (:require
   ;[clairvoyant.core :as trace :include-macros true]
   [gravity.tools :refer [log err]]))



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


(defn generate-coord-mesh
  "create and return a new node mesh used for collisions"
  [coord]
  (let [geometry (get-unique-cube-geometry)
        material (get-unique-material (str 'black))
        sphere (new js/THREE.Mesh geometry material)]
    (.set (.-scale sphere) 0.1 0.1 0.1)
    (.set (.-position sphere)
          (nth coord 0) (nth coord 1) (nth coord 2))
    sphere))

(defn create-field-pt
  [coord]
  (let [field-mesh (generate-coord-mesh coord)
        field-pt (clj->js {})]
    (set! (.-co field-pt) (clj->js coord))
    (set! (.-mesh field-pt) field-mesh)
    (set! (.-castShadow field-mesh) false)
    (set! (.-fieldpt field-mesh) field-pt)
;    (log field-pt)
    field-pt))

(ns gravity.view.field
  (:refer-clojure :exclude [update])
  (:require [gravity.tools :refer [map-map]]
            [gravity.macros :refer-macros [log warn err]]
            [gravity.view.node :as node]
            [clojure.set :as set]
            [clairvoyant.core :as trace :include-macros true]
            ))


;;macro-ize later
(defn coord-x [c]
  (get c 0))

(defn coord-y [c]
  (get c 1))

(defn coord-z [c]
  (get c 2))

; SVM Field stuff

(defn generate-cube-geometry
  "Generate a generic geometry"
  []
  (new js/THREE.BoxGeometry 1 1 1))

(def get-unique-cube-geometry
  (memoize generate-cube-geometry))

(defn generate-coord-mesh
  "create and return a new node mesh used for collisions"
  [coord]
  (let [geometry (get-unique-cube-geometry)
        material (node/get-unique-material (str 'black))
        cube (new js/THREE.Mesh geometry material)]
    (set! (.-hidden cube) true)
    (set! (.-castShadow cube) false)
    (.set (.-position cube)
          (coord-x coord) (coord-y coord) (coord-z coord))
    cube))

(defn show! [mesh scene]
  ;(log "SHOW " mesh scene)
  (when (.-hidden mesh)
    (.add scene mesh))
  (set! (.-hidden mesh) false))

(defn hide! [mesh scene]
  ;(log "HIDE " mesh scene)
  (when-not (.-hidden mesh)
    (.remove scene mesh))
  (set! (.-hidden mesh) true))

(defn scale! [mesh scene scale]
  ;(log "SCALE " mesh scene scale)
  (if (> scale 0)
    (do (show! mesh scene)
        (.set (.-scale mesh) scale scale scale))
    (hide! mesh scene)))

(defn cart [colls]
  (if (empty? colls)
    '(())
    (for [x (first colls)
          more (cart (rest colls))]
      (cons x more))))

(defn sum-of-squares [coord]
  (.sqrt js/Math 
          (+ (.pow js/Math (nth coord 0) 2)
             (.pow js/Math (nth coord 1) 2)
             (.pow js/Math (nth coord 2) 2))))

;;A cube with edges of length 2, centered at [0 0 0]
(defn double-unit-cube-coordinates [density]
  (let [span (map #(-> (* % (/ 2 density))
                       (- 1))
                  (range (+ 1 density)))]
    (cart [span, span, span])))

(defn unit-sphere-coordinates [density]
  (for [coord (double-unit-cube-coordinates density)
        :let [r (sum-of-squares coord)]
        :when (<= r 1)]
    (vec coord)))

(def get-unique-unit-sphere-coordinates
  (memoize unit-sphere-coordinates))

(defn field-density
  [base-density training-ct]
  (let [max-density 25
        saturation-ct 10]
    (+ base-density
       (* (- max-density base-density)
          (.tanh js/Math (/ (or training-ct 0) saturation-ct))))))

(defn prepare-cc-meshes
  "Takes a map of connected components to build subfields around.  The map takes the form:  {cc-id1: #{set-of-nodes1} ... cc-id2: #{set-of-nodes2}.  Returns a map, where the val for :field-pts is a map of ccid to corresponding subfield's field-pts, and the value for :meshes is an array of all the meshes for all subfields"
  [state base-density]
  ;(log "Prepare cc meshes" base-density)
  (let [training (:training-nodes @state)
        new-ccs (map-map (fn [ccid cc]
                           (let [node-set (:nodes cc)
                                 cc-training (set/intersection training node-set)
                                 subfield-meshes (when (> (count cc-training) 0)
                                                   (map generate-coord-mesh
                                                        (get-unique-unit-sphere-coordinates
                                                         (field-density base-density (count cc-training)))))]
                             ;(log "CC" ccid "made with" (count subfield-meshes) "meshes and density" (* base-density (count cc-training)))
                             (assoc cc :meshes subfield-meshes)))
                         (:connected-components @state))]
    (swap! state assoc :connected-components new-ccs)
    ;(log "Prepared cc meshes" training new-ccs)
    ))
  
(defn update-cc-meshes! [state scene field-data]
  (let [ccs (get @state :connected-components)
        n-ccs (count ccs)
        scale (get-in @state [:field :scale])
        classifier (:classifier @state)
        n-meshes (count (reduce into (map (fn [[ccid cc]] (:meshes cc)) ccs)))]
    ;(log "Update cc meshes" field-data n-ccs n-meshes (* 5 n-meshes) ccs)
    (when (= (.-length field-data) (+ (* 5 n-meshes) n-ccs))
      (let [offset (atom 0)
            ct (atom 0)]
        (while (and (aget field-data @offset) (< @ct 10))
          (let [ccid (aget field-data @offset)
                meshes (get-in ccs [ccid :meshes])
                i (atom 0)]
            ;(log "CC" ccid "Expecting" (count meshes) "meshes at" @offset)
            ;(log "Meshes:" meshes "ccid:" ccid "counter" ct "neighbors" (.slice field-data (- @offset @ct)))
            (swap! ct inc)
            (doseq [mesh meshes]
              (let [j (+ 1 @offset (* @i 5))
                    x (aget field-data j)
                    y (aget field-data (+ 1 j))
                    z (aget field-data (+ 2 j))
                    label (aget field-data (+ 3 j))
                    margin (aget field-data (+ 4 j))
                    new-material (node/get-unique-material (classifier label))]
                (swap! i inc)
                ;;position the field point
                (.set (.-position mesh) x y z)
                ;;color it according to the label
                (set! (.-material mesh) new-material)
                ;;size it according to the classification margin
                (scale! mesh scene (* scale margin))))
            (swap! offset #(+ 1 % (* @i 5)))))))))

(defn update-field-scale!
  [state scene scale]
  ;;(.log js/console "update field scale " field scale)
  (when scale
    (doseq [[ccid cc] (:connected-components @state)]
      (doseq [mesh (:meshes cc)]
        (scale! mesh scene scale)))))


 

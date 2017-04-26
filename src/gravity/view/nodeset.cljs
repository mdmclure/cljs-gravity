(ns gravity.view.nodeset
  (:refer-clojure :exclude [update])
  (:require [gravity.macros :refer-macros [log warn err]]
            [gravity.view.node :as node]
            [clojure.set :refer [union]]
            [clairvoyant.core :as trace :include-macros true]
            ))



(defn id->node [id nodes]
  (when (and id nodes)
    (let [size (.-length nodes)
          found (atom nil)]
      (when (> size 0)
        (loop [i 0]
          (let [node (aget nodes i)]
            (when (= id (.-id node))
              (reset! found node)))
          (when (and (not @found) (< i (- size 1)))
            (recur (inc i))))
        @found))))

(defn merge-ccs
  "merge two connected components in the map"
  [ccs ccid1 ccid2]
  (let [node-set1 (get-in ccs [ccid1 :nodes])]
    (-> ccs
        (update-in [ccid2 :nodes] union node-set1)
        (dissoc ccid1))))

(defn node-to-cc-map
  [ccs]
  (reduce (fn [result [ccid cc]]
            (into result
                  (map (fn [node-id] [node-id ccid]) (:nodes cc))))
          {} ccs))


(defn create-links
  "Given a js-array of nodes and a js array of links, will return a THREE.LineSegments.  Also updates connected components in this pass to be efficient"
  [nodes links ccs]
  ;(log "Create Links " (clj->js ccs))
  (let [geometry (new js/THREE.Geometry)
        vertices (.-vertices geometry)
        material (new js/THREE.LineBasicMaterial #js {"color" 0x000000})
        system (new js/THREE.LineSegments geometry material)
        new-ccs (reduce (fn [ccs link]
                          (let [source (id->node (.-source link) nodes)
                                target (id->node (.-target link) nodes)
                                node-to-cc (node-to-cc-map ccs)
                                source-cc (get node-to-cc (.-source link))
                                target-cc (get node-to-cc (.-target link))]
                            ;(log "Link Source: " source-cc source "Target: " target-cc target)
                            ;;side effect: build line segment geometry  
                            (.push vertices (.-position source))
                            (.push vertices (.-position target))
                            ;;if the link connects two distinct ccs, merge them
                            (if (not= source-cc target-cc)
                              (let [high (max source-cc target-cc)
                                    low (min source-cc target-cc)]
                                ;(log "Merge high " high " into low " low)
                                ;; always merge the high into the low
                                (merge-ccs ccs high low))
                              ccs)))
                        ccs links)]
    (set! (.-verticesNeedUpdate geometry) true)
    (set! (.-castShadow system) true)
    ;(log "Created Links " system (clj->js new-ccs))
    {:links system
     :ccs new-ccs}))
 
(defn prepare-nodes
  "Create a array of cloned nodes containing a position and a collider object.
  Return a map {nodes[] colliders[]} meant to be destructured.
  The nodes and the colliders are in the same order and share the same position Vector3."
  [nodes classifier]
  ;(log "Prepare nodes " nodes)
  (let [pairs (map (fn [node]
                     (let [prepared-node (node/create node classifier)
                           mesh (.-mesh prepared-node)]
                       [prepared-node mesh]))
                   nodes)]
    {:nodes (clj->js (mapv first pairs))
     :meshes (clj->js (mapv last pairs))}))
 
(defn update-geometry
  "Update a nodeset Points geometry or a LineSegments geometry"
  [geom-based-item]
  (set! (.-verticesNeedUpdate (.-geometry geom-based-item)) true)
  geom-based-item)


(defn update-positions!
  "Update the nodes' positions according to the raw array given by the force.
  MODIFIED: Because we use and id to identify each node, positions now has an extra n slots (* n 4) total instead of (* n 3)."
  [nodes positions]
  (when (= 0 (mod (.-length positions) 4))
    (let [size (/ (.-length positions) 4)
          ;;new-nodes (atom nil)
          ]
      (loop [i 0]
        (let [j (* i 4)
              id (aget positions j)
              node (id->node id nodes)
              x (aget positions (+ j 1))
              y (aget positions (+ j 2))
              z (aget positions (+ j 3))]
          (when node
            (.set (.-position (.-mesh node)) x y z)
            (.set (.-position node) x y z)))
        
        (when (< i (- size 1))
          (recur (inc i))))
      ;;@new-nodes ;not needed
      )))


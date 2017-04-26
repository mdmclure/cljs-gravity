(ns gravity.view.graph
  (:require
   [gravity.tools :refer [vector-diff-js map-map map-to-jsarray]]
   [gravity.view.node :as node]
   [gravity.view.nodeset :as points]
   [gravity.macros :refer-macros [log warn err]]
   [gravity.view.graph-tools :as tools]
   [gravity.view.events-generator :as events]
   [gravity.view.field :as field]
   [gravity.force.proxy :as force-worker]
   [gravity.field.proxy :as field-worker]
   [clojure.set :as set]
   [clairvoyant.core :as trace :include-macros true]))




;; Init view's parameters


(defn get-components
  "Generate or re-use all the necessary components of the 3D view"
  [user-map dev-mode]
  ;; (if (:canvas user-map)
  ;;   (tools/fill-window! (:canvas user-map)))

  (let [webgl-params (:webgl user-map)
        width (.-width (:canvas user-map))
        height (.-height (:canvas user-map))
        camera (new js/THREE.PerspectiveCamera 75 (/ width height) 0.1 100000 )]

    (set! (.-z (.-position camera)) 100)

    {:scene (new js/THREE.Scene)
     :width width
     :height height
     :camera camera
     :stats (:stats user-map)
     :controls (new js/THREE.OrbitControls camera)
     :renderer (new js/THREE.WebGLRenderer #js {:antialias (:antialias webgl-params)
                                                :canvas (:canvas user-map)})
     :raycaster (new THREE.Raycaster)
     :classifier (:color user-map)
     :force-worker (or (:force-worker user-map)
                       (force-worker/create (:force-worker-path user-map) (:force user-map)))
     :field-worker (or (:field-worker user-map)
                       (field-worker/create (:field-worker-path user-map) (:field user-map)))
     :field (:field user-map)
     :state (atom {:should-run true})
     :first-run (:first-run user-map)}))







;; CALLBACKS



(defn- render-callback
  "Return a function rendering the context"
  [renderer scene camera stats state controls select-circle]
  (fn render []

     (.update controls)

     (when-not (nil? stats)
       (.begin stats))

     (if (get @state :should-run)
       (do
         (when-not (nil? select-circle)
           (let [x1 (-> select-circle .-rotation .-x)
                 y1 (-> select-circle .-rotation .-y)
                 x2 (+ x1 0.01)
                 y2 (+ y1 0.1)]
             (.set (-> select-circle .-rotation) x2 y2 0)))
         (.requestAnimationFrame js/window render)
         (.render renderer scene camera)))

     (when-not (nil? stats)
       (.end stats))))




(defn- start-callback!
  "Return a closure affecting the application state atom
  and triggering the render function once"
  [state render]
  (fn []
     (when-not (:should-run @state)
       (swap! state assoc :should-run true)
       (render))
     nil))



(defn- stop-callback!
  "Return a closure affecting the application state atom"
  [state]
  (fn []
     (swap! state assoc :should-run false) nil))

(defn- resume-force-callback
  "Send a resume event to the force worker"
  [force-worker]
  (fn []
     (force-worker/send force-worker "resume")))

(defn- update-field-connected-components
  [field-worker ccs]
  (let [node-to-cc-map (atom {})
        cc-node-map (map-map (fn [k v]
                               (doseq [node-id (:nodes v)]
                                 (swap! node-to-cc-map
                                        assoc node-id k))
                               v)
                             ccs)
        arr (map-to-jsarray @node-to-cc-map)]
    (field-worker/send field-worker "set-node-fields" arr)))
        
(defn- build-cc-fields
  [state field-worker base-density]
  (let [ccs (:connected-components @state)]
    (update-field-connected-components field-worker ccs)
    (field/prepare-cc-meshes state base-density)))

(defn- clear-cc-meshes
  [ccs scene]
  (doseq [[ccid cc] ccs]
    (doseq [mesh (:meshes cc)]
      (.remove scene mesh))))

(defn- update-field-nodes
  "Ask the field agent for an field update base on the new node positions"
  [field-worker node-position-data]
  (field-worker/send field-worker "set-training-data-3D" node-position-data))

(defn- set-links
  "Remove the old links and add the new ones"
  [state scene field-worker links]
  ;(log "Set links: " links (clj->js @state))
  (let [old-links (:links-set @state)
        nodes (:nodes @state)
        ccs (:connected-components @state)
        base-density (get-in @state [:field :base-density])
        {new-links :links
         new-ccs :ccs} (points/create-links nodes links ccs)
        {added-links :only1
         removed-links :only2}
        (vector-diff-js links (:links @state) "id")]
    (.remove scene old-links)
    (.add scene new-links)
    (swap! state assoc :links links)
    (swap! state assoc :links-set new-links)
    (swap! state assoc :connected-components new-ccs)
    (build-cc-fields state field-worker base-density)
    added-links))

(defn- build-singleton-ccs [nodes]
  (let [ids (mapv #(.-id %) nodes)]
    (zipmap ids (mapv (fn [id] {:nodes (hash-set id)}) ids))))
  
(defn mark-as-training [state field-worker nodes]
  (let [node-ids (map #(.-id %) nodes)
        node-labels (map #(.-group %) nodes)
        label-map (zipmap node-ids node-labels)
        label-arr (map-to-jsarray label-map)]
    (field-worker/send field-worker "set-training-labels" label-arr)
    (swap! state assoc :training-nodes
           (into #{} node-ids))))

(defn- set-nodes
  "Remove the old nodes and add the new ones"
  [state scene field-worker nodes]
  ;(log "Set nodes: " nodes (clj->js @state))
  (let [classifier (:classifier @state)
        old-nodes (:nodes @state)
        {added-nodes :only1
         removed-nodes :only2} (vector-diff-js nodes old-nodes "id")
        {reused-nodes :only1
         _ :only2} (vector-diff-js old-nodes removed-nodes "id")
        {added-nodes :nodes
         added-meshes :meshes} (points/prepare-nodes added-nodes classifier)
        new-nodes (.concat reused-nodes added-nodes)
        new-meshes (clj->js (mapv #(.-mesh %) new-nodes))
        ccs (build-singleton-ccs new-nodes)]
    (doseq [node removed-nodes]
      ;(log "Removing mesh: " node (-> node .-mesh) scene)
      (.remove scene (-> node .-mesh)))
    (doseq [mesh added-meshes]
      ;(log "Adding mesh: " mesh scene)
      (.add scene mesh))
    (clear-cc-meshes (:connected-components @state) scene)
    (swap! state #(-> %
                      (assoc :nodes new-nodes)
                      (assoc :meshes new-meshes)
                      (assoc :connected-components ccs)))
    ;;temporary: add all nodes to training
    (mark-as-training state field-worker nodes)
    ;(set-links state scene field-worker [])
    ;;By only returning the added nodes we send back a signal about whether there were any changes
    added-nodes))


(defn- set-field-scale
  "Change the relative size of the field meshes"
  [state scene scale]
  (swap! state assoc-in [:field :scale] scale)
  (field/update-field-scale! state scene scale))


(defn- set-nodes-callback
  "Allow the user to replace nodes with a new set of nodes.
  MODIFIED: Maintain a clojurescript map to store the nodes internally.  Convert from js on input and to js on output."
  [state scene field-worker]
  (fn [nodes]
    (if (nil? nodes)
      (:nodes @state)
      (set-nodes state scene field-worker nodes))))

(defn- set-links-callback
  [state scene field-worker]
  (fn [links]
    (if (nil? links)
      (:links @state)
      (set-links state scene field-worker links))))

(defn- set-field-scale-callback
  [state scene]
  (fn [scale]
    (if (nil? scale)
      (get-in @state [:field :scale])
      (set-field-scale state scene scale))))



(defn- update-force-callback
  [state force-worker]
  (fn []
    (let [nodes (:nodes @state)
          links (:links @state)]
      (force-worker/send force-worker "set-nodes"
                   (mapv #(clj->js {:id (.-id %)
                                    :position (.-position %)})
                         nodes))
      (force-worker/send force-worker "set-links" links)
      )))




(defn init-force
  [force-worker dev-mode first-run]
  (when (and (not first-run) dev-mode)
    (force-worker/send force-worker "tick")))

(defn notify-force-ready [state event-channel]
  (swap! state assoc-in [:force :ready] true)
  (when (get-in @state [:field :ready])
    (events/notify-user-ready event-channel)))

(defn notify-field-ready [state event-channel]
  (swap! state assoc-in [:field :ready] true)
  (when (get-in @state [:force :ready])
    (events/notify-user-ready event-channel)))

(defn create
  "Initialise a context in the specified element id"
  [user-map chan-out dev-mode]
  (let [{first-run :first-run
         scene :scene
         width :width
         height :height
         camera :camera
         stats :stats
         controls :controls
         renderer :renderer
         raycaster :raycaster
         classifier :classifier
         force-worker :force-worker
         field-worker :field-worker
         state :state
         field :field} (get-components user-map dev-mode)

         canvas (if-not (nil? (:canvas user-map))
                  (:canvas user-map)
                  (.-domElement renderer))

         select-circle (tools/get-circle)
         intersect-plane (tools/get-intersect-plane)
         ;;data-test (demo/get-demo-graph)
         ;;{nodes :nodes
         ;; meshes :meshes} (points/prepare-nodes (.-nodes data-test) classifier)

         ;;links (.-links data-test)
         ;;nodeset (points/create nodes classifier)
         ;;links-set (points/create-links nodes links)
         render (render-callback renderer scene camera stats state controls select-circle)]
    

    ;;     (swap! state assoc :nodes nodes)
    ;;     (swap! state assoc :meshes meshes)
    ;;     (swap! state assoc :links links)
    ;;     (swap! state assoc :links-set links-set)
    (swap! state assoc :classifier classifier)
    (swap! state assoc :select-circle select-circle)
    (swap! state assoc :meshes (array))
    (swap! state assoc :field field)


    ;; renderer
    (.setSize renderer width height)
    (.setClearColor renderer 0x404040)

    ;;shadows
    (when (:shadows (:webgl user-map))
      (set! (-> renderer .-shadowMap .-enabled) true)
      (set! (-> renderer .-shadowMap .-type) js/THREE.PCFSoftShadowMap))



    (force-worker/listen force-worker (fn [event]
                                  (let [message (.-data event)
                                        type (.-type message)
                                        data (.-data message)]
                                    (case type
                                      "ready" (do (init-force force-worker dev-mode first-run)
                                                  (notify-force-ready state chan-out))
                                      "end" (events/notify-user-stable chan-out (:nodes @state))
                                      "nodes-positions" (do
                                                          (points/update-positions! (:nodes @state) data)
                                                          ;;pass the node positions directly on to the field worker
                                                          (when-not (:waiting-on-field? @state)
                                                            (do (swap! state assoc :waiting-on-field? true)
                                                                (update-field-nodes field-worker data)))
                                                          ;;(points/update nodeset)
                                                          (points/update-geometry (:links-set @state))
                                                          )))))
    
    (field-worker/listen field-worker (fn [event]
                                        (let [message (.-data event)
                                              type (.-type message)
                                              data (.-data message)]
                                          (case type
                                            "ready" (notify-field-ready state chan-out)
                                            "field-coordinate-predictions-3D"
                                            (do (field/update-cc-meshes! state scene data)
                                                (swap! state assoc :waiting-on-field? false))))))

    ;; if it's not the first time in dev mode
    (when (and (not first-run) dev-mode)
      (tools/fill-window! canvas)
      (.removeEventListener canvas "mousemove")
      (.removeEventListener canvas "click")
      (events/notify-user-ready chan-out))


    ;;(force-worker/send force-worker "precompute" 50)

    (.add scene select-circle)
    (set! (-> select-circle .-visible) false)

    (.add scene intersect-plane)

    (.addEventListener js/window "resize" (events/onWindowResize canvas renderer camera))



    (let [     ;mouse (events/listen-to-canvas canvas)
          mouse (events/listen-to-canvas js/window.document)]
      (events/apply-events-to mouse canvas camera raycaster intersect-plane controls state force-worker chan-out))






    (let [webgl-params (:webgl user-map)
          background? (:background webgl-params)
          lights? (:lights webgl-params)]
      ;; add background
      (when background?
        (.add scene (tools/get-background)))

      ;; add lights

      (doseq [light (tools/get-lights lights?)]
        (.add scene light)))


    (events/watch-state state :main-watcher)


    (render)

    ;; return closures to user

    {:start (start-callback! state render)
     :stop (stop-callback! state)
     :resume (resume-force-callback force-worker)
     :tick (fn [] (force-worker/send force-worker "tick"))
     :canvas (.-domElement renderer)
     :stats stats
     :nodes (set-nodes-callback state scene field-worker)
     :links (set-links-callback state scene field-worker)
     :updateForce (update-force-callback state force-worker)
     
     :force {:size           (fn [array] (force-worker/send force-worker "size" array))
             :linkStrength   (fn [val] (force-worker/send force-worker "linkStrength" (force-worker/serialize val)))
             :friction       (fn [val] (force-worker/send force-worker "friction" val))
             :linkDistance   (fn [val] (force-worker/send force-worker "linkDistance" (force-worker/serialize val)))
             :charge         (fn [val] (force-worker/send force-worker "charge" (force-worker/serialize val)))
             :gravity        (fn [val] (force-worker/send force-worker "gravity" val))
             :theta          (fn [val] (force-worker/send force-worker "theta" val))
             :alpha          (fn [val] (force-worker/send force-worker "alpha" val))}
     ;;  field closures
     :field {:scale (set-field-scale-callback state scene)
             }
     :selectNode (fn [node-id]
                   (let [node (node-id (:nodes @state))]
                     (swap! state assoc :selected node)
                     (:selected @state)))
     :pinNode (fn [node-id]
                (let [circle (tools/get-circle)
                      node (node-id (:nodes @state))
                      mesh (:mesh node)]
                  (set! (-> mesh .-circle) circle)
                  (.add mesh circle))
                (force-worker/send force-worker "pin" {:id node-id}))
     :unpinNode (fn [node-id]
                  (let [node (node-id (:nodes @state))
                        mesh (:mesh node)]
                    (tools/remove-children mesh)
                    (.remove mesh (.-circle mesh))
                    (force-worker/send force-worker "unpin" {:id node-id})))
     :camera camera
     }))




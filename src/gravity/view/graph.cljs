(ns gravity.view.graph
  (:require
   [gravity.tools :refer [log trace-var vector-diff-js]]
   [gravity.view.node :as node]
   [gravity.view.nodeset :as points]
   [gravity.view.graph-tools :as tools]
   [gravity.view.events-generator :as events]
   [gravity.force.proxy :as worker]
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
     :field-scale (or (:field-scale user-map) 0)
      :force-worker (if (:force-worker user-map)
                      (:force-worker user-map)
                      (worker/create (:worker-path user-map) (:force user-map)))

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
     (worker/send force-worker "resume")))



(defn- set-links
  "Remove the old links and add the new ones"
  [state scene links]
  (let [old-links (:links-set @state)
        nodes (:nodes @state)
        new-links (points/create-links nodes links)
        {added-links :only1
         removed-links :only2}
        (vector-diff-js links (:links @state) "id")]
    (.remove scene old-links)
    (.add scene new-links)
    (swap! state assoc :links links)
    (swap! state assoc :links-set new-links)
    added-links))
  
(defn- set-nodes
  "Remove the old nodes and add the new ones"
  [state scene nodes]
  (let [classifier (:classifier @state)
        old-nodes (:nodes @state)
        {added-nodes :only1
         removed-nodes :only2} (vector-diff-js nodes old-nodes "id")
        {reused-nodes :only1
         _ :only2} (vector-diff-js old-nodes removed-nodes "id")
        {added-nodes :nodes
         added-meshes :meshes} (points/prepare-nodes added-nodes classifier)
        new-nodes (.concat reused-nodes added-nodes)
        new-meshes (clj->js (mapv #(.-mesh %) new-nodes))]
    (doseq [node removed-nodes]
      (.remove scene (-> node .-mesh)))
    (doseq [mesh added-meshes]
      (.add scene mesh))
    (swap! state assoc :nodes new-nodes)
    (swap! state assoc :meshes new-meshes)
    (set-links state scene [])
    ;;By only returning the added nodes we send back a signal about whether there were any changes
    added-nodes))

(defn- set-svms
  "Remove the old svms and add the new ones"
  [state scene force-worker svms]
  (let [classifier (:classifier @state)]
    (swap! state assoc :svms svms)
    (when-not (:field @state)
      (let [{field :field
             meshes :meshes} (points/prepare-field)]
        (doseq [field-mesh meshes]
          (.add scene field-mesh))
        (swap! state assoc :field field)))
    (points/update-field! (:field @state) svms classifier (:field-scale @state) scene)
    svms))

(defn- set-field-scale
  "Remove the old svms and add the new ones"
  [state scene scale]
  (swap! state assoc :field-scale scale)
  (points/update-field-scale! (:field @state) scale scene))


(defn- set-nodes-callback
  "Allow the user to replace nodes with a new set of nodes.
  MODIFIED: Maintain a clojurescript map to store the nodes internally.  Convert from js on input and to js on output."
  [state scene]
  (fn [nodes]
    (if (nil? nodes)
      (:nodes @state)
      (set-nodes state scene nodes))))

(defn- set-links-callback
  [state scene]
  (fn [links]
    (if (nil? links)
      (:links @state)
      (set-links state scene links))))

(defn- set-svms-callback
  [state scene force-worker]
  (fn [svms]
    (if (nil? svms)
       (:svms @state)
       (set-svms state scene force-worker svms))))

(defn- set-field-scale-callback
  [state scene]
  (fn [scale]
    (if (nil? scale)
       (:field-scale @state)
       (set-field-scale state scene scale))))



(defn- update-force-callback
  [state force-worker]
  (fn []
    (let [nodes (:nodes @state)
          links (:links @state)]
      (worker/send force-worker "set-nodes"
                   (mapv #(clj->js {:id (.-id %)
                                    :position (.-position %)})
                         nodes))
      (worker/send force-worker "set-links" links)
      ;;(worker/send force-worker "start")
      )))




(defn init-force
  [force-worker dev-mode first-run]
  (when (and (not first-run) dev-mode)
    (worker/send force-worker "tick")))


(defn create
  "Initialise a context in the specified element id"
  [user-map chan-out dev-mode]
  (let [{	first-run :first-run
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
         field-scale :field-scale
         state :state} (get-components user-map dev-mode)

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
    (swap! state assoc :field-scale field-scale)
    (swap! state assoc :select-circle select-circle)
    (swap! state assoc :meshes (array))


    ;; renderer
    (.setSize renderer width height)
    (.setClearColor renderer 0x404040)

    ;;shadows
    (when (:shadows (:webgl user-map))
      (set! (-> renderer .-shadowMap .-enabled) true)
      (set! (-> renderer .-shadowMap .-type) js/THREE.PCFSoftShadowMap))



    (worker/listen force-worker (fn [event]
                                  (let [message (.-data event)
                                        type (.-type message)
                                        data (.-data message)]
                                    (case type
                                      "ready" (do
                                                (init-force force-worker dev-mode first-run)
                                                (events/notify-user-ready chan-out))
                                      "end" (let [state @state]
                                               (events/notify-user-stable chan-out (:nodes state)))
                                      "nodes-positions" (let [state @state]
                                                          (points/update-positions! (:nodes state) data)
                                                          ;;(points/update nodeset)
                                                          (points/update-geometry (:links-set state))
                                                          )))))




    ;; if it's not the first time in dev mode
    (when (and (not first-run) dev-mode)
      (tools/fill-window! canvas)
      (.removeEventListener canvas "mousemove")
      (.removeEventListener canvas "click")
      (events/notify-user-ready chan-out))


    ;;(worker/send force-worker "precompute" 50)

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
     :tick (fn [] (worker/send force-worker "tick"))
     :canvas (.-domElement renderer)
     :stats stats
     :svms (set-svms-callback state scene force-worker)
     :fieldScale (set-field-scale-callback state scene)
     :nodes (set-nodes-callback state scene)
     :links (set-links-callback state scene)
     :updateForce (update-force-callback state force-worker)

     :force {:size           (fn [array] (worker/send force-worker "size" array))
             :linkStrength   (fn [val] (worker/send force-worker "linkStrength" (worker/serialize val)))
             :friction       (fn [val] (worker/send force-worker "friction" val))
             :linkDistance   (fn [val] (worker/send force-worker "linkDistance" (worker/serialize val)))
             :charge         (fn [val] (worker/send force-worker "charge" (worker/serialize val)))
             :gravity        (fn [val] (worker/send force-worker "gravity" val))
             :theta          (fn [val] (worker/send force-worker "theta" val))
             :alpha          (fn [val] (worker/send force-worker "alpha" val))}

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
                (worker/send force-worker "pin" {:id node-id}))
     :unpinNode (fn [node-id]
                  (let [node (node-id (:nodes @state))
                        mesh (:mesh node)]
                    (tools/remove-children mesh)
                    (.remove mesh (.-circle mesh))
                    (worker/send force-worker "unpin" {:id node-id})))
     :camera camera
     }))




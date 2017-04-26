(ns gravity.view.events-generator
  "Events listeners on the canvas, mouse, etc…"
  (:require
   [gravity.macros :refer-macros [log warn err]]
   [gravity.view.graph-tools :as tools]
   [gravity.force.proxy :as force :refer [send]]
   [cljs.core.async :refer [chan >! <! put!  sliding-buffer]])
  (:require-macros [cljs.core.async.macros :refer [go go-loop alt!]]))




(defn onWindowResize
  "Callback for the window-resize event"
  [canvas renderer camera]
  (fn []
    (.log js/console "on window resize")
    ;;(tools/fill-window! canvas)
     (let [width (.-width canvas)
           height (.-height canvas)]
       (set! (.-aspect camera) (/ width height))
       (.updateProjectionMatrix camera)
       (.setSize renderer width height))
     false))






(defn- get-target
  "Cast a ray to intersect objects under the mouse pointer.
  Return the first intersected or nil"
  [event canvas camera raycaster objects]
  (let [scene-width (.-width canvas)
        scene-height (.-height canvas)
        mouse-pos (new js/THREE.Vector3)
        bounding-rect (.getBoundingClientRect canvas)
        x (-> (.-clientX event)
              (- (.-left bounding-rect))
              (/  scene-width);;(.-offsetWidth canvas))
              (* 2)
              (- 1))
        y (-> (.-clientY event)
              (- (.-top bounding-rect))
              (-)
              (/ scene-height);;(.-offsetHeight canvas))
              (* 2)
              (+ 1))
        cam-position (.-position camera)]
    (.set mouse-pos x y 1)
    (.unproject mouse-pos camera)
    (.set raycaster cam-position (.normalize (.sub mouse-pos cam-position)))
    ;;return
    ;;(.log js/console "Get target x: " x " y " y)
    {:target (when-not (or (nil? objects) (empty? objects))
               (first (.intersectObjects raycaster objects)))
     :in-bounds? (and (> x -1) (< x 1)
                      (> y -1) (< y 1))}
    ))






(defn- move
  "Callback for the mouseMove event on the canvas node"
  [event canvas camera raycaster state chan events-state controls intersect-plane]
  (let [colliders (:meshes @state)
        last-state (:last @events-state)
        {target :target
         in-bounds? :in-bounds?} (get-target event canvas camera raycaster colliders)]
    ;(.log js/console "Move target " target " in-bounds? " in-bounds?)
    (if-not in-bounds?
      (when-not (= :out-of-bounds last-state) 
        (set! (-> controls .-enabled) false)
        (swap! events-state assoc :last :out-of-bounds)
        (.log js/console "OUT of bounds!")
        )
      ;; else (when in-bounds)
      (do
        ;;(.log js/console "last state: " last-state)
         (when (= last-state :out-of-bounds)
           (.log js/console "IN bounds!"))
        (if-not (nil? target)
          (let [node (.-node (.-object target))]
            ;; disable controls
            (set! (-> controls .-enabled) false)
            ;; move plane
            (.copy (-> intersect-plane .-position) (-> node .-position))
            (.lookAt intersect-plane (-> camera .-position))
            ;; send event to the user
            (when-not (= :node-over last-state)
              (swap! events-state assoc :last :node-over)
              (go
                (>! chan {:type :node-over
                          :target node
                          :original-event event}))))
          ;; else (not over anything)
          (when (contains? #{:node-over :drag :up :out-of-bounds} last-state)
            (set! (-> controls .-enabled) true)
            (swap! events-state assoc :last :blur)
            (go (>! chan {:type :node-blur
                          :original-event event}))))))))


(defn- click
  "click event"
  [event canvas camera raycaster state chan]
  (log "click")
  (let [colliders (:meshes @state)
        {target :target
         in-bounds? :in-bounds} (get-target event canvas camera raycaster colliders)]
    (when (and in-bounds? target)
      (let [node (-> target .-object .-node)]
        (go (>! chan {:type :node-click
                      :target node
											:original-event event}))))
    false))


(defn double-click
  "Callback for the click event"
  [event canvas camera raycaster state chan]

  (let [colliders (:meshes @state)
        {target :target
         in-bounds? :in-bounds} (get-target event canvas camera raycaster colliders)]
    (when-not (nil? target)
      (let [node (.-node (.-object target))]
        (go (>! chan {:type :node-dbl-click
                      :target node
											:original-event event})))))
  false)



e

(defn- drag
  [event canvas camera raycaster events-state intersect-plane force-worker chan-out]
  "MODIFIED: now done by id rather than index"
  (let [node (:target @events-state)]
    (when-not (nil? node)
      (let [node (-> node .-object)
            id (-> node .-node :id)
            {intersect :target
             in-bounds? :in-bounds} (get-target event canvas camera raycaster (array intersect-plane))]
        (when-not (nil? intersect)
          ;;(.copy (-> node .-position) (-> intersect .-point))
          (force/send force-worker :set-position (clj->js {:id id
                                                           :position (-> intersect .-point)}))
          (when (= :down (:last @events-state))
            (force/send force-worker "stop")
            (go (>! chan-out {:type :drag-start
                              :target node
															:original-event event})))
          (swap! events-state assoc :last :drag)
          )))))




(defn- down
  [event canvas camera raycaster state events-state force-worker]
  (let [colliders (:meshes @state)
        {target :target
         in-bounds? :in-bounds} (get-target event canvas camera raycaster colliders)]
    (when-not (nil? target)
      (force/send force-worker "stop")
      (swap! events-state assoc :last :down)
      (swap! events-state assoc :target target))))


(defn- up
  [event events-state force-worker chan-out]
  (when (= :drag (:last @events-state))
    (let [target (:target @events-state)]
      (log "drag-end-before")
      (when-not (nil? target)
        (log "drag-end-with-target")
        (go (>! chan-out {:type :drag-end
                          :target (-> target .-object .-node)
													:original-event event})))))
  (swap! events-state assoc :last :up)
  (swap! events-state dissoc :target))




(defn notify-user-ready
  [chan]
  (go (>! chan {:type :ready})))

(defn notify-user-stable
  [chan nodes]
  (go (>! chan {:type :stable :target nodes})))





;; Events factory



(defn- listen-to-mouse-events
  "Take chans with events from the dom and alt! them to generate meaningful events."
  [mouse-down mouse-up mouse-move]
  (let [timeout-time 350
        out-chan (chan 10)
        events-state (atom {})]
    (go-loop []
             (loop []
               (alt! [mouse-down] ([transducted] (do
                                                   (swap! events-state assoc :event :down)
                                                   (go (>! out-chan (merge {:type :down} transducted)))

                                                   (js/setTimeout #(swap! events-state assoc :event nil) timeout-time)
                                                   nil))

                     ;; We stay in this loop while the mouse move without mousedown
                     [mouse-move] ([transducted] (do
                                                   ;;(.log js/console "mousemove detected")
                                                   (go (>! out-chan (merge {:type :move} transducted)))
                                                   (recur)))))
             (loop [nb-drags 0]
               (alt! [mouse-up] ([transducted] (do
                                                 (go (>! out-chan (merge {:type :up} transducted)))

                                                 (when (= :down (:event @events-state))
                                                   ;; the last event was a :down -> we trigger a click
                                                   ;; if we already had a click before it's a double-click
                                                   (if (and
                                                        (:last-was-a-click @events-state)
                                                        (= (:coords transducted) (:last-coords @events-state)))
                                                     (go
                                                      (swap! events-state assoc :last-was-a-click false)
                                                      (>! out-chan (merge {:type :double-click} transducted)))
                                                     ;else it's a simple click
                                                     (go (swap! events-state assoc :last-was-a-click true)
                                                         (>! out-chan (merge {:type :click} transducted))
                                                         (js/setTimeout #(swap! events-state assoc :last-was-a-click false) timeout-time))))

                                                 (swap! events-state assoc :event :up)
                                                 (swap! events-state assoc :last-coords (:coords transducted))

                                                 nil))
                     [mouse-move] ([transducted]
                                   (do
                                     (when (> nb-drags 3)
                                       (swap! events-state assoc :last-was-a-click false)
                                       (swap! events-state assoc :event :drag)
                                       (swap! events-state assoc :last-coords (:coords transducted))
                                       (go (>! out-chan (merge {:type :drag} transducted))))
                                     (recur (inc nb-drags))))))
             (recur))
    out-chan
    ))


(defn listen-to-canvas
  "Listen to a canvas and return a chan of events."
  [canvas]
  (let [transduct-mouse (map (fn [e]
                                ;;(.preventDefault e)
                                ;;(.stopPropagation e)
                                {:event e
                                 :coords {:x (.-offsetX e)
                                          :y (.-offsetY e)}}))
        mousedown-chan (chan (sliding-buffer 1) transduct-mouse)
        mouseup-chan (chan (sliding-buffer 1) transduct-mouse)
        mousemove-chan (chan (sliding-buffer 1) transduct-mouse)]

    (.addEventListener canvas "mousedown" (fn [e] (put! mousedown-chan e) false))
    (.addEventListener canvas "mousemove" (fn [e] (put! mousemove-chan e) false))
    (.addEventListener canvas "mouseup"   (fn [e] (put! mouseup-chan e) false))

    (listen-to-mouse-events mousedown-chan mouseup-chan mousemove-chan)))




(defn apply-events-to
  [mouse canvas camera raycaster intersect-plane controls state force-worker chan-out]
  (let [events-state (atom {})]
    (go-loop []
             (let [event (<! mouse)
                   {type :type
                    coords :coords
                    event :event} event]
               (case type
                 :move (do (move event canvas camera raycaster state chan-out events-state controls intersect-plane))
                 :down (do (down event canvas camera raycaster state events-state force-worker))
                 :up (do (up event events-state force-worker chan-out))
                 :click (do (click event canvas camera raycaster state chan-out))
                 :double-click (do (double-click event canvas camera raycaster state chan-out))
                 :drag (do (drag event canvas camera raycaster events-state intersect-plane force-worker chan-out))
                 ))
             (recur)))
  nil)

;; State watch



(defn put-select-circle-on-node
  [old-state new-state]
  (let [circle (:select-circle new-state)
        old-node (:selected old-state)
        new-node (:selected new-state)]
    (when-not (nil? old-node)
      (set! (-> old-node .-selected) false)
      (.remove (-> old-node .-mesh) circle)
			(set! (-> circle .-visible) true))
    (when-not (nil? new-node)
      (set! (-> new-node .-selected) true)
      (.add (-> new-node .-mesh) circle)
			(set! (-> circle .-visible) true))))


(defn watch-state
  [state watch-id]
  (add-watch state watch-id
             (fn [id state old-state new-state]
                (put-select-circle-on-node old-state new-state)
                )))

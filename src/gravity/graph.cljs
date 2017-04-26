(ns ^:figwheel-always gravity.graph
  (:require [gravity.view.graph :as graph]
            [gravity.view.graph-tools :as tools]
            [gravity.events :as events]
            [gravity.force.proxy :as force-worker]
            [gravity.force.worker :as force-webworker]
            [gravity.field.proxy :as field-worker]
            [gravity.field.worker :as field-webworker]
            [gravity.macros :refer-macros [log]]))

(enable-console-print!)

(defonce app-state (atom {}))


(def default-parameters {:color (.category10 js/d3.scale)
                         :force-worker-path "./force-worker.js"
                         :field-worker-path "./field-worker.js"
                         :stats false
                         :force {:size [1 1 1]
                                 :linkStrength 1
                                 :friction 0.9
                                 :linkDistance 20
                                 :charge 0
                                 :gravity 0.1
                                 :theta 0.8
                                 :alpha 0.1}
                         :field {:scale 0
                                 :base-density 10
                                 :radius-ratio 2
                                 :kernel "linear"}
                         :webgl {:antialias true
                                 :background false
                                 :lights true
                                 :shadows true}})



(defn bind-dev-events
  [graph]

  (let [{on :on
         canvas :canvas} graph]
    (on "node-over" (fn [node]
                       (log :over)
                       (set! (-> canvas .-style .-cursor) "pointer")))
    (on "node-blur" (fn []
                       (log :blur)
                       (set! (-> canvas .-style .-cursor) "inherit")))
    (on "node-select" (fn [node]
                         (log :void)
                         (log [:select (.-name node) node])))
    (on "void-click" (fn []
                        (log [:void])))
    (on "node-click" (fn [node]
                        (log :node-click)
                        (let [select (:selectNode graph)]
                          (select node))))
    (on "node-dbl-click" (fn [node]
                            (log :dbl-click)
                            (let [unpin (:unpinNode graph)
                                  resume (:resume graph)]
                              (unpin node)
                              (resume))))
    (on "drag-start" (fn [node]
                        (log :drag-start)))
    (on "drag-end" (fn [node]
                      (log :drag-end)
                      (log node)
                      (let [pin (:pinNode graph)
                            resume (:resume graph)]
                        (pin node)
                        (resume))))
    (on "ready" (fn []
                   (let [set-nodes (:nodes graph)
                         set-links (:links graph)
                         update-force (:updateForce graph)

                         nodes (clj->js {"foo" {:name "foo" :group 0} "bar" {:name "bar" :group 1}})
                         links (clj->js [{:source "foo" :target "foo"}])]
                     (set-nodes nodes)
                     (set-links links)
                     (update-force)
                     )))))


(defn unbind-old-events
  [last-instance]
  (let [off (-> last-instance .-off)]
    (when-not (nil? off)
      (off))))



(defn init-parameters
  [user-map]
  (let [user-map (js->clj user-map :keywordize-keys true)
        webgl-params (:webgl user-map)
        force-params (:force user-map)
        color (if (:color user-map)
                (:color user-map)
                (:color default-parameters))
        force-merged (merge (:force default-parameters) force-params)
        webgl-merged (merge (:webgl default-parameters) webgl-params)]
    {:color color
     :force force-merged
     :webgl webgl-merged}))


(defn- main
  
  ([user-map]
   (let [graph (main user-map false)]
     (clj->js graph)))

  ([user-map dev-mode]
   (let [chan-out (events/create-chan)
         store (events/create-store)
         graph (graph/create user-map chan-out dev-mode)  ;; <--
         graph (-> graph
                   (merge store)
                   (dissoc :get-callbacks))]
     (events/listen-outgoing-events chan-out store)
     (bind-dev-events graph)
     graph)))




(defn on-js-reload
  ([]
   (let [state @app-state
         last-instance (:last-instance state)]

     (when-not (or (nil? last-instance) (empty? last-instance))
       (unbind-old-events last-instance)
       (apply (:stop last-instance) []))


     (let [graph (main state true)]
       (swap! app-state assoc-in [:last-instance] graph)
       (swap! app-state assoc :first-run false)

       graph))))




(defn ^:export init-dev-mode
  "Set some params to use live-reload in dev mode"
  [user-map]
  (let [user-map (js->clj user-map :keywordize-keys true)
        dev-app-state {:stats (tools/make-stats)
                       :last-instance {}
                       :first-run true}
        params (init-parameters user-map)
        state (merge dev-app-state user-map params)]
    (reset! app-state state)
    (swap! app-state assoc :force-worker (force-worker/create "force-worker/worker.js" (:force state)))
    (swap! app-state assoc :field-worker (field-worker/create "field-worker/worker.js" (:field state)))
    (clj->js
     (on-js-reload))))



(defn ^:export create
	[user-map]
	(let [user-map (js->clj user-map :keywordize-keys true)
				params (init-parameters user-map)
				state (if (:stats user-map)
                                        (merge user-map params {:stats (tools/make-stats)})
                                        (merge user-map params))

				graph (main state false)]
		(clj->js graph)))

(def ^:export create-force-worker gravity.force.worker/create)
(def ^:export create-field-worker gravity.field.worker/create)

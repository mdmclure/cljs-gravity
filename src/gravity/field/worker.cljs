;; (when-not (undefined? js/self.importScripts)
;; 	(.importScripts js/self "../libs/d3.js" "../libs/d3.layout.force3d.js"))

;;---------------------------------

(ns gravity.field.worker
  (:refer-clojure :exclude [str])
  (:import [goog.object])
  (:require [clojure.set :as set]
            [clairvoyant.core :as trace :include-macros true]
            [gravity.tools :refer [map-map jsarray-to-map]]
            [clojure.set :refer [map-invert]]
            [gravity.macros :refer-macros [log-field warn-field]]
            [gravity.view.field :refer [get-unique-unit-sphere-coordinates
                                        coord-x coord-y coord-z field-density]]))

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

(defn str
  [& args]
  (let [arr (clj->js args)]
    (.join arr "")))

(defn eval
  [value]
  (js/eval value))


(def fields (atom nil)) ;map of field id (same as ccid) to field object
(def parameters (atom nil)) ;svm parameters
(def training-labels (atom nil)) ;map of node id to its training label, if any
(def testing-ids (atom nil)) ;hash set of node ids in the test set
(def node->field (atom nil)) ;map of node id to field id (matches ccid)

;; --------------------------------

(defn init
  [params]
  (let [params (js->clj params :keywordize-keys true)]
    (reset! parameters params)
    (answer {:type :ready})
    nil))

(defn set-field-nodes [node-to-field-map]
  (reset! node->field node-to-field-map))

(defn set-training-labels [labels]
  (reset! training-labels labels))

(defn compute-span [x y z coords]
  (.sqrt js/Math
         (apply max (map (fn [c]
                           (+ (.pow js/Math (- (coord-x c) x) 2)
                              (.pow js/Math (- (coord-y c) y) 2)
                              (.pow js/Math (- (coord-z c) z) 2)))
                         coords))))

(defn backfill
  "Put a positive label (1) at the current index and back-fill
  negative labels (-1) from the last filled index up to the
  current index"
  [v index elem]
  (let [num-to-backfill (- (inc index) (count v))
        backfill (take num-to-backfill (repeat elem))]
    (into v backfill)))

(defn backfill-binary-classifier-label-data
  [fields training-cts]
  (let [new-fields (map-map (fn [fid f]
                              (let [training-ct (get @training-cts fid)]
                                (update f :binary-classifiers
                                        (fn [bcs]
                                          (map-map (fn [l bc]
                                                     (update bc :label-data
                                                             (fn [ldata]
                                                               (backfill ldata (dec training-ct) -1))))
                                                   bcs)))))
                            @fields)]
    (reset! fields new-fields)))

(defn add-to-training
  [fields field-id node-id datum i label]
  (let [new-fields (-> @fields
                       ;;Add a positive label to the training data for this label's svm, and while at it, backfill negative labels since the last positive
                       (update-in [field-id :binary-classifiers label :label-data]
                                  (fn [ldata]
                                    (if label
                                      (conj (backfill (or ldata []) (dec i) -1) 1)
                                      ldata)))
                       ;;Add the point to the training data
                       (update-in [field-id :training-data]
                                  (fn [data]
                                    (conj (or data []) datum))))]
    (reset! fields new-fields)))


(defn add-to-testing
  "Adds a testing example to the field. datum is a feature vector"
  [fields field-id node-id datum]
  (swap! fields
         #(-> %
              ;;Keep a record of the node-id order in this field's testing-data for returning predictions
              (update-in [field-id :testing-ids]
                         (fn [ids]
                           (conj ids node-id)))
              ;;Add the point to the testing data
              (update-in [field-id :testing-data]
                         (fn [data]
                           (conj data datum))))))

(defn build-field-coordinates
  "Build the field coordinates"
  [fields radius-ratio base-density]
  (let [new-fields (map-map (fn [fid f]
                             (let [num-training (count (:training-data f))
                                   sphere (get-unique-unit-sphere-coordinates
                                           (field-density base-density num-training))
                                   span (:span-radius f)
                                   coeff (* span radius-ratio)
                                   cent (:centroid f)]
                               (assoc f :coordinates
                                      (mapv (fn [coord]
                                              (let [x (+ (* coeff (coord-x coord)) (coord-x cent))
                                                    y (+ (* coeff (coord-y coord)) (coord-y cent))
                                                    z (+ (* coeff (coord-z coord)) (coord-z cent))]
                                                [x y z]))
                                            sphere))))
                            @fields)]
    (reset! fields new-fields)))


(defn train-fields
  "Train the fields on their respective training data"
  [fields kernel]
  (let [new-fields (map-map (fn [fid f]
                              (let [data (:training-data f)]
                                (update f :binary-classifiers
                                        (fn [bcs]
                                          (if (< (count bcs) 2) bcs
                                              (map-map (fn [l bc]
                                                         (let [svm (new svmjs.SVM)
                                                               ldata (:label-data bc)]
                                        ;(log-field "TRAINING " svm (clj->js data) l (clj->js ldata))
                                                           (.train svm (clj->js data) (clj->js ldata) (clj->js {:kernel kernel}))
                                        ;(log-field "TRAINED " svm (clj->js data) l (clj->js ldata))
                                                           (assoc bc :svm svm)))
                                                       bcs))))))
                            @fields)]
    (reset! fields new-fields)))

;;Given a coordinate (datum) and a map of labels to binary classifiers,
;;Gives back a map of labels to the margins predicted by their corresponding binary classifiers
(defn margin-map [coord binary-classifier-map]
  (when (> (count binary-classifier-map) 1)
    (map-map (fn [label bc]
               (when-let [svm (:svm bc)]
                 (.marginOne svm (clj->js coord))))
             binary-classifier-map)))

(defn predict-field-coordinates-3D
  "Make predictions on the field coordinates. Put answers in a Float32Array since it is Transferable by the web worker."
  [fields]
  (let [coordinates (mapv (fn [[fid f]] (count (:coordinates f))) @fields)
        total-coordinates (reduce + coordinates)
        total-fields (count @fields)
        size (+ total-fields (* total-coordinates 5))
        arr (new js/Float32Array size)
        buffer (.-buffer arr)
        offset (atom 0)]
    ;(log-field "predict field coordinates " size coordinates total-coordinates @fields)
    (doseq [[fid f] @fields]
      (let [start @offset
            coord-ct (count (:coordinates f))
            data-length (* 5 coord-ct)
            classifiers (:binary-classifiers f)]
        (aset arr start fid) ;;Each field's data begins with the field id.
        ;;Note the UI thread knows how many coordinates to expect in each field
        ;(log-field "FIELD" fid "Sending coords for" coord-ct "meshes at" start)
        (if (= (count classifiers) 0)
          (swap! offset inc)
          (loop [i 0]
            (let [j (+ start 1 (* i 5))
                  c (get (:coordinates f) i)
                  margin-map (margin-map c classifiers)
                  max-margin (and margin-map (apply max (vals margin-map)))
                  max-margin-label (if margin-map
                                     (get (map-invert margin-map) max-margin)
                                     (first (keys classifiers)))
                  margin-diffs (and margin-map
                                    (map-map (fn [l margin]
                                               (- max-margin margin))
                                             margin-map))
                  ;;The difference between the max margin and the next best is treated as the n-ary margin
                  top-margin-diff (if margin-map
                                    (apply min (vec (remove #(= % 0) (vals margin-diffs))))
                                    1)]
              ;;3 spots for coordinates
              (aset arr j (coord-x c))
              (aset arr (+ j 1) (coord-y c))
              (aset arr (+ j 2) (coord-z c))
              ;;Then the predicted label
              (aset arr (+ j 3) max-margin-label)
              ;;Then the margin
              (aset arr (+ j 4) top-margin-diff)
              (if (< i (dec coord-ct))
                (recur (inc i))
                (reset! offset (+ j 5))))))))
    ;(log-field "sending 3D field predictions " arr)
    (answer {:type "field-coordinate-predictions-3D" :data arr} [buffer])))
    

(defn predict-field-test-data
  "Make predictions on the test points. This should be agnostic to dimensionality of data"
  [fields]
  
  )



;;NOTE: assumes positions contains the positions for all nodes!
;;positions is a Float32Array of [node-id0, x0, y0, z0, node-id1, <...>, zn]
;;because it's the raw position data from the force-worker (for efficiency).
;;In the non-3d (structural svm) case, position data and training data will
;;be orthogonal, and as fields dont make any sense in that case, the field
;;worker will only be responsible for SVMs
(defn set-training-data-3D [positions]
  (when (= 0 (mod (.-length positions) 4))
    (let [size (/ (.-length positions) 4)
          training-cts (atom nil)
          node-cts (atom nil)
          node-coords (atom nil)
          x-sums (atom nil)
          y-sums (atom nil)
          z-sums (atom nil)]
      (reset! fields nil)
      (loop [i 0]
        (let [j (* i 4)
              id (aget positions j)]
          ;(log-field "node-id:" id)
          (when-let [field-id (get @node->field id)]
            (let [x (aget positions (+ j 1))
                  y (aget positions (+ j 2))
                  z (aget positions (+ j 3))]
              ;;For computing centroids, keep the running totals up to date
              (swap! node-cts update field-id #(if % (inc %) 1))
              (swap! x-sums update field-id #(if % (+ % x) x))
              (swap! y-sums update field-id #(if % (+ % y) y))
              (swap! z-sums update field-id #(if % (+ % z) z))
              (swap! node-coords update field-id (fn [data]
                                                   (conj data [x y z])))
              ;;If it is a training example
              (when (find @training-labels id)  ;;find because label could be nil
                (let [training-ct (or (get @training-cts field-id) 0)
                      training-label (get @training-labels id)]
                  ;;add it to the training data
                  (add-to-training fields field-id id [x y z] training-ct training-label)
                  (swap! training-cts assoc field-id (inc training-ct))))
              ;;If it is a testing example
              (when (contains? @testing-ids id)
                (add-to-testing fields field-id id [x y z])))))
        (when (< i (- size 1))
          (recur (inc i))))
      (backfill-binary-classifier-label-data fields training-cts)
      ;;Now compute (crude) centroids by averaging the coordinates, and compute spans using the centroids
      (let [w-centroids (map-map (fn [fid fobj]
                                   (let [ct (get @node-cts fid)
                                         x (/ (get @x-sums fid) ct)
                                         y (/ (get @y-sums fid) ct)
                                         z (/ (get @z-sums fid) ct)]
                                     (-> fobj
                                         (assoc :centroid [x y z])
                                         (assoc :span-radius (max 1 (compute-span x y z (get @node-coords fid)))))))
                                 @fields)]
        (reset! fields w-centroids))
      (build-field-coordinates fields (:radius-ratio @parameters) (:base-density @parameters))
      (train-fields fields (:kernel @parameters))
      ;(log-field "Fields after training: " @fields)
      (predict-field-coordinates-3D fields)
      (predict-field-test-data fields))))


(defn dispatcher
  "Dispatch a message to the corresponding action (route)."
  [event]
  (let [message (.-data event)
        type (.-type message)
        data (.-data message)]
    (case type
      "init" (init data)
      "set-node-fields" (do ;(log-field "Setting fields " data)
                            (set-field-nodes (jsarray-to-map data)))
      "set-training-labels" (do ;(log-field "Setting training labels " data)
                                (set-training-labels (jsarray-to-map data)))
      "set-training-data-3D" (do ;(log-field "Setting training data " data)
                                 (set-training-data-3D data))

      ;set params
      ;; "base-density" (do (log "Setting base-density " data)
      ;;                    (swap! parameters assoc :base-density data))
      ;; "radius-ratio" (do (log "Setting radius-ratio " data)
      ;;                    (swap! parameters assoc :radius-ratio data))
      ;; "kernel" (do (log "Setting kernel " data)
      ;;              (swap! parameters assoc :kernel data))

      (warn-field (str "Unable to dispatch '" type "'")))))



(defn ^:export create
  "Main entry point"
  []
  (.addEventListener js/self "message" dispatcher))



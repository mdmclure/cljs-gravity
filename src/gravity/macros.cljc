(ns gravity.macros)

(defmacro log
  "Log in the console"
  [& args]
  (let [js-args (map (fn [arg] `(cljs.core/clj->js ~arg)) args)]
    `(.log js/console "[UI]:" ~@js-args)))
  
(defmacro warn
  "Log in the console"
  [& args]
  (let [js-args (map (fn [arg] `(cljs.core/clj->js ~arg)) args)]
    `(.warn js/console "[UI]:" ~@js-args)))

(defmacro err
  "Log in the console"
  [& args]
  (let [js-args (map (fn [arg] `(cljs.core/clj->js ~arg)) args)]
    `(.error js/console "[UI]:" ~@js-args)))

(defmacro log-force
  "Log in the console"
  [& args]
  (let [js-args (map (fn [arg] `(cljs.core/clj->js ~arg)) args)]
    `(.log js/console "[force-worker]:" ~@js-args)))
  
(defmacro warn-force
  "Log in the console"
  [& args]
  (let [js-args (map (fn [arg] `(cljs.core/clj->js ~arg)) args)]
    `(.warn js/console "[force-worker]:" ~@js-args)))

(defmacro log-field
  "Log in the console"
  [& args]
  (let [js-args (map (fn [arg] `(cljs.core/clj->js ~arg)) args)]
    `(.log js/console "[field-worker]:" ~@js-args)))
  
(defmacro warn-field
  "Log in the console"
  [& args]
  (let [js-args (map (fn [arg] `(cljs.core/clj->js ~arg)) args)]
    `(.warn js/console "[field-worker]:" ~@js-args)))



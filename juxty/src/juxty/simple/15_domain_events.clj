(ns juxty.simple.15-domain-events)

;; Domain Event
(def events (atom []))

(defn event-handler
  [b event]
  (let [delta (:delta event)]
    (swap! b update :position (partial + delta))
    :success))

(defn left! [b]
  (if (> (:position @b) -2)
    (->> {:type :movement
          :delta -1}
         (event-handler b))
    :failure))

(defn right!
  ([b]
   (right! b 0))
  ([b wait]
   (if (< (:position @b) 2)
     (->> {:type :movement
           :delta 1}
          ((fn [x] (Thread/sleep wait) x))
          (event-handler b))
     :failure)))

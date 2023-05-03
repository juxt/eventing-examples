(ns juxty.simple.20-event-sourcing)

;; Event Sourcing
(def events (atom []))

(defn to-events
  [event]
  (swap! events conj event)
  event)

(defn event-handler
  [b event]
  (let [delta (:delta event)]
    (swap! b update :position (partial + delta))
    :success))

(defn left! [b]
  (if (> (:position @b) -2)
    (->> {:type :movement
          :delta -1}
         to-events
         (event-handler b))
    :failure))

(defn right!
  ([b]
   (right! b 0))
  ([b wait]
   (if (< (:position @b) 2)
     (->> {:type :movement
           :delta 1}
          to-events
          ((fn [x] (Thread/sleep wait) x))
          (event-handler b))
     :failure)))

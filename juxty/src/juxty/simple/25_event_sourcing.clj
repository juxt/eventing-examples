(ns juxty.simple.25-event-sourcing)

;; Event Sourcing
(def events (ref []))

(defn to-events
  [event]
  (alter events conj event)
  event)

(defn event-handler
  [b event]
  (let [delta (:delta event)]
    (alter b update :position (partial + delta))
    :success))

;; the modification to the aggregate and the write of the domain event happens transactionally
;; if the condition for a command is met.  
(defn left! [b]
  (dosync (if (> (:position @b) -2)
            (->> {:type :movement
                  :delta -1}
                 to-events
                 (event-handler b))
            :failure)))

(defn right!
  ([b]
   (right! b 0))
  ([b wait]
   (dosync (if (< (:position @b) 2)
             (->> {:type :movement
                   :delta 1}
                  to-events
                  ((fn [x] (Thread/sleep wait) x))
                  (event-handler b))
      :failure))))


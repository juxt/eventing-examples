(ns juxty.simple.50-observability
  (:require [juxty.simple.20-event-sourcing :refer [to-events event-handler]]
            [juxty.simple.30-cmd-sourcing :refer [to-cmds]]
            [juxty.simple.40-external-side-effects :refer [external-true-or-false]]))

;; Here is a list of common fields found in a typical event
;; - AggregateId – This field is used to associate the particular event to a specific aggregate root.
;; - Date Time Stamp – Ordering of events is crucial. Replaying events in the wrong order can result is unpredictable outcomes.
;; - UserId – This field is commonly required in a line of business applications and can be used to build audit logs. It is a common field, but not always necessary and depends on the specific domain.
;; - Version – The version number allows the developer to handle concurrency conflicts and partial connection scenarios. For more information, take a look at Handling Concurrency Issues in a CQRS Event Sourced system
;; - ProcessId – At its simplest, this field can be used to tie a series of events back to their originating command. However, it can also be used to ensure the idempotence* of the event.

(defn cmd-handler
  [b cmd]
  (case (:action cmd)
    :move-left
    (if (and (external-true-or-false)
             (> (:position @b) -2))
      (->> {:id (random-uuid) ;; id of the event itself
            :created-at (System/currentTimeMillis) ;; explicity differentiated
            :process-id (:id cmd) ;; audit trail/idempotence
            :aggregate-id (:id @b) ;; what is being changed
            :version nil ;; concurrency control when needed 
            :type :movement
            :delta -1}
           to-events ;; time at which persisted
           (event-handler b) ;; time at which the event was noticed
           )
      :failure)
    :move-right
    (if (and (external-true-or-false)
             (< (:position @b) 2))
      (->> {:id (random-uuid)
            :created-at (System/currentTimeMillis)
            :process-id (:id cmd)
            :aggregate-id (:id @b)
            :version nil
            :type :movement
            :delta 1}
           to-events
           (event-handler b))
      :failure)))

(defn left! [b & overrides]
  (->> overrides
       (apply hash-map)
       (merge {:id (random-uuid)
               :created-at (System/currentTimeMillis)
               :aggregate-id (:id @b)
               :action :move-left})
       to-cmds
       (cmd-handler b)))

(defn right! [b & overrides]
  (->> overrides
       (apply hash-map)
       (merge {:id (random-uuid)
               :created-at (System/currentTimeMillis)
               :aggregate-id (:id @b)
               :action :move-right})
       to-cmds
       (cmd-handler b)))

(defn hydrate
  ([b events]
   (swap! b assoc :position 0)
   (run! (fn [e] (event-handler b e)) events))
  ([b events n]
   (hydrate b (take n events))))

(defn position-at-time
  [events time]
  (reduce + 
          (map :delta
               (take-while (fn [e] (< (:created-at e) time)) events))))


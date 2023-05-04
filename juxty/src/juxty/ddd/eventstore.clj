(ns juxty.ddd.eventstore
  (:require [juxt.trip.core :as trip]
            [clojure.test :refer [deftest is testing]]))

(def eventstore (trip/create-conn))
(def tx-id (atom 0))

(defn reset-eventstore! []
  (reset! eventstore (trip/empty-db))
  (reset! tx-id 0))

(defn save-to-eventstore! 
  "Takes an event and into it
     - injects a db/id uuid
     - injects a db/type of event
   Creates an associated metadata document with
     - a monotonically increasing db/id
     - a db/type of tx-id
     - a reference back to the associated event-id
   Adds both as part of the same transaction
   This allows us to query for an aggregate (DDD) and return its events 
   in order, see `get-events-for-aggregate`"
  [event]
  (let [event-id (random-uuid)
        aggregate-id (:aggregate-id event)]
    (trip/transact! eventstore [(assoc event
                                       :db/id event-id
                                       :db/type :event)
                                {:db/id (swap! tx-id inc)
                                 :db/type :tx-id
                                 :aggregate-id aggregate-id
                                 :event-id event-id}])))

(defn get-events-for-aggregate
  [aggregate-id]
  (let [db (trip/db eventstore)]
    (->> (trip/q '{:find [?t ?e]
                   :in [$ ?aggregate-id]
                   :where [[?e :aggregate-id ?aggregate-id]
                           [?t :db/type :tx-id]
                           [?t :event-id ?e]]}
                 db
                 aggregate-id)
         (into [])
         (sort-by first)
         (map second)
         (trip/pull-many db '[*])
         (map #(dissoc % :db/id :db/type))
         (into []))))

(deftest eventstore-test
  (let [aggregate-id-a (random-uuid)
        event-a1 {:aggregate-id aggregate-id-a
                  :created-at 10000
                  :type :bot-created}
        event-a2 {:aggregate-id aggregate-id-a
                  :created-at 10010
                  :type :nickname-assigned
                  :nickname "Tripy"}]
    (testing "First one ordering"
      (reset-eventstore!)  
      (save-to-eventstore! event-a1)
      (save-to-eventstore! event-a2)
      (is (= [event-a1 event-a2]
             (get-events-for-aggregate aggregate-id-a))))
    (testing "Then the other"
      (reset-eventstore!)  
      (save-to-eventstore! event-a2)
      (save-to-eventstore! event-a1)
      (is (= [event-a2 event-a1]
             (get-events-for-aggregate aggregate-id-a))))
    (testing "More than one aggregate"
      (let [aggregate-id-b (random-uuid)
            event-b1 {:aggregate-id aggregate-id-b
                      :created-at 20000
                      :type :bot-created}
            event-b2 {:aggregate-id aggregate-id-b
                      :created-at 20010
                      :type :nickname-assigned
                      :nickname "XTDBY"}]
        (reset-eventstore!)  
        (save-to-eventstore! event-a1)
        (save-to-eventstore! event-a2)
        (save-to-eventstore! event-b1)
        (save-to-eventstore! event-b2)
        (is (= [event-a1 event-a2]
               (get-events-for-aggregate aggregate-id-a)))
        (is (= [event-b1 event-b2]
               (get-events-for-aggregate aggregate-id-b)))))))

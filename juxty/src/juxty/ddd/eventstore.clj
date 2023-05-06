(ns juxty.ddd.eventstore
  (:require [juxt.trip.core :as trip]
            [clojure.test :refer [deftest is testing]]))

(def eventstore (ref (trip/empty-db)))
(def tx-id (ref 0))

(extend-type clojure.lang.Ref
  trip/DbConnection
  (-db [this] @this)

  (-commit [this tx-report]
    (ref-set this (:db-after tx-report))))

(defn reset-eventstore! []
  (dosync
   (ref-set eventstore (trip/empty-db))
   (ref-set tx-id 0)))

(def publish! (constantly :success)) ;; placeholder

(defn save-to-eventstore!
  "Takes a collection of events, for a single aggregate, and into each event
     - inject a db/id uuid
     - inject a db/type of :event (not used)
   Creates an associated metadata document with
     - a monotonically increasing db/id
     - a db/type of :tx-id
     - an ordered vector of references to the associated event-ids
   Adds both as part of the same transaction.
   Finally publishes to an event bus for further processing by subscribers.
   Does this within a clojure transaction as it's important that store and
   the eventbus are consistent.
   This allows us to query for an aggregate (DDD) and return its events 
   in order, see `get-events-for-aggregate`"
  [events]
  (when (every? #(= (:aggregate-id (first events))
                    (:aggregate-id %))
                events)
    (dosync
     (let [events (mapv #(assoc %
                                :db/id (random-uuid)
                                :db/type :event)
                        events)
           tx-metadata {:db/id (alter tx-id inc)
                        :db/type :tx-id
                        :aggregate-id (:aggregate-id (first events))
                        :event-ids (mapv :db/id events)}]
       (publish! :some-topic events) ;; placeholder
       (trip/transact! eventstore (conj events tx-metadata))))))

(defn get-events-for-aggregate
  [aggregate-id]
  (let [db (trip/db eventstore)]
    (->> (trip/q '{:find [?t ?i]
                   :in [$ ?aggregate-id]
                   :where [[?t :aggregate-id ?aggregate-id]
                           [?t :db/type :tx-id]
                           [?t :event-ids ?i]]}
                 db
                 aggregate-id)
         (sort-by first)
         (map second)
         (reduce concat)
         (trip/pull-many db '[*])
         (mapv #(dissoc % :db/id :db/type)))))

(deftest eventstore-test
  (let [aggregate-id-a (random-uuid)
        event-a1 {:aggregate-id aggregate-id-a
                  :created-at 10000
                  :type :bot-created}
        event-a2 {:aggregate-id aggregate-id-a
                  :created-at 10010
                  :type :nickname-assigned
                  :nickname "Tripy"}]
    (testing "First one ordering direction"
      (reset-eventstore!)
      (save-to-eventstore! [event-a1])
      (save-to-eventstore! [event-a2])
      (is (= [event-a1 event-a2]
             (get-events-for-aggregate aggregate-id-a))))
    (testing "Then the other"
      (reset-eventstore!)
      (save-to-eventstore! [event-a2])
      (save-to-eventstore! [event-a1])
      (is (= [event-a2 event-a1]
             (get-events-for-aggregate aggregate-id-a))))
    (testing "Multiple events in a single transaction"
      (reset-eventstore!)
      (save-to-eventstore! [event-a1 event-a2])
      (is (= [event-a1 event-a2]
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
        (save-to-eventstore! [event-a1])
        (save-to-eventstore! [event-b2])
        (save-to-eventstore! [event-a2])
        (save-to-eventstore! [event-b1])
        (is (= [event-a1 event-a2]
               (get-events-for-aggregate aggregate-id-a)))
        (is (= [event-b2 event-b1]
               (get-events-for-aggregate aggregate-id-b)))
        (testing "Can not mix aggregates in a single transaction"
          (reset-eventstore!)
          (is (some? (save-to-eventstore! [event-a1 event-a2])))
          (is (nil? (save-to-eventstore! [event-a1 event-b2]))))))))

(deftest eventstore-isolation-test
  (let [aggregate-id-a (random-uuid)
        event-a1 {:aggregate-id aggregate-id-a
                  :created-at 10000
                  :type :bot-created}
        event-a2 {:aggregate-id aggregate-id-a
                  :created-at 10010
                  :type :nickname-assigned
                  :nickname "Tripy"}
        aggregate-id-b (random-uuid)
        event-b1 {:aggregate-id aggregate-id-b
                  :created-at 20000
                  :type :bot-created}
        event-b2 {:aggregate-id aggregate-id-b
                  :created-at 20010
                  :type :nickname-assigned
                  :nickname "XTDBY"}]
    (testing "Isolation"
      (reset-eventstore!)
      (future (save-to-eventstore! [event-a1]))
      (future (save-to-eventstore! [event-a2]))
      (future (save-to-eventstore! [event-b2]))
      (future (save-to-eventstore! [event-b1]))
      (Thread/sleep 200)
      (is (= #{event-a1 event-a2}
             (into #{} (get-events-for-aggregate aggregate-id-a))))
      (is (= #{event-b1 event-b2}
             (into #{} (get-events-for-aggregate aggregate-id-b)))))))

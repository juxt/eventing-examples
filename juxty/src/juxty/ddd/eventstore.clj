(ns juxty.ddd.eventstore
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.trip.core :as trip]))

(defn create-conn [] (ref (trip/empty-db)))

(defn reset-conn [conn]
  (dosync
   (ref-set conn (trip/empty-db))))

(extend-type clojure.lang.Ref
  trip/DbConnection
  (-db [this] @this)
  (-transact [this tx-data]
    (let [result (atom nil)]
      (alter this (fn [db]
                    (let [tx-report (trip/with db tx-data)]
                      (reset! result tx-report)
                      (:db-after tx-report))))
      @result)))

(defn- group-by-keys [ks document]
  (->> document
       ((apply juxt ks))
       (interpose "-")
       (apply str)
       keyword))

(defn append
  [db ks documents]
  (let [offset-metadata (or (trip/entity db :offset-metadata)
                            {:db/id :offset-metadata})
        document-groups (group-by (partial group-by-keys ks) documents)
        document-offsets (for [[ks documents] document-groups]
                           (let [offset (get offset-metadata ks 0)
                                 new-offset (+ offset (count documents))
                                 offsets (range offset new-offset)]
                             [(mapv (fn [document offset]
                                      (assoc document
                                             :db/id (random-uuid)
                                             :offset offset))
                                    documents offsets)
                              {ks new-offset}]))
        new-documents (into [] (mapcat first document-offsets))
        new-offset-metadata (reduce merge offset-metadata (map second document-offsets))]
    (conj new-documents new-offset-metadata)))

(defmacro gen-datalog [key-syms]
  `(quote ~(hash-map :find '[(pull ?e [*])]
                     :in (into ['$] (concat
                                     (mapv #(symbol (str "?" (name %))) key-syms)
                                     ['?o]))
                     :where (conj (mapv #(vector '?e (keyword %) (symbol (str "?" (name %)))) key-syms)
                                  '[?e :offset ?o']
                                  '[(>= ?o' ?o)]))))

(defn post-process-datalog-results [query-results]
  (->> query-results
       (into [])
       flatten
       (sort-by :offset)
       (mapv #(dissoc % :db/id))))

(defn events-from [db aggregate-id offset]
  (->> (trip/q (gen-datalog [aggregate-id]) db aggregate-id offset)
       post-process-datalog-results
       (mapv #(dissoc % :offset))))

(defn records-from [db topic partition offset]
  (->> (trip/q (gen-datalog [topic partition]) db topic partition offset)
       post-process-datalog-results))

(defn save-to-event-store! [conn events]
  (dosync (trip/transact! conn (append (trip/db conn) [:aggregate-id] events))))

(defn save-to-log-store! [conn records]
  (dosync (trip/transact! conn (append (trip/db conn) [:topic :partition] records))))


(deftest store-test
  (let [aggregate-id-a (random-uuid)
        event-a1 {:aggregate-id aggregate-id-a
                  :created-at 10000
                  :type :bot-created}
        event-a2 {:aggregate-id aggregate-id-a
                  :created-at 10010
                  :type :nickname-assigned
                  :nickname "Tripy"}
        conn (create-conn)]
    (testing "First one ordering direction"
      (save-to-event-store! conn [event-a1])
      (save-to-event-store! conn [event-a2])
      (is (= [event-a1 event-a2]
             (events-from (trip/db conn) aggregate-id-a 0))))
    (testing "Then the other"
      (reset-conn conn)
      (save-to-event-store! conn [event-a2])
      (save-to-event-store! conn [event-a1])
      (is (= [event-a2 event-a1]
             (events-from (trip/db conn) aggregate-id-a 0))))
    (testing "Multiple events in a single transaction"
      (reset-conn conn)
      (save-to-event-store! conn [event-a1 event-a2])
      (is (= [event-a1 event-a2]
             (events-from (trip/db conn) aggregate-id-a 0))))
    (testing "More than one aggregate"
      (let [aggregate-id-b (random-uuid)
            event-b1 {:aggregate-id aggregate-id-b
                      :created-at 20000
                      :type :bot-created}
            event-b2 {:aggregate-id aggregate-id-b
                      :created-at 20010
                      :type :nickname-assigned
                      :nickname "XTDBY"}]
        (reset-conn conn)
        (save-to-event-store! conn [event-a1 event-b2 event-a2 event-b1])
        (is (= [event-a1 event-a2]
               (events-from (trip/db conn) aggregate-id-a 0)))
        (is (= [event-b2 event-b1]
               (events-from (trip/db conn) aggregate-id-b 0)))
        (testing "Adding records and logs into the same db"
          (let [record-1 {:topic "foo"
                          :partition 1
                          :key :sister
                          :value :angela}
                record-2 {:topic "bar"
                          :partition 6
                          :key :mother
                          :value :june}]
            (reset-conn conn)
            (save-to-event-store! conn [event-a1 event-b2 event-a2 event-b1])
            (save-to-log-store! conn [record-1 record-1 record-2 record-1])
            (is (= [{:topic "foo" :key :sister :value :angela :partition 1 :offset 0}	  
	            {:topic "foo" :key :sister :value :angela :partition 1 :offset 1}
	            {:topic "foo" :key :sister :value :angela :partition 1 :offset 2}]
                   (records-from (trip/db conn) "foo" 1 0)))
            (is (= [event-a1 event-a2]
                   (events-from (trip/db conn) aggregate-id-a 0)))
            (is (= [event-b2 event-b1]
                   (events-from (trip/db conn) aggregate-id-b 0)))))))))

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
                  :nickname "XTDBY"}
        conn (create-conn)]
    (testing "Isolation"
      (reset-conn conn)
      (future (save-to-event-store! conn [event-a1]))
      (future (save-to-event-store! conn [event-a2]))
      (future (save-to-event-store! conn [event-b2]))
      (future (save-to-event-store! conn [event-b1]))
      (Thread/sleep 200)
      (is (= #{event-a1 event-a2}
             (into #{} (events-from (trip/db conn) aggregate-id-a 0))))
      (is (= #{event-b1 event-b2}
             (into #{} (events-from (trip/db conn) aggregate-id-b 0)))))))

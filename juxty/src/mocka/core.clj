(ns mocka.core
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.trip.core :as trip]))

;; [[https://kafka.apache.org/23/javadoc/index.html][Kafka API Docs]]

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

(defn create-conn [] (ref (trip/empty-db)))

(defn reset-conn [conn]
  (dosync
   (ref-set conn (trip/empty-db))))

(defn- topic-partition-keyword [record]
  (->> record
       ((juxt :topic :partition))
       (interpose "-")
       (apply str)
       keyword))

(defn append
  [db records]
  (let [offset-metadata (or (trip/entity db :offset-metadata)
                            {:db/id :offset-metadata})
        record-groups (group-by topic-partition-keyword records)
        record-offsets (for [[topic-partition-keyword records] record-groups]
                         (let [offset (get offset-metadata topic-partition-keyword 0)
                               new-offset (+ offset (count records))
                               offsets (range offset new-offset)]
                           [(mapv (fn [record offset]
                                    (assoc record
                                           :db/id (random-uuid)
                                           :offset offset))
                                  records offsets)
                            {topic-partition-keyword new-offset}]))
        new-records (into [] (mapcat first record-offsets))
        new-offset-metadata (reduce merge offset-metadata (map second record-offsets))]
    (conj new-records new-offset-metadata)))

(defn from
  [db topic partition offset]
  (->> (trip/q '{:find [(pull ?e [*])]
                 :in [$ ?t ?p ?o]
                 :where [[?e :topic ?t]
                         [?e :partition ?p]
                         [?e :offset ?o']
                         [(>= ?o' ?o)]]}
               db topic partition offset)
       (into [])
       flatten
       (sort-by :offset)
       (mapv #(dissoc % :db/id :db/type))))

(deftest logstore-test
  (let [record-1 {:topic "foo"
                  :partition 1
                  :key :sister
                  :value :angela}
        record-2 {:topic "bar"
                  :partition 6
                  :key :mother
                  :value :june}
        conn (create-conn)]
    (dosync
     (trip/transact! conn (append (trip/db conn) [record-1 record-1]))
     (trip/transact! conn (append (trip/db conn) [record-1]))
     (trip/transact! conn (append (trip/db conn) [record-2 record-1 record-2])))
    (is (= {:db/id :offset-metadata
            :foo-1 4
            :bar-6 2}
           (trip/entity (trip/db conn) :offset-metadata)))
    (is (= [{:topic "foo" :key :sister :value :angela :partition 1 :offset 2}
            {:topic "foo" :key :sister :value :angela :partition 1 :offset 3}]
           (from (trip/db conn) "foo" 1 2)))))

;; Model
(defrecord LogValue [tos]) ;; totally ordered sequence
(defrecord Partition [log])
(defrecord Topic [partitions])
(defrecord Producer [key-serialiser value-serialiser])
(defrecord Consumer [partitions offsets key-deserialiser value-deserialiser])

;; Extensible Log Implementation
(defprotocol Log
  (-append [this records] "Returns a new log with records appended")
  (-nth [this offset] "Returns from log the record at offset"))

(defprotocol LogConnection
  "Protocol representing a connection to an immutable log value.  Can commit
   transaction to transition to new states."
  (log [this] "Returns the current `log` value for the connection")
  (commit [this record-data] "Commits record-data to the connection "))

;; API
;; (defn append [log records]
;;   (-append log records))

;; (defn from [log offset]
;;   (-nth log offset))

(defn new-topic [number-of-partitions log-conn]
  (->Topic (repeatedly number-of-partitions #(->Partition (log-conn)))))

(defn new-producer [] (->Producer identity identity))

(defn send [producer topic k v]
  ;; TODO We don't want to send to all partitions....
  (for [partition (:partitions topic)]
    (commit (:log partition) [[k v]])))

(defn new-consumer [] (->Consumer [] (atom []) identity identity))

(defn subscribe [consumer topic]
  (let [new-consumer (-> consumer
                         (assoc :partitions (:partitions topic)))]
    (reset! (:offsets new-consumer) (into [] (repeat (count (:partitions topic)) 0)))
    new-consumer))

(defn poll [consumer]
  (let [results (mapv (fn [partition offset]
                        [(from (log (:log partition)) offset)
                         offset])
                      (:partitions consumer)
                      @(:offsets consumer))]
    (reset! (:offsets consumer) (mapv #(-> % second inc) results))
    results))

;; Default Log Implementation with Vector and Atoms
;; (extend-type LogValue
;;   Log
;;   (-append [this records] (update this :tos concat records))
;;   (-nth [this offset] (nth (:tos this) offset)))

;; (extend-type clojure.lang.Atom
;;   LogConnection
;;   (log [this] @this)
;;   (commit [this records] (swap! this (fn [log] (append log records)))))

;; (defn new-log [] (->LogValue []))
;; (defn log-conn [] (atom (new-log)))

;; ;; Usage
;; (def input-topic (new-topic 1 log-conn))
;; (def producer (-> (new-producer)))
;; (send producer input-topic :bar :bat)
;; (def input-consumer (-> (new-consumer)
;;                         (subscribe input-topic)))
;; (poll input-consumer)


;; (extend-type clojure.lang.Ref
;;   TopicConnection
;;   (producer [this v]
;;     (dosync ))
;;     )

;; (defn producer
;;   "Returns a function that adds a message to a specific topic"
;;   [topic]
;;   (fn [v]
;;     (swap! topic conj v)
;;     v))

;; (defn consumer
;;   "Returns a function that returns the next message on a specific topic.
;;    Optionally pass the intial offset."
;;   ([topic]
;;    (consumer topic 0))
;;   ([topic ioffset]
;;    (let [offset (atom ioffset)]
;;      (fn []
;;        (when-let [result (nth @topic @offset nil)]
;;          (swap! offset inc)
;;          result)))))

;; (defn consumer-last
;;   "Returns a function that returns the next message on a specific topic.
;;    Optionally pass the intial offset."
;;   ([topic]
;;    (consumer topic (dec (count @topic)))))

;; (defn to
;;   "From a topic config use the producer fn to add a message"
;;   [topic-config v]
;;   ((:producer topic-config) v))

;; (defn from
;;   "From a topic config use the consumer fn to read the next message"  
;;   [topic-config]
;;   ((:consumer topic-config)))

;; (defn peek'
;;   "Print the value of x and return the value"
;;   ([x]
;;    (peek' "" x))
;;   ([s x]
;;    (println s)
;;    (pprint x)
;;    x))

;; (defn wait
;;   ([ms x]
;;    (Thread/sleep ms)
;;    x))

;; (defmacro builder
;;   "Expects a list of bindings and a function to execute.  
;;   The function will run every 100ms in it's own thread."
;;   [form & body]
;;   `(atom 
;;     (future
;;       (let ~form
;;         (while true
;;           (Thread/sleep 200)
;;           ~@body)))))

;; (defn l2f
;;   "Last to first helper for thread last"
;;   ([f a b]
;;    (f b a))
;;   ([f a b c]
;;    (f c a b))
;;   ([f a b c d]
;;    (f d a b c)))

;; (defn ->topic-config
;;   ([]
;;    (->topic-config (atom [])))
;;   ([topic]
;;    {:topic topic
;;     :consumer (consumer topic 0)
;;     :producer (producer topic)}))

;; ;; some more advanced things

;; (defn queue
;;   ([] (clojure.lang.PersistentQueue/EMPTY))
;;   ([coll]
;;    (reduce conj clojure.lang.PersistentQueue/EMPTY coll)))


;; (defn ->merge
;;   ([]
;;    (->merge (atom (queue))))
;;   ([q] 
;;    (fn [t1 t2]
;;      (let [v (peek @q)]
;;        (cond
;;          v (do (when t1 (swap! q conj t1))
;;                (when t2 (swap! q conj t2))
;;                (swap! q pop)
;;                v)
;;          (and t1 t2) (do (swap! q conj t2)
;;                          t1)
;;          t1 t1
;;          t2 t2)))))


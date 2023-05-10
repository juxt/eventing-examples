(ns juxty.ddd.isolation
  (:require
   [clojure.test :refer [deftest is testing]]
   [juxt.trip.core :as trip]))

(def eventstore (trip/create-conn))

(defn reset-eventstore [] (reset! eventstore (trip/empty-db)))

(deftest isolation-test
  (testing "Sequential changes should resolve in order called"
    (reset-eventstore)
    (trip/transact! eventstore [[:db/add 1 :foo 1]])
    (trip/transact! eventstore [[:db/add 1 :bar 1]])
    (is (= {:foo 1 :bar 1} (trip/entity (trip/db eventstore) 1 ))))
  (testing "Concurrent changes should resolve serially"
    ;; this test fails intermittently
    (reset-eventstore)
    (future (trip/transact! eventstore [[:db/add 1 :foo 1]]))
    (future (trip/transact! eventstore [[:db/add 1 :bar 1]]))
    (Thread/sleep 200)
    (is (= {:foo 1 :bar 1} (trip/entity (trip/db eventstore) 1 )))))

(defn transact!
  "Sample transact that uses swap instead of reset giving Clojure 
   a chance to retry with updated state - doesn't return transaction report"
  [conn tx-data]
  (swap! conn
         (fn [c]
           (let [tx-report (trip/with c tx-data)]
             (:db-after tx-report)))))

(deftest isolation-test-swap-based-transact
  (testing "Concurrent changes should resolve serially"
    (reset-eventstore)
    (future (transact! eventstore [[:db/add 1 :foo 1]]))
    (future (transact! eventstore [[:db/add 1 :bar 1]]))
    (Thread/sleep 200)
    (is (= {:foo 1 :bar 1} (trip/entity (trip/db eventstore) 1 )))))

(def eventstore-ref (ref (trip/empty-db)))


(extend-type clojure.lang.Ref
  trip/DbConnection
  (-db [this] @this)

  (-commit [this tx-report]
    (ref-set this (:db-after tx-report))))

(defn reset-eventstore-ref! []
  (dosync
   (ref-set eventstore-ref (trip/empty-db))))

(deftest isolation-re-ftest
  (testing "Sequential changes should resolve in order called"
    (reset-eventstore-ref!)
    (dosync (trip/transact! eventstore-ref [[:db/add 1 :foo 1]]))
    (dosync (trip/transact! eventstore-ref [[:db/add 1 :bar 1]]))
    (is (= {:foo 1 :bar 1} (trip/entity (trip/db eventstore-ref) 1 ))))
  (testing "Concurrent changes should resolve serially"
    (reset-eventstore-ref!)
    (future (dosync (trip/transact! eventstore-ref [[:db/add 1 :foo 1]])))
    (future (dosync (trip/transact! eventstore-ref [[:db/add 1 :bar 1]])))
    (future (dosync (trip/transact! eventstore-ref [[:db/add 1 :baz 1]])))
    (future (dosync (trip/transact! eventstore-ref [[:db/add 1 :bux 1]])))
    (future (dosync (trip/transact! eventstore-ref [[:db/add 1 :cux 1]])))
    (future (dosync (trip/transact! eventstore-ref [[:db/add 1 :dox 1]])))
    (future (dosync (trip/transact! eventstore-ref [[:db/add 1 :cat 1]])))
    (future (dosync (trip/transact! eventstore-ref [[:db/add 1 :dog 1]])))
    (Thread/sleep 200)
    (is (= {:foo 1 :bar 1
            :baz 1 :bux 1
            :cux 1 :dox 1
            :cat 1 :dog 1} (trip/entity (trip/db eventstore-ref) 1 )))))

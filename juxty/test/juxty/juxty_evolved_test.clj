(ns juxty.juxty-evolved-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [juxty.juxty-evolved :as sut :refer [bot-cmd-handler producer]]))

(defn bot-db
  []
  (atom {:juxty {:bot-id :juxty
                 :position 0
                 :created-at 1681201515402}
         :cruxy {:bot-id :cruxy
                 :position -2
                 :created-at 1681201515983}}))

(deftest bot-cmd-handler-test
  (with-redefs [sut/external-fail? (constantly false)]
    (let [bot-events (atom [])
          state (bot-db)
          producer (producer bot-events)
          run-cmd (fn [cmd] (-> (bot-cmd-handler state producer cmd)
                                (dissoc :cmd-response-id :created-at)))]
      (testing "create"
        (is (= {:status :success
                :originating-cmd-id 1000}
               (run-cmd {:type :create
                         :cmd-id 1000
                         :bot-id :xtdby})))
        (is (= {:status :failure
                :originating-cmd-id 1001
                :error [:bot-already-found :juxty]}
               (run-cmd {:type :create
                         :cmd-id 1001
                         :bot-id :juxty})))
        (is (= [{:type :creation
                 :bot-id :xtdby
                 :position 0
                 :originating-cmd-id 1000}]
               (mapv (fn [e] (dissoc e :epvent-id :created-at)) @bot-events)))
        (is (= 0 (get-in @state [:xtdby :position]))))
      (testing "move-left"
        (reset! bot-events [])
        (is (= {:status :success
                :originating-cmd-id 1000}
               (run-cmd {:type :move-left
                         :cmd-id 1000
                         :bot-id :juxty})))
        (is (= -1 (get-in @state [:juxty :position])))
        (is (= {:status :failure
                :originating-cmd-id 1001
                :error [:out-of-bounds :left]}
               (run-cmd {:type :move-left
                         :cmd-id 1001
                         :bot-id :cruxy})))
        (is (= -2 (get-in @state [:cruxy :position])))
        (is (= [{:type :movement
                 :bot-id :juxty
                 :delta -1
                 :originating-cmd-id 1000}]
               (mapv (fn [e] (dissoc e :event-id :created-at)) @bot-events)))))))



(ns juxty.juxty-evolved-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [juxty.juxty-evolved :as sut :refer [bot-cmd-handler producer]]))

(def bot-db (atom {:juxty {:bot-id :juxty
                           :position (atom 0)
                           :created-at 1681201515402}
                   :cruxy {:bot-id :cruxy
                           :position (atom -2)
                           :created-at 1681201515983}
                   :bluxy {:bot-id :bluxy
                           :position (atom 2)
                           :created-at 1681201516105}}))

(deftest bot-cmd-handler-test
  (with-redefs [sut/external-fail? (constantly false)]
    (let [bot-events (atom [])
          state bot-db
          producer (producer bot-events)
          run-cmd (fn [cmd] (-> cmd
                                (bot-cmd-handler state producer)
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
               (mapv (fn [e] (dissoc e :event-id :created-at)) @bot-events))))
      (testing "move-left"
        (reset! bot-events [])
        (is (= {:status :success
                :originating-cmd-id 1000}
               (run-cmd {:type :move-left
                         :cmd-id 1000
                         :bot-id :juxty})))
        (is (= {:status :failure
                :originating-cmd-id 1001
                :error [:out-of-bounds :left]}
               (run-cmd {:type :move-left
                         :cmd-id 1001
                         :bot-id :cruxy})))
        (is (= [{:type :movement
                 :bot-id :juxty
                 :delta -1
                 :originating-cmd-id 1000}]
               (mapv (fn [e] (dissoc e :event-id :created-at)) @bot-events))))
      (testing "move-right"
        (reset! bot-events [])
        (is (= {:status :success
                :originating-cmd-id 1000}
               (run-cmd {:type :move-right
                         :cmd-id 1000
                         :bot-id :juxty})))
        (is (= {:status :failure
                :originating-cmd-id 1001
                :error [:out-of-bounds :right]}
               (run-cmd {:type :move-right
                         :cmd-id 1001
                         :bot-id :bluxy})))
        (is (= [{:type :movement
                 :bot-id :juxty
                 :delta 1
                 :originating-cmd-id 1000}]
               (mapv (fn [e] (dissoc e :event-id :created-at)) @bot-events))))
      (testing "multiple-cmds"
        (reset! bot-events [])
        (is (= {:status :success
                :originating-cmd-id 1000}
               (run-cmd {:type :create
                         :cmd-id 1000
                         :bot-id :xtdby})))
        (is (= {:status :success
                :originating-cmd-id 1001}
               (run-cmd {:type :move-right
                         :cmd-id 1001
                         :bot-id :juxty})))
        (is (= {:status :success
                :originating-cmd-id 1002}
               (run-cmd {:type :move-right
                         :cmd-id 1002
                         :bot-id :juxty})))
        (is (= [{:type :creation
                 :bot-id :xtdby
                 :position 0
                 :originating-cmd-id 1000}
                {:type :movement
                 :bot-id :juxty
                 :delta 1
                 :originating-cmd-id 1001}
                {:type :movement
                 :bot-id :juxty
                 :delta 1
                 :originating-cmd-id 1002}]
               (mapv (fn [e] (dissoc e :event-id :created-at)) @bot-events)))))))

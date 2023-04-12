(ns juxty.juxty-evolved-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [juxty.juxty-evolved :as sut :refer [bot-cmd-handler
                                                 bot-event-handler]]))

(defn bot-db
  []
  (atom {:juxty {:bot-id :juxty
                 :position 0
                 :created-at 1681201515402}
         :cruxy {:bot-id :cruxy
                 :position -2
                 :created-at 1681201515983}
         :bluxy {:bot-id :bluxy
                 :position 2
                 :created-at 1681201516105}}))

(defn produce-to!
  [events event]
  (swap! events conj event))

(defn producer
  [events]
  (fn [event]
    (produce-to! events event)))

(deftest bot-cmd-handler-test
  (with-redefs [sut/external-fail? (constantly false)]
    (let [bot-events (atom [])
          state (bot-db)
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

(deftest bot-event-handler-test
  (with-redefs [sut/external-fail? (constantly false)]
    (let [state (bot-db)
          apply-event (fn [event] (-> event
                                      (bot-event-handler state)))]
      (testing "creation"
        (apply-event {:type :creation
                      :bot-id :xtdby
                      :position 0
                      :created-at 1681287092438})
        (is (= 0 (get-in @state [:xtdby :position]))))
      (testing "movement"
        (apply-event {:type :movement
                      :bot-id :xtdby
                      :delta 1
                      :created-at 1681287092438})
        (is (= 1 (get-in @state [:xtdby :position])))
        (doall (map apply-event [{:type :movement
                                  :bot-id :xtdby
                                  :delta 1
                                  :created-at 1681287092438}
                                 {:type :movement
                                  :bot-id :xtdby
                                  :delta -1
                                  :created-at 1681287092439}
                                 {:type :movement
                                  :bot-id :xtdby
                                  :delta -1
                                  :created-at 1681287092440}]))
        (is (= 0 (get-in @state [:xtdby :position])))))))



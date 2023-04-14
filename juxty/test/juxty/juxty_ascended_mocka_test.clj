(ns juxty.juxty-ascended-mocka-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [juxty.juxty-ascended :as sut :refer [bot-cmd-handler
                                                  bot-event-handler
                                                  hydrate]]
            [mocka.mocka :as mocka :refer [to from producer consumer
                                           builder peek wait l2f
                                           ->topic-config]]))


(deftest state-logic-tests
  (let [pending-state (atom {:juxty {}
                             :xtdby {}})
        state (atom {:juxty {}
                     :buxty {}})]
    (is (true? (sut/state-or sut/bot-found? pending-state state :juxty)))
    (is (true? (sut/state-or sut/bot-found? pending-state state :xtdby)))
    (is (true? (sut/state-or sut/bot-found? pending-state state :buxty)))
    (is (false? (sut/state-or sut/bot-found? pending-state state :cruxy)))
    (is (true? (sut/state-and sut/bot-not-found? pending-state state :cruxy)))))

(deftest juxty-ascended-via-mocka
  (with-redefs [sut/external-fail? (constantly false)]
    (let [pending-state (atom {})
          state (atom {})
          events (->topic-config )
          cmds (->topic-config)
          cmd-responses (->topic-config)
          cmd-app (builder [cmds cmds
                            events events
                            cmd-responses cmd-responses]
                           (some->> (from cmds)
                                    (l2f bot-cmd-handler pending-state state (:producer events))
                                    (to cmd-responses)))
          event-app (builder [events events]
                             (some->> (from events)
                                      (l2f bot-event-handler state)))]
      (testing "Appropriately spaced commands"
        ;; send commands
        (to cmds {:type :create :cmd-id 1000 :bot-id :xtdby})
        (to cmds {:type :move-left :cmd-id 1001 :bot-id :xtdby})
        (to cmds {:type :move-left :cmd-id 1002 :bot-id :xtdby})
        (Thread/sleep 1000)
        ;; results
        (is (= [{:type :create, :cmd-id 1000, :bot-id :xtdby}
                {:type :move-left, :cmd-id 1001, :bot-id :xtdby}
                {:type :move-left, :cmd-id 1002, :bot-id :xtdby}]
               @(:topic cmds)))
        (is (= [{:status :success,
                 :originating-cmd-id 1000}
                {:status :success,
                 :originating-cmd-id 1001}
                {:status :success,
                 :originating-cmd-id 1002}]
               (map (fn [e] (dissoc e :cmd-response-id :created-at)) @(:topic cmd-responses))))
        (is (= [{:type :creation,
                 :bot-id :xtdby,
                 :position 0,
                 :originating-cmd-id 1000}
                {:type :movement,
                 :bot-id :xtdby,
                 :delta -1,
                 :originating-cmd-id 1001}
                {:type :movement,
                 :bot-id :xtdby,
                 :delta -1,
                 :originating-cmd-id 1002}]
               (map (fn [e] (dissoc e :event-id :created-at)) @(:topic events))))
        (is (= -2 (get-in @state [:xtdby :position]))))
      (testing "Quick succession commands results in success"
        (to cmds
            {:type :create :cmd-id 1003 :bot-id :boxy}
            {:type :move-left :cmd-id 1004 :bot-id :boxy})
        (Thread/sleep 1000)
        (is (= [{:type :create, :cmd-id 1003, :bot-id :boxy}
                {:type :move-left, :cmd-id 1004, :bot-id :boxy}]
               (take-last 2 @(:topic cmds))))
        (is (= [{:status :success,
                 :originating-cmd-id 1003}
                {:status :success
	         :originating-cmd-id 1004}]
               (map (fn [e] (dissoc e :cmd-response-id :created-at))
                    (take-last 2 @(:topic cmd-responses)))))
        (is (= [{:type :creation,
                 :bot-id :boxy,
                 :position 0,
                 :originating-cmd-id 1003}
                {:type :movement,
                 :bot-id :boxy,
                 :delta -1,
                 :originating-cmd-id 1004}]
               (map (fn [e] (dissoc e :event-id :created-at))
                    (take-last 2 @(:topic events))))))
      (testing "Reset states and hydrate off of the event topic"
        (let [[old-state new-state] (reset-vals! state {})]
          (reset! pending-state {})
          (is (not= old-state new-state))
          (is (= {} new-state))
          (hydrate state (:topic events))
          (is (= old-state @state))
          (clojure.pprint/pprint @state)))
      
      (future-cancel @cmd-app)
      (future-cancel @event-app))))

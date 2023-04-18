(ns juxty.juxty-ascended-mocka-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [juxty.juxty-ascended :as sut :refer [bot-cmd-handler bot-event-handler
                                         hydrate]]
   [mocka.mocka :as mocka :refer [->topic-config builder from to]]))


(deftest state-logic-tests
  (let [pending-state (atom {:juxty {}
                             :xtdby {}})
        state (atom {:juxty {}
                     :buxty {}})]
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
                                    (bot-cmd-handler pending-state state (:producer events))
                                    (to cmd-responses)))
          event-app (builder [events events]
                             (some->> (from events)
                                      (bot-event-handler state)))]
      (testing "Quick succession commands results in success: can move immediately after creation"
        (to cmds {:type :create :cmd-id 1003 :bot-id :boxy})
        (to cmds {:type :move-left :cmd-id 1004 :bot-id :boxy})
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
      (testing "Quick succession commands maintains consistency: successive creation fails"
        (to cmds {:type :create :cmd-id 1005 :bot-id :grixy})
        (to cmds {:type :create :cmd-id 1006 :bot-id :grixy})
        (Thread/sleep 1000)
        (is (= [{:type :create, :cmd-id 1005, :bot-id :grixy}
                {:type :create, :cmd-id 1006, :bot-id :grixy}]
               (take-last 2 @(:topic cmds))))
        (is (= [{:status :success,
                 :originating-cmd-id 1005}
                {:status :failure
	         :originating-cmd-id 1006
                 :error [:bot-already-found :grixy]}]
               (map (fn [e] (dissoc e :cmd-response-id :created-at))
                    (take-last 2 @(:topic cmd-responses)))))
        (is (= [{:type :creation,
                 :bot-id :grixy,
                 :position 0,
                 :originating-cmd-id 1005}]
               (map (fn [e] (dissoc e :event-id :created-at))
                    (take-last 1 @(:topic events))))))
      (testing "Reset states and hydrate off of the event topic"
        (let [[old-state new-state] (reset-vals! state {})]
          (reset! pending-state {})
          (is (not= old-state new-state))
          (is (= {} new-state))
          (hydrate state (:topic events))
          (is (= old-state @state))))
      
      (future-cancel @cmd-app)
      (future-cancel @event-app))))



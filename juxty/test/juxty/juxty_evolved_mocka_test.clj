(ns juxty.juxty-evolved-mocka-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [juxty.juxty-evolved :as sut :refer [bot-cmd-handler
                                                 bot-event-handler]]
            [mocka.mocka :as mocka :refer [to from producer consumer builder peek wait
                                           ->topic-config]]))

(deftest juxty-evolved-via-mocka
  (with-redefs [sut/external-fail? (constantly false)]
    (let [state (atom {})
          events (->topic-config)
          cmds (->topic-config)
          cmd-responses (->topic-config)
          cmd-app (builder [cmds cmds
                            events events
                            cmd-responses cmd-responses]
                           (some->> (from cmds)
                                    (bot-cmd-handler state (:producer events))
                                    (to cmd-responses)))
          event-app (builder [events events]
                             (some->> (from events)
                                      (wait 300)
                                      (bot-event-handler state)))]
      (testing "Appropriately spaced commands"
        ;; send commands
        (to cmds {:type :create :cmd-id 1000 :bot-id :xtdby})
        (Thread/sleep 1000)
        (to cmds {:type :move-left :cmd-id 1001 :bot-id :xtdby})
        (Thread/sleep 1000)
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
      (testing "Quick succession commands result in failure: moving a just created bot fails"
        (to cmds
            {:type :create :cmd-id 1003 :bot-id :boxy}
            {:type :move-left :cmd-id 1004 :bot-id :boxy})
        (Thread/sleep 1000)
        (is (= [{:type :create, :cmd-id 1003, :bot-id :boxy}
                {:type :move-left, :cmd-id 1004, :bot-id :boxy}]
               (take-last 2 @(:topic cmds))))
        (is (not= [{:status :success,
                    :originating-cmd-id 1003}
                   {:status :failure
	            :originating-cmd-id 1004
	            :error [:bot-not-found :boxy]}]
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
      (testing "Quick succession commands break consistency: 2 creation events for 1 bot"
        (to cmds
            {:type :create :cmd-id 1005 :bot-id :grixy}
            {:type :create :cmd-id 1006 :bot-id :grixy})
        (Thread/sleep 1000)
        (is (= [{:type :create, :cmd-id 1005, :bot-id :grixy}
                {:type :create, :cmd-id 1006, :bot-id :grixy}]
               (take-last 2 @(:topic cmds))))
        (is (not= [{:status :success,
                    :originating-cmd-id 1005}
                   {:status :success
	            :originating-cmd-id 1006}]
                  (map (fn [e] (dissoc e :cmd-response-id :created-at))
                       (take-last 2 @(:topic cmd-responses)))))
        (is (not= [{:type :creation,
                    :bot-id :grixy,
                    :position 0,
                    :originating-cmd-id 1005}
                   {:type :creation,
                    :bot-id :grixy,
                    :position 0,
                    :originating-cmd-id 1006}]
                  (map (fn [e] (dissoc e :event-id :created-at))
                       (take-last 2 @(:topic events))))))
      (future-cancel @cmd-app)
      (future-cancel @event-app))))

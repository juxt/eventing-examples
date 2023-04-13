(ns juxty.juxty-evolved-mocka-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [juxty.juxty-evolved :as sut :refer [bot-cmd-handler
                                                 bot-event-handler]]
            [mocka.mocka :as mocka :refer [to from producer consumer builder peek wait l2f]]))

(deftest juxty-evolved-via-mocka
  (with-redefs [sut/external-fail? (constantly false)]
    (let [state (atom {})
          events-topic (atom [])
          cmds-topic (atom [])
          cmd-responses-topic (atom [])
          events {:topic events-topic
                  :consumer (consumer events-topic 0)
                  :producer (producer events-topic)}
          cmds {:topic cmds-topic
                :consumer (consumer cmds-topic 0)
                :producer (producer cmds-topic)}
          cmd-responses {:topic cmd-responses-topic
                         :consumer (consumer cmd-responses-topic 0)
                         :producer (producer cmd-responses-topic)}
          cmd-app (builder [cmds cmds
                            events events
                            cmd-responses cmd-responses]
                           (some->> (from cmds)
                                    (l2f bot-cmd-handler state (:producer events))
                                    (to cmd-responses)))
          event-app (builder [events events]
                             (some->> (from events)
                                      (wait 300)
                                      (l2f bot-event-handler state)))]
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
               @cmds-topic))
        (is (= [{:status :success,
                 :originating-cmd-id 1000}
                {:status :success,
                 :originating-cmd-id 1001}
                {:status :success,
                 :originating-cmd-id 1002}]
               (map (fn [e] (dissoc e :cmd-response-id :created-at)) @cmd-responses-topic)))
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
               (map (fn [e] (dissoc e :event-id :created-at)) @events-topic)))
        (is (= -2 (get-in @state [:xtdby :position]))))
      (testing "Quick succession commands result in failure"
        (to cmds
            {:type :create :cmd-id 1003 :bot-id :boxy}
            {:type :move-left :cmd-id 1004 :bot-id :boxy})
        (Thread/sleep 1000)
        (is (= [{:type :create, :cmd-id 1003, :bot-id :boxy}
                {:type :move-left, :cmd-id 1004, :bot-id :boxy}]
               (take-last 2 @cmds-topic)))
        (is (= [{:status :success,
                 :originating-cmd-id 1003}
                {:status :failure
	         :originating-cmd-id 1004
	         :error [:bot-not-found :boxy]}]
               (map (fn [e] (dissoc e :cmd-response-id :created-at))
                    (take-last 2 @cmd-responses-topic))))
        (is (not= [{:type :creation,
                    :bot-id :boxy,
                    :position 0,
                    :originating-cmd-id 1003}
                   {:type :movement,
                    :bot-id :boxy,
                    :delta -1,
                    :originating-cmd-id 1004}]
                  (map (fn [e] (dissoc e :event-id :created-at))
                       (take-last 1 @events-topic)))))
      (future-cancel @cmd-app)
      (future-cancel @event-app))))

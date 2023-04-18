(ns juxty.juxty-ascended-teleporter-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [juxty.juxty-ascended :as mv]
   [juxty.juxty-client :refer [do-cmd-> retry-cmd run-cmd update-response!]]
   [juxty.juxty-teleporter :as tp]
   [mocka.mocka :as mocka :refer [->topic-config builder from to wait]]))

(deftest juxty-client-retries
  (with-redefs [mv/external-fail? (constantly false)]
    (let [pending-state1 (atom {})
          pending-state2 (atom {})
          state (atom {})
          client-response-state (atom {})
          mv-events (->topic-config)
          mv-cmds (->topic-config)
          tp-events (->topic-config)
          tp-cmds (->topic-config)
          mv-cmd-responses (->topic-config)
          tp-cmd-responses (->topic-config)
          mv-cmd-app (builder [cmds mv-cmds
                               events mv-events
                               cmd-responses mv-cmd-responses]
                              (some->> (from cmds)
                                       (mv/bot-cmd-handler pending-state1 state (:producer events))
                                       (to cmd-responses)))
          mv-event-app (builder [events mv-events]
                                (some->> (from events)
                                         (wait 300)
                                         (mv/bot-event-handler state)))
          tp-cmd-app (builder [cmds tp-cmds
                               events tp-events
                               cmd-responses tp-cmd-responses]
                              (some->> (from cmds)
                                       (tp/bot-cmd-handler pending-state2 state (:producer events))
                                       (to cmd-responses)))
          tp-event-app (builder [events tp-events]
                                (some->> (from events)
                                         (tp/bot-event-handler state)))
          client-app (builder [mv-cmd-responses mv-cmd-responses
                               tp-cmd-responses tp-cmd-responses]
                              (some->> (from mv-cmd-responses)
                                       (update-response! client-response-state))
                              (some->> (from tp-cmd-responses)
                                       (update-response! client-response-state)))
          run-mv-cmd (partial run-cmd mv-cmds client-response-state)
          run-tp-cmd (partial run-cmd tp-cmds client-response-state)]
      (testing "Write consistency within a service"
        (is (= {:status :success :originating-cmd-id 1002}
               (-> (do-cmd->
                    (run-mv-cmd {:type :create :cmd-id 1001 :bot-id :juxty})
                    (run-mv-cmd {:type :move-left :cmd-id 1002 :bot-id :juxty}))
                   (dissoc :cmd-response-id :created-at))))
        (Thread/sleep 1000)
        (is (= -1 (mv/get-bot-position state :juxty))))
      (testing "Separate services have broken write consistency"
        (is (not= {:status :success :originating-cmd-id 1004}
                  (-> (do-cmd->
                       (run-mv-cmd {:type :create :cmd-id 1003 :bot-id :boxy})
                       (run-tp-cmd {:type :teleport :cmd-id 1004 :bot-id :boxy :new-position 55}))
                      (dissoc :cmd-response-id :created-at))))
        (Thread/sleep 1000)
        (is (not= 55 (mv/get-bot-position state :boxy))))
      (testing "Separate services have broken write consistency - work around with client retries"
        (is (= {:status :success}
               (-> (do-cmd->
                    (run-mv-cmd {:type :create :cmd-id 1005 :bot-id :xtdby})
                    (retry-cmd run-tp-cmd {:type :teleport :cmd-id 1006 :bot-id :xtdby :new-position 55}))
                   (dissoc :cmd-response-id :created-at :cmd-id :originating-cmd-id))))
        (Thread/sleep 500)
        (is (= 55 (mv/get-bot-position state :xtdby))))
      
      (run! future-cancel [@mv-cmd-app @mv-event-app @tp-cmd-app @tp-event-app @client-app]))))

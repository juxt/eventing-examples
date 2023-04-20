(ns juxty.juxty-ascended-teleporter-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [juxty.juxty-ascended :as mv]
   [juxty.juxty-client :refer [do-cmds-> retry-cmd run-cmd update-response!]]
   [juxty.juxty-teleporter :as tp]
   [mocka.mocka :as mocka :refer [->topic-config builder from to wait ->merge]]))

(deftest juxty-seperate-services-with-shared-state
  (with-redefs [mv/external-fail? (constantly false)]
    (let [;; Global state
          state (atom {}) ;; effectively a shared external state
          ;; Movement and creations service
          mv-pending-state (atom {})
          mv-events (->topic-config)
          mv-cmds (->topic-config)
          mv-cmd-responses (->topic-config)
          mv-cmd-app (builder [cmds mv-cmds
                               events mv-events
                               cmd-responses mv-cmd-responses]
                              (some->> (from cmds)
                                       (mv/bot-cmd-handler mv-pending-state state (:producer events))
                                       (to cmd-responses)))
          mv-event-app (builder [events mv-events]
                                (some->> (from events)
                                         (wait 300)
                                         (mv/bot-event-handler state)))
          ;; Teleporter service
          tp-pending-state (atom {})
          tp-events (->topic-config)
          tp-cmds (->topic-config)
          tp-cmd-responses (->topic-config)
          tp-cmd-app (builder [cmds tp-cmds
                               events tp-events
                               cmd-responses tp-cmd-responses]
                              (some->> (from cmds)
                                       (tp/bot-cmd-handler tp-pending-state state (:producer events))
                                       (to cmd-responses)))
          tp-event-app (builder [events tp-events]
                                (some->> (from events)
                                         (wait 200)
                                         (tp/bot-event-handler state)))
          ;; Client
          client-response-state (atom {})
          client-app (builder [mv-cmd-responses mv-cmd-responses
                               tp-cmd-responses tp-cmd-responses]
                              (some->> ((->merge)
                                        (from mv-cmd-responses)
                                        (from tp-cmd-responses))
                                       (update-response! client-response-state)))
          run-cmd (partial run-cmd client-response-state)]
      (testing "Write consistency within a service"
        (is (= {:status :success :originating-cmd-id 1002}
               (-> (do-cmds->
                    (run-cmd mv-cmds {:type :create :cmd-id 1001 :bot-id :juxty})
                    (run-cmd mv-cmds {:type :move-left :cmd-id 1002 :bot-id :juxty}))
                   (dissoc :cmd-response-id :created-at))))
        (Thread/sleep 1000)
        (is (= -1 (mv/get-bot-position state :juxty))))
      (testing "Separate services have broken write consistency"
        (is (= {:status :failure
                :originating-cmd-id 1004
                :error [:bot-not-found :boxy]}
                  (-> (do-cmds->
                       (run-cmd mv-cmds {:type :create :cmd-id 1003 :bot-id :boxy})
                       (run-cmd tp-cmds {:type :teleport :cmd-id 1004 :bot-id :boxy :new-position 55}))
                      (dissoc :cmd-response-id :created-at))))
        (is (not= 55 (mv/get-bot-position state :boxy))))
      (testing "Separate services have broken write consistency - work around with client retries
                What else can be done?"
        (is (= {:status :success}
               (-> (do-cmds->
                    (run-cmd mv-cmds {:type :create :cmd-id 1005 :bot-id :xtdby})
                    (retry-cmd
                     run-cmd tp-cmds {:type :teleport :cmd-id 1006 :bot-id :xtdby :new-position 55}))
                   (dissoc :cmd-response-id :created-at :cmd-id :originating-cmd-id))))
        (Thread/sleep 500)
        (is (= 55 (mv/get-bot-position state :xtdby))))
      
      (run! future-cancel [@mv-cmd-app @mv-event-app @tp-cmd-app @tp-event-app @client-app]))))

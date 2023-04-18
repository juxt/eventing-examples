(ns juxty.juxty-client-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [juxty.juxty-ascended :as sut :refer [bot-cmd-handler bot-event-handler
                                         get-bot-position]]
   [juxty.juxty-client :refer [do-cmd-> run-cmd update-response!]]
   [mocka.mocka :as mocka :refer [->topic-config builder from to wait]]))

(deftest juxty-client-test
  (with-redefs [sut/external-fail? (constantly false)]
    (let [pending-state (atom {})
          state (atom {})
          client-response-state (atom {})
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
                                      (wait 500)
                                      (bot-event-handler state)))
          client-app (builder [cmd-responses cmd-responses]
                              (some->> (from cmd-responses)
                                       (update-response! client-response-state)))
          run-cmd (partial run-cmd cmds client-response-state)]
      (testing "Simple synchronous command"
        (is (= {:status :success, :originating-cmd-id 1003}
               (-> (run-cmd {:type :create :cmd-id 1003 :bot-id :boxy})
                   (dissoc :cmd-response-id :created-at))))
        (is (= nil (get-bot-position state :boxy))
            "Eventual consistency between the read and write models: initally nil")
        (Thread/sleep 1000)
        (is (= 0 (get-bot-position state :boxy))
            "Eventual consistency between the read and write models: instantiated to 0"))
      (testing "Multipled successful commands"
        (is (= {:status :success, :originating-cmd-id 1006}
               (-> (do-cmd->
                    (run-cmd {:type :create :cmd-id 1004 :bot-id :juxty})
                    (run-cmd {:type :move-left :cmd-id 1005 :bot-id :juxty})
                    (run-cmd {:type :move-left :cmd-id 1006 :bot-id :juxty}))
                   (dissoc :cmd-response-id :created-at))))
        (is (not= -2 (get-bot-position state :juxty))
            "Eventual consistency between the read and write models: initally nil")
        (Thread/sleep 2000)
        (is (= -2 (get-bot-position state :juxty))
            "Eventual consistency between the read and write models: instantiated to 0"))
      (testing "Failed command short-circuits cmd-success"
        (is (= {:status :failure, :originating-cmd-id 1008
                :error [:bot-already-found :xtdby]}
               (-> (do-cmd->
                    (run-cmd {:type :create :cmd-id 1007 :bot-id :xtdby})
                    (run-cmd {:type :create :cmd-id 1008 :bot-id :xtdby})
                    (run-cmd {:type :move-left :cmd-id 1009 :bot-id :xtdby}))
                   (dissoc :cmd-response-id :created-at))))
        (Thread/sleep 2000)
        (is (= 0 (get-bot-position state :xtdby))
            "Move-left command never is never issued."))
      
      (run! future-cancel [@cmd-app @event-app @client-app]))))

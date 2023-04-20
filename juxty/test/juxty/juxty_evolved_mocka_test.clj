(ns juxty.juxty-evolved-mocka-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [juxty.juxty-evolved :as sut :refer [bot-cmd-handler]]
   [mocka.mocka :as mocka :refer [->topic-config builder from to]]))

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
                                    (to cmd-responses)))]
      (testing "Multiple sequential commands and the bounded invariant"
        ;; send commands
        (to cmds {:type :create :cmd-id 1000 :bot-id :xtdby})
        (to cmds {:type :move-left :cmd-id 1001 :bot-id :xtdby})
        (to cmds {:type :move-left :cmd-id 1002 :bot-id :xtdby})
        (to cmds {:type :move-left :cmd-id 1003 :bot-id :xtdby})
        (Thread/sleep 1000)
        ;; results
        (is (= [{:type :create, :cmd-id 1000, :bot-id :xtdby}
                {:type :move-left :cmd-id 1001 :bot-id :xtdby}
                {:type :move-left :cmd-id 1002 :bot-id :xtdby}
                {:type :move-left :cmd-id 1003 :bot-id :xtdby}]
               @(:topic cmds))
            "Command topic as expected")
        (is (= [{:status :success
                 :originating-cmd-id 1000}
                {:status :success
                 :originating-cmd-id 1001}
                {:status :success
                 :originating-cmd-id 1002}
                {:status :failure
                 :originating-cmd-id 1003
                 :error [:out-of-bounds :left]}]
               (map (fn [e] (dissoc e :cmd-response-id :created-at)) @(:topic cmd-responses)))
            "Command responses as expected")
        (is (= [{:type :creation
                 :bot-id :xtdby
                 :position 0
                 :originating-cmd-id 1000}
                {:type :movement
                 :bot-id :xtdby
                 :delta -1
                 :originating-cmd-id 1001}
                {:type :movement
                 :bot-id :xtdby
                 :delta -1
                 :originating-cmd-id 1002}]
               (map (fn [e] (dissoc e :event-id :created-at)) @(:topic events)))
            "Events generated as expected")
        (is (= -2 (get-in @state [:xtdby :position]))
            "Bot position as expected"))
      (testing "Successive creation fails"
        (to cmds {:type :create :cmd-id 1003 :bot-id :grixy})             
        (to cmds {:type :create :cmd-id 1004 :bot-id :grixy})
        (Thread/sleep 1000)
        (is (= [{:type :create, :cmd-id 1003, :bot-id :grixy}
                {:type :create, :cmd-id 1004, :bot-id :grixy}]
               (take-last 2 @(:topic cmds))))
        (is (= [{:status :success,
                 :originating-cmd-id 1003}
                {:status :failure
	         :originating-cmd-id 1004
                 :error [:bot-already-found :grixy]}]
               (map (fn [e] (dissoc e :cmd-response-id :created-at))
                    (take-last 2 @(:topic cmd-responses)))))
        (is (= [{:type :creation,
                 :bot-id :grixy,
                 :position 0,
                 :originating-cmd-id 1003}]
               (map (fn [e] (dissoc e :event-id :created-at))
                    (take-last 1 @(:topic events))))))
      (future-cancel @cmd-app))))

(ns juxty.juxty-evolved-mocka-test
  (:require [clojure.test :as t :refer [deftest is testing]]
            [juxty.juxty-evolved :as sut :refer [bot-cmd-handler
                                                 bot-event-handler]]
            [mocka.mocka :as mocka :refer [to from producer consumer builder peek]]))

(defn bot-db
  []
  (atom {}))

(defn l2f
  "Last to first helper for thread last"
  ([f a b]
   (f b a))
  ([f a b c]
   (f c a b))
  ([f a b c d]
   (f d a b c)))

(deftest juxty-evolved-via-mocka
  (with-redefs [sut/external-fail? (constantly false)]
    (let [events-topic (atom [])
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
          state (bot-db)
          cmd-app (builder [cmds cmds
                            events events
                            cmd-responses cmd-responses]
                           (some->> (from cmds)
                                    (l2f bot-cmd-handler state (:producer events))
                                    (to cmd-responses)))
          event-app (builder [events events]
                             (some->> (from events)
                                      (l2f bot-event-handler state)))]
      ;; send commands
      (to cmds {:type :create :cmd-id 1000 :bot-id :xtdby})
      (Thread/sleep 400)
      (to cmds {:type :move-left :cmd-id 1001 :bot-id :xtdby})
      (Thread/sleep 400)
      (to cmds {:type :move-left :cmd-id 1002 :bot-id :xtdby})
      (Thread/sleep 400)
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
      (is (= -2 (get-in @state [:xtdby :position])))

      (future-cancel @cmd-app)
      (future-cancel @event-app))))

;; (comment (to cmds
;;                    {:type :create
;;                     :cmd-id 1000
;;                     :bot-id :xtdby}
;;                    {:type :move-left
;;                     :cmd-id 1001
;;                     :bot-id :xtdby}
;;                    {:type :move-left
;;                     :cmd-id 1002
;;                     :bot-id :xtdby})
;;                (Thread/sleep 1000))


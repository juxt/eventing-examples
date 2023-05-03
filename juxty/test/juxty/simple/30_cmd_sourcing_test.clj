(ns juxty.simple.30-cmd-sourcing-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [juxty.simple.utils :refer [reset-bot]]
   [juxty.simple.25-event-sourcing :refer [events]]
   [juxty.simple.30-cmd-sourcing :as sut :refer [cmds left!]]))

(deftest command-sourcing-movement
  (let [juxty (ref {:position 0})]
    (testing "left"
      (reset-bot juxty)
      (is (= :success (left! juxty)))
      (is (= :success (left! juxty)))      
      (is (= -2 (:position @juxty)))
      (is (= [{:type :movement :delta -1}
              {:type :movement :delta -1}]
             @events))
      (is (= [{:type :move-left}
              {:type :move-left}]
             @cmds)))
    (testing
        "3 left commands result in 2 events and 3 commands, and we
        have a cmd log"
      (reset-bot juxty)
      (is (= :success (left! juxty)))
      (is (= :success (left! juxty)))
      (is (= :failure (left! juxty)))
      (is (= -2 (:position @juxty)))
      (is (= [{:type :move-left}
              {:type :move-left}
              {:type :move-left}]
             @cmds))
      (is (= [{:type :movement :delta -1}
              {:type :movement :delta -1}]
             @events)))))

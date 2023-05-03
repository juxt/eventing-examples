(ns juxty.simple.25-event-sourcing-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [juxty.simple.25-event-sourcing :as sut :refer [events left! right!]]
   [juxty.simple.utils :refer [reset-bot]]))

(deftest event-sourcing
  (let [juxty (ref {:position 0})]
    (testing "left"
      (reset-bot juxty)
      (is (= :success (left! juxty)))
      (is (= :success (left! juxty)))      
      (is (= -2 (:position @juxty)))
      (is (= [{:type :movement :delta -1}
              {:type :movement :delta -1}]
             @events)))
    (testing "right"
      (reset-bot juxty)
      (is (= :success (right! juxty)))
      (is (= 1 (:position @juxty))))
    (testing "3 commands result in 2 events"
      (reset-bot juxty)
      (is (= :success (left! juxty)))
      (is (= :success (left! juxty)))
      (is (= :failure (left! juxty)))
      (is (= -2 (:position @juxty)))
      (is (= [{:type :movement :delta -1}
              {:type :movement :delta -1}]
             @events)))))


(deftest event-sourcing-right
  (let [juxty (ref {:position 0})]
    (testing "3 commands run very quickly"
      (reset-bot juxty)
      (let [cmd1 (future (right! juxty 100))
            cmd2 (future (right! juxty 100))
            cmd3 (future (right! juxty 100))]
        (is (some #{:failure} [@cmd1 @cmd2 @cmd3])
            "One of the commands should fail as it breaks the positional invariant")
        (is (= 2 (:position @juxty))
            "The position should be 2")
        (is (= [{:type :movement :delta 1}
                   {:type :movement :delta 1}]
                  @events))))))

(ns juxty.simple.40-external-side-effects-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [juxty.simple.utils :refer [reset-bot random-walk]]
   [juxty.simple.25-event-sourcing :refer [events]]
   [juxty.simple.30-cmd-sourcing :refer [cmds]]
   [juxty.simple.40-external-side-effects :as sut :refer [left! right!]]))

(deftest side-effecting-movement
  (let [juxty (atom {:position 0})
        walk-length (+ 5 (rand-int 6))]
    (testing "combinations"
      (reset-bot juxty)
      (left! juxty)
      (right! juxty)
      (left! juxty)
      (is (= [{:type :move-left}
              {:type :move-right}
              {:type :move-left}] @cmds)))
    (testing "invariants"
      (reset-bot juxty)
      (dorun (random-walk left! right! juxty walk-length))
      (let [position (:position @juxty)]
        (is (= position
               (reduce + (map :delta @events))))
        (is (and (>= position -2)
                 (<= position 2)))
        (is (not= (count @cmds) (count @events))
            "May occasionally be true")))))

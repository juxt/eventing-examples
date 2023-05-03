(ns juxty.simple.50-observability-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [juxty.simple.utils :refer [reset-bot! random-walk halfway-time-helper]]
   [juxty.simple.20-event-sourcing :refer [events]]
   [juxty.simple.30-cmd-sourcing :refer [cmds]]
   [juxty.simple.40-external-side-effects :as external]
   [juxty.simple.50-observability :as sut :refer [left! right! hydrate position-at-time]]))


(deftest observability-movement
  (let [juxty (atom {:position 0
                     :id :the-eternal-bot})
        walk-length (+ 5 (rand-int 6))]
    (testing "combinations"
      (reset-bot! juxty)
      (left! juxty)
      (right! juxty)
      (left! juxty)
      (is (= [{:action :move-left}
              {:action :move-right}
              {:action :move-left}]
             (into [] (map (fn [x] (select-keys x [:action]))) @cmds))))
    (testing "invariants"
      (reset-bot! juxty)
      (dorun (random-walk left! right! juxty walk-length))
      (let [position (:position @juxty)]
        (is (= position
               (reduce + (map :delta @events))))
        (is (and (>= position -2)
                 (<= position 2)))))))

(deftest observability-movement-with-overrides
  (with-redefs [external/external-true-or-false (constantly true)]
    (let [juxty (atom {:position 0
                       :id :the-eternal-bot})]
      (testing "combinations"
        (reset-bot! juxty)
        (left! juxty :id 1001 :created-at 10000)
        (right! juxty :id 1002 :created-at 20000)
        (left! juxty :id 1003 :created-at 30000)
        (is (= [{:id 1001
                 :created-at 10000
                 :aggregate-id :the-eternal-bot
                 :action :move-left} 
                {:id 1002
                 :created-at 20000
                 :aggregate-id :the-eternal-bot
                 :action :move-right}
                {:id 1003
                 :created-at 30000
                 :aggregate-id :the-eternal-bot
                 :action :move-left}]
               @cmds))
        (is (= [{:process-id 1001
                 :aggregate-id :the-eternal-bot
                 :type :movement
                 :delta -1 } 
                {:process-id 1002
                 :aggregate-id :the-eternal-bot
                 :type :movement
                 :delta 1}
                {:process-id 1003
                 :aggregate-id :the-eternal-bot
                 :type :movement
                 :delta -1}]
               (into [] (map (fn [x] (select-keys x [:type :delta :process-id :aggregate-id])))
                     @events)))))))

(deftest hydration
  (let [juxty (atom {:position 0})
        walk-length (+ 10 (rand-int 10))]
    (testing "full hydration"
      (reset-bot! juxty)
      (dorun (random-walk left! right! juxty walk-length))
      (let [current-position (:position @juxty)]
        (hydrate juxty @events)
        (is (= current-position (:position @juxty)))))
    (testing "point-in-time hydration"
      (reset-bot! juxty)
      (dorun (random-walk left! right! juxty walk-length))
      (testing "First event"
        (hydrate juxty @events 1)
        (is (= (:position @juxty)  (-> @events first :delta))))
      (testing "5th event"
        (hydrate juxty @events 5)
        (is (= (:position @juxty)
               (reduce + (map :delta (take 5 @events)))))))))

;; temporal
(deftest temporal
  (let [juxty (atom {:position 0})
        walk-length (+ 10 (rand-int 10))]
    (testing "position-in-time-query"
      (reset-bot! juxty)
      (dorun (random-walk left! right! juxty walk-length))
      (let [halfway-time (halfway-time-helper)
            halfway-event-count (count
                                 (take-while
                                  (fn [e] (< (:created-at e) halfway-time))
                                  @events))]
        (hydrate juxty @events halfway-event-count)
        (is (= (:position @juxty)
             (position-at-time @events halfway-time)))))))


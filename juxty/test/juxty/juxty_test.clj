(ns juxty.juxty-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [juxty.juxty :as sut :refer [->Juxty ->JuxtyBounded ->JuxtyCommand
                                ->JuxtyCS ->JuxtyES ->JuxtySE ->JuxtySE' hydrate left
                                position-at-time right]]))

(deftest movement
  (let [juxty (->Juxty (atom 0))]
    (testing "left"
      (reset! (:position juxty) 0)
      (left juxty)
      (left juxty)
      (is (= -2 @(:position juxty))))
    (testing "right"
      (reset! (:position juxty) 0)
      (right juxty)
      (right juxty)
      (is (= 2 @(:position juxty))))
    (testing "identity"
      (reset! (:position juxty) 0)
      (left juxty)
      (right juxty)
      (is (= 0 @(:position juxty))))))

(deftest bounded-movement
  (let [juxty (->JuxtyBounded (atom 0))]
    (testing "left bound"
      (reset! (:position juxty) -2)
      (left juxty)
      (is (= -2 @(:position juxty))))
    (testing "right bound"
      (reset! (:position juxty) 2)
      (right juxty)
      (is (= 2 @(:position juxty))))))

(deftest command-movement
  (let [juxty (->JuxtyCommand (atom 0))]
    (testing "left"
      (reset! (:position juxty) 0)
      (is (= :success (left juxty)))
      (is (= -1 @(:position juxty))))
    (testing "right"
      (reset! (:position juxty) 0)
      (is (= :success (right juxty)))
      (is (= 1 @(:position juxty))))
    (testing "left-failure"
      (reset! (:position juxty) -2)
      (is (= :failure (left juxty)))
      (is (= -2 @(:position juxty))))))

(deftest event-sourcing-movement
  (let [juxty (->JuxtyES (atom 0) (atom []))]
    (testing "left"
      (reset! (:position juxty) 0)
      (is (= :success (left juxty)))
      (is (= :success (left juxty)))      
      (is (= -2 @(:position juxty)))
      (is (= [{:delta -1} {:delta -1}] @(:events juxty))))
    (testing "right"
      (reset! (:position juxty) 0)
      (is (= :success (right juxty)))
      (is (= 1 @(:position juxty))))
    (testing "3 commands result in 2 events"
      (reset! (:position juxty) 0)
      (reset! (:events juxty) [])
      (is (= :success (left juxty)))
      (is (= :success (left juxty)))
      (is (= :failure (left juxty)))
      (is (= -2 @(:position juxty)))
      (is (= [{:delta -1} {:delta -1}] @(:events juxty))))))

(deftest command-sourcing-movement
  (let [juxty (->JuxtyCS (atom 0) (atom []) (atom []))
        reset (fn [b]
                (reset! (:position b) 0)
                (reset! (:events b) [])
                (reset! (:cmds b) []))]
    (testing "left"
      (reset juxty)
      (is (= :success (left juxty)))
      (is (= :success (left juxty)))      
      (is (= -2 @(:position juxty)))
      (is (= [{:delta -1} {:delta -1}] @(:events juxty)))
      (is (= [{:type :move-left} {:type :move-left}] @(:cmds juxty))))
    (testing "right"
      (reset juxty)
      (is (= :success (right juxty)))
      (is (= 1 @(:position juxty)))
      (is (= [{:type :move-right}] @(:cmds juxty))))
    (testing "3 left commands result in 2 events and 3 commands"
      (reset juxty)
      (is (= :success (left juxty)))
      (is (= :success (left juxty)))
      (is (= :failure (left juxty)))
      (is (= -2 @(:position juxty)))
      (is (= [{:delta -1} {:delta -1}] @(:events juxty)))
      (is (= [{:type :move-left} {:type :move-left} {:type :move-left}] @(:cmds juxty))))))

;; no-longer-deterministic
(defn left-or-right []
  (if (= 0 (rand-int 2))
    left
    right))

(defn left-or-right-seq []
  (lazy-seq (cons (left-or-right) (left-or-right-seq))))

(defn random-walk
  ([b]
   (map (fn [f] (f b)) (left-or-right-seq)))
  ([b n]
   (take n (random-walk b))))

(defn reset [b]
  (reset! (:position b) 0)
  (reset! (:events b) [])
  (reset! (:cmds b) []))

(deftest side-effecting-movement
  (let [juxty (->JuxtySE (atom 0) (atom []) (atom []))
        walk-length (+ 5 (rand-int 6))]
    (testing "combinations"
      (reset juxty)
      (left juxty)
      (right juxty)
      (left juxty)
      (is (= [{:type :move-left} {:type :move-right} {:type :move-left}] @(:cmds juxty))))
    (testing "invariants"
      (reset juxty)
      (dorun (random-walk juxty walk-length))
      (let [position @(:position juxty)]
        (is (= position
               (reduce + (map :delta @(:events juxty)))))
        (is (and (>= position -2)
                 (<= position 2)))))))

(deftest observability-movement
  (let [juxty (->JuxtySE' (atom 0) (atom []) (atom []))
        walk-length (+ 5 (rand-int 6))]
    (testing "combinations"
      (reset juxty)
      (left juxty)
      (right juxty)
      (left juxty)
      (is (= [{:type :move-left} {:type :move-right} {:type :move-left}]
             (into [] (map (fn [x] (select-keys x [:type]))) @(:cmds juxty)))))
    (testing "invariants"
      (reset juxty)
      (dorun (random-walk juxty walk-length))
      (let [position @(:position juxty)]
        (is (= position
               (reduce + (map :delta @(:events juxty)))))
        (is (and (>= position -2)
                 (<= position 2)))))))

(deftest hydration
  (let [juxty (->JuxtySE' (atom 0) (atom []) (atom []))
        walk-length (+ 10 (rand-int 10))]
    (testing "full hydration"
      (reset juxty)
      (dorun (random-walk juxty walk-length))
      (let [current-position @(:position juxty)
            events @(:events juxty)]
        (reset! (:position juxty) 0)
        (hydrate juxty events)
        (is (= current-position @(:position juxty)))))
    (testing "point-in-time hydration"
      (reset juxty)
      (dorun (random-walk juxty walk-length))
      (let [events @(:events juxty)]
        (testing "First event"
          (hydrate juxty events 1)
          (is (= @(:position juxty)  (-> events first :delta))))
        (testing "5th event"
          (hydrate juxty events 5)
          (is (= @(:position juxty)
                 (reduce + (map :delta (take 5 events))))))))))

;; temporal
(defn halfway-time-helper [b]
  (let [events @(:events b)
        start-time (-> events
                       first
                       :created-at)
        end-time (-> events
                     last
                     :created-at)]
    (+ start-time
       (/ (- end-time start-time) 2))))

(deftest temporal
  (let [juxty (->JuxtySE' (atom 0) (atom []) (atom []))
        walk-length (+ 10 (rand-int 10))]
    (testing "position-in-time-query"
      (reset juxty)
      (dorun (random-walk juxty walk-length))
      (let [events @(:events juxty)
            halfway-time (halfway-time-helper juxty)
            halfway-event-count (count
                                 (take-while
                                  (fn [e] (< (:created-at e) halfway-time))
                                  events))]
        (hydrate juxty events halfway-event-count)
        (is (= @(:position juxty)
             (position-at-time juxty halfway-time)))))))

(ns juxty.juxty-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [juxty.juxty :as sut :refer [cmds events hydrate hydrate left! left-cqs!
                                left-cmd! left-es! left-cmd!' position-at-time
                                position-at-time random-walk reset-bot! right-es! right-cmd!']]))

(deftest juxty
  (testing
      "The functions 'left' and 'right' return the new state.  As an
      atom is used to represent the global state, 'left' and 'right'
      wrap the atom access for us.  However, these functions are
      side-effecing thus we lose referentially transparent.  CQS says
      that we shouldn't really treat them as functions anymore."
      (let [juxty (atom {:position 0})]
        (testing "Move Juxty Left"
          (is (= {:position -1} (left! juxty)))
          (is (= {:position -2} (left! juxty)))
          (is (= -2 (:position @juxty))))
        (testing "Hitting the boundary"
          (reset! juxty {:position -2})
          (is (= {:position -2} (left! juxty)))
          (is (= -2 (:position @juxty)))))))

(deftest juxty-command
  (testing
      "We define left-cmd and right-cmd as commands that perform a
      movement action.  Whilst in Clojure they are defined as
      functions it is better to making the mental leap to treat them
      as commands i.e operations over mutable global state.  If we
      wish to know the state we have to separately query it.  With
      regard to Commands not generating a reponse CQS is treated as
      guideline and we do allow the return value of the command to
      indicate details of the execution.  These might include
      validation, exceptions, warnings but in our case the returns are
      a simple :success or :failure."
    (let [juxty (atom {:position 0})]
        (testing "Move Juxty left"
          (is (= :success (left-cqs! juxty)))
          (is (= -1 (:position @juxty))))
        (testing "Hitting the boundary"
          (reset! juxty {:position -2})
          (is (= :failure (left-cqs! juxty)))
          (is (= -2 (:position @juxty)))))))

#_(deftest juxty-command-ids
  (testing
      "One other place where one may further relax the CQS constraint
      of no response is in returning ids for created objects.
      Competing requirements influence the design decisions around the
      generation of ids."
      (let [juxty (atom (->Juxty 0))]
        (testing "Command to move Juxty Left"
          (is (= :success (left-cmd juxty)))
          (is (= -1 (:position @juxty))))
        (testing "left-failure"
          (reset! juxty (->Juxty -2))
          (is (= :failure (left-cmd juxty)))
          (is (= -2 (:position @juxty)))))))

(deftest event-sourcing
  (let [juxty (atom {:position 0})]
    (testing "left"
      (reset! juxty {:position 0})
      (reset! events [])
      (is (= :success (left-es! juxty)))
      (is (= :success (left-es! juxty)))      
      (is (= -2 (:position @juxty)))
      (is (= [{:type :movement
               :delta -1}
              {:type :movement
               :delta -1}] @events)))
    (testing "right"
      (reset! juxty {:position 0})
      (reset! events [])
      (is (= :success (right-es! juxty)))
      (is (= 1 (:position @juxty))))
    (testing "3 commands result in 2 events"
      (reset! juxty {:position 0})
      (reset! events [])
      (is (= :success (left-es! juxty)))
      (is (= :success (left-es! juxty)))
      (is (= :failure (left-es! juxty)))
      (is (= -2 (:position @juxty)))
      (is (= [{:type :movement
               :delta -1}
              {:type :movement
               :delta -1}]
             @events)))))

(deftest command-sourcing-movement
  (let [juxty (atom {:position 0})]
    (testing "left"
      (reset-bot! juxty)
      (is (= :success (left-cmd! juxty)))
      (is (= :success (left-cmd! juxty)))      
      (is (= -2 (:position @juxty)))
      (is (= [{:type :movement
               :delta -1}
              {:type :movement
               :delta -1}]
             @events))
      (is (= [{:type :move-left}
              {:type :move-left}]
             @cmds)))
    (testing "3 left commands result in 2 events and 3 commands"
      (reset-bot! juxty)
      (is (= :success (left-cmd! juxty)))
      (is (= :success (left-cmd! juxty)))
      (is (= :failure (left-cmd! juxty)))
      (is (= -2 (:position @juxty)))
      (is (= [{:type :movement
               :delta -1}
              {:type :movement
               :delta -1}]
             @events))
      (is (= [{:type :move-left}
              {:type :move-left}
              {:type :move-left}]
             @cmds)))))

;; no-longer-deterministic
(deftest side-effecting-movement
  (let [juxty (atom {:position 0})
        walk-length (+ 5 (rand-int 6))]
    (testing "combinations"
      (reset-bot! juxty)
      (left-cmd!' juxty)
      (right-cmd!' juxty)
      (left-cmd!' juxty)
      (is (= [{:type :move-left}
              {:type :move-right}
              {:type :move-left}] @cmds)))
    (testing "invariants"
      (reset-bot! juxty)
      (dorun (sut/random-walk juxty walk-length))
      (let [position (:position @juxty)]
        (is (= position
               (reduce + (map :delta @events))))
        (is (and (>= position -2)
                 (<= position 2)))))))

(deftest hydration
  (let [juxty (atom {:position 0})
        walk-length (+ 10 (rand-int 10))]
    (testing "full hydration"
      (reset-bot! juxty)
      (dorun (random-walk juxty walk-length))
      (let [current-position (:position @juxty)]
        (swap! juxty assoc :position 0)
        (hydrate juxty @events)
        (is (= current-position (:position @juxty)))))
    (testing "point-in-time hydration"
      (reset-bot! juxty)
      (dorun (random-walk juxty walk-length))
      (testing "First event"
        (swap! juxty assoc :position 0)
        (hydrate juxty @events 1)
        (is (= (:position @juxty)  (-> @events first :delta))))
      (testing "5th event"
        (swap! juxty assoc :position 0)
        (hydrate juxty @events 5)
        (is (= (:position @juxty)
               (reduce + (map :delta (take 5 @events)))))))))


;; timestamps and command ids
#_(deftest observability-movement
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

;; temporal
#_(defn halfway-time-helper [b]
  (let [events @(:events b)
        start-time (-> events
                       first
                       :created-at)
        end-time (-> events
                     last
                     :created-at)]
    (+ start-time
       (/ (- end-time start-time) 2))))

#_(deftest temporal
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


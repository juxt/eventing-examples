(ns mocka.mocka-test
  (:require [mocka.mocka :as sut :refer [producer consumer to from peek builder]]
            [clojure.test :as t :refer [deftest is testing]]))

(deftest producer-test
  (let [events-topic (atom [])
        event-producer (producer events-topic)]
    ;; send events individually
    (event-producer 1)
    (event-producer 2)
    (event-producer 3)
    (is (= [1 2 3] @events-topic))))

(deftest consumer-test
  (let [events-topic (atom [])
        event-producer (producer events-topic)]
    (event-producer 1 2 3)
    (testing "single consumer"
      ;; consumer off the topic 1-by-1
      (let [event-consumer (consumer events-topic)]
        (is (= 1 (event-consumer)))
        (is (= 2 (event-consumer)))
        (is (= 3 (event-consumer)))
        (is (nil? (event-consumer)))))
    (testing "secondary consumer with an initial offset"
      (let [second-consumer (consumer events-topic 2)]
        (is (= 3 (second-consumer)))
        (is (nil? (second-consumer)))))))

(deftest topic-configuration-test
  (let [events-topic (atom [])
        events {:topic events-topic
                :producer (producer events-topic)
                :consumer (consumer events-topic 0)}]
    (to events 1 2 3)
    (testing "to and from a topic configuration"
      (is (= 1 (from events)))
      (is (= 2 (from events)))
      (is (= 3 (from events)))
      (is (nil? (from events))))))

(deftest builder-test
  (let [;; Create the topics
        events-topic (atom [])
        outputs-topic-1 (atom [])
        outputs-topic-2 (atom [])
        ;; Create the topic configurations for the different apps
        events-1 {:topic events-topic
                  :consumer (consumer events-topic 0)
                  :producer (producer events-topic)}
        events-2 {:topic events-topic
                  :consumer (consumer events-topic 0)
                  :producer (producer events-topic)}
        outputs-1 {:topic outputs-topic-1
                   :producer (producer outputs-topic-1)}
        outputs-2 {:topic outputs-topic-2
                   :producer (producer outputs-topic-2)}
        ;; Create an app that listens on the events topic and doubles to outputs-1
        app1 (builder [events events-1
                       outputs outputs-1]
                      (some->> (from events)
                               (* 2)
                               (peek "Double: ")
                               (to outputs-1)))
        ;; Create an app that listens on the events topic (different consumer) and triples to outputs-2
        app2 (builder [events events-2
                       outputs outputs-2]
                      (some->> (from events)
                               (* 3)
                               (peek "Triple: ")
                               (to outputs)))]
    (to events-1 1 2 3)
    (Thread/sleep 1000)
    (testing "Transformation"
      (is (= [2 4 6] @outputs-topic-1))
      (is (= [3 6 9] @outputs-topic-2)))
    (future-cancel @app1)
    (future-cancel @app2)))

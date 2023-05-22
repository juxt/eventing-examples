(ns juxty.ddd.domain-test
  (:require [juxty.ddd.domain :as sut]
            [clojure.test :refer [deftest is use-fixtures]]))

(defn _uuids [] (lazy-seq (cons (random-uuid) (_uuids))))
(def uuids (doall (take 100 (_uuids))))

(defn next-uuid [offset]
  (let [n (atom offset)]
    (fn []
      (let [uuid (nth uuids @n)]
        (swap! n inc)
        uuid))))

(defn uuid-fixture [body]
  (with-redefs [random-uuid (next-uuid 0)]
    (body)))

(defn repo-fixture [body]
  (sut/reset-repo!)
  (body))

(use-fixtures :each uuid-fixture repo-fixture)

(deftest create-bot-test
  (is (= {:id (nth uuids 0)
          :upper :arms
          :lower :wheels
          :version 0
          :created-at-repo-version 0}
         (sut/create-bot :arms :wheels)))
  (is (nil? (sut/create-bot :legs :wheels)))
  (is (nil? (sut/create-bot :legs :legs)))
  (is (nil? (sut/create-bot :foo :bar))))

(deftest saving-valid-bots-to-repo-test
  (sut/save-to-repo (sut/create-bot :arms :legs))           ;; 0
  (sut/save-to-repo (sut/create-bot :tentacles :tracks))    ;; 1
  (sut/save-to-repo (sut/create-bot :manipulators :wheels)) ;; 2
  (is (= {:aggregates {(nth uuids 0) {:id (nth uuids 0)
                                      :upper :arms
                                      :lower :legs
                                      :created-at-repo-version 0
                                      :version 0}
                       (nth uuids 1) {:id (nth uuids 1)
                                      :upper :tentacles
                                      :lower :tracks
                                      :created-at-repo-version 1
                                      :version 0}
                       (nth uuids 2) {:id (nth uuids 2)
                                      :upper :manipulators
                                      :lower :wheels
                                      :created-at-repo-version 2
                                      :version 0}}
          :version 3}
         @sut/repo)))

(deftest checking-body-invariant-holds-test
  (some-> (sut/create-bot :arms :legs)
          sut/save-to-repo)
  (is (nil? (some-> (sut/create-bot :arms :legs)
                    sut/save-to-repo)))
  (is (= {:aggregates {(nth uuids 0) {:id (nth uuids 0)
                                      :upper :arms
                                      :lower :legs
                                      :created-at-repo-version 0
                                      :version 0}}
          :version 1}
         @sut/repo)))

(deftest give-nickname-test
  (let [bot (sut/create-bot :arms :legs)]
    (is (= {:id (nth uuids 0)
            :upper :arms
            :lower :legs
            :created-at-repo-version 0
            :nickname "Juxty"
            :version 1}
           (-> bot
               (sut/set-nickname "Juxty"))))
    (is (= {:id (nth uuids 0)
            :upper :arms
            :lower :legs
            :created-at-repo-version 0
            :nickname "Tripy"
            :version 2}
           (-> bot
               (sut/set-nickname "Juxty")
               (sut/set-nickname "Tripy"))))
    (is (nil? (-> bot (sut/set-nickname 12345))))))

(deftest creation-consistency-test
  (let [bot1 (sut/create-bot :arms :legs) ;; different ids same body
        bot2 (sut/create-bot :arms :legs)]
    (sut/save-to-repo bot1)
    (sut/save-to-repo bot2) ;; doesn't get saved
    (is (= {:aggregates {(nth uuids 0) {:id (nth uuids 0)
                                        :upper :arms
                                        :lower :legs
                                        :created-at-repo-version 0
                                        :version 0}}
            :version 1}
           @sut/repo))))

(deftest updating-consistency-test
  (let [bot0 (sut/create-bot :arms :legs)
        bot1 (sut/set-nickname bot0 "Juxty")
        bot2 (sut/set-nickname bot0 "XTDBY")]
    (sut/save-to-repo bot0)
    (sut/save-to-repo bot2)
    (sut/save-to-repo bot1) ;; does nothing
    (is (= {:aggregates {(nth uuids 0) {:id (nth uuids 0)
                                        :upper :arms
                                        :lower :legs
                                        :nickname "XTDBY"
                                        :created-at-repo-version 0
                                        :version 1}}
            :version 1}
           @sut/repo))))

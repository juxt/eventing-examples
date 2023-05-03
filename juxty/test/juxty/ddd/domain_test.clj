(ns juxty.ddd.domain-test
  (:require [juxty.ddd.domain :as sut]
            [clojure.test :refer [deftest testing is use-fixtures]]))

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

(deftest create-bot
  (is (= {:id (nth uuids 0)
          :upper :arms
          :lower :wheels
          :repo-version 0}
         (sut/create-bot :arms :wheels)))
  (is (nil? (sut/create-bot :legs :wheels)))
  (is (nil? (sut/create-bot :legs :legs)))
  (is (nil? (sut/create-bot :foo :bar))))

(deftest saving-valid-bots-to-repo
  (sut/save-to-repo (sut/create-bot :arms :legs))           ;; 0
  (sut/save-to-repo (sut/create-bot :tentacles :tracks))    ;; 1
  (sut/save-to-repo (sut/create-bot :manipulators :wheels)) ;; 2
  (is (= {(nth uuids 0) {:id (nth uuids 0)
                         :upper :arms
                         :lower :legs
                         :repo-version 0}
          (nth uuids 1) {:id (nth uuids 1)
                         :upper :tentacles
                         :lower :tracks
                         :repo-version 1}
          (nth uuids 2) {:id (nth uuids 2)
                         :upper :manipulators
                         :lower :wheels
                         :repo-version 2}
          :version 3}
         @sut/repo)))

(deftest checking-body-invariant-holds
  (some-> (sut/create-bot :arms :legs)
          sut/save-to-repo)
  (is (nil? (some-> (sut/create-bot :arms :legs)
                    sut/save-to-repo)))
  (is (= {(nth uuids 0) {:id (nth uuids 0)
                         :upper :arms
                         :lower :legs
                         :repo-version 0}
          :version 1}
         @sut/repo)))

(deftest consistency
  (let [bot1 (sut/create-bot :arms :legs)
        bot2 (sut/create-bot :arms :legs)]
    (sut/save-to-repo bot1)
    (sut/save-to-repo bot1)
    (sut/save-to-repo bot2)
    (is (= {(nth uuids 0) {:id (nth uuids 0)
                           :upper :arms
                           :lower :legs
                           :repo-version 0}
            :version 1}
           @sut/repo))))

(ns juxty.ddd.domain
  (:require [clojure.spec.alpha :as s]))

;; Repository
(def repo (atom {:version 0}))
(defn reset-repo! [] (reset! repo {:version 0}))

(defn get-by-id
  [id]
  (get @repo id))

(defn select-keys-from-repo
  [repo keys]
  (for [val (vals (dissoc repo :version))]
    (select-keys val keys)))

(defn save-to-repo [aggregate]
  (when (== (:version @repo) (:repo-version aggregate))
    (swap! repo (fn [repo]
                  (assoc repo
                         (:id aggregate) aggregate
                         :version (inc (:version repo)))))))

;; Bot Root Aggregate
(s/def :bot/id uuid?)
(s/def :bot/upper #{:arms :tentacles :manipulators})
(s/def :bot/lower #{:wheels :tracks :legs})
(s/def :bot/repo-version number?)
(s/def :bot/aggregate
  (s/keys :req-un [:bot/id :bot/upper :bot/lower]))

(defn equal-body? [b1 b2]
  (and (= (:lower b1) (:lower b2))
       (= (:upper b1) (:upper b2))))

(defn create-bot
  [upper lower]
  (let [repo @repo
        bot (s/conform :bot/aggregate {:id (random-uuid)
                                       :upper upper
                                       :lower lower
                                       :repo-version (:version repo)})
        bodies (select-keys-from-repo repo [:upper :lower])]
    
    (when (and (not (s/invalid? bot))
               (not-any? (partial equal-body? bot) bodies))
      bot)))

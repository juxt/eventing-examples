(ns juxty.ddd.domain
  (:require [clojure.spec.alpha :as s]))

;; Repository
(def repo (atom {:version 0
                 :aggregates {}}))
(defn reset-repo! [] (reset! repo {:version 0
                                   :aggregates {}}))

(defn get-by-id
  [id]
  (get (:aggregates @repo) id))

(defn select-keys-from-repo
  "Returns in the first position the sequence of selected value objects/entities across all aggregates, and in
  the second position the version of the repo at the time of retrieval."
  [keys]
  (let [repo @repo]
    [(for [val (vals (:aggregates repo))] (select-keys val keys))
     (:version repo)]))

(defn save-to-repo [aggregate]
  (when (== (:version @repo) (:created-at-repo-version aggregate))
    (swap! repo (fn [repo]
                  (-> repo
                      (assoc-in [:aggregates (:id aggregate)] aggregate)
                      (assoc :version (inc (:version repo))))))))

;; Aggregates

(s/def :bot/id uuid?)
(s/def :bot/upper #{:arms :tentacles :manipulators})
(s/def :bot/lower #{:wheels :tracks :legs})
(s/def :bot/created-at-repo-version number?)
(s/def :bot/aggregate
  (s/keys :req-un [:bot/id :bot/upper :bot/lower]))

(defn equal-body? [b1 b2]
  (and (= (:lower b1) (:lower b2))
       (= (:upper b1) (:upper b2))))

(defn create-bot
  [upper lower]
  (let [[bodies version] (select-keys-from-repo [:upper :lower])
        bot (s/conform :bot/aggregate {:id (random-uuid)
                                       :upper upper
                                       :lower lower
                                       :created-at-repo-version version})]
    (when (and (not (s/invalid? bot))
               (not-any? (partial equal-body? bot) bodies))
      bot)))

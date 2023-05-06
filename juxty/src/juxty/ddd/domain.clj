(ns juxty.ddd.domain
  (:require
   [clojure.spec.alpha :as s]))

;; Repository
(def repo (atom {:version 0
                 :aggregates {}}))

(defn reset-repo! [] (reset! repo {:version 0
                                   :aggregates {}}))

(defn get-by-id
  [id]
  (get (:aggregates @repo) id))

(defn select-keys-from-repo
  "Returns in the first position the sequence of selected value objects/entities 
  across all aggregates, and in the second position the version of the repo at the
  time of retrieval."
  [keys]
  (let [repo @repo]
    [(for [val (vals (:aggregates repo))] (select-keys val keys))
     (:version repo)]))

(defn save-to-repo [aggregate]
  (swap! repo
         (fn [repo]
           (let [id (:id aggregate)
                 aggregates (:aggregates repo)
                 repo-version (:version repo)
                 aggregate-version (:version aggregate)]
             (cond
               ;; create
               (and (some? aggregate)
                    (not (contains? aggregates id))
                    (== repo-version (:created-at-repo-version aggregate)))
               (-> repo
                   (assoc-in [:aggregates id] aggregate)
                   (assoc :version (inc (:version repo))))
               ;; update
               (and (contains? aggregates id)
                    (== (get-in aggregates [id :version]) (dec aggregate-version)))
               (-> repo
                   (assoc-in [:aggregates id] aggregate))
               :else
               repo)))))

;; Aggregates
(s/def :bot/id uuid?)
(s/def :bot/upper #{:arms :tentacles :manipulators})
(s/def :bot/lower #{:wheels :tracks :legs})
(s/def :bot/nickname string?)
(s/def :bot/version number?)
(s/def :bot/created-at-repo-version number?)
(s/def :bot/aggregate
  (s/keys :req-un [:bot/id :bot/upper :bot/lower
                   :bot/version :bot/created-at-repo-version]
          :opt-un [:bot/nickname]))

(defn- equal-body? [b1 b2]
  (and (= (:lower b1) (:lower b2))
       (= (:upper b1) (:upper b2))))

(defn create-bot
  "Constructor"
  [upper lower]
  (let [[bodies version] (select-keys-from-repo [:upper :lower])
        bot (s/conform :bot/aggregate {:id (random-uuid)
                                       :upper upper
                                       :lower lower
                                       :created-at-repo-version version
                                       :version 0})]
    (when (and (not (s/invalid? bot))
               (not-any? (partial equal-body? bot) bodies)) ;; query to repo if body combination exists
      bot)))


(defn give-nickname
  "Method"
  [bot nickname] ;; bot = this
  (let [bot (s/conform :bot/aggregate (some-> bot
                                              (assoc :nickname nickname)
                                              (update :version inc)))]
    (when (not (s/invalid? bot)) 
      bot)))

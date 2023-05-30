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

(defn save-to-repo [aggregate]
  (swap! repo
         (fn [repo]
           (let [id (:id aggregate)
                 aggregates (:aggregates repo)
                 aggregate-version (:version aggregate)]
             (cond
               ;; create
               (and (some? aggregate)
                    (not (contains? aggregates id)))
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

;; Aggregate
(s/def :bot/id uuid?)
(s/def :bot/upper #{:arms :tentacles :manipulators})
(s/def :bot/lower #{:wheels :tracks :legs})
(s/def :bot/nickname string?)
(s/def :bot/version number?)
(s/def :bot/aggregate
  (s/keys :req-un [:bot/id :bot/upper :bot/lower
                   :bot/version]
          :opt-un [:bot/nickname]))

(defn human-likeness [b]
  (and (= :arms (:upper b))
       (= :legs (:lower b))))

(defn create-bot
  [upper lower]
  (let [bot (s/conform :bot/aggregate {:id (random-uuid)
                                       :upper upper
                                       :lower lower
                                       :version 0})]
    (when (and (not (s/invalid? bot))
               (not (human-likeness bot))) 
      bot)))

(defn set-nickname
  [bot nickname] ;; bot = this
  (let [bot (s/conform :bot/aggregate (some-> bot
                                              (assoc :nickname nickname)
                                              (update :version inc)))]
    (when (not (s/invalid? bot)) 
      bot)))

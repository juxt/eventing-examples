(ns juxty.juxty)

;; Single Bot state of position
;; {:position 0}

(defn move-left [b]
  (update b :position dec))

(defn move-right [b]
  (update b :position inc))

(defn left! [b]
  (if (> (:position @b) -2)
    (swap! b move-left)
    @b))

(defn right! [b]
  (if (< (:position @b) 2)
    (swap! b move-right)
    @b))

(defn left-cmd! [b]
  (if (> (:position @b) -2)
    (do (swap! b move-left)
        :success)
    :failure))

(defn right-cmd! [b]
  (if (< (:position @b) 2)
    (do (swap! b move-right)
        :success)
    :failure))

(defn create-bot-cmd
  []
  (atom {:position 0}))

;; Event Sourcing
(def events (atom []))

(defn to-events
  [event]
  (swap! events conj event)
  event)

(defn apply-event
  [b event]
  (let [delta (:delta event)]
    (swap! b update :position (partial + delta))
    :success))

(defn left-es! [b]
  (if (> (:position @b) -2)
    (->> {:type :movement
          :delta -1}
         to-events
         (apply-event b))
    :failure))

(defn right-es! [b]
  (if (< (:position @b) 2)
    (->> {:type :movement
          :delta 1}
         to-events
         (apply-event b))
    :failure))

;; Command Sourcing
(def cmds (atom []))

(defn to-cmds
  [cmd]
  (swap! cmds conj cmd)
  cmd)

(defn apply-cmd
  [b cmd]
  (case (:type cmd)
    :move-left
    (if (> (:position @b) -2)
      (->> {:type :movement
            :delta -1}
           to-events
           (apply-event b))
      :failure)
    :move-right
    (if (< (:position @b) 2)
      (->> {:type :movement
            :delta 1}
           to-events
           (apply-event b))
      :failure)))

(defn left-cs! [b]
  (->> {:type :move-left}
       to-cmds
       (apply-cmd b)))

(defn right-cs! [b]
  (->> {:type :move-right}
       to-cmds
       (apply-cmd b)))

(defn reset-bot! [b]
  (reset! b {:position 0})
  (reset! events [])
  (reset! cmds []))

;; Side effecting
(defn external-true-or-false []
  (-> (slurp (str "https://www.random.org/integers/?num=1&min=" 0
                  "&max=" 1
                  "&col=1&base=10&format=plain&rnd=new"))
      clojure.string/trim
      Integer.
      (= 1)))

(defn apply-se-cmd
  [b cmd]
  (case (:type cmd)
    :move-left
    (if (and (external-true-or-false)
             (> (:position @b) -2))
      (->> {:type :movement
            :delta -1}
           to-events
           (apply-event b))
      :failure)
    :move-right
    (if (and (external-true-or-false)
             (< (:position @b) 2))
      (->> {:type :movement
            :delta 1}
           to-events
           (apply-event b))
      :failure)))

(defn left-se! [b]
  (->> {:type :move-left}
       to-cmds
       (apply-se-cmd b)))

(defn right-se! [b]
  (->> {:type :move-right}
       to-cmds
       (apply-se-cmd b)))

(defn left-or-right []
  (if (= 0 (rand-int 2))
    left-se!
    right-se!))

(defn left-or-right-seq []
  (lazy-seq (cons (left-or-right) (left-or-right-seq))))

(defn random-walk
  ([b]
   (map (fn [f] (f b)) (left-or-right-seq)))
  ([b n]
   (take n (random-walk b))))

;; Hydration
(defn hydrate
  ([b events]
   (run! (fn [e] (apply-event b e)) events))
  ([b events n]
   (hydrate b (take n events))))

  ;; Temporal Queries
(defn position-at-time
  [b time]
  (let [events @(:events b)]
    (reduce + 
            (map :delta
                 (take-while (fn [e] (< (:created-at e) time)) events)))))
(comment

  ;; Observability

  (defn new-event'
    [events event]
    (swap! events conj (merge {:event-id (random-uuid)
                               :created-at (System/currentTimeMillis)}
                              event))
    event)

  (defn new-cmd'
    [cmds cmd]
    (swap! cmds conj (merge {:cmd-id (random-uuid)
                             :created-at (System/currentTimeMillis)}
                            cmd))
    cmd)

  (defn apply-cmd-se'
    [b cmd]
    (let [position (:position b)
          events (:events b)
          type (:type cmd)
          id (:id cmd)]
      (case type
        :move-left
        (if (and (> @position -2)
                 (external-true-or-false))
          (->> (new-event' events {:type :movement
                                   :delta -1
                                   :originating-cmd-id id})
               (apply-event b))
          :failure)
        :move-right
        (if (and (< @position 2)
                 (external-true-or-false))
          (->> (new-event' events {:type :movement
                                   :delta 1
                                   :originating-cmd-id id})
               (apply-event b))
          :failure))))

  (defrecord JuxtySE' [position cmds events]
    Bot
    (move-left [b] (->> (new-cmd' cmds {:type :move-left
                                        :id (random-uuid)})
                        (apply-cmd-se' b)))
    (move-right [b] (->> (new-cmd' cmds {:type :move-right
                                         :id (random-uuid)})
                         (apply-cmd-se' b))))

  
  )

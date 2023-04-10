(ns juxty.juxty)

(defprotocol Bot
  "Bot commands"
  (left [b] "Move left")
  (right [b] "Move right")
  (instantiate [b] "Instantiate"))

;; Trivial Implementation
(defrecord Juxty [position]
  ;; Position is an atom to maintain the positional state
  Bot
  (left [b] (update b :position (fn [x] (swap! x dec))))
  (right [b] (update b :position (fn [x] (swap! x inc)))))

;; Bounded evolution
;; always success
(defn binc
  "bounded inc"
  [x]
  (if (>= x 2)
    x
    (inc x)))

(defn bdec
  "bounded inc"
  [x]
  (if (<= x -2)
    x
    (dec x)))

(defrecord JuxtyBounded [position]
  Bot
  (left [b] (update b :position (fn [x] (swap! x bdec))))
  (right [b] (update b :position (fn [x] (swap! x binc)))))

;; Command evolution
;; Introducing command failure
(defrecord JuxtyCommand [position]
  Bot
  (left [b] (let [position (:position b)]
              (if (> @position -2)
                (do (swap! position dec)
                    :success)
                :failure)))
  (right [b] (let [position (:position b)]
               (if (< @position 2)
                 (do (swap! position inc)
                     :success)
                 :failure))))

;; Event Sourcing
(defn new-event
  [events event]
  (swap! events conj event)
  event)

(defn apply-event
  [b event]
  (let [position (:position b)
        delta (:delta event)]
    (swap! position (fn [x] (+ x delta)))
    :success))

(defrecord JuxtyES [position events]
  Bot
  (left [b] (let [position (:position b)]
              (if (<= @position -2)
                :failure
                (->> (new-event events {:delta -1})
                     (apply-event b)))))
  (right [b] (let [position (:position b)]
              (if (>= @position 2)
                :failure
                (->> (new-event events {:delta 1})
                     (apply-event b))))))

;; Command Sourcing
(def new-cmd new-event)

(defn apply-cmd
  [b cmd]
  (let [position (:position b)
        events (:events b)
        type (:type cmd)]
    (case type
      :move-left
      (if (> @position -2)
        (->> (new-event events {:delta -1})
             (apply-event b))
        :failure)
      :move-right
      (if (< @position 2)
        (->> (new-event events {:delta 1})
             (apply-event b))
        :failure))))

(defrecord JuxtyCS [position cmds events]
  Bot
  (left [b] (->> (new-cmd cmds {:type :move-left})
                 (apply-cmd b)))
  (right [b] (->> (new-cmd cmds {:type :move-right})
                 (apply-cmd b))))

;; Side effecting
(defn external-true-or-false []
  (-> (slurp (str "https://www.random.org/integers/?num=1&min=" 0
                  "&max=" 1
                  "&col=1&base=10&format=plain&rnd=new"))
      clojure.string/trim
      Integer.
      (= 1)))

(defn apply-cmd-se
  [b cmd]
  (let [position (:position b)
        events (:events b)
        type (:type cmd)]
    (case type
      :move-left
      (if (and (> @position -2)
               (external-true-or-false))
        (->> (new-event events {:delta -1})
             (apply-event b))
        :failure)
      :move-right
      (if (and (< @position 2)
               (external-true-or-false))
        (->> (new-event events {:delta 1})
             (apply-event b))
        :failure))))

(defrecord JuxtySE [position cmds events]
  Bot
  (left [b] (->> (new-cmd cmds {:type :move-left})
                 (apply-cmd-se b)))
  (right [b] (->> (new-cmd cmds {:type :move-right})
                 (apply-cmd-se b))))

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
  (left [b] (->> (new-cmd' cmds {:type :move-left
                                 :id (random-uuid)})
                 (apply-cmd-se' b)))
  (right [b] (->> (new-cmd' cmds {:type :move-right
                                  :id (random-uuid)})
                 (apply-cmd-se' b))))

;; Hydration
(defn hydrate
  ([b events]
   (reset! (:position b) 0) ;; for convenience
   (dorun (map (fn [e] (apply-event b e))
               events)))
  ([b events n]
   (hydrate b (take n events))))

;; Temporal Queries
(defn position-at-time
  [b time]
  (let [events @(:events b)]
    (reduce + 
            (map :delta
                 (take-while (fn [e] (< (:created-at e) time)) events)))))

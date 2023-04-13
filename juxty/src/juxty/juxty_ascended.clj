(ns juxty.juxty-ascended
  (:require
   [clojure.string]))

;; command handler
(defn bot-found?
  [state bot-id]
  (boolean (get @state bot-id)))

(defn bot-not-found?
  [state bot-id]
  ((complement bot-found?) state bot-id))

(defn get-bot-position
  [state bot-id]
  (get-in @state [bot-id :position]))

(defn external-fail?
  []
  (-> (slurp (str "https://www.random.org/integers/?num=1&min=" 0
                  "&max=" 1
                  "&col=1&base=10&format=plain&rnd=new"))
      clojure.string/trim
      Integer.
      (= 1)))

(defn ->cmd-response
  [m]
  (merge 
   {:cmd-response-id (random-uuid)
    :created-at (System/currentTimeMillis)}
   m))

(defn ->bot-event
  [event]
  (merge {:event-id (random-uuid)
          :created-at (System/currentTimeMillis)}
         event))

(defn create-bot
  [state {:keys [bot-id position created-at] :as _event}]
  (swap! state assoc bot-id {:bot-id bot-id
                             :position position
                             :created-at created-at}))

(defn move-bot
  [state {:keys [bot-id delta] :as _event}]
  (swap! state update-in [bot-id :position] (fn [x] (+ x delta))))

(defn state-or
  [f s1 s2 v]
  (or (f s1 v)
      (f s2 v)))

(defn state-and
  [f s1 s2 v]
  (and (f s1 v)
      (f s2 v)))

(defn bot-cmd-handler
  [cmd pending-state state producer]
  (let [{:keys [type cmd-id bot-id]} cmd]
    (if (external-fail?)
      (->cmd-response {:status :failure
                       :originating-cmd-id cmd-id
                       :error [:external-service-says-no]})
      (case type
        :create
        (if (state-or bot-found? pending-state state bot-id)
          (->cmd-response {:status :failure
                           :originating-cmd-id cmd-id
                           :error [:bot-already-found bot-id]})
          (let [event (->bot-event {:type :creation
                                    :bot-id bot-id
                                    :position 0
                                    :originating-cmd-id cmd-id})]
            
            (when (producer event)
              (create-bot pending-state event)
              (->cmd-response {:status :success
                               :originating-cmd-id cmd-id}))))
        :move-left
        (cond
          (state-and bot-not-found? pending-state state bot-id)
          (->cmd-response {:status :failure
                           :originating-cmd-id cmd-id
                           :error [:bot-not-found bot-id]})
          (<= (state-or get-bot-position pending-state state bot-id)
              -2)
          (->cmd-response {:status :failure
                           :originating-cmd-id cmd-id
                           :error [:out-of-bounds :left]})
          :else
          (let [event (->bot-event {:type :movement
                                    :bot-id bot-id
                                    :delta -1
                                    :originating-cmd-id cmd-id})]
            (when (producer event)
              (move-bot pending-state event)
              (->cmd-response {:status :success
                               :originating-cmd-id cmd-id}))))
        :move-right
        (cond
          (state-and bot-not-found? pending-state state bot-id)
          (->cmd-response {:status :failure
                           :originating-cmd-id cmd-id
                           :error [:bot-not-found bot-id]})            
          (>= (state-or get-bot-position pending-state state bot-id) 2)
          (->cmd-response {:status :failure
                           :originating-cmd-id cmd-id
                           :error [:out-of-bounds :right]})
          :else
          (let [event (->bot-event {:type :movement
                                    :bot-id bot-id
                                    :delta 1
                                    :originating-cmd-id cmd-id})]
            (when (producer event)
              (move-bot pending-state event)
              (->cmd-response {:status :success
                               :originating-cmd-id cmd-id}))))))))

(defn bot-event-handler
  [event state]
  (let [{:keys [type]} event]
    (case type
      :creation
      (create-bot state event)
      :movement
      (move-bot state event))))

(defn hydrate
  ([state events]
   (hydrate state events (count @events)))
  ([state events n]
   (reset! state {})
   (run! (fn [event] (bot-event-handler event state)) (take n @events))))

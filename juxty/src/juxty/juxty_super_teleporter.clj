(ns juxty.juxty-super-teleporter)

(defn ->bot-event
  [event]
  (merge {:event-id (random-uuid)
          :created-at (System/currentTimeMillis)}
         event))

(defn state-and
  [f s1 s2 v]
  (and (f s1 v)
      (f s2 v)))

(defn bot-found?
  [state bot-id]
  (boolean (get @state bot-id)))

(defn bot-not-found?
  [state bot-id]
  ((complement bot-found?) state bot-id))

(defn ->cmd-response
  [m]
  (merge 
   {:cmd-response-id (random-uuid)
    :created-at (System/currentTimeMillis)}
   m))

(defn teleport-bot!
  [state {:keys [bot-id new-position] :as _event}]
  (swap! state assoc-in [bot-id :position] new-position))

(defn create-bot!
  [state {:keys [bot-id created-at] :as _event}]
  (swap! state assoc bot-id {:bot-id bot-id
                             :created-at created-at}))
(defn bot-event-handler
  [state event]
  (let [{:keys [type]} event]
    (case type
      :teleport
      (teleport-bot! state event)
      :creation
      (create-bot! state event)
      state)))

(defn bot-cmd-handler
  [pending-state state producer cmd]
  (let [{:keys [type cmd-id bot-id new-position]} cmd]
    (case type
      :teleport
      (cond
        (state-and bot-not-found? pending-state state bot-id)
        (->cmd-response {:status :failure
                         :originating-cmd-id cmd-id
                         :error [:bot-not-found bot-id]})
        :else
        (when (->> (->bot-event {:type :teleport
                                 :bot-id bot-id
                                 :new-position new-position
                                 :originating-cmd-id cmd-id})
                   producer)
          (->cmd-response {:status :success
                           :originating-cmd-id cmd-id}))))))

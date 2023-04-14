(ns juxty.juxty-grown
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

(defn create-bot!
  [state {:keys [bot-id position created-at] :as _event}]
  (swap! state assoc bot-id {:bot-id bot-id
                             :position position
                             :created-at created-at}))

(defn move-bot!
  [state {:keys [bot-id delta] :as _event}]
  (swap! state update-in [bot-id :position] (fn [x] (+ x delta))))

(defn bot-event-handler
  [state event]
  (let [{:keys [type]} event]
    (case type
      :creation
      (create-bot! state event)
      :movement
      (move-bot! state event))))

(defn bot-cmd-handler
  [state producer cmd]
  (let [{:keys [type cmd-id bot-id]} cmd]
    (if (external-fail?)
      (->cmd-response {:status :failure
                       :originating-cmd-id cmd-id
                       :error [:external-service-says-no]})
      (case type
        :create
        (if (bot-found? state bot-id)
          (->cmd-response {:status :failure
                           :originating-cmd-id cmd-id
                           :error [:bot-already-found bot-id]})
          (when (->> (->bot-event {:type :creation
                                   :bot-id bot-id
                                   :position 0
                                   :originating-cmd-id cmd-id})
                     producer)
            (->cmd-response {:status :success
                             :originating-cmd-id cmd-id})))
        :move-left
        (cond
          (bot-not-found? state bot-id)
          (->cmd-response {:status :failure
                           :originating-cmd-id cmd-id
                           :error [:bot-not-found bot-id]})
          (<= (get-bot-position state bot-id) -2)
          (->cmd-response {:status :failure
                           :originating-cmd-id cmd-id
                           :error [:out-of-bounds :left]})
          :else
          (when (->> (->bot-event {:type :movement
                                   :bot-id bot-id
                                   :delta -1
                                   :originating-cmd-id cmd-id})
                     producer)
            (->cmd-response {:status :success
                             :originating-cmd-id cmd-id})))
        :move-right
        (cond
          (bot-not-found? state bot-id)
          (->cmd-response {:status :failure
                           :originating-cmd-id cmd-id
                           :error [:bot-not-found bot-id]})            
          (>= (get-bot-position state bot-id) 2)
          (->cmd-response {:status :failure
                           :originating-cmd-id cmd-id
                           :error [:out-of-bounds :right]})
          :else
          (when (->> (->bot-event {:type :movement
                                   :bot-id bot-id
                                   :delta 1
                                   :originating-cmd-id cmd-id})
                     producer)
            (->cmd-response {:status :success
                             :originating-cmd-id cmd-id})))))))

(defn hydrate
  ([state events]
   (hydrate state events (count @events)))
  ([state events n]
   (reset! state {})
   (run! (fn [event] (bot-event-handler state event)) (take n @events))))

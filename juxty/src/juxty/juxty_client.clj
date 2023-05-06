(ns juxty.juxty-client
  (:require
   [mocka.core :as mocka :refer [to]]))

(defn cmd-unsuccessful?
  [cmd-response]
  (or (= :timeout (:status cmd-response))
      (= :failure (:status cmd-response))))

(defn cmd-successful?
  [cmd-response]
  (= :success (:status cmd-response)))

(defn create-pending-response!
  [state cmd-id]
  (swap! state assoc cmd-id {:status :pending}))

(defn get-response-status
  [state cmd-id]
  (get-in @state [cmd-id :status]))

(defn get-response
  [state cmd-id]
  (get-in @state [cmd-id :response]))

(defn update-response!
  [state {:keys [originating-cmd-id] :as cmd-response}]
  (when (= :pending (get-response-status state originating-cmd-id))
    (swap! state assoc originating-cmd-id {:status :responded
                                           :response cmd-response})))

(defn cmd-responded?
  [state cmd-id]
  (= :responded (get-response-status state cmd-id)))

(defn run-cmd
  [state submit-topic-config cmd]
  (let [producer (:producer submit-topic-config)
        cmd-id (:cmd-id cmd)
        max-attempts 10]
    (to {:producer producer} cmd)
    (create-pending-response! state cmd-id)
    (loop [responded? false
           attempt 0]
      (cond
        (>= attempt max-attempts)
        {:status :timed-out
         :originating-cmd-id cmd-id}
        responded?
        (get-response state cmd-id)
        :else
        (do (Thread/sleep 200)
            (recur (cmd-responded? state cmd-id) (inc attempt)))))))

(defmacro do-cmds->
  [expr & forms]
  (let [g (gensym)
        steps (map (fn [step] `(if (cmd-unsuccessful? ~g)
                                 ~g
                                 ~step))
                   forms)]
    `(let [~g ~expr
           ~@(interleave (repeat g) (butlast steps))]
       ~(if (empty? steps)
          g
          (last steps)))))

(def fib
  (->> [0 1] 
    (iterate (fn [[a b]] [b (+ a b)]))
    (map first)))

(defn retry-cmd
  [f topic cmd]
  (let [max-attempts 6]
    (loop [response (f topic cmd)
           attempt 0]
      (if (or (>= attempt max-attempts)
              (cmd-successful? response))
        response
        (do (Thread/sleep (* 1000 (nth fib attempt)))
            (recur (f topic (assoc cmd :cmd-id (random-uuid)))
                   (inc attempt)))))))

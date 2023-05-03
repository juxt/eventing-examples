(ns juxty.simple.40-external-side-effects
  (:require
   [clojure.string :as str]
   [juxty.simple.25-event-sourcing :refer [event-handler to-events]]
   [juxty.simple.30-cmd-sourcing :refer [to-cmds]]))


;; External service
(defn external-true-or-false []
  (-> (slurp (str "https://www.random.org/integers/?num=1&min=" 0
                  "&max=" 1
                  "&col=1&base=10&format=plain&rnd=new"))
      str/trim
      Integer.
      (= 1)))

(defn cmd-handler
  [b cmd]
  (case (:type cmd)
    :move-left
    (if (and (external-true-or-false)
             (> (:position @b) -2))
      (->> {:type :movement
            :delta -1}
           to-events
           (event-handler b))
      :failure)
    :move-right
    (if (and (external-true-or-false)
             (< (:position @b) 2))
      (->> {:type :movement
            
            :delta 1}
           to-events
           (event-handler b))
      :failure)))

(defn left! [b]
  (->> {:type :move-left}
       to-cmds
       (cmd-handler b)))

(defn right! [b]
  (->> {:type :move-right}
       to-cmds
       (cmd-handler b)))

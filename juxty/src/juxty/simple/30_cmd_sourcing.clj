(ns juxty.simple.30-cmd-sourcing
  (:require
   [juxty.simple.25-event-sourcing :refer [event-handler to-events]]))

;; Command Sourcing
(def cmds (ref []))

(defn to-cmds
  [cmd]
  (alter cmds conj cmd)
  cmd)

(defn apply-cmd
  [b cmd]
  (case (:type cmd)
    :move-left
    (dosync (if (> (:position @b) -2)
              (->> {:type :movement
                    :delta -1}
                   to-events
                   (event-handler b))
              :failure))
    :move-right
    (dosync (if (< (:position @b) 2)
              (->> {:type :movement
                    :delta 1}
                   to-events
                   (event-handler b))
              :failure))))

(defn left! [b]
  (->> (dosync (->> {:type :move-left}
                    ;;provide a chance for validation
                    to-cmds))
       (apply-cmd b)))

(defn right! [b]
  (->> (dosync (->> {:type :move-right}
                    ;; provide a chance for validation
                    to-cmds))
       (apply-cmd b)))

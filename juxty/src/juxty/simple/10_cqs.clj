(ns juxty.simple.10-cqs
  (:require [juxty.simple.00-pure :refer [move-left move-right]]))

;; CQS Style - a command that performs an action
(defn left! [b]
  (if (> (:position @b) -2)
    (do (swap! b move-left)
        :success)
    :failure))

(defn right! [b]
  (if (< (:position @b) 2)
    (do (swap! b move-right)
        :success)
    :failure))


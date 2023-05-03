(ns juxty.simple.utils
  (:require [juxty.simple.30-cmd-sourcing :as cmd]
            [juxty.simple.25-event-sourcing :as es]))

(defn left-or-right [lf rf]
  (if (= 0 (rand-int 2))
    lf
    rf))

(defn left-or-right-seq [lf rf]
  (lazy-seq (cons (left-or-right lf rf) (left-or-right-seq lf rf))))

(defn random-walk
  ([lf rf b]
   (map (fn [f] (f b)) (left-or-right-seq lf rf)))
  ([lf rf b n]
   (take n (random-walk lf rf b))))

(defn reset-bot [b]
  (dosync 
   (ref-set b {:position 0
               :id :the-eternal-bot})
   (ref-set es/events [])
   (ref-set cmd/cmds [])))

(defn halfway-time-helper []
  (let [start-time (-> @es/events
                       first
                       :created-at)
        end-time (-> @es/events
                     last
                     :created-at)]
    (+ start-time
       (/ (- end-time start-time) 2))))

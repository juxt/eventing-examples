(ns mocka.mocka
  (:require [clojure.pprint :refer [pprint]]))

(defn producer
  "Returns a function that adds a message to a specific topic"
  [topic]
  (fn [v]
    (swap! topic conj v)
    v))

(defn consumer
  "Returns a function that returns the next message on a specific topic.
   Optionally pass the intial offset."
  ([topic]
   (consumer topic 0))
  ([topic ioffset]
   (let [offset (atom ioffset)]
     (fn []
       (when-let [result (nth @topic @offset nil)]
         (swap! offset inc)
         result)))))

(defn consumer-last
  "Returns a function that returns the next message on a specific topic.
   Optionally pass the intial offset."
  ([topic]
   (consumer topic (dec (count @topic)))))

(defn to
  "From a topic config use the producer fn to add a message"
  [topic-config v]
  ((:producer topic-config) v))

(defn from
  "From a topic config use the consumer fn to read the next message"  
  [topic-config]
  ((:consumer topic-config)))

(defn peek'
  "Print the value of x and return the value"
  ([x]
   (peek' "" x))
  ([s x]
   (println s)
   (pprint x)
   x))

(defn wait
  ([ms x]
   (Thread/sleep ms)
   x))

(defmacro builder
  "Expects a list of bindings and a function to execute.  
  The function will run every 100ms in it's own thread."
  [form & body]
  `(atom 
    (future
      (let ~form
        (while true
          (Thread/sleep 200)
          ~@body)))))

(defn l2f
  "Last to first helper for thread last"
  ([f a b]
   (f b a))
  ([f a b c]
   (f c a b))
  ([f a b c d]
   (f d a b c)))

(defn ->topic-config
  []
  (let [topic (atom [])]
    {:topic topic
     :consumer (consumer topic 0)
     :producer (producer topic)}))

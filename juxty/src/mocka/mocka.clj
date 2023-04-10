(ns mocka.mocka)

(defn producer
  "Returns a function that adds messages to a specific topic"
  [topic]
  (fn [& v]
    (swap! topic #(apply conj % v))
    nil))

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

(defn to
  "From a topic config use the producer fn to add a message"
  [topic-config & v]
  (apply (:producer topic-config) v))

(defn from
  "From a topic config use the consumer fn to read the next message"  
  [topic-config]
  ((:consumer topic-config)))

(defn peek
  "Print the value of x and return the value"
  ([x]
   (peek "" x))
  ([s x]
   (println (str s x))
   x))

(defmacro builder
  "Expects a list of bindings and a function to execute.  
  The function will run every 100ms in it's own thread."
  [form & body]
  `(atom 
    (future
      (let ~form
        (while true
          (Thread/sleep 100)
          ~@body)))))

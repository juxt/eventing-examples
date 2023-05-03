(ns juxty.simple.00-pure)

;; Single Bot state of position
;; {:position 0}

;; Pure functions for moving left or right
(defn move-left [b]
  (update b :position dec))

(defn move-right [b]
  (update b :position inc))

;; Movement functions over a Bot atom constrained between -2 and 2
(defn left! [b]
  (if (> (:position @b) -2)
    (swap! b move-left)
    @b))

(defn right! [b]
  (if (< (:position @b) 2)
    (swap! b move-right)
    @b))


;; Can we find something that alters the state of an aggregate that need not be represented by a domain event

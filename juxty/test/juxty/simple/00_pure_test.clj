(ns juxty.simple.00-pure-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [juxty.simple.00-pure :as sut :refer [left!]]))

(deftest juxty
  (testing
      "The functions 'left' and 'right' return the new state.  As an
      atom is used to represent the global state, 'left' and 'right'
      wrap the atom access for us.  However, these functions are
      side-effecing thus we lose referentially transparent.  CQS says
      that we shouldn't really treat them as functions anymore."
      (let [juxty (atom {:position 0})]
        (testing "Move Juxty Left"
          (is (= {:position -1} (left! juxty)))
          (is (= {:position -2} (left! juxty)))
          (is (= -2 (:position @juxty))))
        (testing "Hitting the boundary"
          (reset! juxty {:position -2})
          (is (= {:position -2} (left! juxty)))
          (is (= -2 (:position @juxty)))))))

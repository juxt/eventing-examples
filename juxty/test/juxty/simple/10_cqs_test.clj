(ns juxty.simple.10-cqs-test
  (:require
   [clojure.test :as t :refer [deftest is testing]]
   [juxty.simple.10-cqs :as sut :refer [left!]]))

(deftest juxty-command
  (testing
      "We define left-cmd and right-cmd as commands that perform a
      movement action.  Whilst in Clojure they are defined as
      functions it is better to making the mental leap to treat them
      as commands i.e operations over mutable global state.  If we
      wish to know the state we have to separately query it.  With
      regard to Commands not generating a reponse CQS is treated as
      guideline and we do allow the return value of the command to
      indicate details of the execution.  These might include
      validation, exceptions, warnings but in our case the returns are
      a simple :success or :failure.  One other place where one may
      further relax the CQS constraint of no response is in returning
      ids for created objects.  Competing requirements influence the
      design decisions around the generation of ids."
    (let [juxty (atom {:position 0})]
        (testing "Move Juxty left"
          (is (= :success (left! juxty)))
          (is (= -1 (:position @juxty))))
        (testing "Hitting the boundary"
          (reset! juxty {:position -2})
          (is (= :failure (left! juxty)))
          (is (= -2 (:position @juxty)))))))

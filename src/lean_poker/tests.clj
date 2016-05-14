(ns lean-poker.tests
  (:require
    [lean-poker.player :as player]
    [clojure.test :refer [deftest is]]))

(deftest ranking-hand
  (let [game-state {:players [{:id         1,
                               :name       "Bob",
                               :status     "active",
                               :version    "Default random player",
                               :stack      1590,
                               :bet        0,
                               :hole_cards [{:rank "A", :suit "diamonds"} {:rank "10", :suit "diamonds"}]}
                              {:id         2,
                               :name       "Bob",
                               :status     "active",
                               :version    "Default random player",
                               :stack      1590,
                               :bet        0,
                               :hole_cards []}]}]
    (is (= 200 (player/bet-request game-state)))))

(deftest have-pair
  (let [game-state {:players [{:id         1,
                               :name       "Bob",
                               :status     "active",
                               :version    "Default random player",
                               :stack      1590,
                               :bet        80,
                               :hole_cards [{:rank "A", :suit "hearts"} {:rank "A", :suit "spades"}]}]}]
    (is (= 500 (player/bet-request game-state)))))

(deftest have-flush
  (let [game-state {:community_cards [{:rank "4", :suit "hearts"} {:rank "A", :suit "hearts"} {:rank "6", :suit "hearts"}],
                    :players [{:id         1,
                               :name       "Bob",
                               :status     "active",
                               :version    "Default random player",
                               :stack      1590,
                               :bet        80,
                               :hole_cards [{:rank "A", :suit "hearts"} {:rank "A", :suit "hearts"}]}]}]
    (is (= 1590 (player/bet-request game-state)))))

(deftest inside-have-pair
  (is (not (player/have-pair? [{:rank "A", :suit "diamonds"} {:rank "10", :suit "diamonds"}])))
  (is (true? (player/have-pair? [{:rank "A", :suit "diamonds"} {:rank "A", :suit "diamonds"}]))))

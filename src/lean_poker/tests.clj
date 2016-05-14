(ns lean-poker.tests
  (:require
    [lean-poker.player :as player]
    [clojure.test :refer [deftest is]]))

(deftest ranking-hand
  (let [game-state {:community_cards []
                    :players [{:id         1,
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
  (let [game-state {:community_cards []
                    :players [{:id         1,
                               :name       "Bob",
                               :status     "active",
                               :version    "Default random player",
                               :stack      1590,
                               :bet        80,
                               :hole_cards [{:rank "A", :suit "hearts"} {:rank "A", :suit "spades"}]}]}]
    (is (= 300 (player/bet-request game-state)))))

(deftest bet-at-most-test
  (let [should-raise-50 {:community_cards []
                    :players         [{:id         1,
                                       :name       "Bob",
                                       :status     "active",
                                       :version    "Default random player",
                                       :stack      1590,
                                       :bet        50,
                                       :hole_cards [{:rank "A", :suit "diamonds"} {:rank "10", :suit "diamonds"}]}
                                      {:id         2,
                                       :name       "Bob",
                                       :status     "active",
                                       :version    "Default random player",
                                       :stack      1590,
                                       :bet        0,
                                       :hole_cards []}]}]
    (is (= 50 (player/bet-at-most should-raise-50 100))))
  (let [should-fold {:community_cards []
                         :players         [{:id         1,
                                            :name       "Bob",
                                            :status     "active",
                                            :version    "Default random player",
                                            :stack      1590,
                                            :bet        50,
                                            :hole_cards [{:rank "A", :suit "diamonds"} {:rank "10", :suit "diamonds"}]}
                                           {:id         2,
                                            :name       "Bob",
                                            :status     "active",
                                            :version    "Default random player",
                                            :stack      1590,
                                            :bet        200,
                                            :hole_cards []}]}]
    (is (= 0 (player/bet-at-most should-fold 100)))))

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

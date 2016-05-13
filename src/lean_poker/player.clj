(ns lean-poker.player)

(def version "Clojure-y folding player")

(def game-state {:game_id "550da1cb2d909006e90004b1",
                 :dealer 1,
                 :in_action 1,
                 :minimum_raise 240,
                 :community_cards [{:rank "4", :suit "spades"} {:rank "A", :suit "hearts"} {:rank "6", :suit "clubs"}],
                 :round 0,
                 :current_buy_in 320,
                 :tournament_id "550d1d68cd7bd10003000003",
                 :orbits 7,
                 :players [{:id 0, :name "Albert", :status "active", :version "Default random player", :stack 1010, :bet 320}
                           {:id 1,
                            :name "Bob",
                            :status "active",
                            :version "Default random player",
                            :stack 1590,
                            :bet 80,
                            :hole_cards [{:rank "6", :suit "hearts"} {:rank "6", :suit "spades"}]}
                           {:id 2, :name "Chuck", :status "out", :version "Default random player", :stack 0, :bet 0}],
                 :small_blind 10,
                 :bet_index 0,
                 :pot 400})

(defn have-pair?
  [rank cards]
  (= 2 (count
         (filter #(= rank (:rank %1))

                 cards))))

(defn bet-request
  ([] (bet-request game-state))
  ([game-state]
   (let [{:keys [community_cards players]} game-state
         us (first (filter (fn [player] (seq (:hole_cards player))) players))
         hole-cards (:hole_cards us)]
     (if (have-pair? (:rank (first hole-cards)) hole-cards)
       100
       0))))

(defn showdown
  [game-state]
  nil)

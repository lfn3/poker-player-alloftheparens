(ns lean-poker.player)

(def version "Clojure-y pairs")

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
                            :hole_cards [{:rank "A", :suit "hearts"} {:rank "6", :suit "spades"}]}
                           {:id 2, :name "Chuck", :status "out", :version "Default random player", :stack 0, :bet 0}],
                 :small_blind 10,
                 :bet_index 0,
                 :pot 400})

(def suits ["hearts" "spades" "diamonds" "clubs"])

(defn have-pair?
  [rank cards]
  (= 2 (count
         (filter #(= rank (:rank %1))
                 cards))))

(defn face-card [card]
  (some #{\A \K \Q \J \0} (:rank card)))
(defn is-flush [cards]
  (some identity (map (fn [suit] (= 5 (count (filter #(= suit (:suit %1)) cards)))) suits)))

(defn high-ranked-hand
  [cards]
  (every? identity (map face-card cards)))

(defn get-us [{:keys [players]}]
  (first (filter (fn [player] (seq (:hole_cards player))) players)))

(defn all-in [game-state]
  (let [us (get-us game-state)]
    (:stack us)))

(defn call [game-state]
  (let [highest-bet (apply max (map :bet (:players game-state)))
        our-bet (:bet (get-us game-state))]
    (- highest-bet our-bet)))

(defn after-flop [game-state]
  (some? (:community_cards game-state)))

(defn bet-request
  ([] (bet-request game-state))
  ([game-state]
   (let [{:keys [community_cards players]} game-state
         us (get-us game-state)
         hole-cards (:hole_cards us)
         all-cards (concat hole-cards community_cards)]

     (if (not (after-flop game-state))
       (cond
         (have-pair? (:rank (first hole-cards)) hole-cards) 500
         (high-ranked-hand hole-cards) 200
         :default 0)

       (cond
         (is-flush all-cards) (all-in game-state)
         :default (call game-state))))))                                                ;fold

(defn showdown
  [game-state]
  nil)

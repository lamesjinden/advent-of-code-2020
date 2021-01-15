(ns aoc.day22
  (:require [aoc.util :as u]))

(def day22-input-path "resources/input-day22.txt")

(def sample-input "Player 1:\n9\n2\n6\n3\n1\n\nPlayer 2:\n5\n8\n4\n7\n10")

(defn strs->player [[player & cards]]
  {:player player
   :cards  (mapv #(Integer/parseInt %) cards)})

(defn str->players [str]
  (->> str
       (u/split-on-newline)
       (map #(strs->player %))))

(defn input->players [input-path]
  (->> input-path
       (slurp)
       (str->players)))

(defn play-game [{p1 :player p1-cards :cards} {p2 :player p2-cards :cards}]
  (loop [deck1 p1-cards
         deck2 p2-cards]
    (let [[p1-card & p1-rest] deck1
          [p2-card & p2-rest] deck2]
      (cond
        (nil? p1-card) {:winner {:player p2 :cards deck2}
                        :loser  {:player p1 :cards deck1}}
        (nil? p2-card) {:winner {:player p1 :cards deck1}
                        :loser  {:player p2 :cards deck2}}
        :else (if (> p1-card p2-card)
                (recur (concat p1-rest [p1-card p2-card]) p2-rest)
                (recur p1-rest (concat p2-rest [p2-card p1-card])))))))

(defn score [{{cards :cards} :winner}]
  (->> cards
       (reverse)
       (map-indexed (fn [i x] (* (inc i) x)))
       (reduce +)))

(defn play-rec-game [{p1 :player p1-cards :cards} {p2 :player p2-cards :cards}]
  (loop [deck1 p1-cards
         deck2 p2-cards
         previous-rounds #{}]
    (let [[p1-card & p1-rest] deck1
          [p2-card & p2-rest] deck2]
      (if (contains? previous-rounds [deck1 deck2])
        {:winner {:player p1 :cards deck1}
         :loser  {:player p2 :cards deck2}}
        (let [previous-rounds (conj previous-rounds [deck1 deck2])]
          (cond
            (nil? p1-card) {:winner {:player p2 :cards deck2}
                            :loser  {:player p1 :cards deck1}}
            (nil? p2-card) {:winner {:player p1 :cards deck1}
                            :loser  {:player p2 :cards deck2}}
            (and (>= (count p1-rest) p1-card)
                 (>= (count p2-rest) p2-card)) (let [sub-game (play-rec-game {:player p1 :cards (take p1-card p1-rest)}
                                                                             {:player p2 :cards (take p2-card p2-rest)})]
                                                 (if (= p1 (get-in sub-game [:winner :player]))
                                                   (recur (concat p1-rest [p1-card p2-card]) p2-rest previous-rounds)
                                                   (recur p1-rest (concat p2-rest [p2-card p1-card]) previous-rounds)))
            :else (if (> p1-card p2-card)
                    (recur (concat p1-rest [p1-card p2-card]) p2-rest previous-rounds)
                    (recur p1-rest (concat p2-rest [p2-card p1-card]) previous-rounds))))))))

(defn part1
  ([]
   (part1 (input->players day22-input-path)))
  ([players]
   (->> players
        (apply play-game)
        (score))))

(defn part2
  ([]
   (part2 (input->players day22-input-path)))
  ([players]
   (->> players
        (apply play-rec-game)
        (score))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))

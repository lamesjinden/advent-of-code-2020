(ns aoc.day15
  (:require [clojure.string :as s]))

(def day15-input-path "resources/input-day15.txt")

(def sample-input "0,3,6")

(defn str->numbers [str]
  (as-> str $
        (s/split $ #",")
        (map #(Integer/parseInt %) $)))

(defn input->numbers [input-path]
  (str->numbers (slurp input-path)))

(defn initialize-state [state numbers]
  (->> numbers
       (take (dec (count numbers)))
       (map-indexed (fn [i x] [(inc i) x]))
       (reduce (fn [acc [i x]] (assoc acc x i)) state)
       ((fn [x] (-> x
                    (assoc :last-spoken (last numbers))
                    (assoc :last-turn (count numbers)))))))

(defn take-turn [{:keys [last-turn last-spoken] :as state}]
  (let [turn (inc last-turn)]
    (if (nil? (get state last-spoken))
      (-> state
          (assoc-in [:last-spoken] 0)
          (assoc-in [last-spoken] last-turn)
          (assoc-in [:last-turn] turn)
          )
      (-> state
          (assoc-in [last-spoken] last-turn)
          (assoc-in [:last-spoken] (- last-turn (get state last-spoken)))
          (assoc-in [:last-turn] turn)))))

(defn state-seq [state]
  (lazy-seq
    (cons state
          (state-seq (take-turn state)))))

(defn part1
  ([]
   (part1 (input->numbers day15-input-path) {}))
  ([numbers state]
   (->> (initialize-state state numbers)
        (state-seq)
        (some #(and (= (:last-turn %) 2020) %))
        (:last-spoken))))

(defn part2
  ([]
   (part2 (input->numbers day15-input-path) {}))
  ([numbers state]
   (->> (initialize-state state numbers)
        (state-seq)
        (some #(and (= (:last-turn %) 30000000) %))
        (:last-spoken))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))
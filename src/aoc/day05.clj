(ns aoc.day05
  (:require [clojure.string :as s]))

(def day05-input-path "resources/input-day05.txt")

(defn input->seats [input-path]
  (-> input-path
      (slurp)
      (s/split-lines)))

(defn get-position
  ([row-pattern]
   (let [min 0
         max (-> 2
                 (Math/pow (count row-pattern))
                 (int)
                 (dec))]
     (get-position row-pattern min max)))
  ([row-pattern min max]
   (if (= min max)
     min
     (let [current (first row-pattern)
           half (-> (- max min)
                    (inc)
                    (/ 2))
           next (case current
                  \F [min (dec (+ min half))]
                  \B [(+ min half) max]
                  \L [min (dec (+ min half))]
                  \R [(+ min half) max])]
       (recur (rest row-pattern)
              (first next)
              (last next))))))

(defn get-seat-coordinates [str]
  (when-let [match (re-matches #"^([BF]{7})([LR]{3})$" str)]
    (let [row-match (get match 1)
          column-match (get match 2)
          row (get-position row-match)
          column (get-position column-match)]
      [row column])))

(defn get-seat-id [[row column]]
  (-> row
      (* 8)
      (+ column)))

(defn part1
  ([] (part1 (input->seats day05-input-path)))
  ([seats]
   (->> seats
        (map get-seat-coordinates)
        (map get-seat-id)
        (apply max))))

(defn part2
  ([] (part2 (input->seats day05-input-path)))
  ([seats]
   (let [seat-ids (->> seats
                       (map get-seat-coordinates)
                       (map get-seat-id)
                       (sort))]
     (reduce (fn [x y]
               (if (= 1 (- y x))
                 y
                 (reduced (inc x))))
             seat-ids))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))
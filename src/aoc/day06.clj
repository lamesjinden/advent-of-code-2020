(ns aoc.day06
  (:require [clojure.string :as s]
            [clojure.set :refer [intersection]]))

(def day06-input-path "resources/input-day06.txt")

(defn str->answers [str]
  (->> str
       (s/split-lines)
       (partition-by s/blank?)
       (filter #(not (s/blank? (first %))))))

(defn input->answers [input-path]
  (->> input-path
       (slurp)
       (str->answers)))

(defn part1
  ([]
   (part1 (input->answers day06-input-path)))
  ([answers]
   (->> answers
        (map #(mapcat seq %))
        (map distinct)
        (map count)
        (reduce +))))

(defn part2
  ([]
   (part2 (input->answers day06-input-path)))
  ([answers]
   (->> answers
        (map #(map set %))
        (map #(apply intersection %))
        (map count)
        (reduce +))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))
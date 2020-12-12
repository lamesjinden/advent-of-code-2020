(ns aoc.day10
  (:require [clojure.string :as s]))

(def day10-input-path "resources/input-day10.txt")

(def sample-input1 "16\n10\n15\n5\n1\n11\n7\n19\n6\n12\n4")
(def sample-input "28\n33\n18\n42\n31\n14\n46\n20\n48\n47\n24\n23\n49\n45\n19\n38\n39\n11\n1\n32\n25\n35\n8\n17\n7\n9\n4\n2\n34\n10\n3")

(defn str->numbers [str]
  (->> str
       (s/split-lines)
       (map #(Long/parseLong %))))

(defn input->numbers [input-path]
  (->> input-path
       (slurp)
       (str->numbers)))

(defn part1
  ([]
   (part1 (input->numbers day10-input-path)))
  ([numbers]
   (let [max-jolt (+ 3 (apply max numbers))
         jolts (as-> numbers $
                     (sort $)
                     (conj $ 0)
                     (concat $ [max-jolt]))]
     (->> jolts
          (partition 2 1)
          (map #(- (last %) (first %)))
          (group-by identity)
          ((fn [{ones   1
                 threes 3}]
             (* (count ones)
                (count threes))))))))

(defn count-rec [all node]
  (let [children (get all node)]
    (if (empty? children)
      1
      (->> children
           (map #(count-rec all %))
           (reduce +')))))

(def count-rec-memo
  (memoize (fn [all node]
             (let [children (get all node)]
               (if (empty? children)
                 1
                 (->> children
                      (map #(count-rec-memo all %))
                      (reduce +')))))))

(defn numbers->jolts [numbers]
  (let [max-jolt (+ 3 (apply max numbers))]
    (as-> numbers $
          (sort $)
          (conj $ 0)
          (concat $ [max-jolt])
          (map-indexed #(hash-map :index %1 :value %2) $)
          (vec $))))

(defn jolts->graph [jolts]
  (->> jolts
       (partition-all 4 1)
       (reduce (fn [acc x]
                 (let [[head & remaining] x
                       head-value (:value head)
                       children (->> remaining
                                     (take-while #(<= (:value %) (+ head-value 3)))
                                     (vec))]
                   (assoc acc head children)))
               {})))

(defn part2
  ([]
   (part2 (input->numbers day10-input-path)))
  ([numbers]
   (let [jolts (numbers->jolts numbers)
         graph (jolts->graph jolts)]
     (count-rec-memo graph (first jolts)))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))
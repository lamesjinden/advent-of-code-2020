(ns aoc.day21
  (:require [clojure.string :as s]
            [clojure.set :as set]
            [aoc.util :as u]))

(def day21-input-path "resources/input-day21.txt")

(def sample-input "mxmxvkd kfcds sqjhc nhms (contains dairy, fish)\ntrh fvjkl sbzzf mxmxvkd (contains dairy)\nsqjhc fvjkl (contains soy)\nsqjhc mxmxvkd sbzzf (contains fish)")

(def food-pattern #"(.+) \(contains (.+)\)")

(defn parse-food [str]
  (when-let [match (re-matches food-pattern str)]
    (let [ingredients (as-> (get match 1) $
                            (s/split $ #"\s+")
                            (into #{} $))
          allergens (as-> (get match 2) $
                          (s/split $ #",")
                          (map #(s/trim %) $)
                          (into #{} $))]
      (assoc {} :ingredients ingredients :allergens allergens))))

(defn str->foods [str]
  (->> str
       (s/split-lines)
       (map parse-food)))

(defn input->foods [input-path]
  (->> input-path
       (slurp)
       (str->foods)))

(defn merge-maps [val-in-result val-in-latter]
  (if (list? val-in-result)
    (conj val-in-result val-in-latter)
    (list val-in-result val-in-latter)))

(defn normalize-to-seq [x-or-xs]
  (if (seq? x-or-xs)
    x-or-xs
    (cons x-or-xs nil)))

(defn foods-by-allergens [foods]
  (apply u/indexcat-by #(:allergens %) merge-maps foods))

(defn assign-allergens
  ([allergens-lookup]
   (->> (iterate (partial assign-allergens allergens-lookup) {})
        (reduce (fn [x y]
                  (if (= x y)
                    (reduced x)
                    y)))))
  ([allergens-lookup known]
   (loop [allergens (keys allergens-lookup)
          known known]
     (let [unknown-allergens (->> allergens
                                  (filter #(not (contains? known %))))
           s (first unknown-allergens)]
       (if (nil? s)
         known
         (let [known-set (into #{} (vals known))
               s-ingredients (->> (get allergens-lookup s)
                                  (normalize-to-seq)
                                  (map #(:ingredients %))
                                  (map #(apply disj % known-set)))
               intersect (apply set/intersection s-ingredients)]
           (if (= 1 (count intersect))
             (recur unknown-allergens (assoc known s (first intersect)))
             (recur (rest unknown-allergens) known))))))))

(defn part1
  ([]
   (part1 (input->foods day21-input-path)))
  ([foods]
   (let [allergic-ingredients (->> foods
                                   (foods-by-allergens)
                                   (assign-allergens)
                                   (vals)
                                   (into #{}))]
     (->> foods
          (mapcat #(:ingredients %))
          (filter #(not (contains? allergic-ingredients %)))
          (count)))))

(defn part2
  ([]
   (part2 (input->foods day21-input-path)))
  ([foods]
   (let [allergic-ingredients (->> foods
                                   (foods-by-allergens)
                                   (assign-allergens)
                                   (into (sorted-map))
                                   (vals))]
     (s/join "," allergic-ingredients))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))

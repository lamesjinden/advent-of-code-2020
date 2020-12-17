(ns aoc.day16
  (:require [clojure.string :as s]))

(def day16-input-path "resources/input-day16.txt")

(def sample-input "class: 1-3 or 5-7\nrow: 6-11 or 33-44\nseat: 13-40 or 45-50\n\nyour ticket:\n7,1,14\n\nnearby tickets:\n7,3,47\n40,4,50\n55,2,20\n38,6,12")
(def sample-input2 "class: 0-1 or 4-19\nrow: 0-5 or 8-19\nseat: 0-13 or 16-19\n\nyour ticket:\n11,12,13\n\nnearby tickets:\n3,9,18\n15,1,5\n5,14,9")

(def rule-pattern #"^(.+): (\d+)-(\d+) or (\d+)-(\d+)$")

; {:name "departure location", :ranges [[36 626] [651 973]]
(defn parse-rule [str]
  (let [match (re-find rule-pattern str)]
    {:name   (get match 1)
     :ranges [[(Integer/parseInt (get match 2)) (Integer/parseInt (get match 3))]
              [(Integer/parseInt (get match 4)) (Integer/parseInt (get match 5))]]}))

(defn parse-rules [lines]
  (->> lines
       (map parse-rule)
       (into [])))

; [127,89,149,113,181,131,53,199,103,107,97,179,109,193,151,83,197,101,211,191]
(defn parse-yours [lines]
  (->> lines
       (drop 1)
       (mapcat #(s/split % #","))
       (map #(Integer/parseInt %))
       (vec)))

; [[835,933,819,240,276,334,830,786,120,791,301,770,249,767,177,84,838,85,596,352]
;  [193,697,654,130,5,907,754,925,817,663,938,595,930,868,56,128,598,197,381,452]]
(defn parse-nearby [lines]
  (->> lines
       (drop 1)
       (map (fn [x] (->> (s/split x #",")
                         (map #(Integer/parseInt %))
                         (vec))))
       (vec)))

(defn str->notes [str]
  (let [partitioned (->> str
                         (s/split-lines)
                         (partition-by s/blank?)
                         (filter #(seq (first %)))
                         (vec))
        rules (parse-rules (get partitioned 0))
        yours (parse-yours (get partitioned 1))
        nearby (parse-nearby (get partitioned 2))]
    {:rules  rules
     :yours  yours
     :nearby nearby}))

(defn input->notes [input-path]
  (-> input-path
      (slurp)
      (str->notes)))

(defn range->pred [[lower upper]]
  (fn [x]
    (and (>= x lower)
         (<= x upper))))

(defn ticket-errors-from-ranges [ranges numbers]
  (let [pred (->> ranges
                  (map #(range->pred %)))
        any (apply some-fn pred)]
    (filter #(not (any %)) numbers)))

(defn ticket-errors [rules numbers]
  (let [pred (->> rules
                  (map #(:ranges %))
                  (mapcat identity)
                  (map #(range->pred %)))
        any (apply some-fn pred)]
    (filter #(not (any %)) numbers)))

(defn part1
  ([]
   (part1 (input->notes day16-input-path)))
  ([{:keys [rules nearby]}]
   (->> nearby
        (mapcat #(ticket-errors rules %))
        (reduce + 0))))

;class-candidates: x,2,3
;row-candidates:   1,2,3
;seat-candidates:  x,x,3
;:- seat must be third field b/c it has only 1 candidate
;:- once field three is removed, class must be 2nd because it is the only candidate
;:- once field two is removed, row must be first because it is the only remaining candidate

;((fn [x] (conj x yours)))

(defn field-candidates [{:keys [name ranges]} tickets]
  (let [field-count (count (first tickets))]
    (->> (range field-count)
         (map (fn [i]
                (->> (range (count tickets))
                     (map (fn [j] (get-in tickets [j i]))))))
         (map-indexed (fn [i values]
                        (when (empty? (ticket-errors-from-ranges ranges values))
                          i)))
         (filter some?)
         (apply hash-set)
         ((fn [x] {name x})))))

(defn select-candidates [field-name->candidates]
  (loop [working (into (hash-set) field-name->candidates)
         result {}]
    (if (empty? working)
      result
      (let [only (->> working
                      (some (fn [x]
                              (and (= 1 (count (second (first x))))
                                   x))))
            only-name (first (keys only))
            only-index (first (first (vals only)))
            next-result (assoc result only-name only-index)
            next-working (as-> working $
                               (disj $ only)
                               (map (fn [m]
                                      (let [n (first (keys m))]
                                        (update m n (fn [prev]
                                                      (disj prev only-index))))) $)
                               (into (hash-set) $))]
        (recur next-working next-result)))))

(defn part2
  ([]
   (part2 (input->notes day16-input-path)))
  ([{:keys [rules yours nearby]}]
   (let [valid-tickets (->> nearby
                            (filter #(empty? (ticket-errors rules %)))
                            (mapv vec))
         mappings (->> rules
                       (map #(field-candidates % valid-tickets)))
         selected-candidates (select-candidates mappings)]
     (->> selected-candidates
          (filter (fn [x]
                    (let [name (first x)]
                      (.startsWith name "departure"))))
          (map second)
          (map #(get yours %))
          (reduce *)))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))
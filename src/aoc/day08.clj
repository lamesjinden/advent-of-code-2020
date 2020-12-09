(ns aoc.day08
  (:require [clojure.string :as s]))

(def day08-input-path "resources/input-day08.txt")

(def sample-input "nop +0\nacc +1\njmp +4\nacc +3\njmp -3\nacc -99\nacc +1\njmp -4\nacc +6")

(def instruction-pattern #"(\w+) ([\+-]\d+)")

(defn parse-instruction [str]
  (when-let [match (re-matches instruction-pattern str)]
    (let [operator-str (get match 1)
          argument-str (get match 2)
          operator (keyword operator-str)
          argument (Integer/parseInt argument-str)]
      {:op  (keyword operator)
       :arg argument})))

(defn str->instructions [str]
  (->> str
       (s/split-lines)
       (map parse-instruction)
       (vec)))

(defn input->instructions [input-path]
  (->> input-path
       (slurp)
       (str->instructions)))

(defn create-computer-state
  ([] (create-computer-state 0 0))
  ([accumulator-value starting-instruction]
   {:acc-value accumulator-value
    :op-index  starting-instruction}))

(defn run-one [instructions state]
  (let [op-index (:op-index state)
        instruction (get instructions op-index)
        op (:op instruction)
        arg (:arg instruction)]
    (when op
      (case op
        :nop (update-in state [:op-index] inc)
        :acc (-> state
                 (update-in [:op-index] inc)
                 (update-in [:acc-value] (fn [curr x] (+ curr x)) arg))
        :jmp (update-in state [:op-index] (fn [curr x] (+ curr x)) arg)))))

(defn seq-computer
  ([instructions state] (seq-computer instructions state #{}))
  ([instructions state log]
   (lazy-seq
     (cons state
           (let [op-index (:op-index state)
                 next-state (run-one instructions state)
                 next-log (conj log op-index)]
             (when-not (or (contains? log op-index)
                           (nil? next-state))
               (seq-computer instructions next-state next-log)))))))

(defn part1
  ([]
   (part1 (input->instructions day08-input-path)
          (create-computer-state)))
  ([instructions state]
   (-> instructions
       (seq-computer state)
       (last)
       (:acc-value))))

(defn toggle-instruction [instructions index]
  (let [instruction (get instructions index)
        op (:op instruction)]
    (case op
      :nop (update-in instructions [index :op] (fn [prev x] x) :jmp)
      :jmp (update-in instructions [index :op] (fn [prev x] x) :nop)
      instructions)))

(defn terminated? [instructions states]
  (let [count (count instructions)
        final-state (last states)
        final-index (:op-index final-state)]
    (= count final-index)))

(defn part2
  ([]
   (part2 (input->instructions day08-input-path)
          (create-computer-state)))
  ([instructions state]
   (->> instructions
        (count)
        (range)
        (map-indexed (fn [idx x] (toggle-instruction instructions idx)))
        (map (fn [x] (seq-computer x state)))
        (filter (fn [x] (terminated? instructions x)))
        (first)
        (last)
        (:acc-value))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))
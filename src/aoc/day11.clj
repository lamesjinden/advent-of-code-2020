(ns aoc.day11
  (:require [clojure.string :as s]))

(def day11-input-path "resources/input-day11.txt")

(def sample-input "L.LL.LL.LL\nLLLLLLL.LL\nL.L.L..L..\nLLLL.LL.LL\nL.LL.LL.LL\nL.LLLLL.LL\n..L.L.....\nLLLLLLLLLL\nL.LLLLLL.L\nL.LLLLL.LL")

(defn str->grid [str]
  (->> str
       (s/split-lines)
       (mapv vec)))

(defn input->grid [input-path]
  (-> input-path
      (slurp)
      (str->grid)))

(defn adjacent-seats [grid row column]
  (when (get-in grid [row column])
    (let [up [(dec row) column]
          down [(inc row) column]
          left [row (dec column)]
          right [row (inc column)]
          up-left [(dec row) (dec column)]
          up-right [(dec row) (inc column)]
          down-left [(inc row) (dec column)]
          down-right [(inc row) (inc column)]]
      [up down left right up-left up-right down-left down-right])))

(defn empty-seat? [grid row column]
  (let [value (get-in grid [row column])]
    (= \L value)))

(defn occupied-seat? [grid row column]
  (let [value (get-in grid [row column])]
    (= \# value)))

(defn floor-seat? [grid row column]
  (let [value (get-in grid [row column])]
    (= \. value)))

(defn nextfn [grid row column]
  (let [adjacent-seats (adjacent-seats grid row column)
        adjacent-occupied (->> adjacent-seats
                               (filter #(occupied-seat? grid (first %) (second %)))
                               (count))]
    (cond
      (and (empty-seat? grid row column)
           (zero? adjacent-occupied))
      (fn [grid] (assoc-in grid [row column] \#))
      (and (occupied-seat? grid row column)
           (>= adjacent-occupied 4))
      (fn [grid] (assoc-in grid [row column] \L))
      :else identity)))

(defn part1
  ([]
   (part1 (input->grid day11-input-path)))
  ([grid]
   (let [rows (count grid)
         columns (count (first grid))
         coords (for [row (range rows)
                      column (range columns)]
                  [row column])]
     (loop [grid grid
            prev nil]
       (if (= grid prev)
         (->> grid
              (mapcat identity)
              (filter #(= \# %))
              (count))
         (let [next-grid (->> coords
                              (filter #(not (floor-seat? grid (first %) (second %))))
                              (map #(nextfn grid (first %) (second %)))
                              (reduce (fn [acc x]
                                        (x acc)) grid))]
           (recur next-grid grid)))))))

(defn up-coords [row column]
  (lazy-seq
    (let [up [(dec row) column]]
      (cons up
            (apply up-coords up)))))

(defn down-coords [row column]
  (lazy-seq
    (let [down [(inc row) column]]
      (cons down
            (apply down-coords down)))))

(defn left-coords [row column]
  (lazy-seq
    (let [left [row (dec column)]]
      (cons left
            (apply left-coords left)))))

(defn right-coords [row column]
  (lazy-seq
    (let [right [row (inc column)]]
      (cons right
            (apply right-coords right)))))

(defn up-left-coords [row column]
  (lazy-seq
    (let [up-left [(dec row) (dec column)]]
      (cons up-left
            (apply up-left-coords up-left)))))

(defn up-right-coords [row column]
  (lazy-seq
    (let [up-right [(dec row) (inc column)]]
      (cons up-right
            (apply up-right-coords up-right)))))

(defn down-left-coords [row column]
  (lazy-seq
    (let [down-left [(inc row) (dec column)]]
      (cons down-left
            (apply down-left-coords down-left)))))

(defn down-right-coords [row column]
  (lazy-seq
    (let [down-right [(inc row) (inc column)]]
      (cons down-right
            (apply down-right-coords down-right)))))

(defn visible-seats [grid row column]
  (when (get-in grid [row column])
    (let [seat? (fn [grid row column] (or (empty-seat? grid row column)
                                          (occupied-seat? grid row column)))
          take-first (fn [xs]
                       (->> xs
                            (take-while #(get-in grid %))
                            (some #(and (seat? grid (first %) (second %)) %))))
          up (->> (up-coords row column) (take-first))
          down (->> (down-coords row column) (take-first))
          left (->> (left-coords row column) (take-first))
          right (->> (right-coords row column) (take-first))
          up-left (->> (up-left-coords row column) (take-first))
          up-right (->> (up-right-coords row column) (take-first))
          down-left (->> (down-left-coords row column) (take-first))
          down-right (->> (down-right-coords row column) (take-first))]
      [up down left right up-left up-right down-left down-right])))

(defn nextfn2 [grid row column]
  (let [visible-seats (visible-seats grid row column)
        adjacent-occupied (->> visible-seats
                               (filter #(occupied-seat? grid (first %) (second %)))
                               (count))]
    (cond
      (and (empty-seat? grid row column)
           (zero? adjacent-occupied))
      (fn [grid] (assoc-in grid [row column] \#))
      (and (occupied-seat? grid row column)
           (>= adjacent-occupied 5))
      (fn [grid] (assoc-in grid [row column] \L))
      :else identity
      )))

(defn part2
  ([]
   (part2 (input->grid day11-input-path)))
  ([grid]
   (let [rows (count grid)
         columns (count (first grid))
         coords (for [row (range rows)
                      column (range columns)]
                  [row column])]
     (loop [grid grid
            prev nil]
       (if (= grid prev)
         (->> grid
              (mapcat identity)
              (filter #(= \# %))
              (count))
         (let [next-grid (->> coords
                              (filter #(not (floor-seat? grid (first %) (second %))))
                              (map #(nextfn2 grid (first %) (second %)))
                              (reduce (fn [acc x]
                                        (x acc)) grid))]
           (recur next-grid grid)))))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))
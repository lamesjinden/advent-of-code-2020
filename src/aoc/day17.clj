(ns aoc.day17
  (:require [clojure.string :as s]))

(def day17-input-path "resources/input-day17.txt")

(def sample-input ".#.\n..#\n###")

(defn vec->sorted-map [v]
  (->> v
       (map-indexed #(vector %1 %2))
       (into (sorted-map))))

(defn str->slice [str]
  (->> str
       (s/split-lines)
       (map vec)
       (map vec->sorted-map)
       (vec->sorted-map)))

(defn input->slice [input-path]
  (-> input-path
      (slurp)
      (str->slice)))

(defn cell-active?
  [^Character char]
  (= char \#))

(defn cell-inactive? [^Character char]
  (= char \.))

(defn count-active-of-slice [slice]
  (->> (keys slice)
       (mapcat (fn [key]
                 (let [keys (keys (get slice key))]
                   (map #(vector key %) keys))))
       (map #(get-in slice %))
       (filter cell-active?)
       (count)))

(defn count-active-of-map [m]
  (->> (keys m)
       (map #(get m %))
       (map #(count-active-of-slice %))
       (reduce +)))

(defn cell-neighbors [x y z]
  (let [offsets (for [offset-x [-1 0 1]
                      offset-y [-1 0 1]
                      offset-z [-1 0 1]
                      :when (not (= offset-x offset-y offset-z 0))]
                  [offset-x offset-y offset-z])]
    (->> offsets
         (map (fn [[offset-x offset-y offset-z]]
                [(+ x offset-x)
                 (+ y offset-y)
                 (+ z offset-z)])))))

(defn next-cell [state [col row z]]
  (let [current (get-in state [z row col] \.)
        actives (->> (cell-neighbors col row z)
                     (filter (fn [[x y z]]
                               (cell-active? (get-in state [z y x])))))]
    (cond
      (cell-active? current) (if (or (= (count actives) 2)
                                     (= (count actives) 3))
                               \#
                               \.)
      (cell-inactive? current) (if (= (count actives) 3)
                                 \#
                                 \.))))

(defn next-state [state]
  (let [all-coords (->> (keys state)
                        (mapcat (fn [key-z]
                                  (->> (keys (get state key-z))
                                       (mapcat (fn [key-y]
                                                 (->> (keys (get-in state [key-z key-y]))
                                                      (map (fn [key-x]
                                                             [key-x key-y key-z])))))))))
        potential-coords (->> all-coords
                              (mapcat (fn [all-coord]
                                        (apply cell-neighbors all-coord)))
                              (into #{}))
        result (->> potential-coords
                    (map (fn [[x y z :as all]]
                           (let [next (next-cell state all)]
                             (fn [s]
                               (assoc-in s [z y x] next)))))
                    (reduce (fn [acc f] (f acc)) state))]
    result))

(defn part1
  ([]
   (part1 (sorted-map 0 (input->slice day17-input-path))))
  ([state]
   (->> (range 6)
        (reduce (fn [acc _x] (next-state acc)) state)
        (count-active-of-map))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn count-active-of-directory [d]
  (->> (keys d)
       (map #(get d %))
       (map #(count-active-of-map %))
       (reduce +)))

(defn cell-neighbors2 [x y z w]
  (let [offsets (for [offset-x [-1 0 1]
                      offset-y [-1 0 1]
                      offset-z [-1 0 1]
                      offset-w [-1 0 1]
                      :when (not (= offset-x offset-y offset-z offset-w 0))]
                  [offset-x offset-y offset-z offset-w])]
    (->> offsets
         (map (fn [[offset-x offset-y offset-z offset-w]]
                [(+ x offset-x)
                 (+ y offset-y)
                 (+ z offset-z)
                 (+ w offset-w)])))))

(defn next-cell2 [state [col row z w]]
  (let [current (get-in state [w z row col] \.)
        actives (->> (cell-neighbors2 col row z w)
                     (filter (fn [[x y z w]]
                               (cell-active? (get-in state [w z y x] \.)))))]
    (cond
      (cell-active? current) (if (or (= (count actives) 2)
                                     (= (count actives) 3))
                               \#
                               \.)
      (cell-inactive? current) (if (= (count actives) 3)
                                 \#
                                 \.))))

(defn next-state2 [state]
  (let [
        all-coords (->> (keys state)
                        (mapcat (fn [key-w]
                                  (->> (keys (get state key-w))
                                       (mapcat (fn [key-z]
                                                 (->> (keys (get-in state [key-w key-z]))
                                                      (mapcat (fn [key-y]
                                                                (->> (keys (get-in state [key-w key-z key-y]))
                                                                     (map (fn [key-x]
                                                                            (let [result [key-x key-y key-z key-w]]
                                                                              result)))))))))))))
        potential-coords (->> all-coords
                              (mapcat (fn [all-coord]
                                        (apply cell-neighbors2 all-coord)))
                              (into #{}))
        result (->> potential-coords
                    (map (fn [[x y z w :as all]]
                           (let [next (next-cell2 state all)]
                             (fn [s]
                               (assoc-in s [w z y x] next)))))
                    (reduce (fn [acc f] (f acc)) state))]
    result))

(defn part2
  ([]
   (part2 {0 {0 (input->slice day17-input-path)}}))
  ([state]
   (->> (range 6)
        (reduce (fn [acc _x] (next-state2 acc)) state)
        (count-active-of-directory))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))
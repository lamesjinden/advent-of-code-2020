(ns aoc.day24
  (:require [clojure.string :as s]
            [aoc.util :as u])
  (:import (clojure.lang PersistentQueue)))

(def day24-input-path "resources/input-day24.txt")

(def sample-input "sesenwnenenewseeswwswswwnenewsewsw\nneeenesenwnwwswnenewnwwsewnenwseswesw\nseswneswswsenwwnwse\nnwnwneseeswswnenewneswwnewseswneseene\nswweswneswnenwsewnwneneseenw\neesenwseswswnenwswnwnwsewwnwsene\nsewnenenenesenwsewnenwwwse\nwenwwweseeeweswwwnwwe\nwsweesenenewnwwnwsenewsenwwsesesenwne\nneeswseenwwswnwswswnw\nnenwswwsewswnenenewsenwsenwnesesenew\nenewnwewneswsewnwswenweswnenwsenwsw\nsweneswneswneneenwnewenewwneswswnese\nswwesenesewenwneswnwwneseswwne\nenesenwswwswneneswsenwnewswseenwsese\nwnwnesenesenenwwnenwsewesewsesesew\nnenewswnwewswnenesenwnesewesw\neneswnwswnwsenenwnwnwwseeswneewsenese\nneswnwewnwnwseenwseesewsenwsweewe\nwseweeenwnesenwwwswnew")

(def char->direction {\e :east
                      \w :west
                      \n {\e :north-east
                          \w :north-west}
                      \s {\e :south-east
                          \w :south-west}})

(defn line->directions [str]
  (let [queue (->> str
                   (apply list)
                   (into (PersistentQueue/EMPTY)))]
    (loop [queue queue
           acc []]
      (if (empty? queue)
        acc
        (let [[a b & _] queue]
          (if (contains? #{\e \w} a)
            (recur (pop queue) (conj acc (get-in char->direction [a])))
            (recur (pop (pop queue)) (conj acc (get-in char->direction [a b])))))))))

(defn str->directions-list [str]
  (->> str
       (s/split-lines)
       (map line->directions)))

(defn input->directions-list [input-path]
  (->> input-path
       (slurp)
       (str->directions-list)))

(def direction->offset {:north-east [1 3]
                        :east       [2 0]
                        :south-east [1 -3]
                        :north-west [-1 3]
                        :west       [-2 0]
                        :south-west [-1 -3]})

(defn directions->location [directions]
  (->> directions
       (map (fn [direction] (get direction->offset direction)))
       (reduce (fn [[acc-x acc-y] [x y]]
                 [(+ acc-x x) (+ acc-y y)])
               [0 0])))

(defn part1
  ([] (part1 (input->directions-list day24-input-path)))
  ([directions-list]
   (->> directions-list
        (map directions->location)
        (group-by identity)
        (filter #(= 1 (mod (count (second %)) 2)))
        (count))))

(def adjacent-offsets (vals direction->offset))

(defn adjacent-tiles [[x y]]
  (->> adjacent-offsets
       (map (fn [[off-x off-y]]
              [(+ x off-x) (+ y off-y)]))))

(defn initial-state [directions-list]
  (->> directions-list
       (map directions->location)
       (reduce (fn [acc x]
                 (if (contains? acc x)
                   (if (= :black (get acc x))
                     (assoc acc x :white)
                     (assoc acc x :black))
                   (assoc acc x :black)))
               {})))

(defn next-state [state]
  (let [all-coords (keys state)
        adjacents (mapcat adjacent-tiles all-coords)
        state-augmented (reduce (fn [acc x]
                                  (if (not (contains? acc x))
                                    (assoc acc x :white)
                                    acc))
                                state
                                adjacents)]
    (->> state-augmented
         (map (fn [[xy color]]
                (let [black-count (->> xy
                                       (adjacent-tiles)
                                       (filter #(= :black (get state-augmented %)))
                                       (count))]
                  (cond
                    (and (= color :black)
                         (or (zero? black-count)
                             (> black-count 2))) (fn [s] (assoc s xy :white))
                    (and (= color :white)
                         (= 2 black-count)) (fn [s] (assoc s xy :black))
                    :else identity))))
         (reduce (fn [acc func]
                   (func acc)) state-augmented))))

(defn part2
  ([] (part2 (input->directions-list day24-input-path)))
  ([directions-list]
   (->> directions-list
        (initial-state)
        (iterate next-state)
        (map #(u/count-by second %))
        (take 101)
        (last)
        (:black))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))

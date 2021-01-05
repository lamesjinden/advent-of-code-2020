(ns aoc.util
  (:require [clojure.string :as str]
            [clojure.core.matrix :as m]))

(defn flip-lr
  "returns the result of flipping m horizontally"
  [m] (m/matrix (m/slice-map reverse m)))

(defn flip-ud
  "returns the result of flipping m vertically"
  [m] (m/matrix (reverse (m/rows m))))

(defn rotate-cw
  "returns the result of rotating m 90 degrees clockwise"
  [m] (m/matrix (m/slice-map reverse (m/transpose m))))

(defn rotate-ccw
  "returns the result of rotating m 90 degrees counter-clockwise"
  [m] (m/matrix (reverse (m/transpose m))))

(defn vstack
  "returns the result of joining ms vertically"
  [& ms] (apply m/join-along 0 ms))

(defn hstack
  "returns the result of joining ms horizontally"
  [& ms] (apply m/join-along 1 ms))

(defn split-on-newline
  "returns a lazy sequence of sequences of strings that is the result of
  splitting s on newline characters, partitioning on blank lines,
  then excluding blank elements"
  [s]
  (->> s
       (str/split-lines)
       (partition-by str/blank?)
       (filter #(not (str/blank? %)))))

(defn index-by
  "accepts a number of map instances and produces a single map
  that associates the result of applying f to each map to the
  respective map"
  [keyfn & maps]
  (->> maps
       (map (fn [m]
              [(keyfn m) m]))
       (into {})))

(defn indexcat-by
  "similar to index-by, but where keyfn produces a collection.
  each element of the result of applying keyfn to each map is associated
  to the respective map.
  the resulting maps are merged via merge-with using the provided
  mergefn"
  [keyfn mergefn & maps]
  (->> maps
       (mapcat (fn [m] (map (fn [n] {n m}) (keyfn m))))
       (apply merge-with mergefn)))

(defn coords-2d
  "returns a lazy sequence of all possible x,y pairs representing
  locations within grid. pairs are represented as vectors of 2 elements
  (e.g. [<ROW> <COLUMN>])"
  [grid]
  (let [rows (count grid)
        cols (count (first grid))]
    (for [row (range rows)
          col (range cols)]
      [row col])))
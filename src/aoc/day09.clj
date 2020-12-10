(ns aoc.day09
  (:require [clojure.string :as s]))

(def day09-input-path "resources/input-day09.txt")

(def sample-input "35\n20\n15\n25\n47\n40\n62\n55\n65\n95\n102\n117\n150\n182\n127\n219\n299\n277\n309\n576")

(defn str->stream [str]
  (->> str
       (s/split-lines)
       (map #(Long/parseLong %))))

(defn stream->windows [window-size stream]
  (->> stream
       (partition (inc window-size) 1)
       (map #(hash-map :preamble (->> %
                                      (take window-size)
                                      (vec))
                       :value (last %)))))

(defn valid-window? [{:keys [value preamble]}]
  (for [x preamble
        y preamble
        :when (and (not= x y)
                   (< x y)
                   (= (+ x y) value))]
    [x y]))

(defn input->stream [input-path]
  (->> input-path
       (slurp)
       (str->stream)))

(defn part1
  ([]
   (part1 (input->stream day09-input-path) 25))
  ([stream window-size]
   (->> stream
        (stream->windows window-size)
        (map #(assoc % :addends (valid-window? %)))
        (filter #(empty? (:addends %)))
        (first)
        (:value))))

(defn add-smallest-and-largest [& addends]
  (let [min (apply min addends)
        max (apply max addends)]
    (+ min max)))

(defn part2
  ([]
   (let [stream (input->stream day09-input-path)
         sum (part1 stream 25)]
     (part2 stream sum)))
  ([stream sum]
   (let [sequences (sequence
                     (comp (mapcat #(partition % 1 stream))
                           (filter #(= sum (apply + %)))
                           (take 1))
                     (range 2 (count stream)))]
     (->> sequences
          (first)
          (apply add-smallest-and-largest)))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))

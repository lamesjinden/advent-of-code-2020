(ns aoc.day01)

(def day01-input-path "resources/input-day01.txt")

(defn input->ints [input-path]
  (->> input-path
       (slurp)
       (clojure.string/split-lines)
       (map #(Integer/parseInt %))))

(def target-sum 2020)

(defn summing-predicate [target & xs]
  (== target (apply + xs)))

(defn run [discriminator tuples]
  (->> tuples
       (filter #(apply discriminator %))
       (first)
       (apply *)))

(defn part1
  ([]
   (let [input-data (input->ints day01-input-path)
         discriminator (partial summing-predicate target-sum)]
     (part1 input-data discriminator)))
  ([input-data discriminator]
   (let [tuples (for [x input-data
                      y input-data]
                  [x y])]
     (run discriminator tuples))))

(defn part2
  ([]
   (let [input-data (input->ints day01-input-path)
         discriminator (partial summing-predicate target-sum)]
     (part2 input-data discriminator)))
  ([input-data discriminator]
   (let [tuples (for [x input-data
                      y input-data
                      z input-data]
                  [x y z])]
     (run discriminator tuples))))

;; clj -m aoc.day01

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))
(ns aoc.day03)

(def day03-input-path "resources/input-day03.txt")

(defn input->board [input-path]
  (->> input-path
       (slurp)
       (clojure.string/split-lines)))

(defn get-next [{:keys [right down x y]}]
  {:x (+ x down)
   :y (+ y right)})

(defn get-coordinates [board slope start]
  (let [row-count (count board)
        next-args (merge slope start)
        {next-x :x :as next} (get-next next-args)]
    (lazy-seq
      (when (< next-x row-count)
        (cons next
              (get-coordinates board slope next))))))

(defn tree? [board {:keys [:x :y]}]
  (let [column-count (count (first board))
        normalize-y (mod y column-count)]
    (= \# (get-in board [x normalize-y]))))

(defn part1
  ([]
   (let [board (input->board day03-input-path)
         slope {:right 3 :down 1}
         start {:x 0 :y 0}]
     (part1 board slope start)))
  ([board slope start]
   (->> (get-coordinates board slope start)
        (filter #(tree? board %))
        (count))))

(defn part2
  ([]
   (let [board (input->board day03-input-path)
         slopes [{:right 1 :down 1}
                 {:right 3 :down 1}
                 {:right 5 :down 1}
                 {:right 7 :down 1}
                 {:right 1 :down 2}]
         start {:x 0 :y 0}]
     (part2 board slopes start)))
  ([board slopes start]
   (->> slopes
        (map #(part1 board % start))
        (reduce *))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))

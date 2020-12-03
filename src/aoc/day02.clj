(ns aoc.day02)

(def day02-input-path "resources/input-day02.txt")

(defn input->lines [input-path]
  (->> input-path
       (slurp)
       (clojure.string/split-lines)))

(def line-pattern #"(\d+)-(\d+) (\w): (.+)")

;; part 1

(defn parse-line-part1 [line]
  (if-let [match (re-matches line-pattern line)]
    {:min      (-> (match 1) (Integer/parseInt))
     :max      (-> (match 2) (Integer/parseInt))
     :char     (-> (match 3) (vec) (first))
     :password (match 4)}
    nil))

(defn validate-policy-part1 [policy]
  (let [min-occurrence (:min policy)
        max-occurrence (:max policy)
        policy-char (:char policy)
        password (:password policy)
        frequencies (frequencies password)
        frequency (get frequencies policy-char)]
    (and (some? frequency)
         (>= frequency min-occurrence)
         (<= frequency max-occurrence))))

(defn part1
  ([]
   (let [input-data (input->lines day02-input-path)]
     (part1 input-data)))
  ([input-data]
   (->> input-data
        (map parse-line-part1)
        (filter validate-policy-part1)
        (count))))

;; part 2

(defn parse-line-part2 [line]
  (if-let [match (re-matches line-pattern line)]
    {:first-index  (-> (match 1) (Integer/parseInt) (dec))
     :second-index (-> (match 2) (Integer/parseInt) (dec))
     :char         (-> (match 3) (vec) (first))
     :password     (match 4)}
    nil))

(defn validate-policy-part2 [policy]
  (let [first-index (:first-index policy)
        second-index (:second-index policy)
        policy-char (:char policy)
        password (:password policy)
        first-match (= policy-char (get password first-index))
        second-match (= policy-char (get password second-index))]
    (or (and first-match (not second-match))
        (and (not first-match) second-match))))

(defn part2
  ([]
   (let [input-data (input->lines day02-input-path)]
     (part2 input-data)))
  ([input-data]
   (->> input-data
        (map parse-line-part2)
        (filter validate-policy-part2)
        (count))))

;; clj -m aoc.day02

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))

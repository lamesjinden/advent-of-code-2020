(ns aoc.day19
  (:require [clojure.string :as s]
            [instaparse.core :as insta]))

(def day19-input-path "resources/input-day19.txt")

(def sample-input "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\"\n\nababbb\nbababa\nabbbab\naaabbb\naaaabbb")

(def simple-rule-pattern #"(\d+): \"(\w)\"")
(def compound-rule-pattern #"(\d+): (.*)")

(defn str->defs [str]
  (let [ors (s/split str #"\s*\|\s*")
        concats (map #(s/split % #"\s+") ors)
        ids (map (fn [xs] (mapv #(Integer/parseInt %) xs)) concats)]
    ids))

(defn str->rule [str]
  (let [simple (re-matches simple-rule-pattern str)
        compound (re-matches compound-rule-pattern str)]
    (cond
      simple {:id   (Integer/parseInt (get simple 1))
              :char (first (get simple 2))}
      compound {:id  (Integer/parseInt (get compound 1))
                :def (last compound)})))

(defn strs->rules [strs]
  (->> strs
       (map str->rule)
       (into {} (map (fn [x] [(:id x) x])))))

(defn str->rules-messages [str]
  (let [lines (s/split-lines str)
        partitioned (partition-by s/blank? lines)
        rules (first partitioned)
        rules-graph (strs->rules rules)
        messages (last partitioned)]
    {:rules    rules-graph
     :messages messages}))

(defn input->rules-messages [input-path]
  (-> input-path
      (slurp)
      (str->rules-messages)))

(defn rules->parser [rules]
  (let [grammar (str (reduce
                       (fn [acc [id {def :def char :char}]]
                         (.append acc (if (nil? char)
                                        (format "%s = %s\n" id def)
                                        (format "%s = '%s'\n" id char))))
                       (StringBuilder.)
                       rules))
        parser (insta/parser grammar)]
    parser))

(defn parsed? [parser msg]
  (let [result (parser msg)]
    (if (insta/get-failure result)
      nil
      result)))

(defn part1
  ([]
   (let [rules-messages (input->rules-messages day19-input-path)]
     (part1 rules-messages)))
  ([{:keys [rules messages]}]
   (part1 rules messages))
  ([rules messages]
   (let [parser (rules->parser rules)]
     (->> messages
          (filter #(parsed? parser %))
          (count)))))

(def p2-sample-input "42: 9 14 | 10 1\n9: 14 27 | 1 26\n10: 23 14 | 28 1\n1: \"a\"\n11: 42 31\n5: 1 14 | 15 1\n19: 14 1 | 14 14\n12: 24 14 | 19 1\n16: 15 1 | 14 14\n31: 14 17 | 1 13\n6: 14 14 | 1 14\n2: 1 24 | 14 4\n0: 8 11\n13: 14 3 | 1 12\n15: 1 | 14\n17: 14 2 | 1 7\n23: 25 1 | 22 14\n28: 16 1\n4: 1 1\n20: 14 14 | 1 15\n3: 5 14 | 16 1\n27: 1 6 | 14 18\n14: \"b\"\n21: 14 1 | 1 14\n25: 1 1 | 1 14\n22: 14 14\n8: 42\n26: 14 22 | 1 20\n18: 15 15\n7: 14 5 | 1 21\n24: 14 1\n\nabbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\nbbabbbbaabaabba\nbabbbbaabbbbbabbbbbbaabaaabaaa\naaabbbbbbaaaabaababaabababbabaaabbababababaaa\nbbbbbbbaaaabbbbaaabbabaaa\nbbbababbbbaaaaaaaabbababaaababaabab\nababaaaaaabaaab\nababaaaaabbbaba\nbaabbaaaabbaaaababbaababb\nabbbbabbbbaaaababbbbbbaaaababb\naaaaabbaabaaaaababaa\naaaabbaaaabbaaa\naaaabbaabbaaaaaaabbbabbbaaabbaabaaa\nbabaaabbbaaabaababbaabababaaab\naabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")

(defn replace-rules [rules]
  (-> rules
      (assoc 8 (str->rule "8: 42 | 42 8"))
      (assoc 11 (str->rule "11: 42 31 | 42 11 31"))))

(defn part2
  ([]
   (let [rules-messages (input->rules-messages day19-input-path)]
     (part2 rules-messages)))
  ([{:keys [rules messages]}]
   (part2 rules messages))
  ([rules messages]
   (let [replaced-rules (replace-rules rules)]
     (part1 replaced-rules messages))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))
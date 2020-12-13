(ns aoc.day07
  (:require [clojure.string :as s]
            [instaparse.core :as insta]))

(def day07-input-path "resources/input-day07.txt")

(def input-sample "light red bags contain 1 bright white bag, 2 muted yellow bags.\ndark orange bags contain 3 bright white bags, 4 muted yellow bags.\nbright white bags contain 1 shiny gold bag.\nmuted yellow bags contain 2 shiny gold bags, 9 faded blue bags.\nshiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.\ndark olive bags contain 3 faded blue bags, 4 dotted black bags.\nvibrant plum bags contain 5 faded blue bags, 6 dotted black bags.\nfaded blue bags contain no other bags.\ndotted black bags contain no other bags.")
(def input-sample2 "shiny gold bags contain 2 dark red bags.\ndark red bags contain 2 dark orange bags.\ndark orange bags contain 2 dark yellow bags.\ndark yellow bags contain 2 dark green bags.\ndark green bags contain 2 dark blue bags.\ndark blue bags contain 2 dark violet bags.\ndark violet bags contain no other bags.")

(def parser
  (insta/parser
    "<line> = name <' bags contain '> ('no other bags' | bags) <'.'>
    name = #'\\w+ \\w+'
    count = #'\\d+'
    depend= count <' '> name <' bag' ['s']>
    bags = (depend | depend <', '>)+"))

(defn extract-subject [parse-tree]
  (-> parse-tree
      (first)
      (second)))

(defn extract-dependency [depend-node]
  (let [count (-> depend-node
                  (nth 1)
                  (second)
                  (Integer/parseInt))
        name (-> depend-node
                 (nth 2)
                 (second))]
    [name count]))

(defn extract-dependencies [parse-tree]
  (let [bags-tokens (second parse-tree)]
    (if (= :bags (first bags-tokens))
      (->> bags-tokens
           (drop 1)
           (map extract-dependency)
           (into {}))
      nil)))

(defn str->rule [str]
  (let [parse-tree (parser str)
        subject (extract-subject parse-tree)
        dependencies (extract-dependencies parse-tree)]
    {subject dependencies}))

(defn str->rules [str]
  (->> str
       (s/split-lines)
       (map str->rule)))

(defn input->rules [input-path]
  (->> input-path
       (slurp)
       (str->rules)))

(defn invert-rules [rules]
  (let [rule-names (keys rules)]
    (reduce (fn [acc y]
              (let [contain (->> rule-names
                                 (filter (fn [x]
                                           (let [deps (get rules x)]
                                             (contains? deps y))))
                                 (set))]
                (assoc acc y contain)))
            {}
            rule-names)))

(defn containers-of
  ([bag->containers subject]
   (loop [working (atom (apply list (get bag->containers subject)))
          acc (atom #{})]
     (if (empty? @working)
       @acc
       (let [bag (peek @working)
             containers (get bag->containers bag)]
         (swap! acc conj bag)
         (swap! working #(apply conj (pop %) containers))
         (recur working acc))))))

(defn part1
  ([]
   (part1 (input->rules day07-input-path) "shiny gold"))
  ([rules subject]
   (let [containers->bags (into {} rules)
         bag->containers (invert-rules containers->bags)]
     (->> subject
          (containers-of bag->containers)
          (count)))))

(defn contained-by [containers->bags subject]
  (loop [working (atom (list (list subject)))
         acc (atom (list))]
    (if (empty? @working)
      @acc
      (let [path (peek @working)
            bag (last path)
            children (containers->bags bag)]
        ;add this entry to results
        (swap! acc conj path)
        ;process children
        (let [child-work (->> children
                              (mapcat (fn [[name count]]
                                        (->> (range count)
                                             (map (fn [_]
                                                    (concat path (list name))))))))]
          (swap! working #(apply conj (pop %) child-work))
          (recur working acc))))))

(defn part2
  ([]
   (part2 (input->rules day07-input-path) "shiny gold"))
  ([rules subject]
   (let [containers->bags (into {} rules)]
     (->> subject
          (contained-by containers->bags)
          (filter #(> (count %) 1))
          (count)))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))

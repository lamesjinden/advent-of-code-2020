(ns aoc.day14
  (:require [clojure.string :as s])
  (:import (clojure.lang PersistentQueue)))

(def day14-input-path "resources/input-day14.txt")
(def sample-input "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X\nmem[8] = 11\nmem[7] = 101\nmem[8] = 0")

(def mask-pattern #"^mask = ([X10]{36})$")

(defn parse-mask [str]
  (when-let [match (re-matches mask-pattern str)]
    {:type  :mask
     :value (get match 1)}))

(def mem-pattern #"^mem\[(\d+)\] = (\d+)$")

(defn parse-mem [str]
  (when-let [match (re-matches mem-pattern str)]
    {:type    :write
     :address (get match 1)
     :value   (get match 2)}))

(defn str->program [str]
  (->> str
       (s/split-lines)
       (map #(or (parse-mask %)
                 (parse-mem %)))))

(defn input->str [input-path]
  (slurp input-path))

(defn input->program [input-path]
  (->> input-path
       (input->str)
       (str->program)))

(defn num->binary [^Long num]
  (let [as-binary (Long/toBinaryString num)
        zeros (->> (repeat \0)
                   (take (- 36 (count as-binary)))
                   (vec))]
    (->> (concat zeros as-binary)
         (char-array)
         (String/valueOf))))

(defn str->num [^String str]
  (Long/parseLong str))

(defn assoc-str [str idx ^Character char]
  (let [chars (vec str)
        updated (assoc chars idx char)
        arr (char-array updated)]
    (String/valueOf arr)))

(defn mask-value [^String value ^String mask]
  (let [binary-value (num->binary (Long/parseLong value))]
    (reduce (fn [acc index]
              (case (get mask index)
                \X acc
                \0 (assoc-str acc index \0)
                \1 (assoc-str acc index \1)))
            binary-value
            (range (count binary-value)))))

(defn execute-instruction [{mask :mask :as state} {:keys [type value address]}]
  (cond
    (= :mask type) (assoc-in state [:mask] value)
    (= :write type) (assoc-in state [:memory (num->binary (str->num address))] (mask-value value mask))))

(defn execute-program [state program]
  (reduce execute-instruction
          state
          program))

(defn binary->num [^String value]
  (.longValue (new BigInteger value 2)))

(defn eval-state [state]
  (->> (:memory state)
       (vals)
       (map binary->num)
       (reduce + 0)))

(defn create-state [] {:mask nil :memory {}})

(defn part1
  ([] (part1 (input->program day14-input-path) (create-state)))
  ([program state]
   (as-> state $
         (execute-program $ program)
         (eval-state $))))

(def sample-input2 "mask = 000000000000000000000000000000X1001X\nmem[42] = 100\nmask = 00000000000000000000000000000000X0XX\nmem[26] = 1")

(defn decode-mask [^String value ^String mask]
  (let [binary-value (num->binary (Long/parseLong value))]
    (reduce (fn [acc index]
              (let [m (get mask index)]
                (case m
                  \X (assoc-str acc index \X)
                  \0 acc
                  \1 (assoc-str acc index \1))))
            binary-value
            (range (count binary-value)))))

(defn realize-addresses [^String floating-address]
  (loop [work (conj PersistentQueue/EMPTY floating-address)
         result []]
    (if (empty? work)
      (vec result)
      (let [current (peek work)
            popped (pop work)
            floating-index (.indexOf current (str \X))]
        (if (= -1 floating-index)
          (recur popped (conj result current))
          (let [with-zero (assoc-str current floating-index \0)
                with-one (assoc-str current floating-index \1)
                work-next (conj popped with-zero with-one)]
            (recur work-next result)))))))

(defn apply-mask [state address value]
  (assoc-in state [:memory address] (num->binary (str->num value))))

(defn execute-instruction2 [{mask :mask :as state} {:keys [type value address]}]
  (cond
    (= :mask type) (assoc-in state [:mask] value)
    (= :write type) (let [decoded (decode-mask address mask)
                          realized (realize-addresses decoded)]
                      (reduce #(apply-mask %1 %2 value) state realized))))

(defn execute-program2 [state program]
  (reduce execute-instruction2
          state
          program))

(defn part2
  ([] (part2 (input->program day14-input-path) (create-state)))
  ([program state]
   (as-> state $
         (execute-program2 $ program)
         (eval-state $))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))

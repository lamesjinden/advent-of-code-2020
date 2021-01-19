(ns aoc.day25
  (:require [clojure.string :as s]))

(def day25-input-path "resources/input-day25.txt")

(def sample-card-public-key 5764801)
(def sample-door-public-key 17807724)

(def p1-mod 20201227)

(defn input->components [input-path]
  (let [[card-public-key door-public-key] (->> input-path
                                               (slurp)
                                               (s/split-lines))]
    {:card {:public-key (Integer/parseInt card-public-key)}
     :door {:public-key (Integer/parseInt door-public-key)}}))

(defn apply-subject-numbers [components]
  (-> components
      (assoc-in [:card :subject-number] 7)
      (assoc-in [:door :subject-number] 7)))

(defn apply-loop-size [components card-loop-size door-loop-size]
  (-> components
      (assoc-in [:card :loop-size] card-loop-size)
      (assoc-in [:door :loop-size] door-loop-size)))

(defn power-mod [^Long base ^Long exp ^Long m]
  (let [base (BigInteger/valueOf base)
        exp (BigInteger/valueOf exp)
        m (BigInteger/valueOf m)]
    ;thanks Java!
    (.modPow base exp m)))

(defn get-loop-size [subject-number public-key]
  (let [source (->> (iterate inc 1))
        xform (comp
                (filter (fn [i]
                          (let [m (power-mod subject-number i p1-mod)]
                            (= public-key m))))
                (take 1))]
    (first (transduce xform conj source))))

(defn transform-subject-number [subject-number loop-size]
  (power-mod subject-number loop-size p1-mod))

(defn get-encryption-key [{{card-loop-size  :loop-size
                            card-public-key :public-key} :card
                           {door-loop-size  :loop-size
                            door-public-key :public-key} :door}]
  (transform-subject-number door-public-key card-loop-size))

(defn part1
  ([]
   (->> (input->components day25-input-path)
        (apply-subject-numbers)
        (part1)))
  ([{{card-public-key :public-key card-subject-number :subject-number} :card
     {door-public-key :public-key door-subject-number :subject-number} :door
     :as                                                               components}]
   (let [card-loop-size (get-loop-size card-subject-number card-public-key)
         door-loop-size (get-loop-size door-subject-number door-public-key)
         components (apply-loop-size components card-loop-size door-loop-size)
         encryption-key (get-encryption-key components)]
     encryption-key)))

(defn -main []
  (let [answer (part1)]
    (println "answer:" answer)))
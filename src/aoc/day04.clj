(ns aoc.day04
  (:require [clojure.string :as s]))

(def day04-input-path "resources/input-day04.txt")

(defn input->passports [input-path]
  (->> input-path
       (slurp)
       (s/split-lines)
       (partition-by s/blank?)
       (filter #(not (s/blank? (first %))))
       (map #(mapcat (fn [x] (s/split x #"\s+")) %))
       (map #(map (fn [x] (s/split x #"\:")) %))
       (map #(into {} %))))

(def required-fields #{"byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"})
(def optional-fields #{"cid"})

(defn part1
  ([]
   (part1 (input->passports day04-input-path)))
  ([passports]
   (->> passports
        (filter #(every? % required-fields))
        (count))))

(defn validate-year [year min max]
  [year]
  (when-let [match (re-matches #"^\d\d\d\d$" year)]
    (let [parsed (Integer/parseInt match)]
      (and (>= parsed min)
           (<= parsed max)))))

(defn validate-byr
  "(Birth Year) - four digits; at least 1920 and at most 2002."
  [x]
  (validate-year x 1920 2002))

(defn validate-iyr
  "(Issue Year) - four digits; at least 2010 and at most 2020."
  [x]
  (validate-year x 2010 2020))

(defn validate-eyr
  "(Expiration Year) - four digits; at least 2020 and at most 2030."
  [x]
  (validate-year x 2020 2030))

(defn validate-hgt
  "(Height) - a number followed by either cm or in:
      If cm, the number must be at least 150 and at most 193.
      If in, the number must be at least 59 and at most 76."
  [x]
  (when-let [match (re-matches #"^(\d+)(in|cm)$" x)]
    (let [value (Integer/parseInt (get match 1))
          units (get match 2)]
      (case units
        "cm" (and (>= value 150)
                  (<= value 193))
        "in" (and (>= value 59)
                  (<= value 76))))))

(defn validate-hcl
  "(Hair Color) - a # followed by exactly six characters 0-9 or a-f."
  [x]
  (some? (re-matches #"^#(\w{6})$" x)))

(def valid-eye-colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defn validate-ecl
  "(Eye Color) - exactly one of: amb blu brn gry grn hzl oth."
  [x]
  (contains? valid-eye-colors x))

(defn validate-pid
  "(Passport ID) - a nine-digit number, including leading zeroes."
  [x]
  (some? (re-matches #"^(\d{9})$" x)))

(defn validate-cid
  "(Country ID) - ignored, missing or not."
  [_]
  true)

(def field-validators {"byr" validate-byr
                       "iyr" validate-iyr
                       "eyr" validate-eyr
                       "hgt" validate-hgt
                       "hcl" validate-hcl
                       "ecl" validate-ecl
                       "pid" validate-pid})

(defn valid-passport? [passport]
  (and (every? passport required-fields)
       (every? (fn [[k v]]
                 (let [validator (or (get field-validators k)
                                     (fn [_] true))]
                   (validator v)))
               passport)))

(defn part2
  ([]
   (part2 (input->passports day04-input-path)))
  ([passports]
   (->> passports
        (filter valid-passport?)
        (count))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))
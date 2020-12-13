(ns aoc.day12
  (:require [clojure.string :as s]))

(def day12-input-path "resources/input-day12.txt")
(def sample-input "F10\nN3\nF7\nR90\nF11")

(def instruction-pattern #"^([NSEWLRF])(\d+)$")

(defn parse-instruction [str]
  (when-let [match (re-matches instruction-pattern str)]
    (let [action (get match 1)
          value (Integer/parseInt (get match 2))]
      {:action action
       :value  value})))

(defn str->instructions [str]
  (->> str
       (s/split-lines)
       (map parse-instruction)))

(defn input->instructions [input-path]
  (-> input-path
      (slurp)
      (str->instructions)))

(defn create-state
  ([]
   (create-state :east [0 0] [10 1]))
  ([initial-direction initial-location waypoint]
   {:direction       initial-direction
    :location        initial-location
    :waypoint-offset waypoint}))

(def next-direction {:east  {"R" {90  :south
                                  180 :west
                                  270 :north}
                             "L" {90  :north
                                  180 :west
                                  270 :south}}
                     :west  {"R" {90  :north
                                  180 :east
                                  270 :south}
                             "L" {90  :south
                                  180 :east
                                  270 :north}}
                     :north {"R" {90  :east
                                  180 :south
                                  270 :west}
                             "L" {90  :west
                                  180 :south
                                  270 :east}}
                     :south {"R" {90  :west
                                  180 :north
                                  270 :east}
                             "L" {90  :east
                                  180 :north
                                  270 :west}}})

(defn next-state [{direction :direction :as state} {:keys [action value]}]
  (case action
    "N" (update-in state [:location 0] #(+ % value))
    "S" (update-in state [:location 0] #(- % value))
    "E" (update-in state [:location 1] #(+ % value))
    "W" (update-in state [:location 1] #(- % value))
    "L" (update-in state [:direction] #(get-in next-direction [% "L" value]))
    "R" (update-in state [:direction] #(get-in next-direction [% "R" value]))
    "F" (case direction
          :east (update-in state [:location 1] #(+ % value))
          :west (update-in state [:location 1] #(- % value))
          :north (update-in state [:location 0] #(+ % value))
          :south (update-in state [:location 0] #(- % value)))))

(defn distance [{[initial-x initial-y] :location} {[end-x end-y] :location}]
  (+ (Math/abs ^int (- initial-x end-x))
     (Math/abs ^int (- initial-y end-y))))

(defn part1
  ([]
   (part1 (input->instructions day12-input-path) (create-state)))
  ([instructions initial-state]
   (->> instructions
        (reduce #(next-state %1 %2) initial-state)
        (distance initial-state))))

(defn rotate-clockwise [[x y] degrees]
  (case degrees
    90 [y (* -1 x)]
    180 [(* -1 x) (* -1 y)]
    270 [(* -1 y) x]))

(defn rotate-counterclockwise [[x y] degrees]
  (case degrees
    90 [(* -1 y) x]
    180 [(* -1 x) (* -1 y)]
    270 [y (* -1 x)]))

(defn move-forward
  ([{waypoint-offset :waypoint-offset :as state} steps]
   (let [reducer (fn [acc _]
                   (update acc :location
                           (fn [prev]
                             [(+ (first prev) (first waypoint-offset))
                              (+ (second prev) (second waypoint-offset))])))]
     (->> (range steps)
          (reduce reducer state)))))

(defn next-state2 [state {:keys [action value]}]
  (case action
    "N" (update-in state [:waypoint-offset 1] #(+ % value))
    "S" (update-in state [:waypoint-offset 1] #(- % value))
    "E" (update-in state [:waypoint-offset 0] #(+ % value))
    "W" (update-in state [:waypoint-offset 0] #(- % value))
    "L" (update state :waypoint-offset #(rotate-counterclockwise % value))
    "R" (update state :waypoint-offset #(rotate-clockwise % value))
    "F" (move-forward state value)))

(defn part2
  ([]
   (part2 (input->instructions day12-input-path) (create-state)))
  ([instructions initial-state]
   (->> instructions
        (reduce #(next-state2 %1 %2) initial-state)
        (distance initial-state))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))
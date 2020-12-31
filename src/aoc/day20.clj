(ns aoc.day20
  (:require [clojure.string :as s]
            [clojure.core.matrix :as m]))

(def day20-input-path "resources/input-day20.txt")

(def sample-input "Tile 2311:\n..##.#..#.\n##..#.....\n#...##..#.\n####.#...#\n##.##.###.\n##...#.###\n.#.#.#..##\n..#....#..\n###...#.#.\n..###..###\n\nTile 1951:\n#.##...##.\n#.####...#\n.....#..##\n#...######\n.##.#....#\n.###.#####\n###.##.##.\n.###....#.\n..#.#..#.#\n#...##.#..\n\nTile 1171:\n####...##.\n#..##.#..#\n##.#..#.#.\n.###.####.\n..###.####\n.##....##.\n.#...####.\n#.##.####.\n####..#...\n.....##...\n\nTile 1427:\n###.##.#..\n.#..#.##..\n.#.##.#..#\n#.#.#.##.#\n....#...##\n...##..##.\n...#.#####\n.#.####.#.\n..#..###.#\n..##.#..#.\n\nTile 1489:\n##.#.#....\n..##...#..\n.##..##...\n..#...#...\n#####...#.\n#..#.#.#.#\n...#.#.#..\n##.#...##.\n..##.##.##\n###.##.#..\n\nTile 2473:\n#....####.\n#..#.##...\n#.##..#...\n######.#.#\n.#...#.#.#\n.#########\n.###.#..#.\n########.#\n##...##.#.\n..###.#.#.\n\nTile 2971:\n..#.#....#\n#...###...\n#.#.###...\n##.##..#..\n.#####..##\n.#..####.#\n#..#.#..#.\n..####.###\n..#.#.###.\n...#.#.#.#\n\nTile 2729:\n...#.#.#.#\n####.#....\n..#.#.....\n....#..#.#\n.##..##.#.\n.#.####...\n####.#.#..\n##.####...\n##..#.##..\n#.##...##.\n\nTile 3079:\n#.#.#####.\n.#..######\n..#.......\n######....\n####.#..#.\n.#...#.##.\n#.#####.##\n..#.###...\n..#.......\n..#.###...")

(defn header->id [header]
  (as-> header $
        (re-matches #"Tile (\d+):" $)
        (-> $ (get 1))
        (Integer/parseInt $)))

(defn body->edges [body]
  (let [top (String/valueOf (char-array (first body)))
        bottom (String/valueOf (char-array (last body)))
        left (String/valueOf (char-array (map first body)))
        right (String/valueOf (char-array (map last body)))]
    {:top    top
     :bottom bottom
     :left   left
     :right  right}))

(defn strs->tile [strs]
  (let [header (first strs)
        tile-id (header->id header)
        body (->> strs
                  (drop 1)
                  (mapv vec))
        edges (body->edges body)]
    {:header  header
     :body    body
     :tile-id tile-id
     :edges   edges}))

(defn str->tiles [str]
  (->> str
       (s/split-lines)
       (partition-by s/blank?)
       (filter #(< 1 (count %)))
       (map strs->tile)))

(defn input->tiles [input-path]
  (-> input-path
      (slurp)
      (str->tiles)))

(defn reverse-str [str]
  (.toString (doto (StringBuilder.)
               (.append str)
               (.reverse))))

(defn index-by-edges [tiles]
  (->> tiles
       (mapcat (fn [{edges :edges :as tile}]
                 (->> (vals edges)
                      (map #(hash-map % tile (reverse-str %) tile)))))
       (apply merge-with list)))

(defn corner? [edges-lookup {{:keys [top bottom left right]} :edges :as tile}]
  (cond
    (and (seq? (get edges-lookup top))
         (seq? (get edges-lookup right))
         (map? (get edges-lookup bottom))
         (map? (get edges-lookup left))) (assoc tile :corner [:top :right])
    (and (seq? (get edges-lookup top))
         (seq? (get edges-lookup left))
         (map? (get edges-lookup bottom))
         (map? (get edges-lookup right))) (assoc tile :corner [:top :left])
    (and (seq? (get edges-lookup bottom))
         (seq? (get edges-lookup right))
         (map? (get edges-lookup top))
         (map? (get edges-lookup left))) (assoc tile :corner [:bottom :right])
    (and (seq? (get edges-lookup bottom))
         (seq? (get edges-lookup left))
         (map? (get edges-lookup top))
         (map? (get edges-lookup right))) (assoc tile :corner [:bottom :left])))

(defn corners [tiles]
  (let [edges-lookup (index-by-edges tiles)]
    (->> tiles
         (map #(corner? edges-lookup %))
         (filter #(:corner %)))))

(defn part1
  ([]
   (part1 (input->tiles day20-input-path)))
  ([tiles]
   (->> (corners tiles)
        (map :tile-id)
        (reduce *))))

(defn flip-lr [m] (m/matrix (m/slice-map reverse m)))
(defn flip-ud [m] (m/matrix (reverse (m/rows m))))
(defn rotate-cw [m] (m/matrix (m/slice-map reverse (m/transpose m))))
(defn rotate-ccw [m] (m/matrix (reverse (m/transpose m))))
(defn vstack [& ms] (apply m/join-along 0 ms))
(defn hstack [& ms] (apply m/join-along 1 ms))

(defn as-top-right [{body :body [tb lr] :corner :as corner}]
  (cond
    ;already top-left
    (and (= tb :bottom)
         (= lr :right)) corner
    ;top-right - rotate ccw x1
    (and (= tb :bottom)
         (= lr :left)) (->> body
                            (rotate-ccw)
                            (assoc corner :body))
    ;bottom-left - rotate cw x1
    (and (= tb :top)
         (= lr :right)) (->> body
                             (rotate-cw)
                             (assoc corner :body))
    ;bottom-right - rotate cw/ccw x2
    (and (= tb :top)
         (= lr :left)) (->> body
                            (rotate-cw)
                            (rotate-cw)
                            (assoc corner :body))))

(def rotations {:top    {:top    identity
                         :right  rotate-cw
                         :bottom (comp rotate-cw rotate-cw)
                         :left   rotate-ccw}
                :bottom {:top    (comp rotate-cw rotate-cw)
                         :right  rotate-ccw
                         :bottom identity
                         :left   rotate-cw}
                :right  {:top    rotate-ccw
                         :right  identity
                         :bottom rotate-cw
                         :left   (comp rotate-cw rotate-cw)}
                :left   {:top    rotate-cw
                         :right  (comp rotate-cw rotate-cw)
                         :bottom rotate-ccw
                         :left   identity}})

(defn orient-tile
  "returns tile with body updated. body could be rotated and/or flipped according
  to matching the edge equal to 'by' and transforming such that the matched edge
  becomes the edge named by 'as'"
  [{:keys [body edges] :as tile} by as]
  (let [[edge-id _edge] (some (fn [[_ v :as x]]
                                (cond (= by v) x
                                      (= by (reverse-str v)) x))
                              edges)
        rotation (get-in rotations [edge-id as])
        body-rotated (rotation body)
        rotated-edge (as (body->edges body-rotated))
        body-flipped (if (= by (reverse-str rotated-edge))
                       (if (contains? #{:top :bottom} as)
                         (flip-lr body-rotated)
                         (flip-ud body-rotated))
                       body-rotated)]
    (assoc tile :body body-flipped)))

(defn matching-tile [edge-lookup {tile-id :tile-id} edge]
  (let [matching-tiles (get edge-lookup edge)]
    (if (map? matching-tiles)
      ;not a shared edge
      nil
      (first (filter #(not (= tile-id (:tile-id %))) matching-tiles)))))

(defn arrange-tiles [tiles corners]
  (let [edge-lookup (index-by-edges tiles)
        top-right (as-top-right (first corners))]
    (loop [grid []
           row []
           {body :body :as t} top-right]
      (let [edges (body->edges body)
            right-tile (matching-tile edge-lookup t (:right edges))
            bottom-tile (matching-tile edge-lookup t (:bottom edges))]
        (cond
          ;filling out row
          (not (nil? right-tile)) (let [next-oriented (orient-tile right-tile (:right edges) :left)]
                                    (recur grid (conj row t) next-oriented))
          ;end of row; move to next row
          (and (nil? right-tile)
               (not (nil? bottom-tile))) (let [complete-row (conj row t)
                                               {row-start-body :body
                                                :as            row-start} (first row)
                                               {bottom :bottom} (body->edges row-start-body)
                                               next-row-start (matching-tile edge-lookup row-start bottom)
                                               next-row-start-oriented (orient-tile next-row-start bottom :top)]
                                           (recur (conj grid complete-row) [] next-row-start-oriented))
          ;end of grid
          (and (nil? right-tile)
               (nil? bottom-tile)) (conj grid (conj row t)))))))

(defn remove-border [{body :body}]
  (m/to-nested-vectors (m/submatrix body [[1 (- (m/row-count body) 2)] [1 (- (m/column-count body) 2)]])))

(defn remove-borders [tiles-grid]
  (->> tiles-grid
       (map (fn [row]
              (->> row
                   (map remove-border))))))

(defn merge-bodies [bodies]
  (->> bodies
       (map (fn [row]
              (apply hstack row)))
       (apply vstack)))

(def sea-monster-str "                  # \n#    ##    ##    ###\n #  #  #  #  #  #   ")

(def sea-monster (mapv vec (s/split-lines sea-monster-str)))

(defn find-needle
  ([needle haystack]
   (let [[needle-rows needle-cols] (m/shape needle)
         [haystack-rows haystack-cols] (m/shape haystack)]
     (->> (for [row (range haystack-rows)
                col (range haystack-cols)
                :let [row-offset (+ row needle-rows)
                      col-offset (+ col needle-cols)]
                :when (and (<= row-offset haystack-rows)
                           (<= col-offset haystack-cols))]
            [row col])
          (map (fn [[row col]]
                 (find-needle needle haystack row col))))))
  ([needle haystack row-offset col-offset]
   (->> needle
        (map-indexed (fn [i line]
                       (map-indexed (fn [j char]
                                      (when (= \# char)
                                        (let [row (+ i row-offset)
                                              col (+ j col-offset)]
                                          (if (= \# (get-in haystack [row col]))
                                            [row col]
                                            false))))
                                    line)))
        (apply concat))))

(defn pattern-coords [grid pattern]
  (->> grid
       (find-needle pattern)
       (filter #(not-any? false? %))                        ; any false indicates a failed match
       (mapcat identity)
       (into #{})))

(defn character-coords [grid character]
  (let [[rows cols] (m/shape grid)
        all-coords (for [row (range rows)
                         col (range cols)]
                     [row col])]
    (filter (fn [[row col]]
              (= character (get-in grid [row col]))) all-coords)))

(defn part2
  ([]
   (part2 (input->tiles day20-input-path)))
  ([tiles]
   (let [corners (corners tiles)
         arranged (arrange-tiles tiles corners)
         borderless (remove-borders arranged)
         grid (merge-bodies borderless)
         ; todo rotate/flip sea-monster until a match - sea-monster is aligned already, luckily
         monster-coords (pattern-coords grid sea-monster)]
     (->> (character-coords grid \#)
          (filter #(not (contains? monster-coords %)))
          (count)))))

(defn -main []
  (let [part1-answer (part1)
        part2-answer (part2)]
    (println "part 1:" part1-answer)
    (println "part 2:" part2-answer)))
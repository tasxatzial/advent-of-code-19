(ns day03.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn str->int
  "Converts a string to integer."
  [s]
  (Integer/parseInt s))

(defn split-by-wire
  "Splits the input by \n"
  [s]
  (clojure.string/split s #"\n"))

(defn parse-wire
  "Parses the input of each wire and returns a
  vector consisting of vectors. Each subvector denotes
  a path segment with its first element denoting the
  direction and the second element the number of steps
  in that direction."
  [s]
  (let [wire-path-input (clojure.string/split s #",")
        direction #(first %)
        number #(str->int (apply str (rest %)))]
    (mapv #(vector (direction %) (number %))
          wire-path-input)))

;parse all input
(def parsed-input
  (let [wire-input (split-by-wire (slurp input-file))]
    (mapv parse-wire wire-input)))

(def wire1-instructions (first parsed-input))
(def wire2-instructions (second parsed-input))

(defn next-location
  "Finds the next wire location given a [x y] location and an
  instruction of the form [direction number]"
  [[x y] [direction number]]
  (case direction
    \R [(+ x number) y]
    \L [(- x number) y]
    \U [x (+ y number)]
    \D [x (- y number)]))

(defn wire-path
  "Returns a vector of locations based on the given wire instructions.
  Each location is denoted by a vector of two elements. First element
  denotes the wire direction (one of \\R \\L \\U \\D) and the second
  element denotes the corresponding wire end location."
  [wire-instructions]
  (loop [locations [[\I [0 0]]]
         wire-instructions wire-instructions]
    (if (seq wire-instructions)
      (let [last-loc (second (last locations))
            [direction _ :as instruction] (first wire-instructions)
            new-loc (next-location last-loc instruction)]
        (recur (conj locations [direction new-loc]) (rest wire-instructions)))
      locations)))

; --------------------------
; problem 1

; R R
(defn common-points-horizontal
  "Finds the common (integer) points of the two horizontal math
  vectors denoted as [[x1 y1] [x2 _]] and [[x3 y3] [x4 _]]."
  [[x1 y1] [x2 _] [x3 y3] [x4 _]]
  (if (and (= y1 y3) (<= x1 x4) (<= x3 x2))
    (for [x (range (max x1 x3) (inc (min x2 x4)))]
      [x y1])
    []))

; R U
(defn common-points-perpendicular
  "Finds the common point of the two perpendicular math
  vectors denoted as [[x1 _] [x2 y2]] and [[_ y3] [x4 y4]]."
  [[x1 _] [x2 y2] [_ y3] [x4 y4]]
  (if (and (<= x1 x4 x2) (<= y3 y2 y4))
    [[x4 y2]]
    []))

(defn common-points-vertical
  "Finds the common (integer) points of the two vertical math
  vectors denoted as [[x1 y1] [_ y2]] and [[x3 y3] [_ y4]]."
  [[x1 y1] [_ y2] [x3 y3] [_ y4]]
  (if (and (= x1 x3) (<= y1 y4) (<= y3 y2))
    (for [y (range (max y1 y3) (inc (min y2 y4)))]
      [x1 y])
    []))

(defn common-points
  "Finds the common (integer) points of two vertical or
  perpendicular math vectors denoted as [[_ l1] [d2 l2]] and [_ l3] [d4 l4].
  - d2 and d4 denote the orientation of each vector (one of \\R \\L \\U \\D)
  - l1, l3 denote the vector start points.
  - l2, l4 denote the vector end points.
  The explicit vector orientations d2 and d4 should match the implicit
  orientations described by the vector start and end points."
  [[_ l1] [d2 l2] [_ l3] [d4 l4]]
  (case d2
    \R (case d4
         \R (common-points-horizontal l1 l2 l3 l4)
         \U (common-points-perpendicular l1 l2 l3 l4)
         \L (common-points-horizontal l1 l2 l4 l3)
         \D (common-points-perpendicular l1 l2 l4 l3))
    \L (case d4
         \R (common-points-horizontal l2 l1 l3 l4)
         \U (common-points-perpendicular l2 l1 l3 l4)
         \L (common-points-horizontal l2 l1 l4 l3)
         \D (common-points-perpendicular l2 l1 l4 l3))
    \U (case d4
         \R (common-points-perpendicular l3 l4 l1 l2)
         \U (common-points-vertical l1 l2 l3 l4)
         \L (common-points-perpendicular l4 l3 l1 l2)
         \D (common-points-vertical l1 l2 l4 l3))
    \D (case d4
         \R (common-points-perpendicular l3 l4 l2 l1)
         \U (common-points-vertical l2 l1 l3 l4)
         \L (common-points-perpendicular l4 l3 l2 l1)
         \D (common-points-vertical l2 l1 l4 l3))))

(defn common-points-segment
  "Finds the common points of a wire-path and a wire segment
  denoted by [[_ l1] [d2 l2]].
  - l1 is the start location of the segment.
  - d2 is the direction of the segment (one of \\R \\L \\U \\D).
  - k2 us the end location of the segment."
  [wire-path [_ l1] [d2 l2]]
  (loop [[[_ l3] & rest-loc] wire-path
         points []]
    (if-let [[d4 l4] (first rest-loc)]
      (let [common-points (common-points [_ l1] [d2 l2] [_ l3] [d4 l4])]
        (if (seq common-points)
          (recur rest-loc (into points common-points))
          (recur rest-loc points)))
      points)))

(defn common-points-wires
  "Finds the common points of the two wire-paths."
  [wire1-path wire2-path]
  (loop [[[_ l1] & rest-loc] wire2-path
         points []]
    (if-let [[d2 l2] (first rest-loc)]
      (let [common-points (common-points-segment wire1-path [_ l1] [d2 l2])]
        (if (seq common-points)
          (recur rest-loc (into points common-points))
          (recur rest-loc points)))
      points)))

(defn manhattan-distance
  "Returns the manhattan distance of [x y] from [0 0]."
  [[x y]]
  (+ (Math/abs ^int x) (Math/abs ^int y)))

(defn min-manhattan-distance
  "Returns the minimum manhattan distance from [0 0] of the common points
  of the two wires. Both wires are expected to start from [0 0] therefore
  this point is excluded."
  []
  (let [wire1-path (wire-path wire1-instructions)
        wire2-path (wire-path wire2-instructions)
        cross-points (rest (common-points-wires wire1-path wire2-path))
        distances (mapv manhattan-distance cross-points)]
    (apply min distances)))

; ---------------------------------------
; results

(defn day02-1
  []
  (min-manhattan-distance))

(defn -main
  []
  (println (day02-1)))

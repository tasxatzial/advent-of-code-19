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

(defn parse-wire-input
  "Parses the input of each wire and returns a
  vector consisting of 2-element vectors. Each vector denotes
  an instruction with its first element denoting the
  direction and the second element the number of steps
  in that direction."
  [s]
  (let [instructions (clojure.string/split s #",")
        direction #(first %)
        number #(str->int (apply str (rest %)))]
    (mapv #(vector (direction %) (number %))
          instructions)))

;parse all input
(def parsed-input
  (let [wire-input (split-by-wire (slurp input-file))]
    (mapv parse-wire-input wire-input)))

(def wire1-instructions (first parsed-input))
(def wire2-instructions (second parsed-input))

(defn next-location
  "Finds the next wire location given a [x y] location and an
  instruction of the form [direction steps]"
  [[x y] [direction steps]]
  (case direction
    \R [(+ x steps) y]
    \L [(- x steps) y]
    \U [x (+ y steps)]
    \D [x (- y steps)]))

(defn create-wire-path
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

(def memoized-create-wire-path (memoize create-wire-path))

; --------------------------
; problem 1

; R R
(defn find-common-points-horizontal
  "Finds the common (integer) points of the two horizontal math
  vectors denoted as [[x1 y1] [x2 _]] and [[x3 y3] [x4 _]]."
  [[x1 y1] [x2 _] [x3 y3] [x4 _]]
  (if (and (= y1 y3) (<= x1 x4) (<= x3 x2))
    (for [x (range (max x1 x3) (inc (min x2 x4)))]
      [x y1])
    []))

; R U
(defn find-common-points-perpendicular
  "Finds the common point of the two perpendicular math
  vectors denoted as [[x1 _] [x2 y2]] and [[_ y3] [x4 y4]]."
  [[x1 _] [x2 y2] [_ y3] [x4 y4]]
  (if (and (<= x1 x4 x2) (<= y3 y2 y4))
    [[x4 y2]]
    []))

(defn find-common-points-vertical
  "Finds the common (integer) points of the two vertical math
  vectors denoted as [[x1 y1] [_ y2]] and [[x3 y3] [_ y4]]."
  [[x1 y1] [_ y2] [x3 y3] [_ y4]]
  (if (and (= x1 x3) (<= y1 y4) (<= y3 y2))
    (for [y (range (max y1 y3) (inc (min y2 y4)))]
      [x1 y])
    []))

(defn find-common-points
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
         \R (find-common-points-horizontal l1 l2 l3 l4)
         \U (find-common-points-perpendicular l1 l2 l3 l4)
         \L (find-common-points-horizontal l1 l2 l4 l3)
         \D (find-common-points-perpendicular l1 l2 l4 l3))
    \L (case d4
         \R (find-common-points-horizontal l2 l1 l3 l4)
         \U (find-common-points-perpendicular l2 l1 l3 l4)
         \L (find-common-points-horizontal l2 l1 l4 l3)
         \D (find-common-points-perpendicular l2 l1 l4 l3))
    \U (case d4
         \R (find-common-points-perpendicular l3 l4 l1 l2)
         \U (find-common-points-vertical l1 l2 l3 l4)
         \L (find-common-points-perpendicular l4 l3 l1 l2)
         \D (find-common-points-vertical l1 l2 l4 l3))
    \D (case d4
         \R (find-common-points-perpendicular l3 l4 l2 l1)
         \U (find-common-points-vertical l2 l1 l3 l4)
         \L (find-common-points-perpendicular l4 l3 l2 l1)
         \D (find-common-points-vertical l2 l1 l4 l3))))

(defn find-common-points-segment
  "Finds the common points of a wire and a wire segment denoted by [[_ l1] [d2 l2]].
  - l1 is the start location of the segment.
  - d2 is the direction of the segment (one of \\R \\L \\U \\D).
  - k2 is the end location of the segment."
  [wire-path [_ l1] [d2 l2]]
  (loop [[[_ l3] & rest-loc] wire-path
         points []]
    (if-let [[d4 l4] (first rest-loc)]
      (let [new-common-points (find-common-points [_ l1] [d2 l2] [_ l3] [d4 l4])]
        (if (seq new-common-points)
          (recur rest-loc (into points new-common-points))
          (recur rest-loc points)))
      points)))

(defn find-common-points-paths
  "Finds the common points of the given wires."
  [wire1-path wire2-path]
  (loop [[[_ l1] & rest-path] wire2-path
         common-points []]
    (if-let [[d2 l2] (first rest-path)]
      (let [new-common-points (find-common-points-segment wire1-path [_ l1] [d2 l2])]
        (if (seq new-common-points)
          (recur rest-path (into common-points new-common-points))
          (recur rest-path common-points)))
      common-points)))

(def memoized-find-common-points-paths (memoize find-common-points-paths))

(defn manhattan-distance
  "Returns the manhattan distance of [x y] from [0 0]."
  [[x y]]
  (+ (Math/abs ^int x) (Math/abs ^int y)))

(defn min-manhattan-distance
  "Returns the minimum manhattan distance from [0 0] of the common points
  of the two wires. Both wires are expected to start from [0 0] therefore
  this point is excluded."
  []
  (let [wire1-path (memoized-create-wire-path wire1-instructions)
        wire2-path (memoized-create-wire-path wire2-instructions)
        common-path-points (rest (memoized-find-common-points-paths wire1-path wire2-path))
        distances (mapv manhattan-distance common-path-points)]
    (apply min distances)))

; --------------------------
; problem 2

(defn add-wire-path-steps
  "Updates the given wire-path by appending to each path location the number of steps
  from the start of the path to that location."
  [wire-path wire-instructions]
  (loop [[path-segment & rest-path] (rest wire-path)
         [[_ steps] & rest-instructions] wire-instructions
         new-path []
         total-steps 0]
    (if path-segment
      (let [new-segment (conj path-segment (+ total-steps steps))
            new-path (conj new-path new-segment)
            new-total-steps (+ total-steps steps)]
        (recur rest-path rest-instructions new-path new-total-steps))
      new-path)))

(defn calc-point-steps
  "Calculates the minimum steps from the start of the wire-path to a wire
  location [x y]. Wire locations can be any integer point on a horizontal or
  vertical path segment (denoted by two consecutive wire-path elements)."
  [wire-path [x y]]
  (loop [[[d1 [x1 y1] dist1] & rest-path] wire-path]
    (if d1
      (if-let [[_ [x2 y2] _] (first rest-path)]
        (cond
          (= y y1) (cond
                     (<= x1 x x2) (+ dist1 (- x x1))
                     (<= x2 x x1) (+ dist1 (- x1 x)))
          (= x x1) (cond
                     (<= y1 y y2) (+ dist1 (- y y1))
                     (<= y2 y y1) (+ dist1 (- y1 y)))
          :else (recur rest-path))))))

(defn calc-common-points-steps
  "Calls calc-point-steps for each point in the given common-points and collects
  the steps in a vector."
  [wire-path common-points]
  (mapv #(calc-point-steps wire-path %) common-points))

(defn calc-min-steps-sum
  "Finds the fewest combined steps the wires must take to reach a common point."
  []
  (let [wire1-path (add-wire-path-steps (memoized-create-wire-path wire1-instructions) wire1-instructions)
        wire2-path (add-wire-path-steps (memoized-create-wire-path wire2-instructions) wire2-instructions)
        common-points-wires (memoized-find-common-points-paths wire1-path wire2-path)
        wire1-distances (calc-common-points-steps wire1-path common-points-wires)
        wire2-distances (calc-common-points-steps wire2-path common-points-wires)]
    (apply min (map + wire1-distances wire2-distances))))

; ---------------------------------------
; results

(defn day03-1
  []
  (min-manhattan-distance))

(defn day03-2
  []
  (calc-min-steps-sum))

(defn -main
  []
  (println (day03-1))
  (println (day03-2)))

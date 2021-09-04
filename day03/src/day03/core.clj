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
  "Returns a vector of locations based on the given wire instructions."
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
  "Finds the common (integer) points of the two horizontal
  vectors denoted as [[x1 y1] [x2 _]] and [[x3 y3] [x4 _]].
  [x1 y1] and [x3 y3] are their starting points."
  [[x1 y1] [x2 _] [x3 y3] [x4 _]]
  (if (and (= y1 y3) (<= x1 x4) (<= x3 x2))
    (for [x (range (max x1 x3) (inc (min x2 x4)))]
      [x y1])
    []))

; R U
(defn common-points-perpendicular
  "Finds the common point of the two perpendicular
   vectors denoted as [[x1 _] [x2 y2]] and [[_ y3] [x4 y4]].
   [x1 _] and [_ y3] are their starting points."
  [[x1 _] [x2 y2] [_ y3] [x4 y4]]
  (if (and (<= x1 x4 x2) (<= y3 y2 y4))
    [[x4 y2]]
    []))

(defn common-points-vertical
  "Finds the common (integer) points of the two vertical
  vectors denoted as [[x1 y1] [_ y2]] and [[x3 y3] [_ y4]].
  [x1 y1] and [x3 y3] are their starting points."
  [[x1 y1] [_ y2] [x3 y3] [_ y4]]
  (if (and (= x1 x3) (<= y1 y4) (<= y3 y2))
    (for [y (range (max y1 y3) (inc (min y2 y4)))]
      [x1 y])
    []))

(defn -main
  []
  (println (wire-path wire1-instructions)))

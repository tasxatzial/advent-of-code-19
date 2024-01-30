(ns day03.core
  (:gen-class)
  (:require [clojure.set :as set]))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-wire-segment
  "Parses a string that has the form \"X123\" to the vector of two elements.
  The first element is a char that denotes a direction. The second element
  is an integer that denotes the segment length."
  [s]
  [(.charAt s 0) (Integer/parseInt (subs s 1))])

(defn parse-wire-path
  "Parses a string that represents a wire path, to a vector of its segments.
  The form of each segment is described in the function parse-wire-segment."
  [s]
  (->> (clojure.string/split s #",")
       (map parse-wire-segment)))

(defn parse-file
  "Reads and parses the input file into a vector of two wire paths.
  The form of each path is described in the function parse-wire-path."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (mapv parse-wire-path)))

(def memoize-input-file->wire-paths (memoize parse-file))

(defn create-segment-locations
  "Returns a vector of all locations of a segment that starts
  from [x y] and has the given direction and length."
  [[x y] [direction length]]
  (let [r (range 1 (inc length))]
    (case direction
      \R (mapv #(vector (+ x %) y) r)
      \L (mapv #(vector (- x %) y) r)
      \U (mapv #(vector x (+ y %)) r)
      \D (mapv #(vector x (- y %)) r))))

(defn create-wire-locations
  "Returns a vector of all locations [x y] in a wire path. Initial location
  is [0 0]. The form of the path is described in the function parse-wire-path."
  [path]
  (loop [locations [[0 0]]
         path path]
    (if (seq path)
      (let [segment (first path)
            new-locations (create-segment-locations (peek locations) segment)]
        (recur (into locations new-locations) (rest path)))
      locations)))

; --------------------------
; problem 1

(defn compute-manhattan-distance
  "Returns the manhattan distance of the given location [x y]."
  [loc]
  (+ (Math/abs ^int (loc 0)) (Math/abs ^int (loc 1))))

(defn find-distance-of-closest-intersection
  "Returns the minimum Manhattan distance among common locations in
  the two given collections of locations."
  [locations1 locations2]
  (let [common-locations (set/intersection (set locations1) (set locations2))]
    (->> (disj common-locations [0 0])                      ;point [0 0] doesn't count
         (map compute-manhattan-distance)
         (apply min))))

; --------------------------
; problem 2

(defn find-index
  "Returns the index of the first occurrence of loc in locations."
  [loc locations]
  (loop [idx 0
         [location & rest-locations] locations]
    (if location
      (if (= location loc)
        idx
        (recur (inc idx) rest-locations))
      -1)))

(defn find-min-steps-to-intersection
  "Returns the fewest combined steps required to reach each common
  location in the two given collections of locations."
  [locations1 locations2]
  (let [common-locations (set/intersection (set locations1) (set locations2))
        find-wire1-index #(find-index % locations1)
        find-wire2-index #(find-index % locations2)]
    (->> (disj common-locations [0 0])
         (map #(+ (find-wire1-index %) (find-wire2-index %)))
         (apply min))))

; ---------------------------------------
; results

(defn day03
  [f]
  (let [wire-paths (memoize-input-file->wire-paths)
        wire1-path (first wire-paths)
        wire2-path (second wire-paths)
        wire1-locations (create-wire-locations wire1-path)
        wire2-locations (create-wire-locations wire2-path)]
    (f wire1-locations wire2-locations)))

(defn day03-1
  []
  (day03 find-distance-of-closest-intersection))

(defn day03-2
  []
  (day03 find-min-steps-to-intersection))

(defn -main
  []
  (println (time (day03-1)))
  (println (time (day03-2))))

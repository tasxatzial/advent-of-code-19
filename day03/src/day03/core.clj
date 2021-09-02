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

(def wire1-path (first parsed-input))
(def wire2-path (second parsed-input))

(defn next-location
  "Finds the next wire location given a [x y] location and an
  instruction of the form [direction number]"
  [[x y] [direction number]]
  (case direction
    \R [(+ x number) y]
    \L [(- x number) y]
    \U [x (+ y number)]
    \D [x (- y number)]))

(defn wire-locations
  "Returns a vector of locations based on the given wire path."
  [wire-path]
  (loop [locations [[0 0]]
         wire-path wire-path]
    (if (seq wire-path)
      (let [last-loc (last locations)
            instruction (first wire-path)
            new-loc (next-location last-loc instruction)]
        (recur (conj locations new-loc) (rest wire-path)))
      locations)))

; --------------------------
; problem 1


(defn -main
  []
  (println (wire-locations wire1-path)))

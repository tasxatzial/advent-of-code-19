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

(defn -main
  []
  (println wire1-path))

(ns day01.core
  (:gen-class))

; --------------------------
; common

(def input "resources\\input.txt")

(defn str->int
  "Converts a string to integer."
  [s]
  (Integer/parseInt s))

(defn parse
  "Splits the input string by \n and converts it into a list of numbers."
  [s]
  (map str->int (clojure.string/split s #"\n")))

(def parsed-input (parse (slurp input)))

; --------------------------
; problem 1

(defn fuel
  [module]
  (Math/round (- (Math/floor (/ module 3)) 2)))

; ---------------------------------------
; results

(defn day01-1
  []
  (apply + (map fuel parsed-input)))

(defn -main
  []
  (println (day01-1)))

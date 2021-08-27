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

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))

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
  "Returns the required fuel for a given module"
  [module]
  (Math/round (- (Math/floor (/ module 3)) 2)))

; --------------------------
; problem 2

(defn fuel-list
  "Returns the fuel list for a given module"
  [module]
  (loop [fuels []
         module module]
    (let [f (fuel module)]
      (if (pos? f)
        (recur (conj fuels f) f)
        fuels))))

(defn total-fuel
  "Returns the total fuel for a given module"
  [module]
  (apply + (fuel-list module)))

; ---------------------------------------
; results

(defn day01-1
  []
  (apply + (map fuel parsed-input)))

(defn day01-2
  []
  (apply + (map total-fuel parsed-input)))

(defn -main
  []
  (println (day01-1))
  (println (day01-2)))

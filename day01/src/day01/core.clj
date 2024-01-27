(ns day01.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-file
  "Reads and parses the input file into a vector of numbers."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map #(Integer/parseInt %))))

(def memoized-input-file->masses (memoize parse-file))

; --------------------------
; problem 1

(defn p1_calc-fuel
  "Returns the required fuel for a given mass."
  [mass]
  (Math/round (- (Math/floor (/ mass 3)) 2)))

; --------------------------
; problem 2

(defn p2_calc-fuel
  "Returns the required fuel for a given mass."
  [mass]
  (loop [total-fuel 0
         mass mass]
    (let [fuel (p1_calc-fuel mass)]
      (if (pos? fuel)
        (recur (+ total-fuel fuel) fuel)
        total-fuel))))

; ---------------------------------------
; results

(defn day01-1
  []
  (->> (memoized-input-file->masses)
       (map p1_calc-fuel)
       (reduce +)))

(defn day01-2
  []
  (->> (memoized-input-file->masses)
       (map p2_calc-fuel)
       (reduce +)))

(defn -main
  []
  (println (day01-1))
  (println (day01-2)))

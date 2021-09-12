(ns day06.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-input
  "Parses the input and returns a map of orbits. A key
  represents the object that orbits the corresponding value."
  [s]
  (let [lines (clojure.string/split s #"\n")]
    (reduce (fn [result line]
              (let [[obj1 obj2] (clojure.string/split line #"\)")]
                (conj result [(keyword obj2) (keyword obj1)])))
            {} lines)))

(def orbits (parse-input (slurp input-file)))

; --------------------------
; problem 1

(defn count-object-orbits
  "Counts the orbits of obj."
  [obj]
  (loop [result 0
         obj (obj orbits)]
    (if obj
      (recur (inc result) (obj orbits))
      result)))

(defn count-orbits
  "Counts the total orbits."
  []
  (reduce (fn [result [obj1 _]]
            (+ result (count-object-orbits obj1)))
          0 orbits))

; ---------------------------------------
; results

(defn day06-1
  []
  (count-orbits))

(defn -main
  []
  (println (day06-1)))

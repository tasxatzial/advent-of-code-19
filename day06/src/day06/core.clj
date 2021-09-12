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

(defn -main
  []
  (println orbits))

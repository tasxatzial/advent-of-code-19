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
  "Counts the number of objects the given obj orbits."
  [obj]
  (loop [result 0
         obj (obj orbits)]
    (if obj
      (recur (inc result) (obj orbits))
      result)))

(defn count-orbits
  "Counts the total number of orbits."
  []
  (reduce (fn [result [obj1 _]]
            (+ result (count-object-orbits obj1)))
          0 orbits))

; --------------------------
; problem 2

(defn find-object-orbits
  "Returns a vector of the objects the given obj orbits."
  [obj]
  (loop [obj-orbits []
         obj (obj orbits)]
    (if obj
      (recur (conj obj-orbits obj) (obj orbits))
      obj-orbits)))

(defn count-orbital-transfers
  "Counts the number of orbital transfers (in the given
  sequence of object-orbits) required to reach the given obj.
  Returns -1 if obj cannot be reached."
  [obj object-orbits]
  (loop [result 0
         [curr-obj & rest-objects] object-orbits]
    (if curr-obj
      (if (= curr-obj obj)
        result
        (recur (inc result) rest-objects))
      -1)))

; ---------------------------------------
; results

(defn day06-1
  []
  (count-orbits))

(defn -main
  []
  (println (find-object-orbits :YOU)))

(ns day06.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-line
  "Parses an input line into a vector of two keywords. For example
  \"W6M)G8S\" is parsed into [:W6M :G8S]."
  [s]
  (->> (clojure.string/split s #"\)")
       reverse
       (mapv keyword)))

(defn parse-file
  "Parses the input and returns a map. Keys and values are keywords.
  Keys represent the objects that orbit their corresponding value."
  []
  (->> input-file
       slurp
       clojure.string/split-lines
       (map parse-line)
       (into {})))

(def memoized-input-file->orbits (memoize parse-file))

; -------------------------
; problem 1

(defn count-object-orbits
  "Counts the number of objects the given obj orbits."
  [obj]
  (let [orbits (memoized-input-file->orbits)]
    (loop [result 0
           obj (obj orbits)]
      (if obj
        (recur (inc result) (obj orbits))
        result))))

(defn count-orbits
  "Counts the total number of orbits."
  []
  (->> (memoized-input-file->orbits)
       keys
       (map count-object-orbits)
       (reduce +)))

; --------------------------
; problem 2

(defn find-object-orbits
  "Returns a vector of the objects the given obj orbits."
  [obj]
  (->> obj
       (iterate (memoized-input-file->orbits))
       (take-while some?)
       rest
       vec))

(defn count-orbital-transfers
  "Returns the number of objects required to reach the given obj.
  Returns -1 if obj cannot be reached."
  [obj object-orbits]
  (let [after-obj (drop-while #(not= % obj) object-orbits)]
    (if (seq after-obj)
      (- (count object-orbits) (count after-obj))
      -1)))

(defn find-min-orbital-transfers
  "Finds the minimum number of orbital transfers required to move from
  the object :YOU is orbiting to the object :SAN is orbiting."
  []
  (let [you-orbits (find-object-orbits :YOU)
        santa-orbits (find-object-orbits :SAN)
        santa-orbits-set (set santa-orbits)]
    (loop [index 0
           min-transfers Integer/MAX_VALUE]
      (if-let [obj (get you-orbits index)]
        (if (contains? santa-orbits-set obj)
          (recur (inc index) (min min-transfers (+ index (count-orbital-transfers obj santa-orbits))))
          (recur (inc index) min-transfers))
        min-transfers))))

; ---------------------------------------
; results

(defn day06-1
  []
  (count-orbits))

(defn day06-2
  []
  (find-min-orbital-transfers))

(defn -main
  []
  (println (day06-1))
  (println (day06-2)))

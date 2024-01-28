(ns day02.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn parse-file
  "Reads and parses the input file into a vector of numbers."
  []
  (mapv #(Integer/parseInt %)
        (clojure.string/split (-> input-file slurp) #"[,\n]")))

(def memoized-input-file->program (memoize parse-file))

(defn init-program
  "Updates the program with input1 and input2 at indices
  1 and 2 respectively."
  [input1 input2]
  (-> (memoized-input-file->program)
      (assoc 1 input1)
      (assoc 2 input2)))

; --------------------------
; problem 1

(defn exec-instruction
  "Executes the given instruction and returns the result.
  Returns nil if the code isn't 1 or 2."
  [code param1 param2]
  (case code
    1 (+ param1 param2)
    2 (* param1 param2)
    nil))

(defn exec-program
  "Executes the instructions and returns the final program."
  [program]
  (loop [i 0
         program program]
    (let [opcode (get program i)]
      (if (= opcode 99)
        program
        (let [param1 (get program (get program (inc i)))
              param2 (get program (get program (+ i 2)))
              write-idx (get program (+ i 3))
              result (exec-instruction opcode param1 param2)]
          (recur (+ i 4) (assoc program write-idx result)))))))

; --------------------------
; problem 2

(defn find-inputs
  "Returns a vector of the two inputs that we need to initialize
  the program with so that the given output is at index 0 when
  the program halts."
  [output]
  (loop [candidates (for [x (range 100) y (range 100)] [x y])]
    (when (seq candidates)
      (let [candidate (first candidates)
            initial-program (apply init-program candidate)]
        (if (= output (first (exec-program initial-program)))
          candidate
          (recur (rest candidates)))))))

; ---------------------------------------
; results

(defn day02-1
  []
  (first (exec-program (init-program 12 2))))

(defn day02-2
  []
  (let [[input1 input2] (find-inputs 19690720)]
    (+ input2 (* 100 input1))))

(defn -main
  []
  (println (day02-1))
  (println (day02-2)))

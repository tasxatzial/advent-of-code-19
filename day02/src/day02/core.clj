(ns day02.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(defn str->int
  "Converts a string to integer."
  [s]
  (Integer/parseInt s))

(defn parse
  "Splits the input string by , or \n and converts it into a list of numbers."
  [s]
  (mapv str->int (clojure.string/split s #"[,\n]")))

(def intcodes (parse (slurp input-file)))

(defn init-intcodes
  "Initializes intcodes with input1 and input2 at positions
  1 and 2 respectively."
  [input1 input2]
  (assoc (assoc intcodes 1 input1) 2 input2))

; --------------------------
; problem 1

; replace position 1 with value 12, position 2 with value 2
(def updated-intcodes (init-intcodes 12 2))

(defn opcode-result
  "Returns a result given an opcode (1 or 2) and two inputs.
  Returns -1 if opcode is not 1 or 2."
  [code input1 input2]
  (case code
    1 (+ input1 input2)
    2 (* input1 input2)
    -1))

(defn run-intcodes
  "Runs the list of intcodes and returns the updated list."
  [codes]
  (loop [i 0
         codes codes]
    (let [opcode (get codes i)
          input1-pos (get codes (inc i))
          input1 (get codes input1-pos)
          input2-pos (get codes (+ i 2))
          input2 (get codes input2-pos)
          output-pos (get codes (+ i 3))
          result (opcode-result opcode input1 input2)]
      (if (= -1 result)
        codes
        (recur (+ i 4) (assoc codes output-pos result))))))

; --------------------------
; problem 2

(defn find-inputs
  "Returns the inputs that produce the given output."
  [output]
  (let [candidates (for [x (range 100)
                         y (range 100)]
                     [x y])]
    (loop [candidates candidates]
      (when (seq candidates)
        (let [[x y] (first candidates)
              updated-intcodes (init-intcodes x y)]
          (if (= output (first (run-intcodes updated-intcodes)))
            [x y]
            (recur (rest candidates))))))))

; ---------------------------------------
; results

(defn day02-1
  []
  (first (run-intcodes updated-intcodes)))

(defn day02-2
  []
  (let [[input1 input2] (find-inputs 19690720)]
    (+ input2 (* 100 input1))))

(defn -main
  []
  (println (day02-1))
  (println (day02-2)))

(ns day02.core
  (:gen-class))

; --------------------------
; common

(def input "resources\\input.txt")

(defn str->int
  "Converts a string to integer."
  [s]
  (Integer/parseInt s))

(defn parse
  "Splits the input string by , or \n and converts it into a list of numbers."
  [s]
  (mapv str->int (clojure.string/split s #"[,\n]")))

(def parsed-input (parse (slurp input)))

; --------------------------
; problem 1

; replace position 1 with value 12, position 2 with value 2
(def modified-parsed-input (assoc (assoc parsed-input 1 12) 2 2))

(defn opcode-result
  "Returns a result given an opcode (1 or 2) and two inputs.
  Returns -1 if opcode is not 1 or 2."
  [code input1 input2]
  (case code
    1 (+ input1 input2)
    2 (* input1 input2)
    -1))

(defn run-intcodes
  "Runs the list of intcodes."
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

; ---------------------------------------
; results

(defn day01-1
  []
  (first (run-intcodes modified-parsed-input)))

(defn -main
  []
  (println (day01-1)))

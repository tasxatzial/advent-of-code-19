(ns day04.core
  (:gen-class))

; --------------------------
; common

(defn int->digits
  "Converts an integer to a sequence of its digits."
  [s]
  (map #(Character/digit ^char % 10) (str s)))

(def low-pass 172851)
(def high-pass 675869)

(defn count-valid-passwords
  "Counts the valid passwords contained in [low-pass high-pass].
  Accepts a predicate function."
  [valid-password?]
  (->> (range low-pass (inc high-pass))
       (map int->digits)
       (filter valid-password?)
       count))

; --------------------------
; problem 1

(defn p1_valid-password?
  "Checks whether the given list of digits represents a valid password.
  Ignores the range rule."
  [digits]
  (and (apply <= digits)
       (not (apply < digits))))

; --------------------------
; problem 2

(defn p2_valid-password?
  "Checks whether the given list of digits represents a valid password.
  Ignores the range rule."
  [digits]
  (and (apply <= digits)
       (some #(= 2 (count %))
             (partition-by identity digits))))

; ---------------------------------------
; results

(defn day04-1
  []
  (count-valid-passwords p1_valid-password?))

(defn day04-2
  []
  (count-valid-passwords p2_valid-password?))

(defn -main
  []
  (println (day04-1))
  (println (day04-2)))

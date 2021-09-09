(ns day04.core
  (:gen-class))

(defn int->digits
  "Converts an integer to a sequence of its digits."
  [s]
  (map #(Character/digit ^char % 10)
       (str s)))

(def low-num 172851)
(def high-num 675869)

; --------------------------
; problem 1

(defn valid-password?
  [digits]
  (and (apply <= digits) (not (apply < digits))))

(defn count-valid-passwords1
  "Counts the valid passwords contained in [low-num high-num]"
  []
  (->> (range low-num (inc high-num))
       (map int->digits)
       (filter valid-password?)
       count))

; ---------------------------------------
; results

(defn day04-1
  []
  (count-valid-passwords1))

(defn -main
  []
  (println (day04-1)))

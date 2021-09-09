(ns day04.core
  (:gen-class))

(defn int->digits
  "Converts an integer to a sequence of its digits."
  [s]
  (map #(Character/digit ^char % 10) (str s)))

(def low-num 172851)
(def high-num 675869)

; --------------------------
; problem 1

(defn valid-password1?
  "Checks whether a digits list represents a valid password.
  Ignores the range rule."
  [digits]
  (and (apply <= digits)
       (not (apply < digits))))

(defn count-valid-passwords
  "Counts the valid passwords contained in [low-num high-num]"
  [check-password-fn]
  (->> (range low-num (inc high-num))
       (map int->digits)
       (filter check-password-fn)
       count))

; --------------------------
; problem 2

(defn valid-password2?
  "Checks whether a digits list represents a valid password.
  Ignores the range rule."
  [digits]
  (and (apply <= digits)
       (some #(= 2 (count %))
             (partition-by identity digits))))

; ---------------------------------------
; results

(defn day04-1
  []
  (count-valid-passwords valid-password1?))

(defn day04-2
  []
  (count-valid-passwords valid-password2?))

(defn -main
  []
  (println (day04-1))
  (println (day04-2)))

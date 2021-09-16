(ns day08.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(def wide 25)
(def tall 6)

(defn parse-input-file
  "Parses the input file and creates the layers of the image.
  The returned structure is a list of layers. Each layer
  consists of 'tall' number of lists, each list contains
  'wide' total numbers."
  []
  (->> (slurp input-file)
       (map #(Character/digit ^char % 10))
       (partition wide)
       (partition tall)))

(def layers (parse-input-file))

(defn -main
  []
  (println layers))

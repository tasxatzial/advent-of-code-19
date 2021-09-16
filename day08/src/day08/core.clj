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

; --------------------------
; problem 1

(defn count-layer-num
  "Counts the number of nums in the given layer."
  [layer num]
  (reduce (fn [result row]
            (+ result (count (filter #(= num %) row))))
          0 layer))

(defn find-min-zeros-layer
  "Finds the layer with the min number of zeros."
  []
  (let [layer-zeros (mapv #(count-layer-num % 0) layers)
        min-zeros (apply min layer-zeros)]
    (loop [index 0]
      (let [zero-count (get layer-zeros index)]
        (if (= zero-count min-zeros)
          (nth layers index)
          (recur (inc index)))))))

; ---------------------------------------
; results

(defn day08-1
  []
  (let [min-zeros-layer (find-min-zeros-layer)
        ones-count (count-layer-num min-zeros-layer 1)
        twos-count (count-layer-num min-zeros-layer 2)]
    (* ones-count twos-count)))

(defn -main
  []
  (println (day08-1)))

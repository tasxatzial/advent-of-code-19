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
       (partition (* wide tall))))

(def layers (parse-input-file))

; --------------------------
; problem 1

(defn count-layer-num
  "Counts the number of nums in the given layer."
  [layer num]
  (count (filter #(= num %) layer)))

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

; --------------------------
; problem 2

(defn find-first-visible
  "Finds the first visible pixel in position index."
  [index]
  (let [i-pixels (map #(nth % index) layers)]
    (some #(and (not= 2 %) %) i-pixels)))

(defn decode-image
  "Decodes the image by finding the first visible pixel in every position."
  []
  (let [layer-pixel-count (count (first layers))]
    (loop [result []
           index 0]
      (if (= layer-pixel-count index)
        (partition wide result)
        (recur (conj result (find-first-visible index))
               (inc index))))))

; ---------------------------------------
; results

(defn day08-1
  []
  (let [min-zeros-layer (find-min-zeros-layer)
        ones-count (count-layer-num min-zeros-layer 1)
        twos-count (count-layer-num min-zeros-layer 2)]
    (* ones-count twos-count)))

(defn day08-2
  []
  (decode-image))

(defn -main
  []
  (println (day08-1))
  (println (day08-2)))

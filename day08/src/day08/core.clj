(ns day08.core
  (:gen-class))

; --------------------------
; common

(def input-file "resources\\input.txt")

(def image-width 25)
(def image-height 6)

(defn parse-file
  "Reads and parses the input file into a vector of vectors.
  Each vector corresponds to an image layer and contains image-height
  number of vectors each containing image-width integers."
  []
  (->> input-file
       slurp
       (map #(Character/digit ^char % 10))
       (partition (* image-width image-height))
       (map #(partition image-width %))
       (map #(map vec %))
       (mapv vec)))

(def memoized-input-file->image (memoize parse-file))

; --------------------------
; problem 1

(defn count-value
  "Returns the val count in the given layer."
  [layer val]
  (->> layer
       (map #(filter #{val} %))
       (map count)
       (reduce +)))

(defn find-min-zeros-layer
  "Finds the layer with the min number of zeros."
  []
  (loop [[layer & rest-layers] (memoized-input-file->image)
         min-layer nil
         min-zeros (* image-width image-height)]
    (if layer
      (let [zero-count (count-value layer 0)]
        (if (< zero-count min-zeros)
          (recur rest-layers layer zero-count)
          (recur rest-layers min-layer min-zeros)))
      min-layer)))

; --------------------------
; problem 2

(defn get-pixel-val
  "Returns the value of the pixel at the given layer,row,col."
  [layer row col]
  (get-in layer [row col]))

(defn get-pixel-vals
  "Returns a sequence of the pixel values at the given row,col."
  [row col]
  (->> (memoized-input-file->image)
       (map #(get-pixel-val % row col))))

(defn find-first-visible
  "Finds the value of the first visible pixel at the given row,col."
  [row col]
  (->> (get-pixel-vals row col)
       (some #(and (not= 2 %) %))))

(defn decode-image
  "Returns the decoded image as a string."
  []
  (let [decoded-pixels (for [row (range image-height)
                             col (range image-width)]
                         (find-first-visible row col))]
    (->> decoded-pixels
         (partition image-width)
         (map #(apply str %))
         (clojure.string/join "\n"))))

; ---------------------------------------
; results

(defn day08-1
  []
  (let [min-layer (find-min-zeros-layer)
        ones-count (count-value min-layer 1)
        twos-count (count-value min-layer 2)]
    (* ones-count twos-count)))

(defn day08-2
  []
  (decode-image))

(defn -main
  []
  (println (day08-1))
  (println (day08-2)))

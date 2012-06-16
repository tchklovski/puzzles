(ns path-count.state
  "State representation for the path count puzzle")

;; ### Sugar and enums
(def empty-cell 0)
(def taken-cell 1)
(def start-cell 2)
(def finish-cell 3)

(defrecord State [cells
                  total-length row-length
                  start-idx finish-idx])

;; we make "taken" be 0 rather than 1 so that `zero?` means filled
(def mark-cell-taken bit-clear)
(def mark-cell-empty bit-set)
(def cell-empty? bit-test)

;; ### Inputting State
(let [length-one? #(= 1 (count %))
      consistent-lengths? #(length-one? (distinct (map count %)))
      rows-have-one? (fn [rows elt]
                       (length-one? (filter #(= elt %) (apply concat rows))))]
  (defn make-state [rows]
    {:pre [(consistent-lengths? rows)
           (rows-have-one? rows start-cell)
           (rows-have-one? rows finish-cell)]
     ;; for bit-based single int impl
     :post [;; TODO WARNING: assumes 64 bits in a number we're twidding bits in
            ;; could just check instead
            (< (:total-length %) 64)
            ;; more than one column:
            (> (:row-length %) 1)
            ;; more than one row:
            (< (:row-length %) (:total-length %))]}
    (let [cells (vec (apply concat rows))
          total-length (count cells)
          row-length (count (first rows))
          idx #(.indexOf cells %)
          start-idx (idx start-cell)
          finish-idx (idx finish-cell)
          cells (assoc cells start-idx taken-cell, finish-idx empty-cell)
          ;; internally, we indicate empties with 1s, so that all filled is 0.
          cell-indices (keep-indexed
                        (fn [idx cell] (when (= empty-cell cell) idx)) cells)
          cells (reduce mark-cell-empty 0 cell-indices)]
      (State. cells total-length row-length start-idx finish-idx))))

;; ### Outputting State
(defn render-state
  "Return a collection of vectors depicting a given `state` --
   a map of :cells, :row-length, :start-idx, :finish-idx and "
  [{:keys [cells row-length total-length start-idx finish-idx]}]
  (let [cells (mapv #(if (cell-empty? cells %) empty-cell taken-cell)
                    (range total-length))
        cells (assoc cells start-idx start-cell, finish-idx finish-cell)
        ;; seems bit setting is about 4x faster
        ;; (time (dotimes [_ 10e7] 3))
        ;; (time (dotimes [_ 10e7] (bit-set 0 3)))
        ;; (time (dotimes [_ 10e7] (bit-set 0 3) (bit-set 2 3)))
        ;; (time (dotimes [_ 10e7] [0 0 0]))
        ;; (time (dotimes [_ 10e7] (assoc [0 0 0] 1 1)))
        rows (map vec (partition row-length cells))]
    rows))

(defn show-state [state]
  (doseq [row (render-state state)] (println row)))

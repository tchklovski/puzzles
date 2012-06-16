(ns quora.datacenter-cooling.state
  "State representation for the Quora \"datacenter cooling\"
   [puzzle](http://www.quora.com/challenges#datacenter_cooling)")

;; http://www.quora.com/about/challenges#datacenter_cooling

;; ## Working with "state" of datacenter

;; overall approach: we iterate over the map, reducing the problem to
;; summing number of layouts over more filled-in maps, with a
;; prunded depth first search. For example, we can move right from source,
;; or move down -- so the overall solution is the sum of two solutions.
;; We treat cells in which we already put the "duct" as 1 ("occupied").
;; The finish is (initially) unoccupied.

;; ### Optimizations
;; you should be able to have some invariants/checks about what is solvable
;; -- eg a corner that one can't get out of, or a disconnected component
;; don't need to be evaluated.
;; constant factor speedups -- better data structures for state:
;; we use a single 64 bit int and twiddle its bits. that limits the solver
;; to at most 64 cells, and is a limitation of the current implementation.
;; it should be possible to add multi-int support for larger, but note that
;; solve times may still be going up exponentially, so even 128 cell (2 int)
;; may not be solvable in a reasonable amt of time.
;; NOTE -- would be nice to plot solution times vs. grid size

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

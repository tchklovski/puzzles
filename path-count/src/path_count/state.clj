(ns path-count.state
  "State representation for the path count puzzle")

;; ### Sugar and enums
(def empty-cell 0)
(def taken-cell 1)
(def start-cell 2)
(def finish-cell 3)

(defrecord State [cells
                  total-length row-length
                  start-idx finish-idx edge-tester good-edge-cells])

;; we make "taken" be 0 rather than 1 so that `zero?` means filled
(def mark-cell-taken bit-clear)
(def mark-cell-empty bit-set)
(def cell-empty? bit-test)

;; ### Inputting State

(defn pack-cells
  "Returns a single long int that bit packs information on which cells are
   empty or occupied. Internally, we indicate empties with 1s, so that a
   grid that is fully filled is represented by 0."
  ;; seems bit setting is about 4x faster than vector "mutation"
  ;; (time (dotimes [_ 10e7] 3))
  ;; (time (dotimes [_ 10e7] (bit-set 0 3))) ;; 1.7 secs
  ;; (time (dotimes [_ 10e7] (bit-set 0 3) (bit-set 2 3)))
  ;; (time (dotimes [_ 10e7] [0 0 0]))
  ;; (time (dotimes [_ 10e7] (assoc [0 0 0] 1 1)))
  ;; (do (def foo (boolean-array (repeat 64 false))) (time (dotimes [_ 10e7] (aset (booleans (aclone (booleans foo))) 2 true)))) ;; 3.8 secs
  [cells-coll]
  (let [idx-if-empty (fn [idx cell] (when (= empty-cell cell) idx))
        cell-indices (keep-indexed idx-if-empty cells-coll)]
    (reduce mark-cell-empty 0 cell-indices)))

(defn make-edge-tester
  "Returns iFn which for a valid offset returns whether that offset is on the
   grid edge."
  [row-length total-length]
  (let [col #(rem % row-length)
        top? #(< % row-length)
        bottom? #(> % (- total-length row-length))
        left? #(zero? (col %))
        right? #(= (dec row-length) (col %))
        edge? (some-fn top? bottom? left? right?)]
    (mapv edge? (range total-length))))

(let [consistent-lengths? #(apply = (map count %))
      length-one? #(= 1 (count %))
      rows-have-one? (fn [rows elt]
                       (length-one? (filter #(= elt %) (apply concat rows))))]
  (defn make-state
    "WARNING: will only work on puzzles of up to 64 cells, since
     Clojure bitwise operations are on underlying Java longs"
    [rows]
    {:pre [(consistent-lengths? rows)
           (rows-have-one? rows start-cell)
           (rows-have-one? rows finish-cell)]
     :post [(< (:total-length %) 64)
            ;; more than one column:
            (> (:row-length %) 1)
            ;; more than one row:
            (< (:row-length %) (:total-length %))]}
    (let [cells (vec (apply concat rows))
          total-length (count cells)
          row-length (count (first rows))
          edge-tester (make-edge-tester row-length total-length)
          good-edge-cells nil
          idx #(.indexOf cells %)
          start-idx (idx start-cell)
          finish-idx (idx finish-cell)
          cells (pack-cells (assoc cells
                                start-idx taken-cell, finish-idx empty-cell))]
      (State. cells total-length row-length start-idx finish-idx edge-tester
              good-edge-cells))))

;; Some tests on states
(defn start-matches-finish?
  "Returns logical true iff start is same position as finish"
  [{:keys [start-idx finish-idx]}]
  (= start-idx finish-idx))

(defn finish-cell?
  "Returns logical true iff `offset` is same as state's `finish-idx`"
  [{finish :finish-idx} offset]
  (= offset finish))

(def filled?
  "Returns logical true iff there are no empty cells in this map"
  (comp zero? :cells))

;; ### Outputting State
(defn render-state
  "Return a collection of vectors depicting a given `state` --
   a map which includes `:cells`, `:row-length`, `:total-length`,
   `:start-idx`, `:finish-idx`"
  [{:keys [cells row-length total-length start-idx finish-idx]}]
  (let [unpack-offset #(if (cell-empty? cells %) empty-cell taken-cell)
        cells (mapv unpack-offset (range total-length))
        cells (assoc cells start-idx start-cell, finish-idx finish-cell)
        rows (map vec (partition row-length cells))]
    rows))

(defn show-state [state]
  (doseq [row (render-state state)] (println row)))

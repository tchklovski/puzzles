(ns quora.datacenter-cooling.state
  "Datacenter cooling duct puzzle")

;; http://www.quora.com/about/challenges#datacenter_cooling

;; ## Working with "state" of datacenter

;; overall approach: that we recur on the map, reducing the problem to
;; summing number of layouts over more filled-in maps. For example,
;; we can move right from source, or move down -- so the overall solution
;; is the sum of two solutions. We treat cells in which we already put the
;; "duct" as 1 ("occupied").

;; ### Optimizations
;; you should be able to have some invariants/checks about what is solvable
;; -- eg a corner that one can't get out of, or a disconnected component
;; don't need to be evaluated.
;; use transients to mutate vectors?

;; ### Sugar and enums
(def empty-cell 0)
(def taken-cell 1)
(def start-cell 2)
(def finish-cell 3)

(def empty-cell? #(= empty-cell %))

;; ### Inputting State
(let [length-one? #(= 1 (count %))
      consistent-lengths? #(length-one? (distinct (map count %)))
      rows-have-one? (fn [rows elt]
                       (length-one? (filter #(= elt %) (apply concat rows))))]
  (defn make-state [rows]
    {:pre [(consistent-lengths? rows)
           (rows-have-one? rows start-cell)
           (rows-have-one? rows finish-cell)]}
    (let [cells (vec (apply concat rows))
          row-length (count (first rows))
          idx #(.indexOf cells %)
          start-idx (idx start-cell)
          finish-idx (idx finish-cell)
          num-empty (count (filter empty-cell? cells))
          cells (assoc cells start-idx taken-cell, finish-idx empty-cell)]
      {:cells cells
       :row-length row-length
       :start-idx start-idx
       :finish-idx finish-idx
       ;:num-empty num-empty
       })))

;; ### Grid movement
(defn adjacent-offsets
  "Return the offsets in 1D structure that would be adjacent
   (up,down,left,right) from `offset` in 2D grid obtained by wrapping
   by `row-length`"
  [row-length cells-length offset]
  (for [v (if (= row-length 1)
            [-1 1]
            [(- row-length) -1 1 row-length])
        :let [v (+ offset v)]
        :when (< -1 v cells-length)] v))

(defn extend-to
  "Mark current start-idx as taken-cell, and make offset new start-idx
   Offset is assumed to be adjacent to current start (not checked)"
  [{:keys [cells start-idx] :as state} offset]
  (assoc state
    :start-idx offset,
    :cells (assoc cells offset taken-cell)))

(defn next-states
  "Return a collection of states"
  [{:keys [row-length cells start-idx] :as state}]
  (let [offsets (adjacent-offsets row-length (count cells) start-idx)
        empty-offsets (filter #(empty-cell? (get cells %)) offsets)]
    ;; move start
    (map #(extend-to state %) empty-offsets)))

(defn sum [args] (apply + args))
(comment
  (defn score
    "Main routine to count the number of valid layouts possible for this state"
    [state]
    (if-let [nexts (seq (next-steps state))]
      (sum (map score nexts))
      (score-leaf state))))


;; ### Grid tests
(def filled?
  "Returns logical true iff there are no empty cells in this map"
  ;; Optimization: keep track of number of empties explicitly
  ;; or, if we trim, it would trim to 1x1
  #(not-any? empty-cell? (:cells %)))

(defn start-matches-finish?
  "Logical true iff start is same position as finish"
  [{:keys [start-idx finish-idx]}]
  (= start-idx finish-idx))

(def successfully-covered? (every-pred filled? start-matches-finish?))

;; ### Outputting State
(defn render-state
  "Return a collection of vectors depicting a given `state` --
   a map of :cells, :row-length, :start-idx, :finish-idx and "
  [{:keys [cells row-length start-idx finish-idx]}]
  (let [cells (assoc cells start-idx start-cell, finish-idx finish-cell)
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

(def test-state
  (make-state
   [[2 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [3 0 0 0 0 1 1]]))

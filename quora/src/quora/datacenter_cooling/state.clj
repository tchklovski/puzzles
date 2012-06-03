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
(comment
  (defn next-steps
    "Return a collection of states"
    [{:keys [start-idx cells row-length] :as state}]
    (let [
          new-starts (open-neighbors start-idx)]
      (map (partial take-step state) new-starts))))

(defn sum [args] (apply + args))
(comment
  (defn score
    "Main routine to count the number of valid layouts possible for this state"
    [state]
    (if-let [nexts (seq (next-steps state))]
      (sum (map score nexts))
      (score-leaf state))))

;; ### Inputting State
(defn pos-to-idx
  "Given row length and a 2D row-major position, convert to 1D index offset"
  [row-length [row col]]
  (+ (* row-length row)
     col))

(defn idx-to-pos
  "Given row length and a 1D offset, convert to 2D position"
  [row-length offset]
  ((juxt quot rem) offset row-length))

(def empty-cell 0)
(def taken-cell 1)
(def start-cell 2)
(def finish-cell 3)

(def empty-cell? #(= empty-cell %))

(defn filled?
  "Returns logical true iff there are no empty cells in this map"
  ;; Optimization: keep track of number of empties explicitly
  ;; or, if we trim, it would trim to 1x1
  [state]
  (not-any? empty-cell? (:cells state)))

(defn start-matches-finish?
  "Logical true iff start is same position as finish"
  [{:keys [start-idx finish-idx]}]
  (= start-idx finish-idx))

(def successfully-covered? (every-pred filled? start-matches-finish?))

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
          cells (assoc cells start-idx taken-cell, finish-idx taken-cell)
          num-empty (count (filter empty-cell? cells))]
      {:cells cells
       :row-length row-length
       :start-idx start-idx
       :finish-idx finish-idx
       ;:num-empty num-empty
       })))

(def sample-state
  "An example of the state the search operates on."
  {:cells [1 0 0, 0 0 0, 0 0 1]
   :row-length 3, :start-idx 0, :finish-idx 8})

(comment
  (expect-> [[2 0 0]
             [0 0 0]
             [0 0 3]]
            make-state {:cells [1 0 0 0 0 0 0 0 1]
                        :row-length 3
                        :start-idx 0
                        :finish-idx 8}
            render-state '([2 0 0] [0 0 0] [0 0 3])))

;; ### Outputting State
(defn render-state
  "Return a collection of vectors depicting a given `state` --
   a map of :cells, :row-length, :start-idx, :finish-idx and "
  [state]
  (let [{:keys [cells row-length start-idx finish-idx]} state
        ;; seems bit setting is about 4x faster
        ;; (time (dotimes [_ 10e7] 3))
        ;; (time (dotimes [_ 10e7] (bit-set 0 3)))
        ;; (time (dotimes [_ 10e7] (bit-set 0 3) (bit-set 2 3)))
        ;; (time (dotimes [_ 10e7] [0 0 0]))
        ;; (time (dotimes [_ 10e7] (assoc [0 0 0] 1 1)))
        cells (assoc cells start-idx start-cell, finish-idx finish-cell)
        rows (map vec (partition row-length cells))]
    rows))

(defn show-state [state]
  (doseq [row (render-state state)] (println row)))



;; testmaker: "code spinning mode":
;; on eval of nonexistent fn, offer a skeleton of its definition
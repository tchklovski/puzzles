(ns path-count.search
  "Search (core logic) for a search puzzle"
  (:use [path-count.state]))


;; ## Working with "state"

;; overall approach: we iterate over the map, reducing the problem to
;; summing number of layouts over more filled-in maps, with a
;; prunded depth first search. For example, we can move right from source,
;; or move down -- so the overall solution is the sum of two solutions.
;; We treat cells which we already visited as 1 ("occupied").
;; The start is initially occupied and finish unoccupied.

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

(defn score-leaf
  "Returns a score if we can determine it without looking at more cases,
   nil otherwise"
  [state]
  (when (start-matches-finish? state)
    (if (filled? state) 1 0)))

;; ### Grid movement
(defn neighbors*
  "Return the offsets in 1D structure that would be adjacent
   (up,down,left,right) from `offset` in 2D grid obtained by wrapping
   by `row-length`"
  [row-length cells-length offset]
  (let [rem (rem offset row-length)]
    (for [v (cond
              (zero? rem)              [(- row-length)    1 row-length]
              (= (inc rem) row-length) [(- row-length) -1   row-length]
              :else                    [(- row-length) -1 1 row-length] )
          :let [v (+ offset v)]
          :when (< -1 v cells-length)]
      v)))

(def neighbors (memoize neighbors*))

(defn extend-to
  "Mark current start-idx as taken-cell, and make offset new start-idx
   Offset is assumed to be adjacent to current start (not checked)"
  [{:keys [cells start-idx] :as state} offset]
  (assoc state
    :start-idx offset,
    :cells (mark-cell-taken cells offset)))

(defn empty-neighbors
  "Returns a colleciton of offsets (cells) that correspond to empty
   neighbor cells, given a state and an offset"
  [{:keys [cells row-length total-length]} offset]
  (->> offset
       (neighbors row-length total-length)
       (filter #(cell-empty? cells %))))

(defn prune-next-offsets
  "For non-finish next offsets, compute how many
   neighbors each of them has. then prune non-viable cases"
  ;; NB: could maybe do more about checking the finish cell as well
  [{:keys [finish-idx] :as state} next-offsets]
  (let [neighbor-counts (map #(and (not= finish-idx %)
                                    (count (empty-neighbors state %)))
                             next-offsets)]
    (when (every? #(not= 0 %) neighbor-counts)
      (let [num-ones (count (filter #(= 1 %) neighbor-counts))]
        (case num-ones
          0 next-offsets
          1 [(nth next-offsets
                  (.indexOf (vec neighbor-counts) 1))]
          nil)))))

(defn next-states
  "Return a collection of states. Prunes states that are not likely to be
   viable"
  [{:keys [start-idx] :as state}]
  (->> start-idx
       (empty-neighbors state)
       (prune-next-offsets state)
       (map #(extend-to state %))))

;; ### Counting number of good states
(defn sum [args]
  (reduce (fn [sum v] ((fnil + 0) v sum)) 0 args))

(declare num-calls)

(defn count-layouts
  "Main routine to count the number of valid layouts possible for this state"
  [state]
  (swap! num-calls inc)
  (or (score-leaf state)
      (when-let [nexts (seq (next-states state))]
        (sum (map count-layouts nexts)))
      0))

(def num-calls (atom 0))
(defn calls-in-layout [state]
  (def num-calls (atom 0))
  (count-layouts state)
  @num-calls)

(comment
  ;; use tis to
  (do (use 'path-count.search :reload)
      (use 'path-count.samples :reload)
      (time (calls-in-layout large-test-state))))

;; ## Misc Notes

;; TODO: Opt: after first touch of the border, we set up "allowed touch"
;; at edges of stretch of ones along border contiguous to the first touch;
;; all other along the border are disallowed. At consequent touch of border,
;; we update the allowed spots.
;; upon allowed touch, the disallowed touch is updated

;; can have `check-board` fn which sanity checks upfront to see if solvable...

;; opt: could try to discard "hopeless" states faster
;; opt -- better search -- trigger possible discard when edges touched and
;; isolated islands exist -- track total number of configs considered.

;; num empty neighbors datastructure -- for empty cells

;; Other observations: n, the number of steps taken (or remaining) lets you
;; know what to store of a computation -- at every step, you only need to
;; have all the variants of n steps solved.
;; wonder how big the search space gets for the richest config


;;(defn hash-key [{:keys cells start-idx}] [cells start-idx])

;; can we leverage lazy seqs?
;; is recurring somehow better?

;; note that we know if it's filled by how many steps we took -- but in current
;; approach the zero? test is cheap

;; lazy fib for inspiration of how this search may be implemented
;; (def fib (lazy-cat [0 1] (map + fib (rest fib))))

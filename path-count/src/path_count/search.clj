(ns path-count.search
  "Search (core logic) for a search puzzle"
  (:use [path-count.state]))


;; ## Working with "state"

;; overall approach: we iterate over the map, reducing the problem to
;; summing number of layouts over more filled-in maps, with a
;; prunded depth first search. For example, we can move right from source,
;; or move down -- so the overall solution is the sum of two solutions.
;; We treat cells which we already visited same as "occupied".
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

;; so, we treat the edge as "occupied" -- by adding a row above, below, and
;; "to the left". then we don't need edge checks, only neighbors.
;; then, we play the following game --
;; as soon as you get near (even diagonally) a foreign occupied, you mark the
;; entire uncrossable component as "self". now, whenever you have a self-touch,
;; of the surrounding 8, if you have any alternation of empty/full/empty/full,
;; (or more) you won't be able to complete, b/c you've encircled one or the
;; other. other than that, you gotta have at most one non-ending of degree 1,
;; and you have to pick it as next if you do.
;; "occupied" is a static map -- one for game.
;; self can overlap with it. we and the self and occupied.

;; we do need more than an int -- perhaps int-arrays with bit-setting
;; neighbor checks can be bit-shifts and masks and "and"s

(defn score-leaf
  "The contract is that it returns a score if it can figure it out, or
   nil if counting over next steps should be done"
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
  "Mark offset as new start-idx, and mark it taken.
   Offset is assumed to be adjacent to current start (not checked)"
  [{:keys [cells start-idx] :as state} offset]
  (assoc state
    :start-idx offset,
    :cells (mark-cell-taken cells offset)))

(defn empty-neighbors
  "Returns a collection of offsets (cells) that correspond to empty
   neighbor cells, given a state and an offset"
  [{:keys [cells row-length total-length]} offset]
  (->> offset
       (neighbors row-length total-length)
       (filter #(cell-empty? cells %))))

(defn taken-neighbors
  "Returns a collection of offsets (cells) that correspond to taken
   neighbor cells, given a state and an offset"
  [{:keys [cells row-length total-length]} offset]
  (->> offset
       (neighbors row-length total-length)
       (remove #(cell-empty? cells %))))

(defn prune-next-offsets
  "For non-finish next offsets, compute how many empty neighbors each
   of them has. Prune the non-viable cases"
  ;; btw, score-leaf checks the finish cell
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
          ;; when more than 1 empty has just one empty neighbor,
          ;; state is not viable
          nil)))))
;; x
;; xx
;;   s
;;   x
;; x
;; oo
;;  s
;;  x
;; cells -- and edge testing -- should be on a boolean array, rather
;; than on bit-packed?

;; we do this neat -- but a bit complex in implementation thing:
;; once we touch an edge, all subsequent touches of the edge must
;; be adjacent to this stretch of edge touch (allowing for taken cells)
;; ow we will have unreachable cells; still, the implementation is more
;; complex than i wish it were
;; we can simplify greatly by just discarding second touches which have two
;; empty edge neighbors
(defn edge-touch? [{:keys [edge-tester start-idx]}]
  (edge-tester start-idx))

(defn taken-edge-neighbors [{:keys [edge-tester] :as state} offset]
  (filter edge-tester (taken-neighbors state offset)))

(defn good-edge-touch? [{:keys [good-edge-cells start-idx] :as state}]
  (or (not good-edge-cells) ; the initial touch is "good"
      (some good-edge-cells (taken-edge-neighbors state start-idx))))

(defn edge-filled-stretch
  "Return collection of cells (at most 2) that are empty if we walk along
   taken edge cells from `start-idx` (which should be on edge too)"
  [{:keys [start-idx edge-tester good-edge-cells] :as state}]
  (loop [fronteer [start-idx] known #{start-idx}]
    (let [new-fronteer
          (remove known
                  (distinct (mapcat #(taken-edge-neighbors state %)
                                    (remove (or good-edge-cells #{})
                                            fronteer))))
          new-known (into known fronteer)]
      (if (seq new-fronteer)
        (recur new-fronteer new-known)
        new-known))))

(defn do-update-edge-touch [{:keys [good-edge-cells] :as state}]
  ;; NOTE: for speed, this can be a bitmap
  ;; has to start as nil though
  (let [stretch (edge-filled-stretch state)]
    (when (or (and good-edge-cells (some good-edge-cells stretch))
              (not good-edge-cells))
      (assoc state :good-edge-cells (into stretch good-edge-cells)))))

(defn update-edge-touch
  "Returns nil if this edge touch invalidated the state;
   otherwise returns the updated state"
  [state]
   (if (edge-touch? state)
    (when (good-edge-touch? state) (do-update-edge-touch state))
    state))

(defn next-states
  "Return a collection of states. Prunes states that are not viable"
  [{:keys [start-idx] :as state}]
  (->> start-idx
       (empty-neighbors state)
       (prune-next-offsets state)
       (map #(extend-to state %))
       (keep update-edge-touch)))

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
      ;; if no next states, we ran into a dead end
      0))

(def num-calls (atom 0))
(defn calls-in-layout [state]
  (def num-calls (atom 0))
  (count-layouts state)
  @num-calls)

(comment
  ;; use this to run the states
  (do (use 'path-count.search :reload)
      (use 'path-count.samples :reload)
      (time (calls-in-layout large-test-state))))

;; ## Misc Notes

;; discarding "hopeless" states sooner is key to cutting down avg base of
;; exponent in this exponential search

;; Opt: after first touch of the border, we set up "allowed touch"
;; at edges of stretch of ones along border contiguous to the first touch;
;; all other along the border are disallowed. At consequent touch of border,
;; we update the allowed spots.
;; upon allowed touch, the disallowed touch is updated
;; note that touch can be of self, not just border!

;; for handling borders, it may be more elegant to surround board with filled
;; cells, but we don't have enough bits for that :(

;; need to have requested I/O behavior

;; can have `possibly-solvable?` fn which sanity checks upfront and returns
;; 0 for provably unsolvable...

;; isolated islands exist -- track total number of configs considered.

;; num empty neighbors datastructure -- for empty cells

;; Other observations: n, the number of steps taken (or remaining) lets you
;; know what to store of a computation -- at every step, you only need to
;; have all the variants of n steps solved.
;; wonder how big the search space gets for the richest config

;; note that we know if it's filled by how many steps we took -- but in current
;; approach the zero? test is cheap

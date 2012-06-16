(ns quora.datacenter-cooling.state
  "Datacenter cooling duct puzzle")

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

(declare score num-calls)

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

;; TODO: partition into 3 files state (up to here), test configs, and logic

;; ### Test States
(def test-state
;; with neighbor-empty-count optimization: "Elapsed time: 540491.481086 msecs"
;; 41942385 (42M) solutions
;;  (time (score test-state))
  (make-state
   [[2 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [3 0 0 0 0 1 1]]))

(def tiny-test-state
  (make-state
   [[2 0]
    [3 0]]))

(def small-test-state
  (make-state
   [[2 0 0 0]
    [0 0 0 0]
    [0 0 3 1]]))

(def medium-test-state
;;  (time (score medium-test-state))
  ;; pre defrecord: "Elapsed time: 6554.52121 msecs"
  ;; post defrecrod, first edge filtered: "Elapsed time: 5183.211706 msecs"
  ;; all calls: 1670522 calls; first edge filtered: 1338135
  ;; 378
  (make-state
   [[2 0 0 0 0]
    [0 0 0 0 0]
    [0 0 0 0 0]
    [0 0 0 0 0]
    [0 0 0 0 0]
    [0 0 0 0 3]]))

(def populated-test-state
  (make-state
   [[1 1 1]
    [0 0 2]
    [3 1 0]]))

(def large-test-state
  ;; (time (score large-test-state))
  "Elapsed time: 23073.343341 msecs"
  ;; boolean filter: "Elapsed time: 14189.361021 msecs"
  ;; 606
  ;; with an extra row, "Elapsed time: 153472.73219 msecs"
  ;; 2471
  ;; 5032974 calls
  (make-state
   [[2 0 0 0 0]
    [0 0 0 0 0]
    [0 0 0 0 0]
    [0 0 0 0 0]
    [0 0 0 0 0]
    [0 0 0 0 0]
    [3 0 0 1 1]]))

(def huge-test-state
  (make-state
   [[2 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [0 0 0 0 0 0 0]
    [3 0 0 0 0 1 1]]))
;; ### Grid tests

;; TODO: can use bitmaps to do fast tests for the pieces we care about
(comment
  (defn location-fn-maker
    "Returns fn which takes offset and returns keyword indicating which
   location it is, eg :inside"
    [row-length total-length]
    (let [col #(rem % row-length)
          top? #(< % row-length)
          bottom? #(> % (- total-length row-length))
          left? #(zero? (col %))
          right? #(= (dec row-length) (col %))
          ;; sets to 1 the bits for which fn returns logical true
          make-bitmap #(reduce (fn [bitmap idx]
                                  (if (% idx)
                                    (bit-set bitmap idx)
                                    bitmap))
                               0 (range total-length))
          inside-bitmap (make-bitmap (some-fn top? bottom? left? right?))
          left-bitmap (make-bitmap left?)
          left-bitmap (make-bitmap left?)]
      (fn [offset]
        (condp bit-test offset
          inside-bitmap :inside
          top-bitmap    (if (bit-test offset)) :top-left)))))

(defn start-matches-finish?
  "Logical true iff start is same position as finish"
  [{:keys [start-idx finish-idx]}]
  (= start-idx finish-idx))

(defn finish-cell?
  "Logical true iff start is same position as finish"
  [{:keys [finish-idx]} offset]
  (= offset finish-idx))

(defn filled? [{cells :cells}]
  "Returns logical true iff there are no empty cells in this map"
  ;; Optimization: keep track of number of empties explicitly
  ;; or, if we trim, it would trim to 1x1
  (zero? cells))

;; note that we know if it's filled by how many steps we took
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

(defn empty-neighbors [{:keys [cells row-length total-length]} offset]
  (->> offset
       (neighbors row-length total-length)
       (filter #(cell-empty? cells %))))

(defn next-states
  "Return a collection of states"
  [{:keys [start-idx finish-idx] :as state}]
  (let [next-offsets (empty-neighbors state start-idx)
        neighbor-counts (map #(and (not= finish-idx %)
                                   (count (empty-neighbors state %)))
                             next-offsets)]
    (when (every? #(not= 0 %) neighbor-counts)
      (let [num-ones (count (filter #(= 1 %) neighbor-counts))
            next-offsets   (case num-ones
                             0 next-offsets
                             1 [(nth next-offsets
                                     (.indexOf (vec neighbor-counts) 1))]
                             nil)]
        (map #(extend-to state %) next-offsets)))))

;; if some empty next is not finish, and has only

;; Counting / Scoring
(defn sum [args]
  (reduce (fn [sum v] (+ sum (or v 0))) 0 args))

;; opt: could try to discard "hopeless" states faster
;; opt -- better search -- trigger possible discard when edges touched and
;; isolated islands exist -- track total number of configs considered.


(defn score
  "Main routine to count the number of valid layouts possible for this state"
  [state]
  (swap! num-calls inc)
  (or (score-leaf state)
      (if-let [nexts (seq (next-states state))]
        ;; observation: if one branch dead-ends, that means the things that led to it
        ;; up to and *including* the first that had more than 1 option are all bad --
        ;; so the whole tree of going the other way can be tossed!
        ;; can we do nexts3 etc?
        ;; so "single next" can be chased before we do the nexts2 analysis?
        ;; can we leverage lazy seqs?
        ;; is recurring somehow better?
        (sum (map score nexts))
        0
)))
;; there must be at most one of the following, and it must neighbor the start state:
;; an empty that's not a finish with less than two empty cells nearby


(comment
  (let [nexts2 (map next-states nexts)]
    (if (some empty? nexts2)
      (sum (map score-leaf nexts))
      (sum (map (fn [n nn] (or (score-leaf n)
                               (sum (map score nn))))
                nexts nexts2)))))

;;(defn hash-key [{:keys cells start-idx}] [cells start-idx])

(def num-calls (atom 0))
(defn count-calls-in-scoring [state]
  (def num-calls (atom 0))
  (score state)
  @num-calls)

(comment
  (do (use 'quora.datacenter-cooling.state :reload)
      (time (count-calls-in-scoring medium-test-state))))

;; ## Misc Notes

;; lazy fib for inspiration of how this search may be implemented
;; (def fib (lazy-cat [0 1] (map + fib (rest fib))))

;; can have `check-board` fn which sanity checks upfront to see if solvable...
;;
;; must not NPE on other boards -- with a dead end?
;;
;; num empty neighbors datastructure -- for empty cells

;; Other observations: n, the number of steps taken (or remaining) lets you
;; know what to store of a computation -- at every step, you only need to
;; have all the variants of n steps solved.
;; wonder how big the search space gets for the richest config


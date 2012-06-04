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

(defrecord State [cells row-length start-idx finish-idx])

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
          cells (assoc cells start-idx taken-cell, finish-idx empty-cell)]
      (State. cells row-length start-idx finish-idx))))

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

;; ### Test States
(def test-state
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
   [[2 0 0]
    [0 0 3]]))

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

(def large-test-state
  ;; (time (score large-test-state))
  "Elapsed time: 23073.343341 msecs"
  (make-state
   [[2 0 0 0 0]
    [0 0 0 0 0]
    [0 0 0 0 0]
    [0 0 0 0 0]
    [0 0 0 0 0]
    [0 0 0 0 0]
    [3 0 0 1 1]]))

;; ### Grid tests
(defn start-matches-finish?
  "Logical true iff start is same position as finish"
  [{:keys [start-idx finish-idx]}]
  (= start-idx finish-idx))

(def filled?
  "Returns logical true iff there are no empty cells in this map"
  ;; Optimization: keep track of number of empties explicitly
  ;; or, if we trim, it would trim to 1x1
  #(not-any? empty-cell? (:cells %)))

(def successfully-covered? (every-pred start-matches-finish? filled?))

;; ### Grid movement
(defn adjacent-offsets
  "Return the offsets in 1D structure that would be adjacent
   (up,down,left,right) from `offset` in 2D grid obtained by wrapping
   by `row-length`"
  [row-length cells-length offset]
  (let [rem (rem offset row-length)]
    (for [v (cond
              (= row-length 1)         [-1 1]
              (zero? rem)              [(- row-length)    1 row-length]
              (= (inc rem) row-length) [(- row-length) -1   row-length]
              :else                    [(- row-length) -1 1 row-length] )
          :let [v (+ offset v)]
          :when (< -1 v cells-length)] v)))

(defn extend-to
  "Mark current start-idx as taken-cell, and make offset new start-idx
   Offset is assumed to be adjacent to current start (not checked)"
  [{:keys [cells start-idx] :as state} offset]
  (assoc state
    :start-idx offset,
    :cells (assoc cells offset taken-cell)))

(defn next-states
  "Return a collection of states"
  [{:keys [cells start-idx row-length] :as state}]
  (let [offsets (adjacent-offsets row-length (count cells) start-idx)
        empty-offsets (filter #(empty-cell? (get cells %)) offsets)]
    (map #(extend-to state %) empty-offsets)))

;; Counting / Scoring
(defn sum [args]
  (reduce (fn [sum v] (if v (+ sum v) sum)) 0 args))

;; opt: could try to discard "hopeless" states faster
;; opt -- better datastructures (bit-packing, no map)
;; opt -- better search -- trigger possible discard when edges touched and
;; isolated islands exist -- track total number of configs considered.

;; eval defrecord for speed

;; opt -- if a 2x2 corner does not have a finish in it, and the edge is empty but
;; near it is not, that may be a reason to give up
;; maybe becomes less important as grid gets bigger?

;; maybe memoization would pay off if we do it when we only have a few cells empty?
;; (eg 5 or 10) -- the number of items cached is not too high, but they are useful

;; check if grid is bisected -- and we have emppties on both sides
;; seems like it would happen/matter more in large grid?  -- would want to maintain
;; which rows have empties and either update counts on each step or recalc on edge touches

(defn get-edge [{:keys [cells row-length start-idx]}]
  (cond (< start-idx row-length) (subvec cells 0 row-length),
   ;;      (zero? (rem start-idx row-length))  ...
   ;;     (>= start-idx (- (count cells) row-length))
   ;;     (subvec cells (- (count cells) row-length)),
        :else nil))

(defn edge-ok?
  "if we have 4 or groups along an edge, that means there is isolated empties"
  [edge] (< (count (partition-by identity edge)) 4))

(def in-play? (comp (some-fn nil? edge-ok?) get-edge))

(declare score num-calls)

(defn score-aux
  "Actual recursive scoring, no preconditions"
  [state]
  (if-let [nexts (seq (next-states state))]
    (sum (map score nexts))
    (when (successfully-covered? state) 1)))

(defn score
  "Main routine to count the number of valid layouts possible for this state"
  [state]
  (swap! num-calls inc)
  (when (in-play? state) (score-aux state)))

(def num-calls (atom 0))

(defn score* [state]
  (def num-calls (atom 0))
  (score state)
  @num-calls)


;; (def fib (lazy-cat [0 1] (map + fib (rest fib))))
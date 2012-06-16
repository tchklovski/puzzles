(ns quora.datacenter-cooling.search
  "Search (core logic) for the Quora \"datacenter cooling\"
   [puzzle](http://www.quora.com/challenges#datacenter_cooling)"
  (:use [quora.datacenter-cooling.state]))

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
  (reduce (fn [sum v] ((fnil + 0) v sum)) 0 args))

;; opt: could try to discard "hopeless" states faster
;; opt -- better search -- trigger possible discard when edges touched and
;; isolated islands exist -- track total number of configs considered.

(declare num-calls)

(defn score
  "Main routine to count the number of valid layouts possible for this state"
  [state]
  (swap! num-calls inc)
  (or (score-leaf state)
      (when-let [nexts (seq (next-states state))]
        (sum (map score nexts)))
      0))

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
;; num empty neighbors datastructure -- for empty cells

;; Other observations: n, the number of steps taken (or remaining) lets you
;; know what to store of a computation -- at every step, you only need to
;; have all the variants of n steps solved.
;; wonder how big the search space gets for the richest config


;;(defn hash-key [{:keys cells start-idx}] [cells start-idx])

;; can we leverage lazy seqs?
;; is recurring somehow better?
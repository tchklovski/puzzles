(ns path-count.test.state
  (:use :reload [path-count.state])
  (:use [midje.sweet]))

(def sample-state
  "An example of the state the search operates on."
  {:cells 510 ;; 1's for "empty" cells, ie powers of 2 of 2 through 8 - so 2 short of 512
   :total-length 9, :row-length 3, :start-idx 0, :finish-idx 8
   :edge-tester [true true true true nil true true true true]
   :good-edge-cells nil})

(def sample-completed-state
  "An example of completed state."
  {:cells 0
   :row-length 3, :total-length 9, :start-idx 8, :finish-idx 8
   :edge-tester [true true true true nil true true true true]
   ;;    :good-edge-cells nil
   })

(fact "make-state"
  (let [sample-rows [[2 0 0]
                     [0 0 0]
                     [0 0 3]]]
    (make-state sample-rows) => sample-state))

(facts "start-matches-finish?"
  sample-state =not=> start-matches-finish?
  sample-completed-state => start-matches-finish?)

(facts "filled?"
  sample-state =not=> filled?
  sample-completed-state => filled?)

(facts "finish-cell?"
  (finish-cell? sample-state 8) => true
  (finish-cell? sample-state 0) => false
  (finish-cell? sample-state 7) => false)

(facts "edge?"
  (let [edge? #((make-edge-tester (:row-length sample-state)
                                  (:total-length sample-state)) %)]
    (every? edge? #{0 1 2 3 5 6 7 8}) => true
    4 =not=> edge?))

(facts "render-state"
  (render-state sample-state) => '([2 0 0] [0 0 0] [0 0 3])
  (render-state sample-completed-state) => '([1 1 1] [1 1 1] [1 1 3]))

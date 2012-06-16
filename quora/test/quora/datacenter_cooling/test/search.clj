(ns quora.datacenter-cooling.test.search
  (:use [quora.datacenter-cooling
         [state :only [make-state render-state]]
         samples
         search])
  (:use [midje.sweet]))

(def sample-state
  "An example of the state the search operates on."
  {:cells 510 ;; 1's for "empty" cells, ie powers of 2 of 2 through 8 - so 2 short of 512
   :total-length 9, :row-length 3, :start-idx 0, :finish-idx 8})

(def sample-completed-state
  "An example of completed state."
  {:cells 0
   :row-length 3, :total-length 9, :start-idx 8, :finish-idx 8})

(facts "neighbors"
  (neighbors 3 9 0) => [1 3]
  (neighbors 3 9 2) => [1 5]
  (neighbors 3 9 3) => [0 4 6]
  (neighbors 3 9 4) => [1 3 5 7]
  (neighbors 3 9 8) => [5 7])

(facts "next-states"
  (let [state (make-state [[2 1] [0 3]])]
    (next-states state)) => [(make-state [[1 1]
                                          [2 3]])])

(facts "filled?"
  sample-state =not=> filled?
  sample-completed-state => filled?)

(facts "start-matches-finish?"
  sample-state =not=> start-matches-finish?
  sample-completed-state => start-matches-finish?)

(facts "render-state"
  (render-state sample-state) => '([2 0 0] [0 0 0] [0 0 3])
  (render-state sample-completed-state) => '([1 1 1] [1 1 1] [1 1 3]))

(facts "scoring"
  (score tiny-test-state) => 1
  (score small-test-state) => 2
  (score populated-test-state) => 0)
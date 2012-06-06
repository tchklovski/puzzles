(ns quora.datacenter-cooling.test.state
  (:use :reload [quora.datacenter-cooling.state])
  (:use [midje.sweet]))

(def sample-state
  "An example of the state the search operates on."
  {:cells 510 ;; 1's for "empty" cells, ie powers of 2 of 2 through 8 - so 2 short of 512
   :total-length 9, :row-length 3, :start-idx 0, :finish-idx 8})

(def sample-completed-state
  "An example of completed state."
  {:cells 0
   :row-length 3, :total-length 9, :start-idx 8, :finish-idx 8})

(fact "make-state"
  (let [sample-rows [[2 0 0]
                     [0 0 0]
                     [0 0 3]]]
    (make-state sample-rows) => sample-state))

(facts "adjacent-offsets"
  (adjacent-offsets 3 9 0) => [1 3]
  (adjacent-offsets 3 9 2) => [1 5]
  (adjacent-offsets 3 9 3) => [0 4 6]
  (adjacent-offsets 3 9 4) => [1 3 5 7]
  (adjacent-offsets 3 9 8) => [5 7])

(facts "next-states"
  (let [state (make-state [[2 1] [0 3]])]
    (next-states state)) => [(make-state [[1 1]
                                          [2 3]])])

(facts "successfully-covered?"
  sample-state =not=> successfully-covered?
  sample-completed-state => successfully-covered?)

(facts "render-state"
  (render-state sample-state) => '([2 0 0] [0 0 0] [0 0 3])
  (render-state sample-completed-state) => '([1 1 1] [1 1 1] [1 1 3]))

(facts "scoring"
  (score tiny-test-state) => 1
  (score small-test-state) => 1)
(ns path-count.test.state
  (:use :reload [path-count.state])
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

(facts "render-state"
  (render-state sample-state) => '([2 0 0] [0 0 0] [0 0 3])
  (render-state sample-completed-state) => '([1 1 1] [1 1 1] [1 1 3]))

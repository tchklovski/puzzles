(ns quora.datacenter-cooling.samples
  "Sample states for the Quora \"datacenter cooling\"
   [puzzle](http://www.quora.com/challenges#datacenter_cooling)"
  (:use [quora.datacenter-cooling.state :only (make-state)]))

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

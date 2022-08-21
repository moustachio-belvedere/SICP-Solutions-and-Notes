#! /bin/racket
#lang sicp

;; replacing 4.35 algo `an-integer-between` with `an-integer-starting-from`
;; will not work as the latter is an infinite range of choices and due
;; to the way backtracking works it will try and find triples of the form
;; (1, 1, n) for all positive integers n indefinitely and will never
;; ascend back to the choice points for `i` or `j`. Thus no triple will ever
;; be found.

(define (pythagorean-triples)
  (let ((k (an-integer-starting-from 1)))
    (let ((j (an-integer-between 1 k)))
      (let ((i (an-integer-between 1 j)))
        (require (= (+ (* i i) (* j j))
                    (* k k)))
        (list i j k)))))

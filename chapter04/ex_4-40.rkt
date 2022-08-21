#! /bin/racket
#lang sicp

;; 192 possibilities before distinct criterion applied

;; one way to encode all this logic (with some manual work
;; required before implementation) is to simply remove
;; the impossible choices (before distinction) from the
;; `amb` choices, i.e.

(let ((baker    (amb 1 2 3 4))
      (cooper   (amb 2 4))
      (fletcher (amb 2 4))
      (miller   (amb 3 4 5))
      (smith    (amb 1 2 4 5))))

;; but I think the approach the book is suggesting is
;; more like:

;; note start with fletcher as he has less choices available
;; from outset, then cooper as two reqs can be satisfied
(let ((fletcher (amb 1 2 3 4 5)))
       (require (not (= fletcher 5)))
       (require (not (= fletcher 1)))
  (let ((cooper (amb 1 2 3 4 5)))
           (require (not (= cooper 1)))
           (require
            (not (= (abs (- fletcher cooper)) 1)))
    (let ((baker (amb 1 2 3 4 5)))
          (require (not (= baker 5)))
      (let ((miller (amb 1 2 3 4 5)))
             (require (> miller cooper))
        (let ((smith (amb 1 2 3 4 5)))
               (require
                (not (= (abs (- smith fletcher)) 1)))

               (require
                (distinct? (list baker cooper fletcher
                                 miller smith)))
)))))

;; the above two approaches could be combined in some even
;; more optimal way. for example:

(let ((fletcher (amb 2 4))
      (cooper (amb 2 4)))
        (require (not (= (abs (- fletcher cooper)) 1)))
  (let ((miller (amb 3 4 5)))
        (require (> miller cooper))
    (let ((smith (amb 1 2 4 5)))
          (require (not (= (abs (- smith fletcher)) 1)))
      (let ((baker (amb 1 2 3 4)))
             (require (distinct? (list baker cooper fletcher miller smith)))

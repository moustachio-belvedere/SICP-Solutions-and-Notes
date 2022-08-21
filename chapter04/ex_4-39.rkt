#! /bin/racket
#lang sicp

;; the order does not affect the result itself, but:
;; Yes the order affects the speed. The optimum order will depend
;; on implementation details but it's likely that the simple inequalities
;; will be most performant, the subtraction absolute inequalities slightly
;; less so, and the `distinct` requirement will be _least_ performant.

;; So something like this ordering will be much better:

(require (not (= baker 5)))
(require (not (= cooper 1)))
(require (not (= fletcher 5)))
(require (not (= fletcher 1)))
(require (> miller cooper))
(require
 (not (= (abs (- smith fletcher)) 1)))
(require
 (not (= (abs (- fletcher cooper)) 1)))
(require
  (distinct? (list baker cooper fletcher miller smith)))

;; importantly, the distinct criterion is the least likely to
;; be satisified. (Only 24/625 probability.) As it is also most
;; costly to test, this makes the original implementation doubly
;; inefficient.
;;
;; this is in contrast, for example, to the simple floor criteria
;; - baker not on top: 4/5
;; - cooper not on bottom: 4/5
;; - miller higher than cooper: 2/5

;; also, as a side note:

(require (not (= fletcher 5)))
(require (not (= fletcher 1)))

;; could be easily simplified to

(require (not (or (= fletcher 5) (= fletcher 1))))

;; though the performance gain may be negligible.

;; however, along these lines, it would be interesting
;; to see if combining other conditions in this way
;; affected performance in any way.

#! /bin/racket
#lang sicp
(#%require "utils_stream.rkt")

(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq 
  (stream-map 
   accum 
   (stream-enumerate-interval 1 20)))

(define y (stream-filter even? seq))
(stream-ref y 7)
;; sum
;;   1 = 1
;;   3 = ... +  2
;;   6 = ... +  3
;;  10 = ... +  4
;;  15 = ... +  5
;;  21 = ... +  6
;;  28 = ... +  7
;;  36 = ... +  8
;;  45 = ... +  9
;;  55 = ... + 10
;;  66 = ... + 11
;;  78 = ... + 12
;;  91 = ... + 13
;; 105 = ... + 14
;; 120 = ... + 15
;; 136 = ... + 16

;; stream
;; (6, 10, 28, 36, 66, 78, 120, 136 (promise ...
;;
;; { note, stream-ref treats streams as 0
;;   indexed, hence 8 even sums are returned }

;; So sum after (stream-ref y 7) should be
;; 136

(define z 
  (stream-filter 
   (lambda (x) 
     (= (remainder x 5) 0)) seq))

(newline)
(display-stream z)

;; ! denotes mutation occurence
;; ...
;; ...
;; { none of the earlier sums mutate as their
;;   values have been memoized }
;; ...
;; ...
;; 153 = ... + 17 !
;; 171 = ... + 18 !
;; 190 = ... + 19 !
;; 210 = ... + 20 !

;; so, final sum value will be 210
;; which agrees with (/ (* n (+ n 1)) 2)

;; but the values displayed by (display-stream...
;; wil be just those intermediate values which
;; are multiples of 5, i.e.
;; (10, 15, 45, 55, 105, 120, 190, 210)

;; IF the results were not memoized, so the function
;; `accum` was re-run in full for all the summations
;; the second call would act additively.
;;
;; The final sum would be
;; 136 (from the first summations) +
;; 210 (from the second summations) = 
;; 246.

;; the multiples of 5 would be different also
;; as all the stream terms in the second call
;; are shifted by +136. I.e.
;; 137, 139, 142, 146, 151, 157, 164, 172, 181,
;; 191, 202, 214, 227, 241, 256, 272, 289, 307,
;; 326, 346
;; NO multiples of 5!

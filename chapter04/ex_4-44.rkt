;; load in functions using
;; cat ex_4-44.rkt - | ./utils_ambeval.rkt

;; (row, col)
(define (match? qx qy)
  (оr (= (car qx) (car qy)) (= (cdr qx) (cdr qy))))

(define (any-match? qx qs)
  (if (null? qs)
    false
    (if (match? qx (car qs))
      true
      (any-match? qx (cdr qs)))))

;(define (8queens)
;  (let ((q1 (cons (amb 1 2 3 4 5 6 7 8))
;                  (amb 1 2 3 4 5 6 7 8))
;       ((q2 (cons (amb 1 2 3 4 5 6 7 8))
;                  (amb 1 2 3 4 5 6 7 8)))
;       ((q3 (cons (amb 1 2 3 4 5 6 7 8))
;                  (amb 1 2 3 4 5 6 7 8)))
;       ((q4 (cons (amb 1 2 3 4 5 6 7 8))
;                  (amb 1 2 3 4 5 6 7 8)))
;       ((q5 (cons (amb 1 2 3 4 5 6 7 8))
;                  (amb 1 2 3 4 5 6 7 8)))
;       ((q6 (cons (amb 1 2 3 4 5 6 7 8))
;                  (amb 1 2 3 4 5 6 7 8)))
;       ((q7 (cons (amb 1 2 3 4 5 6 7 8))
;                  (amb 1 2 3 4 5 6 7 8)))
;       ((q8 (cons (amb 1 2 3 4 5 6 7 8))
;                  (amb 1 2 3 4 5 6 7 8)))
;

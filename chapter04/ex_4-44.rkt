;; load in functions using
;; cat ex_4-44.rkt - | ./utils_ambeval.rkt

(define (require p)
    (if (not p) (amb)))

;; (row, col)
(define (match qx qy) (bor (= (car qx) (car qy)) (= (cdr qx) (cdr qy))))

(define (any-match qx qs)
  (if (null? qs)
    false
    (if (match qx (car qs))
      true
      (any-match qx (cdr qs)))))

(define (8queens)
  (let ((q1 (cons (amb 1 2 3 4 5 6 7 8)
                  (amb 1 2 3 4 5 6 7 8))))
  (let ((q2 (cons (amb 1 2 3 4 5 6 7 8)
                  (amb 1 2 3 4 5 6 7 8))))
       (require (not (match q1 q2)))
  (let ((q3 (cons (amb 1 2 3 4 5 6 7 8)
                  (amb 1 2 3 4 5 6 7 8))))
       (require (not (any-match q3 (list q1 q2))))
  (let ((q4 (cons (amb 1 2 3 4 5 6 7 8)
                  (amb 1 2 3 4 5 6 7 8))))
       (require (not (any-match q4 (list q1 q2 q3))))
  (let ((q5 (cons (amb 1 2 3 4 5 6 7 8)
                  (amb 1 2 3 4 5 6 7 8))))
       (require (not (any-match q5 (list q1 q2 q3 q4))))
  (let ((q6 (cons (amb 1 2 3 4 5 6 7 8)
                  (amb 1 2 3 4 5 6 7 8))))
       (require (not (any-match q6 (list q1 q2 q3 q4 q5))))
  (let ((q7 (cons (amb 1 2 3 4 5 6 7 8)
                  (amb 1 2 3 4 5 6 7 8))))
       (require (not (any-match q7 (list q1 q2 q3 q4 q5 q6))))
  (let ((q8 (cons (amb 1 2 3 4 5 6 7 8)
                  (amb 1 2 3 4 5 6 7 8))))
       (require (not (any-match q8 (list q1 q2 q3 q4 q5 q6 q7))))
       (list q1 q2 q3 q4 q5 q6 q7 q8))))))))))

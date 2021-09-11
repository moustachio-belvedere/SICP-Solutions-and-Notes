#lang sicp

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low 
            (enumerate-interval 
             (+ low 1) 
             high))))

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) 
            (append (cdr list1) 
                    list2))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op 
                      initial 
                      (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate 
                       (cdr sequence))))
        (else  (filter predicate 
                       (cdr sequence)))))

(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

(define (permutations s)
  (if (null? s)   ; empty set?
      (list nil)  ; sequence containing empty set
      (flatmap (lambda (x)
                 (map (lambda (p) 
                        (cons x p))
                      (permutations 
                       (remove x s))))
               s)))

(define empty-board nil)

(define (cols-check toprow layout)
  (cond ((null? layout) #t)
        ((= toprow (car layout)) #f)
        (else (cols-check toprow (cdr layout)))))
    

(define (diag-check toprow layout count)
  (cond ((null? layout) #t)
        ((or (= toprow (+ (car layout) count))
             (= toprow (- (car layout) count))) #f)
        (else (diag-check toprow (cdr layout) (+ count 1)))))

(define (safe? k layout)
  (if (> (length layout) 1)
      (and (cols-check (car layout) (cdr layout))
           (diag-check (car layout) (cdr layout) 1))
      #t))

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) 
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position 
                    new-row 
                    k 
                    rest-of-queens))
                 (enumerate-interval 
                  1 
                  board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define (n-queens n)
    (display "Board size ")
    (display n)
    (display ": ")
    (display (length (queens n)))
    (newline))
        
(n-queens 1) 
(n-queens 2) 
(n-queens 3) 
(n-queens 4) 
(n-queens 5) 
(n-queens 6) 
(n-queens 7) 
(n-queens 8) 
(n-queens 9) 
(n-queens 10) 
(n-queens 11) 
(n-queens 12) 

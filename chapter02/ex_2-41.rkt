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

(define (unique-pairs n)
  (flatmap
    (lambda (i)
      (map (lambda (j)
             (list i j))
           (enumerate-interval 1 (- i 1))))
    (enumerate-interval 1 n)))

(define (get-trip i)
  (map (lambda (x)
         (append (list i) x))
  (unique-pairs (- i 1))))

(define (unique-triplets n)
  (flatmap get-trip
           (enumerate-interval 3 n)))

(unique-triplets 4)

(define (sumeqto-s? x s)
  (= (apply + x) s))

(define (triples-sum-to-s s n)
  (filter sumeqto-s? (unique-triplets n)))

; first consider unordered triplets, then consider ordered triplets

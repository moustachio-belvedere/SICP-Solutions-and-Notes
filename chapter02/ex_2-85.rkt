#lang sicp
(#%require "utils_round.rkt")
(#%require "utils_binreduce.rkt")
(#%require "utils_getput.rkt")
(#%require "utils_typeraise.rkt")

;; a comprehensive answer to this question
;; would require a proper algorithm
;; for converting real to its closest
;; representable rational approximation

;; for now, just use the scheme 'rationalize'
;; function

(define (attach-tag type-tag contents)
        (cons type-tag contents))

(define (square x) (* x x))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-int)
        (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: 
              CONTENTS" datum))))

(define (raise x)
  ((get 'raise (type-tag x)) x))

(define (project x)
  ((get 'project (type-tag x)) x))

(define (raise-coerce lst)
  (define (sizzle? val)
    (define (glitterator num sozzles)
      (cond ((null? sozzles) (error "Type not registered in hierarchy"))
            ((eq? (car sozzles) (type-tag val)) num)
            (else (glitterator (+ num 1) (cdr sozzles)))))

    (glitterator 0 (list 'scheme-int
                         'rational
                         'scheme-real
                         'complex)))

  (define (top-sizzle lst)
    (apply max (map sizzle? lst)))

  (define (consistent-type? x)
    (apply = (map sizzle? x)))

  (let ((tmax (top-sizzle lst)))
  (if (consistent-type? lst)
      lst
      (raise-coerce (map (lambda (x)
                         (if (< (sizzle? x) tmax)
                             (raise x)
                             x))
                         lst)))))

(define (apply-generic op . args)
  (cond ((>= (length args) 2)
         (let ((coerced (raise-coerce args)))
           (let ((tt (car (map type-tag coerced))))
             (let ((proc (get op tt)))
                  (if proc
                      (apply proc (map contents coerced))
                      ; (drop (apply proc (map contents coerced)))
                      (display "PROC NOT FOUND FOR TYPES"))))))
        (else
        (let ((type-tags (map type-tag args)))
          (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                ;(drop (apply proc (map contents args)))
                (error
                  "No method for these types: 
                   APPLY-GENERIC"
                  (list op type-tags))))))))

(define (add . x) (apply apply-generic 'add x))
(define (equ? . x) (apply apply-generic 'equ? x))

(define (install-scheme-int-package)
  (define (tag x)
    (attach-tag 'scheme-int x))
  (put 'add 'scheme-int
       (lambda x (apply + x)))
  (put 'equ? 'scheme-int
       (lambda x (apply = x)))
  (put 'make 'scheme-int
       (lambda (x) (tag (exact-round x))))
  (put 'raise 'scheme-int
       (lambda (x) (make-rational (contents x) 1)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (equ? x)
    (and (apply = (map numer x))
         (apply = (map denom x))))
  (put 'equ? 'rational 
       (lambda x (equ? x)))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise 'rational
       (lambda (x) (make-scheme-real (/ (numer (contents x))
                                        (denom (contents x))))))
  (put 'project 'rational
       (lambda (x) (make-scheme-int (/ (numer (contents x))
                                       (denom (contents x))))))
  'done)

(define (install-scheme-real-package)
  (define (tag x)
    (attach-tag 'scheme-real x))
  (define (nice-real x)
    (let ((i (truncate x))
          (dp 10))
      (let ((d (exact-round (* (- x i) (expt 10 dp)))))
        (+ i (* d (expt 10 (* -1 dp)))))))
  (put 'add 'scheme-real
       (lambda x (apply + x)))
  (put 'equ? 'scheme-real
       (lambda x (apply = x)))
  (put 'make 'scheme-real
       (lambda (x) (tag (nice-real (square (sqrt x))))))
  (put 'raise 'scheme-real
       (lambda (x) (make-complex-from-real-imag
                     (contents x) 0)))
  (put 'project 'scheme-real
       (lambda (x) (let ((cx (rationalize (contents x) 1/10)))
                       (make-rational (exact-round (numerator cx))
                                      (exact-round (denominator cx))))))

  'done)

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) 
    (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) 
    (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) 
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  'done)

(define (install-complex-package)
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 
          'rectangular) 
     x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) 
     r a))
  ;; internal procedures
  (define (real-part z) 
    (apply-generic 'real-part z))
  (define (imag-part z) 
    (apply-generic 'imag-part z))
  (define (magnitude z) 
    (apply-generic 'magnitude z))
  (define (angle z) 
    (apply-generic 'angle z))
  (define (tag x) (attach-tag 'complex x))

  (define (add-complex z1 z2)
    (make-from-real-imag 
     (+ (real-part z1) (real-part z2))
     (+ (imag-part z1) (imag-part z2))))
  (put 'add 'complex
       (lambda z 
         (tag (binreduce add-complex z))))
  (define (equ? x)
    (and (apply = (map real-part x))
         (apply = (map imag-part x))))
  (put 'equ? 'complex
       (lambda x (equ? x)))
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
  (put 'project 'complex
       (lambda (z)
         (make-scheme-real (real-part z))))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)

  'done)

(install-scheme-int-package)
(install-rational-package)
(install-scheme-real-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define (make-scheme-int n)
  ((get 'make 'scheme-int) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-scheme-real n)
  ((get 'make 'scheme-real) n))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (drop x)
  (display x)(newline)
  (let ((project (get 'project (type-tag x))))
  (if project
    (let ((projected (project x)))
      (let ((raiser (get 'raise (type-tag projected))))
        (if (equ? (raiser projected) x)
            (drop (project x))
            x)
      x)))))

(drop (make-complex-from-real-imag 3 2))
(drop (make-complex-from-real-imag 3 0))

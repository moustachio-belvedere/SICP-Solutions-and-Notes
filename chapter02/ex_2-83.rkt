#lang sicp
(#%require "utils_round.rkt")
(#%require "utils_binreduce.rkt")
(#%require "utils_getput.rkt")
(#%require "utils_typeraise.rkt")

;; modify scheme-int -> scheme-int using 'exact-round'
;; function imported from racket

;; add scheme-real type and install

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

(define (square x) (* x x))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: 
              TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: 
              CONTENTS" datum))))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
            "No method for these types: 
             APPLY-GENERIC"
            (list op type-tags))))))

(define (install-scheme-int-package)
  (define (tag x)
    (attach-tag 'scheme-int x))
  (put 'make 'scheme-int
       (lambda (x) (tag (exact-round x))))
  (put 'raise 'scheme-int
       (lambda (x) (make-rational (contents x) 1)))
  'done)

(define (install-scheme-real-package)
  (define (tag x)
    (attach-tag 'scheme-real x))
  (define (nice-real x)
    (let ((i (truncate x))
          (dp 10))
      (let ((d (exact-round (* (- x i) (expt 10 dp)))))
        (+ i (* d (expt 10 (* -1 dp)))))))
  (put 'make 'scheme-real
       (lambda (x) (tag (nice-real (square (sqrt x))))))
  (put 'raise 'scheme-real
       (lambda (x) (make-complex-from-real-imag
                     (contents x) 0)))

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
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'raise 'rational
       (lambda (x) (make-scheme-real (/ (numer (contents x))
                                        (denom (contents x))))))
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
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) 
         (tag (make-from-mag-ang r a))))
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

(define (raise x)
  ((get 'raise (type-tag x)) x))

(make-scheme-int 5.4)
(raise (make-scheme-int 5))
(raise (make-rational 3 2))
(raise (make-scheme-real 2.0))

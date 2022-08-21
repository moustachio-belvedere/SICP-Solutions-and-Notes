#lang sicp
(#%require "utils_getput.rkt")
(#%require "utils_typecoerce.rkt")
(#%require "utils_binreduce.rkt")

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
      contents
      (cons type-tag contents)))

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

;; NOTE: this strategy will fail if conversions are only
;; defined on types adjacent in the type hierarchy.
;; E.g. if (complex complex scheme-number) and we have conversions
;; (integer -> real) and (real -> complex) but not (integer -> complex).
(define (coercer args)
  (define (coerce-iter fixed-args acc-args remaining-args)
    (cond ((null? fixed-args) #f)
          ((null? remaining-args) acc-args)
          ((eq? (type-tag (car fixed-args)) (type-tag (car remaining-args)))
                (coerce-iter fixed-args
                             (append acc-args (list (car remaining-args)))
                             (cdr remaining-args)))
          (else (let ((tcur->tfixed (get-coercion (type-tag (car remaining-args)) (type-tag (car fixed-args)))))
                (if tcur->tfixed
                        (coerce-iter fixed-args
                                     (append acc-args (list (tcur->tfixed (car remaining-args))))
                                     (cdr remaining-args))
                        (coerce-iter (cdr fixed-args)
                                     '()
                                     args))))))
  (coerce-iter args '() args))

(define (apply-generic op . args)
  (cond ((>= (length args) 2)
         (let ((coerced (coercer args)))
           (let ((tt (if coerced
                         (car (map type-tag coerced))
                         'no-type)))
             (let ((proc (get op tt)))
                  (if proc
                      (apply proc (map contents coerced))
                      (display "PROC NOT FOUND FOR TYPES"))))))
        (else
        (let ((type-tags (map type-tag args)))
          (let ((proc (get op type-tags)))
            (if proc
                (apply proc (map contents args))
                (error
                  "No method for these types: 
                   APPLY-GENERIC"
                  (list op type-tags))))))))

(define (square x) (* x x))
(define (add . x) (apply apply-generic 'add x))

(define (install-scheme-number-package)
  (define (tag x)
    (attach-tag 'scheme-number x))
  (put 'add 'scheme-number
       (lambda x (apply + x)))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  'done)

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add 'rational
       (lambda x (tag (binreduce add-rat x))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
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

(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (make-rational n d)
  ((get 'make 'rational) n d))

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; coercion
(put-coercion 'scheme-number 'rational
              (lambda (x) (make-rational x 1)))

(put-coercion 'scheme-number 'complex
              (lambda (x) (make-complex-from-real-imag x 0)))

;; stide-steps key issues which surface in later exercises
(put-coercion 'rational 'complex
              (lambda (x) (make-complex-from-real-imag (/ (cadr x) (cddr x)) 0)))

;; coercer tests
(add (make-scheme-number 2)
     (make-rational 3 2))

(add (make-scheme-number 3)
     (make-complex-from-real-imag 3 5)
     (make-rational 5 7))
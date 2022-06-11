#! /bin/racket
#lang sicp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; util functions available in metacircular evaluator
(define (tagged-list? exp tag)
  (if (pair? exp)
      (eq? (car exp) tag)
      false))

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-variable exp)
  (if (symbol? (cadr exp))
      (cadr exp)
      (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
      (caddr exp)
      (make-lambda
       (cdadr exp)   ; formal parameters
       (cddr exp)))) ; body

(define (make-lambda parameters body)
  (cons 'lambda (cons parameters body)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; also test some forward looking variable usage?
;; e.g. (define u (+ w 1)) in below but without
;; circular dependency in w.

;; try variables only test first
(define exp1 (list
  '(define u 1)
  '(define v (+ 1 u))
  '(define w (+ 1 u v))
  '(define (x p) (+ p 1))
  '(+ u v w)))
;; should become
;;  (let ((u '*unassigned*)
;;        (v '*unassigned*)
;;        (w '*unassigned*))
;;        (x '*unassigned*))
;;    (set! u 1)
;;    (set! v (+ 1 u))
;;    (set! w (+ 1 u v))
;;    (set! x (lambda (p) (+ p 1)))
;;    (+ u v w))

(define (scan-out-defines proc-body)
  ;; helper func for adding a list to the end
  ;; of another list otherwise untouched
  (define (add-to-end l1 item)
    (append l1 (list item)))

  ;; helper func for repetitive let add logic
  (define (new-let let-acc top-expr)
    (let ((lvar (definition-variable top-expr)))
      (list 'let (add-to-end (cadr let-acc)
                             (list lvar '*unassigned*)))))

  ;; helper func for repetitive body modification logic
  (define (new-bod mod-body top-expr)
    (let ((lvar (definition-variable top-expr))
          (lval (definition-value top-expr)))
      (add-to-end mod-body
                  (list 'set! lvar lval))))

  (define (has-lets? let-acc)
    (not (null? (cadr let-acc))))

  ;; main logic
  (define (traverse-body let-acc mod-body orig-body)
    (if (null? orig-body)
        (if (has-lets? let-acc)
            (append let-acc mod-body)
            mod-body)
        (let ((top-expr (car orig-body)))
             (if (definition? top-expr)
                 (traverse-body (new-let let-acc top-expr)
                                (new-bod mod-body top-expr)
                                (cdr orig-body))
                 (traverse-body let-acc
                                (add-to-end mod-body top-expr)
                                (cdr orig-body))))))

  (traverse-body (list 'let '()) '() proc-body))

(scan-out-defines exp1)

;; no defines should lead to no let usage
(define exp2 (list
  '(+ x 1)))

(scan-out-defines exp2)

;; with a few small modifications, this works
;; as installed in ex_4-16.rkt.

;; makes more sense to install in `make-procedure`
;; as it only needs to be done once and is fundamentally
;; part of the implementation of a given function, rather
;; than simply a behavioural quirk that only occurs
;; when the body of the function is accessed.

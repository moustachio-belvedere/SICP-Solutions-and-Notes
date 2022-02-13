#!/bin/racket
#lang sicp
(#%require "utils_agenda.rkt")

(define (make-wire)
  (let ((signal-value 0) 
        (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each 
                  action-procedures))
          'done))
    (define (accept-action-procedure! proc)
      (set! action-procedures 
            (cons proc action-procedures))
      ;'ok)
      (proc))
    (define (dispatch m)
      (cond ((eq? m 'get-signal) 
             signal-value)
            ((eq? m 'set-signal!) 
             set-my-signal!)
            ((eq? m 'add-action!) 
             accept-action-procedure!)
            (else (error "Unknown operation: 
                          WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
      'done
      (begin ((car procedures))
             (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))
(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid signal" s))))

(define (probe name wire)
  (add-action! 
   wire
   (lambda ()
     ;(newline)
     (display "name: ")
     (display name)
     (display ", time= ")
     (display (current-time the-agenda))
     (display ", new-val= ")
     (display (get-signal wire))
     (newline))))

;; make the agenda...
(define the-agenda (make-agenda))

(define (after-delay delay action)
  (add-to-agenda! 
   (+ delay (current-time the-agenda))
   action
   the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
      'done
      (let ((first-item 
             (first-agenda-item the-agenda)))
        (first-item)
        (remove-first-agenda-item! the-agenda)
        (propagate))))

;; set-up delay and inverter (why is this program designed in such a labyrinthine way? It's gross.)
(define inverter-delay 2)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value 
           (logical-not (get-signal input))))
      (after-delay 
       inverter-delay
       (lambda ()
         (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

;; wire up
(define input-1 (make-wire))
(define output-1 (make-wire))

(probe 'in input-1)
(probe 'out output-1)
(inverter input-1 output-1)

(set-signal! input-1 1)
(propagate)

;; the procedure passed to the wire is the component
;; that actually adds the event to the agenda. thus
;; if it the procedure is not called by the wire object,
;; it will not be added to the agenda and so not occur.

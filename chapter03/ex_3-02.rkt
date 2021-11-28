#lang sicp

(define (make-monitored proc)
  (define call-amount 0)

  (define (dispatch uinput)
    (cond ((eq? uinput 'how-many-calls?) call-amount)
          ((eq? uinput 'reset-counter) (set! call-amount 0))
          (else (set! call-amount (+ call-amount 1)) (proc uinput))))
  dispatch)

(define mon-sqrt (make-monitored sqrt))

(mon-sqrt 'how-many-calls?)
(mon-sqrt 225)
(mon-sqrt 'how-many-calls?)
(mon-sqrt 196)
(mon-sqrt 'how-many-calls?)
(mon-sqrt 'reset-counter)
(mon-sqrt 'how-many-calls?)



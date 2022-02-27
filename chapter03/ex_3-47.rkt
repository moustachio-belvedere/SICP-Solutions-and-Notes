;; original
(define (make-mutex)
  (let ((cell (list false)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))

(define (clear! cell) (set-car! cell false))

(define (test-and-set! cell)
  (if (car cell)
      true
      (begin (set-car! cell true)
             false)))

;; semaphore in terms of mutices
(define (make-semaphore size)
  (let ((mutices (list))
        (set-serial (make-serializer))
        (rel-serial (make-serializer)))

    (define (acquire-mutex)
      (if (= (length mutices) size)
          (acquire-mutex)
          (begin 
            (set! mutices (cons (make-mutex) mutices))
            ((car mutices) 'acquire)))) ;; this is pointless, because I haven't used the Mutex abstraction in a sensible way

    (define (release-one)
      (if (null? mutices)
          'none-to-release
          (begin ((car mutices) 'release)
                 (set! mutices (cdr mutices)))))

    (define (the-semaphore s)
      (cond ((eq? m 'acquire)
             (set-serial acquire-mutex))
            ((eq? m 'release)
             (rel-serial release-one))))

    the-semaphore))

;; alternative semaphore in terms of mutices (implicitly, via serializer)
(define (make-semaphore size)
  (let ((free-mutices size)
        (set-serial (make-serializer))
        (rel-serial (make-serializer)))

    (define (acquire-mutex)
      (if (= free-mutices 0)
          (acquire-mutex)
          (set! mutices (- mutices 1))))

    (define (release-one)
      (if (< free-mutices size)
          (set! free-mutices (+ free-mutices 1))))

    (define (the-semaphore s)
      (cond ((eq? m 'acquire)
             (set-serial acquire-mutex))
            ((eq? m 'release)
             (rel-serial release-one))))

    the-semaphore))

;; semaphore in terms of `test-and-set!`
(define (make-semaphore size)
  (define (list-of-falses size)
    (if (= size 1)
      (cons #f '())
      (cons #f (list-of-falses (- size 1)))))

  (let ((cells (list-of-falses size)))
    (define (search-for-free icells)
      (cond ((null? icells)
              (search-for-free cells))
            ((test-and-set! icells)
              (search-for-free (cdr icells)))))

    (define (release-one icells)
      (cond ((null? icells) 'none-to-free)
            ((car icells) (set-car! icell false))
            (else (release-one (cdr icells)))))

    (define (the-semaphore s)
      (cond ((eq? m 'acquire)
             (search-for-free cells))
            ((eq? m 'release)
             (release-one cells))))

    the-semaphore))

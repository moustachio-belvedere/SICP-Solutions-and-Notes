;; load in functions using
;; cat ex_4-45.rkt - | ./utils_ambeval.rkt

;; (parse '(the professor lectures to the student in the class with the cat))

;; 1) "the professor lectures to the student, in the class with the cat"
;; - the professor is lecturing the student, in a classroom that happens to have a cat in it
;;
;; 2) "the professor lectures to the student in the class, with the cat"
;; - the professor is 'with' his cat, and is lecturing to the student in the class
;; - the professor is _lecturing_ ẃith his cat, to the student
;;
;; 3) "the professor lectures to (the student in the class with the cat)"
;; - the professor is lecturing to a student who happens to 'with' a cat

;; parse 1
;;--------
;; (simple-noun-phrase (article the) (noun professor))
;; (verb-phrase
;;    (verb-phrase (verb-phrase (verb lectures) (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
;;                 (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
;;    (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat))))
;;
;;

;; parse 2
;;--------
;; (simple-noun-phrase (article the) (noun professor))
;; (verb-phrase
;;    (verb-phrase (verb lectures)
;;                 (prep-phrase (prep to) (simple-noun-phrase (article the) (noun student))))
;;    (prep-phrase (prep in)
;;                 (noun-phrase (simple-noun-phrase (article the) (noun class))
;;                              (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat))))))

;; parse 3
;;--------
;; (simple-noun-phrase (article the) (noun professor))
;; (verb-phrase (verb-phrase (verb lectures)
;;                           (prep-phrase (prep to)
;;                                        (noun-phrase (simple-noun-phrase (article the) (noun student))
;;                                                     (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))))
;;              (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat))))


;; parse 4
;;--------
;; (simple-noun-phrase (article the) (noun professor))
;; (verb-phrase (verb lectures)
;;              (prep-phrase (prep to)
;;                           (noun-phrase (noun-phrase (simple-noun-phrase (article the) (noun student))
;;                                                     (prep-phrase (prep in) (simple-noun-phrase (article the) (noun class))))
;;                           (prep-phrase (prep with) (simple-noun-phrase (article the) (noun cat))))))

;; parse 5
;;--------
;; (simple-noun-phrase (article the) (noun professor))
;; (verb-phrase (verb lectures) (prep-phrase (prep to)
;;                              (noun-phrase (simple-noun-phrase (article the) (noun student))
;;                                           (prep-phrase (prep in)
;;                                                        (noun-phrase (simple-noun-phrase (article the) (noun class))
;;                                                                     (prep-phrase (prep with)
;;                                                                                  (simple-noun-phrase (article the) (noun cat))))))))

(define (require p)
    (if (not p) (amb)))

(define nouns
  '(noun student professor cat class))

(define verbs
  '(verb studies lectures eats sleeps))

(define articles '(article the a))

(define prepositions
  '(prep for to in by with))

(define (parse-prepositional-phrase)
  (list 'prep-phrase
        (parse-word prepositions)
        (parse-noun-phrase)))

(define (parse-verb-phrase)
  (define (maybe-extend verb-phrase)
    (amb
     verb-phrase
     (maybe-extend
      (list 'verb-phrase
            verb-phrase
            (parse-prepositional-phrase)))))
  (maybe-extend (parse-word verbs)))

(define (parse-sentence)
  (list 'sentence
         (parse-noun-phrase)
         (parse-verb-phrase)))

(define (parse-simple-noun-phrase)
  (list 'simple-noun-phrase
        (parse-word articles)
        (parse-word nouns)))

(define (parse-noun-phrase)
  (define (maybe-extend noun-phrase)
    (amb
     noun-phrase
     (maybe-extend
      (list 'noun-phrase
            noun-phrase
            (parse-prepositional-phrase)))))
  (maybe-extend (parse-simple-noun-phrase)))

(define *unparsed* '())

(define (parse input)
  (set! *unparsed* input)
  (let ((sent (parse-sentence)))
    (require (null? *unparsed*))
    sent))

(define (parse-word word-list)
  (require (not (null? *unparsed*)))
  (require (memq (car *unparsed*)
                 (cdr word-list)))
  (let ((found-word (car *unparsed*)))
    (set! *unparsed* (cdr *unparsed*))
    (list (car word-list) found-word)))

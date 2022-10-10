#lang racket

(module+ test
  (require rackunit))

(provide symbol-append discard-void-syntax-object make-id
         (struct-out one-arg-adr-modes)
         (struct-out ind-arg-adr-modes)
         (struct-out idx-arg-adr-modes))

(define (make-id stx id-template . ids)
  (let ([str (apply format id-template (map syntax->datum ids))])
    (datum->syntax stx (string->symbol str))))

;; create a new symbol with the given appendix
(define (symbol-append symbol appendix)
  (string->symbol (format "~a~a" (syntax->datum symbol) appendix)))

(module+ test #| symbol-append |#
  (check-match (symbol-append #'some '_or)
               'some_or))

;; return the one of the two that is neither void nor its syntax->datum is void
(define (discard-void-syntax-object a b)
  (if (or (void? a) (void? (syntax->datum a)))
      b
      a))

(module+ test #| discard-void-syntax-object |#
  (check-match (syntax->datum (discard-void-syntax-object (void) #'some))
               'some)

  (check-match (syntax->datum (discard-void-syntax-object #'some (void)))
               'some)

  (check-match (syntax->datum (with-syntax ([void-syn (void)]) (discard-void-syntax-object #'void-syn #'some)))
               'some)

  (check-match (syntax->datum (with-syntax ([void-syn (void)]) (discard-void-syntax-object #'some #'void-syn)))
               'some))

(struct one-arg-adr-modes (relative? accumulator? immediate? zero-page? absolute?))

(struct ind-arg-adr-modes (indirect-x? indirect-y? indirect?))

(struct idx-arg-adr-modes (absolute-x? absolute-y? zero-page-x? zero-page-y?))

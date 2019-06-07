#lang racket

(module+ test
  (require rackunit))

(provide symbol-append discard-void-syntax-object
         (struct-out one-arg-adr-modes)
         (struct-out ind-arg-adr-modes)
         (struct-out idx-arg-adr-modes))

(define (symbol-append symbol appendix)
  (string->symbol (format "~a~a" (syntax->datum symbol) appendix)))

(module+ test
  (check-match (symbol-append #'some '_or)
               'some_or))

(define (discard-void-syntax-object a b)
  (if (or (void? a) (void? (syntax->datum a)))
      b
      a))

(module+ test
  (check-match (syntax->datum (discard-void-syntax-object (void) #'some))
               'some)

  (check-match (syntax->datum (discard-void-syntax-object #'some (void)))
               'some)

  (check-match (syntax->datum (with-syntax ([void-syn (void)]) (discard-void-syntax-object #'void-syn #'some)))
               'some)

  (check-match (syntax->datum (with-syntax ([void-syn (void)]) (discard-void-syntax-object #'some #'void-syn)))
               'some))

(struct one-arg-adr-modes (relative? accumulator? immediate? zero-page? absolute?))

(struct ind-arg-adr-modes (indirect-x? indirect-y?))

(struct idx-arg-adr-modes (absolute-x? absolute-y? zero-page-x?))

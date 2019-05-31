#lang racket

(module+ test
  (require rackunit))

(provide symbol-append discard-void-syntax-object)

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

#lang racket

;; (require racket/provide-syntax)
(require "6510-syntax-utils.rkt")


(module+ test
  (require rackunit))

(provide accumulator-mode)

(define (accumulator-mode opcode operand)
  (with-syntax ([operand-value (syntax->datum operand)]
                [symbol-acc (symbol-append opcode '_acc)])
    (when (equal? 'A (syntax->datum #'operand-value))
      #'(symbol-acc))))

(module+ test
  (check-equal? (syntax->datum (accumulator-mode #'LDA #'A))
                  '(LDA_acc))
  (check-eq? (accumulator-mode #'LDA #'B)
             (void))
  (check-eq? (accumulator-mode #'LDA #'"$10")
             (void)))

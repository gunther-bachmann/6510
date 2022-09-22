#lang racket

;; (require racket/provide-syntax)
(require (for-syntax "6510-syntax-utils.rkt"))


(module+ test
  (require (for-syntax rackunit)))

;; (provide (for-syntax accumulator-mode))

(define-for-syntax (accumulator-mode opcode operand)
  (with-syntax ([operand-value (syntax->datum operand)]
                [symbol-acc (symbol-append opcode '_acc)])
    (when (equal? 'A (syntax->datum #'operand-value))
      #'(symbol-acc))))

(module+ test
  (begin-for-syntax
    (check-equal? (syntax->datum (accumulator-mode #'LDA #'A))
                  '(LDA_acc))
    (check-eq? (accumulator-mode #'LDA #'B)
               (void))
    (check-eq? (accumulator-mode #'LDA #'"$10")
               (void))))

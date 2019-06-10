#lang racket

;; test playground for 'define-for-syntax' functions

(require (only-in "6510-syntax-utils.rkt" symbol-append))
(require "6510-utils.rkt")

(module+ test
  (require rackunit))

(define (absolute-mode opcode operand)
  (with-syntax ([operand-value (syntax->datum operand)]
                [symbol-abs (symbol-append opcode '_abs)])
    (if (6510-number-string? (syntax->datum #'operand-value))
        (with-syntax ([op-number (parse-number-string (syntax->datum operand))])
          (when (<= 256 (syntax->datum #'op-number))
            #'(symbol-abs op-number)))
        (when (6510-label-string? (syntax->datum #'operand-value))
          #'(symbol-abs operand-value)))))

(module+ test
  (check-match (syntax->datum (absolute-mode #'JSR #'"$1000"))
               '(JSR_abs 4096))
  (check-match (syntax->datum (absolute-mode #'JSR #'":out"))
               '(JSR_abs ":out"))
  (check-match (syntax->datum (zero-page-mode #'LDA #'"$10"))
               '(LDA_zp (parse-number-string "$10"))))


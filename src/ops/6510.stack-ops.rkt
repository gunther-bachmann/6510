#lang racket
#|

 provide 6510 stack push/pop instructions:
  PHA
  PHP
  PLA
  PLP

 |#

(require "../6510-addressing.rkt")

(provide PHA PHP PLA PLP)

(module+ test
  (require "../6510-test-utils.rkt"))

(define-opcode PHA ((implicit . #x48)))

(define-opcode PHP ((implicit . #x08)))

(define-opcode PLA ((implicit . #x68)))

(define-opcode PLP ((implicit . #x28)))

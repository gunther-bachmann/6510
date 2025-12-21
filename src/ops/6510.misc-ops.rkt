#lang racket
#|

 provide 6510 miscellaneous instructions:
  BIT
  BRK
  NOP

 |#


(require "../ast/6510-command.rkt")
(require "../scheme-asm/6510-addressing.rkt")

(provide BIT BRK NOP)

(module+ test
  (require "../6510-test-utils.rkt"))

(define-opcode BIT
  ((zero-page . #x24)
   (absolute . #x2c)))

(define-opcode BRK ((implicit . #x00)))

(module+ test #| BRK |#
  (check-match  (BRK)
                (ast-opcode-cmd _ '(#x00))))

(define-opcode NOP ((implicit . #xea)))

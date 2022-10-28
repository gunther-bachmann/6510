#lang racket

(require "6510-addressing-utils.rkt")
(require "6510-addressing.rkt")
(require "6510-command.rkt")

(provide BIT BRK NOP) 

(module+ test
  (require "6510-test-utils.rkt"))

(define-opcode BIT
  ((zero-page . #x24)
   (absolute . #x2c)))

(define-opcode BRK ((implicit . #x00)))

(module+ test #| BRK |#
  (check-equal?  (BRK)
                 (ast-opcode-cmd '(#x00))))

(define (NOP)
  (ast-opcode-cmd '(#xea)))

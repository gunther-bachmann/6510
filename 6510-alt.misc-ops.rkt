#lang racket

(require "6510-alt-utils.rkt")
(require "6510-alt-addressing.rkt")

(provide BIT BRK NOP) 

(module+ test
  (require "6510-test-utils.rkt"))

(define-opcode BIT
  ((zero-page . #x24)
   (absolute . #x2c)))

(define-opcode BRK ((implicit . #x00)))

(module+ test #| BRK |#
  (check-equal?  (BRK)
                 '(opcode #x00)))

(define (NOP)
  '(opcode #xea))

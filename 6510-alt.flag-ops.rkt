#lang racket

(require "6510-alt-utils.rkt")
(require "6510-alt-addressing.rkt")

(provide CLC CLD CLI CLV SEC SED SEI) 

(module+ test
  (require "6510-test-utils.rkt"))

(define-opcode CLC ((implicit . #x18)))
(define-opcode CLD ((implicit . #xd8)))
(define-opcode CLI ((implicit . #x58)))
(define-opcode CLV ((implicit . #xb8)))
(define-opcode SEC ((implicit . #x38)))
(define-opcode SED ((implicit . #xf8)))
(define-opcode SEI ((implicit . #x78)))

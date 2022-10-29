#lang racket

(require "6510-addressing.rkt")

(provide PHA PHP PLA PLP)

(module+ test
  (require "6510-test-utils.rkt"))

(define-opcode PHA ((implicit . #x48)))

(define-opcode PHP ((implicit . #x08)))

(define-opcode PLA ((implicit . #x68)))

(define-opcode PLP ((implicit . #x28)))

(define-opcode RTI ((implicit . #x40)))

(define-opcode RTS ((implicit . #x60)))

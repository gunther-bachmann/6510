#lang racket

(require "6510-alt-utils.rkt")
(require "6510-alt.rkt")

(module+ test
  (require rackunit))

(module+ test
  (check-equal?
   (list
    (ASL $10)
    (BEQ $F0)
    (BRK)
    (JMP $FFD2)
    (NOP)
    (SBC $10,x)
    (STX $3A,y))
   '((opcode 6 16)
     (rel-opcode 240 240)
     (opcode 0)
     (opcode 76 210 255)
     (opcode 234)
     (opcode 245 16)
     (opcode 150 58))))

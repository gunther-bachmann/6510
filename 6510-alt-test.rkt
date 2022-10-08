#lang racket

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
    (JMP ($FFFE))
    (NOP)
    (SBC $10,x)
    (STX $3A,y))
   '((opcode 6 16)
     (rel-opcode 240 240)
     (opcode 0)
     (opcode #x4c #xD2 #xFF)
     (opcode #x6c #xFE #xFF)
     (opcode 234)
     (opcode 245 16)
     (opcode 150 58))))

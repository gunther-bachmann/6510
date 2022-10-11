#lang racket

(require "6510-alt.rkt")

(module+ test
  (require rackunit))

(module+ test #| smoke test |#
  #| test that the first step in compilation workds
   | even when used in a totally different file (importing the macros)
   |#
  (check-equal?
   (list
    (ASL $10)
    (BEQ $F0)
    (BRK)
    (JMP $FFD2)
    (JMP ($FFFE))
    (JMP (some))
    (SBC some)
    (SBC >some)
    (BEQ some)
    (NOP)
    (SBC $10,x)
    (STX $3A,y)
    (asc "some"))
   '((opcode 6 16)
     (rel-opcode 240 240)
     (opcode 0)
     (opcode #x4c #xD2 #xFF)
     (opcode #x6c #xFE #xFF)
     (opcode #x6c (resolve-word "some"))
     (decide (((resolve-byte "some") opcode 229)
              ((resolve-word "some") opcode 237)))
     (opcode 229 (resolve-byte ">some"))
     (rel-opcode 240 (resolve-relative "some"))
     (opcode 234)
     (opcode 245 16)
     (opcode 150 58)
     (byte 115 111 109 101))))

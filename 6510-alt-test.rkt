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
    ;; (JMP (some))
    (SBC some)
    ;; (SBC >some)
    (BEQ some)
    (NOP)
    (SBC $10,x)
    (STX $3A,y)
    (asc "some"))
   (list
    (ast-opcode-cmd '(6 16))
    (ast-rel-opcode-cmd '(240 240))
    (ast-opcode-cmd '(0))
    (ast-opcode-cmd '(#x4c #xD2 #xFF))
    (ast-opcode-cmd '(#x6c #xFE #xFF))
    ;; (ast-opcode-cmd '(#x6c) (ast-resolve-word-scmd "some" 'absolute))
    '(decide (((resolve-byte "some") opcode 229)
              ((resolve-word "some") opcode 237)))
    ;; (ast-opcode-cmd '(229) (ast-resolve-byte-scmd "some") 'high)
    (ast-unresolved-rel-opcode-cmd '(240) (ast-resolve-byte-scmd "some" 'relative))
    (ast-opcode-cmd '(234))
    (ast-opcode-cmd '(245 16))
    (ast-opcode-cmd '(150 58))
    (ast-bytes-cmd '(115 111 109 101)))))

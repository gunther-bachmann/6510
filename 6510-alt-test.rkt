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
   (list
    (ast-opcode-cmd '(#x06 #x10))
    (ast-rel-opcode-cmd '(240 240))
    (ast-opcode-cmd '(#x00))
    (ast-opcode-cmd '(#x4c #xD2 #xFF))
    (ast-opcode-cmd '(#x6c #xFE #xFF))
    (ast-unresolved-opcode-cmd '(#x6c) (ast-resolve-word-scmd "some"))
    (ast-decide-cmd
     (list (ast-unresolved-opcode-cmd '(#xe5) (ast-resolve-byte-scmd "some" 'low-byte))
           (ast-unresolved-opcode-cmd '(#xed) (ast-resolve-word-scmd "some"))))
    (ast-unresolved-opcode-cmd '(#xe5) (ast-resolve-byte-scmd "some" 'high-byte))
    (ast-unresolved-rel-opcode-cmd '(#xf0) (ast-resolve-byte-scmd "some" 'relative))
    (ast-opcode-cmd '(#xea))
    (ast-opcode-cmd '(#xf5 #x10))
    (ast-opcode-cmd '(#x96 #x3a))
    (ast-bytes-cmd '(#x73 #x6f #x6d #x65)))))

#lang racket
#|

 provide 6510 flag manipulation instructions:
  CLC
  CLD
  CLV
  SEC
  SED
  SEI

 |#

(require "../scheme-asm/6510-addressing.rkt")

(provide CLC CLD CLI CLV SEC SED SEI) 

(module+ test
  (require "../ast/6510-command.rkt")
  (require "../6510-test-utils.rkt"))

(define-opcode CLC ((implicit . #x18)))

(module+ test
  (check-equal? (CLC (#:line 10 #:org-cmd "clc"))
                (ast-opcode-cmd '(#x18))))

(define-opcode CLD ((implicit . #xd8)))
(define-opcode CLI ((implicit . #x58)))
(define-opcode CLV ((implicit . #xb8)))
(define-opcode SEC ((implicit . #x38)))
(define-opcode SED ((implicit . #xf8)))
(define-opcode SEI ((implicit . #x78)))

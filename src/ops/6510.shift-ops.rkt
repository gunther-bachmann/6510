#lang racket
#|

 provide 6510 shift instructions:
  ASL
  LSR
  ROL
  ROR

 |#

(require "../scheme-asm/6510-addressing.rkt")
(require "../ast/6510-command.rkt")


(provide ASL LSR ROL ROR) 

(module+ test
  (require "../6510-test-utils.rkt"))

(define-opcode ASL
  ((accumulator . #x0a)
   (zero-page   . #x06)
   (zero-page-x . #x16)
   (absolute    . #x0e)
   (absolute-x  . #x1e)))

(module+ test #| ASL |#
  (check-equal? (ASL A)
                (ast-opcode-cmd '() '(#x0a)))
  (check-equal? (ASL (#:line 17 #:org-cmd "asl a") A)
                (ast-opcode-cmd '() '(#x0a)))
  (check-equal? (ASL $10)
                (ast-opcode-cmd '() '(#x06 #x10)))
  (check-equal? (ASL (#:line 17 #:org-cmd "asl $10") $10)
                (ast-opcode-cmd '() '(#x06 #x10)))
  (check-equal? (ASL $10,x)
                (ast-opcode-cmd '() '(#x16 #x10)))
  (check-equal? (ASL $1000)
                (ast-opcode-cmd '() '(#x0e #x00 #x10)))
  (check-equal? (ASL $1000,x)
                (ast-opcode-cmd '() '(#x1e #x00 #x10)))
  (check-equal? (ASL (#:line 17 #:org-cmd "asl $1000,x") $1000,x)
                (ast-opcode-cmd '() '(#x1e #x00 #x10))))

(define-opcode LSR
  ((zero-page   . #x46)
   (implicit    . #x4a)
   (absolute    . #x4e)
   (zero-page-x . #x56)
   (absolute-x  . #x5e)))

(define-opcode ROL
  ((zero-page   . #x26)
   (implicit    . #x2a)
   (absolute    . #x2e)
   (zero-page-x . #x36)
   (absolute-x  . #x3e)))

(define-opcode ROR
  ((zero-page   . #x66)
   (implicit    . #x6a)
   (absolute    . #x6e)
   (zero-page-x . #x76)
   (absolute-x  . #x7e)))

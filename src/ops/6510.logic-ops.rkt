#lang racket
#|

 provide 6510 logical instructions:
  AND
  EOR
  ORA

|#

(require "../scheme-asm/6510-addressing.rkt")

(provide AND EOR ORA)

(module+ test
  (require "../ast/6510-command.rkt")
  (require "../6510-test-utils.rkt"))

(define-opcode AND
  ((indirect-x   . #x21)
    (zero-page   . #x25)
    (immediate   . #x29)
    (absolute    . #x2d)
    (indirect-y  . #x31)
    (zero-page-x . #x35)
    (absolute-y  . #x39)
    (absolute-x  . #x3d)))

(define-opcode EOR
  ((indirect-x  . #x41)
   (zero-page   . #x45)
   (immediate   . #x49)
   (absolute    . #x4d)
   (indirect-y  . #x51)
   (zero-page-x . #x55)
   (absolute-y  . #x59)
   (absolute-x  . #x5d)))

(define-opcode ORA
  ((indirect-x  . #x01)
   (zero-page   . #x05)
   (immediate   . #x09)
   (absolute    . #x0d)
   (indirect-y  . #x11)
   (zero-page-x . #x15)
   (absolute-y  . #x19)
   (absolute-x  . #x1d)))

(module+ test #| ora |#
  (check-match (ORA ("$10",x))
               (ast-opcode-cmd _ '(#x01 #x10)))
  (check-match (ORA "$10")
               (ast-opcode-cmd _ '(#x05 #x10)))
  (check-match (ORA "!$10")
               (ast-opcode-cmd _ '(#x09 #x10)))
  (check-match (ORA "$1011")
               (ast-opcode-cmd _ '(#x0d #x11 #x10)))
  (check-match (ORA ("$10"),y)
               (ast-opcode-cmd _ '(#x11 #x10)))
  (check-match (ORA "$10",x)
               (ast-opcode-cmd _ '(#x15 #x10)))
  (check-match (ORA "$1011",y)
               (ast-opcode-cmd _ '(#x19 #x11 #x10)))
  (check-match (ORA "$1011",x)
               (ast-opcode-cmd _ '(#x1d #x11 #x10))))

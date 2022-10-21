#lang racket

(require "6510-alt-utils.rkt")
(require "6510-alt-addressing.rkt")
(require "6510-alt-command.rkt")

(provide AND EOR ORA) 

(module+ test
  (require "6510-test-utils.rkt"))

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
               (ast-opcode-cmd '(#x01 #x10)))
  (check-match (ORA "$10")
               (ast-opcode-cmd '(#x05 #x10)))
  (check-match (ORA "!$10")
               (ast-opcode-cmd '(#x09 #x10)))
  (check-match (ORA "$1011")
               (ast-opcode-cmd '(#x0d #x11 #x10)))
  (check-match (ORA ("$10"),y)
               (ast-opcode-cmd '(#x11 #x10)))
  (check-match (ORA "$10",x)
               (ast-opcode-cmd '(#x15 #x10)))
  (check-match (ORA "$1011",y)
               (ast-opcode-cmd '(#x19 #x11 #x10)))
  (check-match (ORA "$1011",x)
               (ast-opcode-cmd '(#x1d #x11 #x10))))

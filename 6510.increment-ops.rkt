#lang racket

(require "6510-addressing-utils.rkt")
(require "6510-addressing.rkt")
(require "6510-command.rkt")

(provide DEC DEX DEY INC INX INY) 

(module+ test
  (require "6510-test-utils.rkt"))

(define-opcode DEC
  ((zero-page . #xc6) (absolute . #xce) (absolute-x . #xde)  (zero-page-x . #xd6)))

(define-opcode DEX ((implicit . #xCA)))

(module+ test #| DEX |#
  (check-equal? (DEX)
                (ast-opcode-cmd '(#xca))))

(define-opcode DEY ((implicit . #x88)))

(define-opcode INC
  ((zero-page . #xe6) (absolute . #xee) (absolute-x . #xfe) (zero-page-x . #xf6)))

(module+ test
  (check-match (INC "$10")
               (ast-opcode-cmd '(#xE6 #x10)))

  (check-match (INC "$10",x)
               (ast-opcode-cmd '(#xF6 #x10)))

  (check-match (INC "$1000")
               (ast-opcode-cmd '(#xEE #x00 #x10)))

  (check-match (INC "$1000",x)
               (ast-opcode-cmd '(#xFE #x00 #x10))))

(define-opcode INX ((implicit . #xe8)))

(define-opcode INY ((implicit . #xc8)))

#lang racket

(require "6510-alt-utils.rkt")
(require "6510-alt-addressing.rkt")

(provide DEC DEX DEY INC INX INY) 

(module+ test
  (require "6510-test-utils.rkt"))

;;--------------------------------------------------------------------------------
;; https://docs.racket-lang.org/reference/syntax-util.html
;; (format-id ...)

;; https://blog.racket-lang.org/2011/04/writing-syntax-case-macros.html
;;--------------------------------------------------------------------------------

(define-opcode DEC
  ((zero-page . #xc6) (absolute . #xce) (absolute-x . #xde)  (zero-page-x . #xd6)))

(define-opcode DEX ((implicit . #xCA)))

(module+ test #| DEX |#
  (check-equal? (DEX)
                '(opcode #xca)))

(define-opcode DEY ((implicit . #x88)))

(define-opcode INC
  ((zero-page . #xe6) (absolute . #xee) (absolute-x . #xfe) (zero-page-x . #xf6)))

(module+ test
  (check-match (INC "$10")
               '(opcode #xE6 #x10))

  (check-match (INC "$10",x)
               '(opcode #xF6 #x10))

  (check-match (INC "$1000")
               '(opcode #xEE #x00 #x10))

  (check-match (INC "$1000",x)
               '(opcode #xFE #x00 #x10)))

(define-opcode INX ((implicit . #xe8)))

(define-opcode INY ((implicit . #xc8)))

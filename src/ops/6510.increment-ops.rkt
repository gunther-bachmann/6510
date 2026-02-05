#lang racket
#|

 provide 6510 in-/decrement instructions:
  DEC
  DEX
  DEY
  INC
  INX
  INY

 |#

(require "../scheme-asm/6510-addressing.rkt")

(provide DEC DEX DEY INC INX INY)

(module+ test
  (require "../ast/6510-command.rkt")
  (require "../6510-test-utils.rkt"))

(define-opcode DEC
  ((zero-page   . #xc6) ;; clocks: 5
   (absolute    . #xce) ;; clocks: 6
   (absolute-x  . #xde) ;; clocks: 7
   (zero-page-x . #xd6) ;; clocks: 6
   ))

(define-opcode DEX ((implicit . #xCA))) ;; clocks: 2

(module+ test #| DEX |#
  (check-match (DEX)
               (ast-opcode-cmd _ '(#xca))))

(define-opcode DEY ((implicit . #x88))) ;; clocks: 2

(define-opcode INC
  ((zero-page   . #xe6) ;; clocks: 5
   (absolute    . #xee) ;; clocks: 6
   (absolute-x  . #xfe) ;; clocks: 7
   (zero-page-x . #xf6) ;; clocks: 6
   ))

(module+ test
  (check-match (INC "$10")
               (ast-opcode-cmd _ '(#xE6 #x10)))

  (check-match (INC "$10",x)
               (ast-opcode-cmd _ '(#xF6 #x10)))

  (check-match (INC "$1000")
               (ast-opcode-cmd _ '(#xEE #x00 #x10)))

  (check-match (INC "$1000",x)
               (ast-opcode-cmd _ '(#xFE #x00 #x10))))

(define-opcode INX ((implicit . #xe8))) ;; clocks: 2

(define-opcode INY ((implicit . #xc8))) ;; clocks: 2

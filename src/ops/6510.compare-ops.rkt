#lang racket
#|

 provide 6510 compare instructions:
  CMP
  CPX
  CPY 

 |#

(require "../scheme-asm/6510-addressing.rkt")

(provide CMP CPX CPY) 

(module+ test
  (require "../6510-test-utils.rkt"))

(define-opcode CMP
  ((indirect-x . #xc1)
  (zero-page   . #xc5)
  (immediate   . #xc9)
  (absolute    . #xcd)
  (indirect-y  . #xd1)
  (zero-page-x . #xd5)
  (absolute-y  . #xd9)
  (absolute-x  . #xdd)))

(define-opcode CPX
  ((immediate . #xe0) (zero-page . #xe4) (absolute . #xec)))

(define-opcode CPY
  ((immediate . #xc0) (zero-page . #xc4) (absolute . #xcc)))

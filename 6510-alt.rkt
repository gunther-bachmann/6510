#lang racket

(require "6510-alt-utils.rkt")
(require (for-syntax "6510-alt-utils.rkt"))
(require (for-syntax "6510-alt-addressing.rkt"))

(require (for-syntax "6510-utils.rkt"))

(provide ASL BEQ BRK JMP NOP SBC STX)

(module+ test
  (require rackunit))

;;--------------------------------------------------------------------------------
;; https://docs.racket-lang.org/reference/syntax-util.html
;; (format-id ...)

;; https://blog.racket-lang.org/2011/04/writing-syntax-case-macros.html
;;--------------------------------------------------------------------------------

(define-syntax (define-opcode stx)
    (syntax-case stx ()
      ([_ mnemonic addressing-modes]
       (with-syntax ((nstx (make-id stx "~a" #'nstx)))
         #`(define-syntax (mnemonic nstx) 
             (syntax-case nstx ()
               ([_]         (no-op  nstx #'addressing-modes))
               ([_ op]      (one-op nstx #'addressing-modes #'op))
               ([_ op1 op2] (two-op nstx #'addressing-modes #'op1 #'op2))))))))

(define-opcode ASL
  ((accumulator . #x0a)
   (zero-page   . #x06)
   (zero-page-x . #x16)
   (absolute    . #x0e)
   (absolute-x  . #x1e)))

(module+ test #| ASL |#
  (check-equal? (ASL A)
                '(opcode #x0a))
  (check-equal? (ASL $10)
                '(opcode #x06 #x10))
  (check-equal? (ASL $10,x)
                '(opcode #x16 #x10))
  (check-equal? (ASL $1000)
                '(opcode #x0e #x00 #x10))
  (check-equal? (ASL $1000,x)
                '(opcode #x1e #x00 #x10)))

(define-opcode BEQ ((relative . #xf0)))

(module+ test #| BEQ |#
  (check-equal? (BEQ $10)
                '(rel-opcode #xf0 #x10)))

(define-opcode BRK ((implicit . #x00)))

(module+ test #| BRK |#
  (check-equal?  (BRK)
                 '(opcode #x00)))

(define-opcode JMP
  ((absolute . #x4C)
   (indirect . #x6c)))

(module+ test #| JMP |#
  (check-equal? (JMP $FFD2)
                '(opcode #x4c #xd2 #xff))
  (check-equal? (JMP ($FFD2))
                '(opcode #x6c #xd2 #xff)))

(define (NOP)
  '(opcode #xea))

(define-opcode SBC
  ((immediate   . #xe9)
   (zero-page   . #xe5)
   (zero-page-x . #xf5)
   (absolute    . #xed)
   (absolute-x  . #xfd)
   (absolute-y  . #xf9)
   (indirect-x  . #xe1)
   (indirect-y  . #xf1)))

(module+ test #| SBC |#
  (check-equal? (SBC !$11)
                '(opcode #xe9 #x11))
  (check-equal? (SBC $10)
                '(opcode #xe5 #x10))
  ;; (check-equal? (SBC >some)
  ;;               '(opcode #xe5 "some:1:h"))
  ;; (check-equal? (SBC <some)
  ;;               '(opcode #xe5 "some:1:l"))
  (check-equal? (SBC $10,x)
                '(opcode #xf5 #x10))
  (check-equal? (SBC $FF10)
                '(opcode #xed #x10 #xff))
  ;; (check-equal? (SBC some)
  ;;               '(decide (word . (opcode #xed "some:2"))
  ;;                        (byte . (opcode #xe5 "some:1"))))
  (check-equal? (SBC $1112,x)
                '(opcode #xfd #x12 #x11))
  (check-equal? (SBC $1000,y)
                '(opcode #xf9 #x00 #x10))
  (check-equal? (SBC ($11,x))
                '(opcode #xe1 #x11))
  (check-equal? (SBC ($11),y)
                '(opcode #xf1 #x11)))

(define-opcode STX
  ((zero-page   . #x86)
   (absolute    . #x8e)
   (zero-page-y . #x96)))

(module+ test #| STX |#  
  (check-equal? (STX $10)
                '(opcode #x86 #x10))
  (check-equal? (STX $1012)
                '(opcode #x8e #x12 #x10))
  (check-equal? (STX $10,y)
                '(opcode #x96 #x10)))

;; idea: delayed decision about byte/word operand

;; '('decision-byte-word ":label" (opcode #x00 #x00) (opcode #x01 #x00 #x00)) ;; if :label is byte value use first opcode, else use second
;; is transformed to
;; '(opcode #x00 #x00) ;; in case label is a byte value
;; '(opcode #x01 #x00 #x00) ;; in case label is a word value



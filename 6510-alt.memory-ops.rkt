#lang racket

(require "6510-alt-utils.rkt")
(require "6510-alt-addressing.rkt")

(provide LDA LDX LDY STA STX STY) 

(module+ test
  (require "6510-test-utils.rkt"))

(define-opcode LDA
  ((immediate   . #xA9)
   (zero-page   . #xA5)
   (zero-page-x . #xB5)
   (absolute    . #xAD)
   (absolute-x  . #xBD)
   (absolute-y  . #xB9)
   (indirect-x  . #xA1)
   (indirect-y  . #xB1)))

(module+ test #| lda |#
  (check-match (LDA "!$10")
               '(opcode #xA9 16))
  (check-match (LDA "$17")
               '(opcode #xa5 #x17))
  (check-match (LDA "$178F")
               '(opcode #xad #x8F #x17))
  (check-match (LDA "$10",x)
               '(opcode #xB5 16))
  (check-match (LDA "$A000",x)
               '(opcode #xBD #x00 #xA0))
  (check-match (LDA "$A000",y)
               '(opcode #xB9 #x00 #xA0))
  (check-match (LDA ("$A0"),y )
               '(opcode #xB1 #xA0))
  (check-match (LDA ("$A0",x) )
               '(opcode #xA1 #xA0)))

(define-opcode LDX
  ((immediate   . #xA2)
   (zero-page   . #xA6)
   (zero-page-y . #xB6)
   (absolute    . #xAE)
   (absolute-y  . #xBE)))

(module+ test #| LDX |#
  (check-equal? (LDX !$10)
                '(opcode #xA2 #x10))
  (check-equal? (LDX $10)
                '(opcode #xA6 #x10))
  (check-equal? (LDX $10,y)
                '(opcode #xB6 #x10))
  (check-equal? (LDX $1020)
                '(opcode #xAE #x20 #x10))
  (check-equal? (LDX $1020,y)
                '(opcode #xBE #x20 #x10))
  (check-equal? (LDX hello)
                '(decide (((resolve-byte "hello") opcode #xa6)
                          ((resolve-word "hello") opcode #xae))))
  (check-equal? (LDX (#:line 17 :#org-cmd "ldx hello") hello)
                '(decide (((resolve-byte "hello") opcode #xa6)
                          ((resolve-word "hello") opcode #xae)))))

(define-opcode LDY
  ((immediate   . #xA0)
   (zero-page   . #xA4)
   (zero-page-x . #xB4)
   (absolute    . #xAC)
   (absolute-x  . #xBC)))

(define-opcode STA
  ((zero-page   . #x85)
   (zero-page-x . #x95)
   (absolute    . #x8d)
   (absolute-x  . #x9d)
   (absolute-y  . #x99)
   (indirect-x  . #x81)
   (indirect-y  . #x91)))

(module+ test
  (check-match (STA "$17")
               '(opcode #x85 23))

  (check-match (STA "$1728")
               '(opcode #x8d #x28 #x17))

  (check-match (STA ("$17",x))
               '(opcode #x81 #x17))

  (check-match (STA ("$28"),y)
               '(opcode #x91 #x28))

  (check-match (STA "$1728",x)
               '(opcode #x9d #x28 #x17))

  (check-match (STA "$1728",y)
               '(opcode #x99 #x28 #x17))

  (check-match (STA "$28",x)
               '(opcode #x95 #x28)))

(define-opcode STX
  ((zero-page   . #x86)
   (absolute    . #x8e)
   (zero-page-y . #x96)))

(module+ test #| STX |#  
  (check-equal? (STX $10)
                '(opcode #x86 #x10))
  (check-equal? (STX some)
                '(decide (((resolve-byte "some") opcode #x86)
                          ((resolve-word "some") opcode #x8e))))
  (check-equal? (STX $1012)
                '(opcode #x8e #x12 #x10))
  (check-equal? (STX $10,y)
                '(opcode #x96 #x10))
  (check-equal? (STX some,y)
                '(opcode #x96 (resolve-byte "some"))))

(define-opcode STY
  ((zero-page   . #x84)
   (absolute    . #x8c)
   (zero-page-x . #x94)))


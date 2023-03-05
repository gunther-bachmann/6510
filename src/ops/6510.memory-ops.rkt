#lang racket
#|

 provide 6510 memory load/store instructions:
  LDA
  LDX
  LDY
  STA
  STX
  STY

 |#

(require "../scheme-asm/6510-addressing.rkt")

(provide LDA LDX LDY STA STX STY)

(module+ test
  (require "../ast/6510-command.rkt")
  (require "../6510-test-utils.rkt"))

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
               (ast-opcode-cmd '(#xA9 16)))
  (check-match (LDA "$17")
               (ast-opcode-cmd '(#xa5 #x17)))
  (check-match (LDA "$178F")
               (ast-opcode-cmd '(#xad #x8F #x17)))
  (check-match (LDA "$10",x)
               (ast-opcode-cmd '(#xB5 16)))
  (check-match (LDA "$A000",x)
               (ast-opcode-cmd '(#xBD #x00 #xA0)))
  (check-match (LDA "$A000",y)
               (ast-opcode-cmd '(#xB9 #x00 #xA0)))
  (check-match (LDA ("$A0"),y )
               (ast-opcode-cmd '(#xB1 #xA0)))
  (check-match (LDA ("$A0",x) )
               (ast-opcode-cmd '(#xA1 #xA0))))

(define-opcode LDX
  ((immediate   . #xA2)
   (zero-page   . #xA6)
   (zero-page-y . #xB6)
   (absolute    . #xAE)
   (absolute-y  . #xBE)))

(module+ test #| LDX |#
  (check-equal? (LDX !$10)
                (ast-opcode-cmd '(#xA2 #x10)))
  (check-equal? (LDX $10)
                (ast-opcode-cmd '(#xA6 #x10)))
  (check-equal? (LDX $10,y)
                (ast-opcode-cmd '(#xB6 #x10)))
  (check-equal? (LDX $1020)
                (ast-opcode-cmd '(#xAE #x20 #x10)))
  (check-equal? (LDX $1020,y)
                (ast-opcode-cmd '(#xBE #x20 #x10)))
  (check-equal? (LDX hello)
                (ast-decide-cmd
                 (list (ast-unresolved-opcode-cmd '(#xa6) (ast-resolve-byte-scmd "hello" 'low-byte))
                       (ast-unresolved-opcode-cmd '(#xae) (ast-resolve-word-scmd "hello")))))
  (check-equal? (LDX (#:line 17 :#org-cmd "ldx hello") hello)
                (ast-decide-cmd
                 (list (ast-unresolved-opcode-cmd '(#xa6) (ast-resolve-byte-scmd "hello" 'low-byte))
                       (ast-unresolved-opcode-cmd '(#xae) (ast-resolve-word-scmd "hello"))))))

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
               (ast-opcode-cmd '(#x85 23)))

  (check-match (STA "$1728")
               (ast-opcode-cmd '(#x8d #x28 #x17)))

  (check-match (STA ("$17",x))
               (ast-opcode-cmd '(#x81 #x17)))

  (check-match (STA ("$28"),y)
               (ast-opcode-cmd '(#x91 #x28)))

  (check-match (STA "$1728",x)
               (ast-opcode-cmd '(#x9d #x28 #x17)))

  (check-match (STA "$1728",y)
               (ast-opcode-cmd '(#x99 #x28 #x17)))

  (check-match (STA "$28",x)
               (ast-opcode-cmd '(#x95 #x28))))

(define-opcode STX
  ((zero-page   . #x86)
   (absolute    . #x8e)
   (zero-page-y . #x96)))

(module+ test #| STX |#  
  (check-equal? (STX $10)
                (ast-opcode-cmd '(#x86 #x10)))
  (check-equal? (STX some) 
                (ast-decide-cmd
                 (list (ast-unresolved-opcode-cmd '(#x86) (ast-resolve-byte-scmd "some" 'low-byte))
                       (ast-unresolved-opcode-cmd '(#x8e) (ast-resolve-word-scmd "some")))))
  (check-equal? (STX $1012)
                (ast-opcode-cmd '(#x8e #x12 #x10)))
  (check-equal? (STX $10,y)
                (ast-opcode-cmd '(#x96 #x10)))
  (check-equal? (STX some,y)
                (ast-unresolved-opcode-cmd '(#x96) (ast-resolve-byte-scmd "some" 'low-byte))))

(define-opcode STY
  ((zero-page   . #x84)
   (absolute    . #x8c)
   (zero-page-x . #x94)))


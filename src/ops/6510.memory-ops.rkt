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
  ((immediate   . #xA9)         ;; tim: 2
   (zero-page   . #xA5)         ;; tim: 3
   (zero-page-x . #xB5)         ;; tim: 4
   (absolute    . #xAD)         ;; tim: 4
   (absolute-x  . #xBD)         ;; tim: 4+
   (absolute-y  . #xB9)         ;; tim: 4+
   (indirect-x  . #xA1)         ;; tim: 6
   (indirect-y  . #xB1)))       ;; tim: 5+

(module+ test #| lda |#
  (check-match (LDA "!$10")
               (ast-opcode-cmd _ '(#xA9 16)))
  (check-match (LDA "$17")
               (ast-opcode-cmd _ '(#xa5 #x17)))
  (check-match (LDA "$178F")
               (ast-opcode-cmd _ '(#xad #x8F #x17)))
  (check-match (LDA "$10",x)
               (ast-opcode-cmd _ '(#xB5 16)))
  (check-match (LDA "$A000",x)
               (ast-opcode-cmd _ '(#xBD #x00 #xA0)))
  (check-match (LDA "$A000",y)
               (ast-opcode-cmd _ '(#xB9 #x00 #xA0)))
  (check-match (LDA ("$A0"),y )
               (ast-opcode-cmd _ '(#xB1 #xA0)))
  (check-match (LDA ("$A0",x) )
               (ast-opcode-cmd _ '(#xA1 #xA0))))

(define-opcode LDX
  ((immediate   . #xA2)         ;; tim: 2
   (zero-page   . #xA6)         ;; tim: 3
   (zero-page-y . #xB6)         ;; tim: 4
   (absolute    . #xAE)         ;; tim: 4
   (absolute-y  . #xBE)))       ;; tim: 4+

(module+ test #| LDX |#
  (check-match (LDX !$10)
                (ast-opcode-cmd _ '(#xA2 #x10)))
  (check-match (LDX $10)
                (ast-opcode-cmd _ '(#xA6 #x10)))
  (check-match (LDX $10,y)
                (ast-opcode-cmd _ '(#xB6 #x10)))
  (check-match (LDX $1020)
                (ast-opcode-cmd _ '(#xAE #x20 #x10)))
  (check-match (LDX $1020,y)
                (ast-opcode-cmd _ '(#xBE #x20 #x10)))
  (check-match (LDX hello)
                (ast-decide-cmd
                 _
                 (list (ast-unresolved-opcode-cmd _ '(#xa6) (ast-resolve-byte-scmd "hello" 'low-byte))
                       (ast-unresolved-opcode-cmd _ '(#xae) (ast-resolve-word-scmd "hello")))))
  (check-match (LDX '(#:line 17 :#org-cmd "ldx hello") hello)
                (ast-decide-cmd
                 '(#:line 17 :#org-cmd "ldx hello")
                 (list (ast-unresolved-opcode-cmd '(#:line 17 :#org-cmd "ldx hello") '(#xa6) (ast-resolve-byte-scmd "hello" 'low-byte))
                       (ast-unresolved-opcode-cmd '(#:line 17 :#org-cmd "ldx hello") '(#xae) (ast-resolve-word-scmd "hello"))))))

(define-opcode LDY
  ((immediate   . #xA0)         ;; tim: 2
   (zero-page   . #xA4)         ;; tim: 3
   (zero-page-x . #xB4)         ;; tim: 4
   (absolute    . #xAC)         ;; tim: 4
   (absolute-x  . #xBC)))       ;; tim: 4+

(define-opcode STA
  ((zero-page   . #x85)         ;; tim: 3
   (zero-page-x . #x95)         ;; tim: 4
   (absolute    . #x8d)         ;; tim: 4
   (absolute-x  . #x9d)         ;; tim: 5
   (absolute-y  . #x99)         ;; tim: 5
   (indirect-x  . #x81)         ;; tim: 6
   (indirect-y  . #x91)))       ;; tim: 6

(module+ test
  (check-match (STA "$17")
               (ast-opcode-cmd _ '(#x85 23)))

  (check-match (STA "$1728")
               (ast-opcode-cmd _ '(#x8d #x28 #x17)))

  (check-match (STA ("$17",x))
               (ast-opcode-cmd _ '(#x81 #x17)))

  (check-match (STA ("$28"),y)
               (ast-opcode-cmd _ '(#x91 #x28)))

  (check-match (STA "$1728",x)
               (ast-opcode-cmd _ '(#x9d #x28 #x17)))

  (check-match (STA "$1728",y)
               (ast-opcode-cmd _ '(#x99 #x28 #x17)))

  (check-match (STA "$28",x)
               (ast-opcode-cmd _ '(#x95 #x28))))

(define-opcode STX
  ((zero-page   . #x86)         ;; tim: 3
   (absolute    . #x8e)         ;; tim: 4
   (zero-page-y . #x96)))       ;; tim: 4

(module+ test #| STX |#  
  (check-match (STX $10)
                (ast-opcode-cmd _ '(#x86 #x10)))
  (check-match (STX some)
                (ast-decide-cmd
                 _
                 (list (ast-unresolved-opcode-cmd _ '(#x86) (ast-resolve-byte-scmd "some" 'low-byte))
                       (ast-unresolved-opcode-cmd _ '(#x8e) (ast-resolve-word-scmd "some")))))
  (check-match (STX $1012)
                (ast-opcode-cmd _ '(#x8e #x12 #x10)))
  (check-match (STX $10,y)
                (ast-opcode-cmd _ '(#x96 #x10)))
  (check-match (STX some,y)
                (ast-unresolved-opcode-cmd _ '(#x96) (ast-resolve-byte-scmd "some" 'low-byte))))

(define-opcode STY
  ((zero-page   . #x84)         ;; tim: 3
   (absolute    . #x8c)         ;; tim: 4
   (zero-page-x . #x94)))       ;; tim: 4

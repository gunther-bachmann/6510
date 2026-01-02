#lang racket
#|

 provide 6510 arithmetic instructions:
  ADC
  SBC

 |#

(require "../scheme-asm/6510-addressing.rkt")

(provide ADC SBC)

(module+ test
  (require "../ast/6510-command.rkt")
  (require "../6510-test-utils.rkt"))

(define-opcode ADC
  ((immediate   . #x69)
   (zero-page   . #x65)
   (zero-page-x . #x75)
   (absolute    . #x6d)
   (absolute-x  . #x7d)
   (absolute-y  . #x79)
   (indirect-x  . #x61)
   (indirect-y  . #x71)))

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
  (check-match (SBC !$11)
               (ast-opcode-cmd _ '(#xe9 #x11)))
  (check-match (SBC !>some)
               (ast-unresolved-opcode-cmd _ '(#xe9) (ast-resolve-byte-scmd "some" 'high-byte)))
  (check-match (SBC !>some+4)
               (ast-unresolved-opcode-cmd _ '(#xe9) (ast-resolve-byte-scmd "some+4" 'high-byte)))
  (check-match (SBC !some)
               (ast-unresolved-opcode-cmd _ '(#xe9) (ast-resolve-byte-scmd "some" 'low-byte)))
  (check-match (SBC $10)
               (ast-opcode-cmd _ '(#xe5 #x10)))
  (check-match (SBC >some)
               (ast-unresolved-opcode-cmd _ '(#xe5) (ast-resolve-byte-scmd "some" 'high-byte)))
  (check-match (SBC <some)
               (ast-unresolved-opcode-cmd _ '(#xe5) (ast-resolve-byte-scmd "some" 'low-byte)))
  (check-match (SBC $10,x)
               (ast-opcode-cmd _ '(#xf5 #x10)))
  (check-match (SBC <some,x)
               (ast-unresolved-opcode-cmd _ '(#xf5) (ast-resolve-byte-scmd "some" 'low-byte)))
  (check-match (SBC $FF10)
               (ast-opcode-cmd _ '(#xed #x10 #xff)))
  (check-match (SBC some)
               (ast-decide-cmd
                   _
                   (list (ast-unresolved-opcode-cmd _ '(#xe5) (ast-resolve-byte-scmd "some" 'low-byte))
                         (ast-unresolved-opcode-cmd _ '(#xed) (ast-resolve-word-scmd "some")))))
  (check-match (SBC some-3)
               (ast-decide-cmd
                   _
                   (list (ast-unresolved-opcode-cmd _ '(#xe5) (ast-resolve-byte-scmd "some-3" 'low-byte))
                         (ast-unresolved-opcode-cmd _ '(#xed) (ast-resolve-word-scmd "some-3")))))
  (check-match (SBC $1112,x)
               (ast-opcode-cmd _ '(#xfd #x12 #x11)))
  (check-match (SBC $1000,y)
               (ast-opcode-cmd _ '(#xf9 #x00 #x10)))
  (check-match (SBC some,x)
               (ast-decide-cmd
                   _
                   (list (ast-unresolved-opcode-cmd _ '(#xf5) (ast-resolve-byte-scmd "some" 'low-byte))
                         (ast-unresolved-opcode-cmd _ '(#xfd) (ast-resolve-word-scmd "some")))))
  (check-match (SBC some,y)
               (ast-unresolved-opcode-cmd _ '(#xf9) (ast-resolve-word-scmd "some")))
  (check-match (SBC ($11,x))
               (ast-opcode-cmd _ '(#xe1 #x11)))
  (check-match (SBC (some,x))
               (ast-unresolved-opcode-cmd _ '(#xe1) (ast-resolve-byte-scmd "some" 'low-byte)))
  (check-match (SBC (some+1,x))
               (ast-unresolved-opcode-cmd _ '(#xe1) (ast-resolve-byte-scmd "some+1" 'low-byte)))
  (check-match (SBC (<some,x))
               (ast-unresolved-opcode-cmd _ '(#xe1) (ast-resolve-byte-scmd "some" 'low-byte)))
  (check-match (SBC ($11),y)
               (ast-opcode-cmd _ '(#xf1 #x11)))
  (check-match (SBC (>some),y)
               (ast-unresolved-opcode-cmd _ '(#xf1) (ast-resolve-byte-scmd "some" 'high-byte)))
  (check-match (SBC (some),y)
               (ast-unresolved-opcode-cmd _ '(#xf1) (ast-resolve-byte-scmd "some" 'low-byte)))
  (check-match (SBC (some-2),y)
               (ast-unresolved-opcode-cmd _ '(#xf1) (ast-resolve-byte-scmd "some-2" 'low-byte))))

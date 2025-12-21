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
  (check-equal? (drop-meta-info (SBC !$11))
                (drop-meta-info (ast-opcode-cmd '() '(#xe9 #x11))))
  (check-equal? (drop-meta-info (SBC !>some))
                (drop-meta-info (ast-unresolved-opcode-cmd '() '(#xe9) (ast-resolve-byte-scmd "some" 'high-byte))))
  (check-equal? (drop-meta-info (SBC !>some+4))
                (drop-meta-info (ast-unresolved-opcode-cmd '() '(#xe9) (ast-resolve-byte-scmd "some+4" 'high-byte))))
  (check-equal? (drop-meta-info (SBC !some))
                (drop-meta-info (ast-unresolved-opcode-cmd '() '(#xe9) (ast-resolve-byte-scmd "some" 'low-byte)))
                "only option is to resolve to byte")
  (check-equal? (drop-meta-info (SBC $10))
                (drop-meta-info (ast-opcode-cmd '() '(#xe5 #x10))))
  (check-equal? (drop-meta-info (SBC >some))
                (drop-meta-info (ast-unresolved-opcode-cmd '() '(#xe5) (ast-resolve-byte-scmd "some" 'high-byte))))
  (check-equal? (drop-meta-info (SBC <some))
                (drop-meta-info (ast-unresolved-opcode-cmd '() '(#xe5) (ast-resolve-byte-scmd "some" 'low-byte))))
  (check-equal? (drop-meta-info (SBC $10,x))
                (drop-meta-info (ast-opcode-cmd '() '(#xf5 #x10))))
  (check-equal? (drop-meta-info (SBC <some,x))
                (drop-meta-info (ast-unresolved-opcode-cmd '() '(#xf5) (ast-resolve-byte-scmd "some" 'low-byte))))
  (check-equal? (drop-meta-info (SBC $FF10))
                (drop-meta-info (ast-opcode-cmd '() '(#xed #x10 #xff))))
  (check-equal? (drop-meta-info (SBC some))
                (drop-meta-info (ast-decide-cmd
                                 '()
                                 (list (ast-unresolved-opcode-cmd '() '(#xe5) (ast-resolve-byte-scmd "some" 'low-byte))
                                       (ast-unresolved-opcode-cmd '() '(#xed) (ast-resolve-word-scmd "some")))))
                "two options resolve to byte (zero page) or word (absolute)")
  (check-equal? (drop-meta-info (SBC some-3))
                (drop-meta-info (ast-decide-cmd
                                 '()
                                 (list (ast-unresolved-opcode-cmd '() '(#xe5) (ast-resolve-byte-scmd "some-3" 'low-byte))
                                       (ast-unresolved-opcode-cmd '() '(#xed) (ast-resolve-word-scmd "some-3")))))
                "two options resolve to byte (zero page) or word (absolute)")
  (check-equal? (drop-meta-info (SBC $1112,x))
                (drop-meta-info (ast-opcode-cmd '() '(#xfd #x12 #x11))))
  (check-equal? (drop-meta-info (SBC $1000,y))
                (drop-meta-info (ast-opcode-cmd '() '(#xf9 #x00 #x10))))
  (check-equal? (drop-meta-info (SBC some,x))
                (drop-meta-info (ast-decide-cmd
                                 '()
                                 (list (ast-unresolved-opcode-cmd '() '(#xf5) (ast-resolve-byte-scmd "some" 'low-byte))
                                       (ast-unresolved-opcode-cmd '() '(#xfd) (ast-resolve-word-scmd "some")))))
                "two options resolve to byte (zero page-x) or word (absolute-x)")
  (check-equal? (drop-meta-info (SBC some,y))
                (drop-meta-info (ast-unresolved-opcode-cmd '() '(#xf9) (ast-resolve-word-scmd "some")))
                "only option is to resolve to byte (zero-page-y)")
  (check-equal? (drop-meta-info (SBC ($11,x)))
                (drop-meta-info (ast-opcode-cmd '() '(#xe1 #x11))))
  (check-equal? (drop-meta-info (SBC (some,x)))
                (drop-meta-info (ast-unresolved-opcode-cmd '() '(#xe1) (ast-resolve-byte-scmd "some" 'low-byte)))
                "only option is to resolve to byte")
  (check-equal? (drop-meta-info (SBC (some+1,x)))
                (drop-meta-info (ast-unresolved-opcode-cmd '() '(#xe1) (ast-resolve-byte-scmd "some+1" 'low-byte)))
                "only option is to resolve to byte")
  (check-equal? (drop-meta-info (SBC (<some,x)))
                (drop-meta-info (ast-unresolved-opcode-cmd '() '(#xe1) (ast-resolve-byte-scmd "some" 'low-byte))))
  (check-equal? (drop-meta-info (SBC ($11),y))
                (drop-meta-info (ast-opcode-cmd '() '(#xf1 #x11))))
  (check-equal? (drop-meta-info (SBC (>some),y))
                (drop-meta-info (ast-unresolved-opcode-cmd '() '(#xf1) (ast-resolve-byte-scmd "some" 'high-byte))))
  (check-equal? (drop-meta-info (SBC (some),y))
                (drop-meta-info (ast-unresolved-opcode-cmd '() '(#xf1) (ast-resolve-byte-scmd "some" 'low-byte)))
                "only option is to resolve to byte")
  (check-equal? (drop-meta-info (SBC (some-2),y))
                (drop-meta-info (ast-unresolved-opcode-cmd '() '(#xf1) (ast-resolve-byte-scmd "some-2" 'low-byte)))
                "only option is to resolve to byte"))

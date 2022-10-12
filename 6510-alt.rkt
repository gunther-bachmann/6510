#lang racket

(require "6510-alt-utils.rkt")
(require (for-syntax "6510-syntax-utils.rkt"))
(require "6510-alt-addressing.rkt")

(require "6510-utils.rkt")
(require (for-syntax "6510-utils.rkt"))

(require "6510-alt.logic-ops.rkt")
(require "6510-alt.branch-ops.rkt")
(require "6510-alt.arithmetic-ops.rkt")
(require "6510-alt.increment-ops.rkt")
(require "6510-alt.flag-ops.rkt")
(require "6510-alt.memory-ops.rkt")
(require "6510-alt.transfer-ops.rkt")

(provide (all-from-out "6510-alt.logic-ops.rkt"))
(provide (all-from-out "6510-alt.branch-ops.rkt"))
(provide (all-from-out "6510-alt.arithmetic-ops.rkt"))
(provide (all-from-out "6510-alt.increment-ops.rkt"))
(provide (all-from-out "6510-alt.flag-ops.rkt"))
(provide (all-from-out "6510-alt.memory-ops.rkt"))
(provide (all-from-out "6510-alt.transfer-ops.rkt"))

(provide ASL LSR ROL ROR;; shift
         BIT BRK NOP ;; misc
         CMP CPX CPY ;; compare
         JMP JSR RTI RTS ;; sub routines
         PHA PHP PLA PLP  ;; stack
         ) 

(provide label word-const byte-const byte word asc) ;; meta commands

(provide (all-from-out "6510-alt-utils.rkt"))

(module+ test
  (require "6510-test-utils.rkt"))

;;--------------------------------------------------------------------------------
;; https://docs.racket-lang.org/reference/syntax-util.html
;; (format-id ...)

;; https://blog.racket-lang.org/2011/04/writing-syntax-case-macros.html
;;--------------------------------------------------------------------------------

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

(define-opcode BIT
  ((zero-page . #x24)
   (absolute . #x2c)))

(define-opcode BRK ((implicit . #x00)))

(module+ test #| BRK |#
  (check-equal?  (BRK)
                 '(opcode #x00)))

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
  ((immediate . #xe0) (zero-page . #xe4) (absolute #xec)))

(define-opcode CPY
  ((immediate . #xc0) (zero-page . #xc4) (absolute . #xcc)))

(define-opcode JMP
  ((absolute . #x4C)
   (indirect . #x6c)))

(module+ test #| JMP |#
  (check-equal? (JMP $FFD2)
                '(opcode #x4c #xd2 #xff))
  (check-equal? (JMP some)
                '(opcode #x4c (resolve-word "some")))
  (check-equal? (JMP ($FFD2))
                '(opcode #x6c #xd2 #xff))
  (check-equal? (JMP (some))
                '(opcode #x6c (resolve-word "some"))))

(define-opcode JSR ((absolute . #x20)))

(module+ test #| JSR |#
  (check-equal? (JSR $FFD2)
                '(opcode #x20 #xd2 #xff)))

(define-opcode LSR
  ((zero-page   . #x46)
   (implicit    . #x4a)
   (absolute    . #x4e)
   (zero-page-x . #x56)
   (absolute-x  . #x5e)))

(define (NOP)
  '(opcode #xea))

(define-opcode PHA ((implicit . #x48)))

(define-opcode PHP ((implicit . #x08)))

(define-opcode PLA ((implicit . #x68)))

(define-opcode PLP ((implicit . #x28)))

(define-opcode ROL
  ((zero-page   . #x26)
   (implicit    . #x2a)
   (absolute    . #x2e)
   (zero-page-x . #x36)
   (absolute-x  . #x3e)))

(define-opcode ROR
  ((zero-page   . #x66)
   (implicit    . #x6a)
   (absolute    . #x6e)
   (zero-page-x . #x76)
   (absolute-x  . #x7e)))

(define-opcode RTI ((implicit . #x40)))

(define-opcode RTS ((implicit . #x60)))

(define-syntax (label stx)
  (syntax-case stx ()
    ([_ str]
     #'`(label-def ,(->string #'str)))))

(module+ test #| label |#
  (check-equal?
   (label some)
   '(label-def "some"))
  (check-equal?
   (label "some")
   '(label-def "some")))

(define-syntax (byte-const stx)
  (syntax-case stx ()
    ([_ label byte]
     (and (6510-number-string? (->string #'byte))
        (in-byte-range? (parse-number-string (->string #'byte))))
     #'`(byte-const-def
         ,(->string #'label)
         ,(parse-number-string (->string #'byte))))))

(module+ test #| byte-const |#
  (check-equal?
   (byte-const some 10)
   '(byte-const-def "some" 10))
  (check-equal?
   (byte-const some %10)
   '(byte-const-def "some" #b10))
  (check-equal?
   (byte-const some $10)
   '(byte-const-def "some" #x10))
  (check-exn exn:fail:syntax? (λ () (expand #'(byte-const some $100)))))

(define-syntax (word-const stx)
  (syntax-case stx ()
    ([_ label word]
     (and (6510-number-string? (->string #'word))
        (in-word-range? (parse-number-string (->string #'word))))
     #'`(word-const-def
         ,(->string #'label)
         ,(parse-number-string (->string #'word))))))

(module+ test #| word-const |#
  (check-equal?
   (word-const some 1000)
   '(word-const-def "some" 1000))
  (check-equal?
   (word-const some %100010001000)
   '(word-const-def "some" #b100010001000))
  (check-equal?
   (word-const some $2000)
   '(word-const-def "some" #x2000))
  (check-exn exn:fail:syntax? (λ () (expand #'(word-const some $10000)))))

(define-syntax (byte stx)
  (syntax-case stx ()
    ([_ byte ...]
     (with-syntax (((is-byte-number ...)
                    (map (λ (val)
                           (let ((str-val (->string val)))
                             (and (6510-number-string? str-val)
                                (in-byte-range? (parse-number-string str-val)))))
                         (syntax->list #'(byte ...)))))
       (all #'(is-byte-number ...)))
     #'`(byte-value ,(parse-number-string (->string #'byte)) ...))))

(module+ test #| byte |#
  (check-equal? (byte $10 $FF $D2 %10010000 %11111111)
                '(byte-value #x10 #xFF #xD2 #b10010000 #b11111111))
  (check-equal? (byte "$10" "$FF" "$D2" "%10010000" "%11111111")
                '(byte-value #x10 #xFF #xD2 #b10010000 #b11111111))
  (check-exn exn:fail:syntax? (λ () (expand #'(byte $10 $100)))))

(define-for-syntax (parse-syntax-number val-stx)
  (parse-number-string (->string val-stx)))

(define-for-syntax (all stx-list)
  (foldl (λ (l r) (and (syntax->datum l) r))
         #t
         (syntax->list stx-list)))

(define-syntax (word stx)
  (syntax-case stx ()
    ([_ word ...]
     (with-syntax (((is-word-number ...)
                    (map (λ (val)                           
                           (and (6510-number-string? (->string val))
                              (in-word-range? (parse-syntax-number val))))
                         (syntax->list #'(word ...)))))
       (all #'(is-word-number ...)))
     (with-syntax (((low-bytes ...) (map (λ (word-num) (low-byte (parse-syntax-number word-num)))
                                       (syntax->list #'(word ...))))
                   ((high-bytes ...) (map (λ (word-num) (high-byte (parse-syntax-number word-num)))
                                        (syntax->list #'(word ...)))))
       #'`(byte-value ,@(map syntax->datum
                             (flatten (map list (syntax->list #'(low-bytes ...))
                                           (syntax->list #'(high-bytes ...))))))))))

(module+ test #| word |#
  (check-equal? (word $1000 $FFD2 %1001000011111111)
                '(byte-value #x00 #x10 #xD2 #xFF #b11111111 #b10010000))
  (check-exn exn:fail:syntax? (λ () (expand #'(word $1000 $10000)))))

(define (c64-char->byte char)
  (char->integer char))

(define-syntax (asc stx)
  (syntax-case stx ()
    ([_ str]
     (string? (syntax->datum #'str))
     #'`(byte-value ,@(map c64-char->byte (string->list (->string #'str)))))))

(module+ test #| asc |#
  (check-equal? (asc "some")
                '(byte-value 115 111 109 101)))

;; (define (opcode . args) (append (list 'opcode) args))



;; --------------------------------------------------------------------------------
;; additional syntax (ideas)
;;
;; (require-byte some other more)               -> (resolve-required-byte "some" "other" "more")
;; (require-word just-one)                      -> (resolve-required-word "just-one")
;; (provide-byte (as label-high >local-label)   -> (resolve-provided-byte "label-high" (resolve-byte ">some"))
;;               (as label-low <local-label)
;;               a-byte-const)
;; (provide-word local-label)
;; (origin $C000)

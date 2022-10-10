#lang racket

(require "6510-alt-utils.rkt")
;; (require (for-syntax "6510-alt-utils.rkt"))
(require (for-syntax "6510-syntax-utils.rkt"))
(require (for-syntax "6510-alt-addressing.rkt"))

(require (for-syntax "6510-utils.rkt"))

(provide ASL BEQ BRK JMP NOP SBC STX)
(provide (all-from-out "6510-alt-utils.rkt"))

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
                '(rel-opcode #xf0 #x10))
  (check-equal? (BEQ some)
                '(rel-opcode #xf0 (resolve-relative "some"))))

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
  (check-equal? (JMP some)
                '(opcode #x4c (resolve-word "some")))
  (check-equal? (JMP ($FFD2))
                '(opcode #x6c #xd2 #xff))
  (check-equal? (JMP (some))
                '(opcode #x6c (resolve-word "some"))))

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
  (check-equal? (SBC !>some)
                '(opcode #xe9 (resolve-byte ">some")))
  (check-equal? (SBC !some)
                '(opcode #xe9 (resolve-byte "some"))
                "only option is to resolve to byte")
  (check-equal? (SBC $10)
                '(opcode #xe5 #x10))
  (check-equal? (SBC >some)
                '(opcode #xe5 (resolve-byte ">some")))
  (check-equal? (SBC <some)
                '(opcode #xe5 (resolve-byte "<some")))
  (check-equal? (SBC $10,x)
                '(opcode #xf5 #x10))
  (check-equal? (SBC <some,x)
                '(opcode #xf5 (resolve-byte "<some")))
  (check-equal? (SBC $FF10)
                '(opcode #xed #x10 #xff))
  (check-equal? (SBC some)
                '(decide (((resolve-byte "some") . (opcode #xe5))
                          ((resolve-word "some") . (opcode #xed))))
                "two options resolve to byte (zero page) or word (absolute)")
  (check-equal? (SBC $1112,x)
                '(opcode #xfd #x12 #x11))
  (check-equal? (SBC $1000,y)
                '(opcode #xf9 #x00 #x10))
  (check-equal? (SBC some,x)
                '(decide (((resolve-byte "some") . (opcode #xf5))
                          ((resolve-word "some") . (opcode #xfd))))
                "two options resolve to byte (zero page-x) or word (absolute-x)")
  (check-equal? (SBC some,y)
                '(opcode #xf9 (resolve-word "some"))
                "only option is to resolve to byte (zero-page-y)")
  (check-equal? (SBC ($11,x))
                '(opcode #xe1 #x11))
  (check-equal? (SBC (some,x))
                '(opcode #xe1 (resolve-byte "some"))
                "only option is to resolve to byte")
  (check-equal? (SBC (<some,x))
                '(opcode #xe1 (resolve-byte "<some")))
  (check-equal? (SBC ($11),y)
                '(opcode #xf1 #x11))
  (check-equal? (SBC (>some),y)
                '(opcode #xf1 (resolve-byte ">some")))  
  (check-equal? (SBC (some),y)
                '(opcode #xf1 (resolve-byte "some"))
                "only option is to resolve to byte"))

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

(require "6510-utils.rkt")

(define-syntax (label stx)
  (syntax-case stx ()
    ([_ str]
     #'`(label-def ,(->string (syntax->datum #'str))))))

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
     (and (6510-number-string? (->string (syntax->datum #'byte)))
        (in-byte-range? (parse-number-string (->string (syntax->datum #'byte)))))
     #'`(byte-const-def
         ,(->string (syntax->datum #'label))
         ,(parse-number-string (->string (syntax->datum #'byte)))))))

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
     (and (6510-number-string? (->string (syntax->datum #'word)))
        (in-word-range? (parse-number-string (->string (syntax->datum #'word)))))
     #'`(word-const-def
         ,(->string (syntax->datum #'label))
         ,(parse-number-string (->string (syntax->datum #'word)))))))

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
                           (let ((str-val (->string (syntax->datum val))))
                             (and (6510-number-string? str-val)
                                (in-byte-range? (parse-number-string str-val)))))
                         (syntax->list #'(byte ...)))))
       (all #'(is-byte-number ...)))
     #'`(byte-value ,(parse-number-string (->string (syntax->datum #'byte))) ...))))

(module+ test #| byte |#
  (check-equal? (byte $10 $FF $D2 %10010000 %11111111)
                '(byte-value #x10 #xFF #xD2 #b10010000 #b11111111))
  (check-exn exn:fail:syntax? (λ () (expand #'(byte $10 $100)))))

(define-for-syntax (parse-syntax-number val-stx)
  (parse-number-string (->string (syntax->datum val-stx))))

(define-for-syntax (all stx-list)
  (foldl (λ (l r) (and (syntax->datum l) r))
         #t
         (syntax->list stx-list)))

(define-syntax (word stx)
  (syntax-case stx ()
    ([_ word ...]
     (with-syntax (((is-word-number ...)
                    (map (λ (val)                           
                           (and (6510-number-string? (->string (syntax->datum val)))
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

;; --------------------------------------------------------------------------------
;; additional syntax (ideas)
;;
;; (asc "some")                                 -> (byte-values 115 111 109 101)
;; (require-byte some other more)               -> (resolve-required-byte "some" "other" "more")
;; (require-word just-one)                      -> (resolve-required-word "just-one")
;; (provide-byte (as label-high >local-label)   -> (resolve-provided-byte "label-high" (resolve-byte ">some"))
;;               (as label-low <local-label)
;;               a-byte-const)
;; (provide-word local-label)
;; (origin $C000)

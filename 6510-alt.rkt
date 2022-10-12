#lang racket

(require "6510-alt-utils.rkt")
(require (for-syntax "6510-syntax-utils.rkt"))
(require (for-syntax "6510-alt-addressing.rkt"))

(require "6510-utils.rkt")
(require (for-syntax "6510-utils.rkt"))

(provide ADC AND ASL
         BCC BCS BEQ BIT BMI BNE BPL BRK BVC BVS
         CLC CLD CLI CLV CMP CPX CPY
         DEC DEX DEY
         EOR
         INC INX INY
         JMP JSR
         LDA LDX LDY LSR
         NOP
         ORA
         PHA PHP PLA PLP 
         ROL ROR RTI RTS
         SBC SEC SED SEI STA STX STY
         TAX TAY TSX TXA TXS TYA) 
(provide label word-const byte-const byte word asc
         ;; opcode
         ) ;; meta commands

(provide (all-from-out "6510-alt-utils.rkt"))

(module+ test
  (require "6510-test-utils.rkt"))

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

(define-opcode ADC
  ((immediate  . #x69)
   (zero-page  . #x65)
   (zero-page-x . #x75)
   (absolute    . #x6d)
   (absolute-x  . #x7d)
   (absolute-y  . #x79)
   (indirect-x  . #x61)
   (indirect-y  . #x71)))

(define-opcode AND
  ((indirect-x   . #x21)
    (zero-page   . #x25)
    (immediate   . #x29)
    (absolute    . #x2d)
    (indirect-y  . #x31)
    (zero-page-x . #x35)
    (absolute-y  . #x39)
    (absolute-x  . #x3d)))

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

(define-opcode BCC ((relative . #x90)))
(define-opcode BCS ((relative . #xb0)))
(define-opcode BEQ ((relative . #xf0)))

(module+ test #| BEQ |#
  (check-equal? (BEQ $10)
                '(rel-opcode #xf0 #x10))
  (check-equal? (BEQ some)
                '(rel-opcode #xf0 (resolve-relative "some"))))

(define-opcode BIT
  ((zero-page . #x24)
   (absolute . #x2c)))

(module+ test
  (check-match (BEQ "$FC")
               '(rel-opcode #xF0 #xfc))
  (check-match (BEQ "some")
               '(rel-opcode #xF0 (resolve-relative "some"))))

(define-opcode BMI ((relative . #x30)))

(define-opcode BNE ((relative . #xd0)))
(define-opcode BPL ((relative  . #x10)))

(define-opcode BRK ((implicit . #x00)))

(module+ test #| BRK |#
  (check-equal?  (BRK)
                 '(opcode #x00)))

(define-opcode BVC ((relative . #x50)))
(define-opcode BVS ((relative . #x70)))

(define-opcode CLC ((implicit . #x18)))
(define-opcode CLD ((implicit . #xd8)))
(define-opcode CLI ((implicit . #x58)))
(define-opcode CLV ((implicit . #xb8)))

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

(define-opcode DEC
  ((zero-page . #xc6) (absolute . #xce) (absolute-x . #xde)  (zero-page-x . #xd6)))

(define-opcode DEX ((implicit . #xCA)))

(module+ test #| DEX |#
  (check-equal? (DEX)
                '(opcode #xca)))

(define-opcode DEY ((implicit . #x88)))

(define-opcode EOR
  ((indirect-x  . #x41)
   (zero-page   . #x45)
   (immediate   . #x49)
   (absolute    . #x4d)
   (indirect-y  . #x51)
   (zero-page-x . #x55)
   (absolute-y  . #x59)
   (absolute-x  . #x5d)))

(define-opcode INC
  ((zero-page . #xe6) (absolute . #xee) (absolute-x . #xfe) (zero-page-x . #xf6)))

(module+ test
  (check-match (INC "$10")
               '(opcode #xE6 #x10))

  (check-match (INC "$10",x)
               '(opcode #xF6 #x10))

  (check-match (INC "$1000")
               '(opcode #xEE #x00 #x10))

  (check-match (INC "$1000",x)
               '(opcode #xFE #x00 #x10)))

(define-opcode INX ((implicit . #xe8)))

(define-opcode INY ((implicit . #xc8)))


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
                '(opcode #xBE #x20 #x10)))

(define-opcode LDY
  ((immediate   . #xA0)
   (zero-page   . #xA4)
   (zero-page-x . #xB4)
   (absolute    . #xAC)
   (absolute-x  . #xBC)))

(define-opcode LSR
  ((zero-page   . #x46)
   (implicit    . #x4a)
   (absolute    . #x4e)
   (zero-page-x . #x56)
   (absolute-x  . #x5e)))

(define (NOP)
  '(opcode #xea))

(define-opcode ORA
  ((indirect-x  . #x01)
   (zero-page   . #x05)
   (immediate   . #x09)
   (absolute    . #x0d)
   (indirect-y  . #x11)
   (zero-page-x . #x15)
   (absolute-y  . #x19)
   (absolute-x  . #x1d)))

(module+ test #| ora |#
  (check-match (ORA ("$10",x))
               '(opcode #x01 #x10))
  (check-match (ORA "$10")
               '(opcode #x05 #x10))
  (check-match (ORA "!$10")
               '(opcode #x09 #x10))
  (check-match (ORA "$1011")
               '(opcode #x0d #x11 #x10))
  (check-match (ORA ("$10"),y)
               '(opcode #x11 #x10))
  (check-match (ORA "$10",x)
               '(opcode #x15 #x10))
  (check-match (ORA "$1011",y)
               '(opcode #x19 #x11 #x10))
  (check-match (ORA "$1011",x)
               '(opcode #x1d #x11 #x10)))

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

(define-opcode SEC ((implicit . #x38)))

(define-opcode SED ((implicit . #xf8)))

(define-opcode SEI ((implicit . #x78)))

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

(define-opcode TAX ((implicit . #xaa)))

(define-opcode TAY ((implicit . #xa8)))

(define-opcode TSX ((implicit . #xba)))

(define-opcode TXA ((implicit . #x8a)))

(define-opcode TXS ((implicit . #x9a)))

(define-opcode TYA ((implicit . #x98)))


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

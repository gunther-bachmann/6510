#lang racket

(require (for-syntax "6510-syntax-utils.rkt"))
(require (for-syntax "6510-utils.rkt"))

(module+ test
  (require rackunit)
  (begin-for-syntax
    (require rackunit)))


;;--------------------------------------------------------------------------------
(define-for-syntax (make-id stx id-template . ids)
  (let ([str (apply format id-template (map syntax->datum ids))])
    (datum->syntax stx (string->symbol str))))

;; https://docs.racket-lang.org/reference/syntax-util.html
;; (format-id ...)

;; https://blog.racket-lang.org/2011/04/writing-syntax-case-macros.html

;;--------------------------------------------------------------------------------
(define (NOP)
  '('opcode #xea))

(require (for-syntax racket))

(define-for-syntax (byte-operand? num-str)
  (or (and (symbol? num-str)
        (byte-operand? (symbol->string num-str)))
     (and (string? num-str)
        (eq? 3 (string-length num-str))
        (string-prefix? num-str "$"))))

(define-for-syntax (byte-operand num-str)
  (if (symbol? num-str)
      (byte-operand (symbol->string num-str))
      (string->number (substring num-str 1) 16)))

(define-for-syntax (word-operand? num-str)
  (or (and (symbol? num-str)
        (word-operand? (symbol->string num-str)))
     (and (string? num-str)
        (eq? 5 (string-length num-str))
        (string-prefix? num-str "$"))))

(define-for-syntax (word-operand num-str)
  (if (symbol? num-str)
      (word-operand (symbol->string num-str))
      (string->number (substring num-str 1) 16)))

(define-for-syntax (immediate-byte-operand? sym)
  (or (and (symbol? sym)
        (immediate-byte-operand (symbol->string sym)))
     (and (string? sym)
        (string-prefix? sym "!")
        (byte-operand? (substring sym 1)))))

(define-for-syntax (immediate-byte-operand sym)
  (if (symbol? sym)
        (immediate-byte-operand (symbol->string sym))
        (byte-operand (substring sym 1))))

(define-for-syntax (indirect-x-operand? sym)
  (and (list? sym)
     (byte-operand? (car sym))
     (equal? (cadr sym) ',x)))

(define-for-syntax (indirect-x-operand sym)  
  (byte-operand (car sym)))

(define-for-syntax (indirect-y-operand? sym)
  (and (list? sym)
     (byte-operand? (car sym))))

(define-for-syntax (indirect-y-operand sym)  
  (byte-operand (car sym)))

(begin-for-syntax
  (define (create-mnemonic-for-accumulator-pair-f2 pair)
    (with-syntax ((lpair pair))
      #'(create-mnemonic-for-accumulator-pair lpair))
    ;; (datum->syntax stx #'(create-mnemonic-for-accumulator-pair lpair))
    ))
(begin-for-syntax
  (define (create-mnemonic-for-accumulator-pair-f stx pair)
    (with-syntax ((lpair (datum->syntax stx (syntax->datum pair))))
      ;; #'(create-mnemonic-for-accumulator-pair lpair)
      (datum->syntax stx #'(create-mnemonic-for-accumulator-pair lpair))
      )))

;; (map (lambda (pair) (with-syntax ((mn (car pair))
;;                                (op (cdr pair)))
;;                    #'(create-mnemonic-for-accumulator mn op)))
;;        '((LSX . #x11)
;;          (LSY . #x12)
;;          (LSZ . #x13)))

;; (expand-once #'(create-mnemonic-for-accumulator-pair (LSR . #x4a )))
;; (expand-once (expand-once (expand-once #'(cm4a4 ((LSX . #x11))))))
;; (expand-once (expand-once (expand-once #'(cm4a5 ((LSX . #x11))))))


;; (expand-once (expand-once (expand-once #'(cm4a5 ((LSX . #x11))))))
;; (cm4a5 ((LSX . #x11)))
;; (def-addressing-mode-accumulator ((LSR . #x4a))) ;; (LSR A)
;; (def-addressing-mode-immediate   ((ADC . #x77))) ;; (ADC #$25) (ADC #18) (ADC #%01001) (ADC #:<LABEL) (ADC #:>LABEL) (ADC #:LABEL)


;; example of working on lists while generating ids that are in scope ----------------------------------------
(define-syntax (create-mnemonic-for-accumulator-pair stx)
  (syntax-case stx ()
    ([_ pair ]
     (with-syntax ((mnemonic (datum->syntax stx (car (syntax->datum #'pair))))
                   (bytecode (datum->syntax stx (cdr (syntax->datum #'pair))))
                   (nstx (make-id stx "~a" #'nstx)))
       #'(define-syntax (mnemonic nstx)
         (syntax-case nstx ()
           ([_ op]
            (eq? (syntax->datum #'op) 'A) ;; will show error in editor when not eq
            #''('opcode bytecode))))))))

(define-syntax (cm4a4 stx)
  (syntax-case stx  ()
    [(_ (pair ...))
     (with-syntax ([(transformed-pair ...)
                    ;; (map (lambda (lpair) (create-mnemonic-for-accumulator-pair-f stx (datum->syntax #'stx lpair))) (syntax->list #'(pair ...)))
                    ;; (map create-mnemonic-for-accumulator-pair-f2 (syntax->list #'(pair ...)))
                    (map (lambda (lpair) `(create-mnemonic-for-accumulator-pair ,(datum->syntax stx lpair))) (syntax->list #'(pair ...)))
                    ])
;;       (datum->syntax stx #'(begin transformed-pair ...))
       (datum->syntax stx (syntax->datum #'(begin transformed-pair ...))))]))

(cm4a4 ((LSR . #x4a)
        (LSX . #x11)
        (LSY . #x12)
        (TSA . #x13)))

(module+ test #| cm4a4 |#
  (check-equal? (LSX A)
                '('opcode #x11))
  (check-equal? (LSR A)
                '('opcode #x4a))
  (check-equal? (LSY A)
                '('opcode #x12))
  (check-equal? (TSA A)
                '('opcode #x13)))

(define-for-syntax (addressing-mode sym addressing-modes)
  (findf (lambda (el) (eq? (car el) sym)) (syntax->datum addressing-modes)))

(define-syntax (define-opcode stx)
  (syntax-case stx ()
    ([_ mnemonic addressing-modes]
     (with-syntax ((scoped-mnemonic (datum->syntax stx (syntax->datum #'mnemonic)))
                   (accumulator-addressing ;; (findf (lambda (el) (eq? (car el) 'accumulator)) (syntax->datum #'addressing-modes))
                    (addressing-mode 'accumulator #'addressing-modes)
                                           )
                   (zero-page-addressing (findf (lambda (el) (eq? (car el) 'zero-page)) (syntax->datum #'addressing-modes)))
                   (relative-addressing (findf (lambda (el) (eq? (car el) 'relative)) (syntax->datum #'addressing-modes)))
                   (absolute-addressing (findf (lambda (el) (eq? (car el) 'absolute)) (syntax->datum #'addressing-modes)))
                   (absolute-x-addressing (findf (lambda (el) (eq? (car el) 'absolute-x)) (syntax->datum #'addressing-modes)))
                   (absolute-y-addressing (findf (lambda (el) (eq? (car el) 'absolute-y)) (syntax->datum #'addressing-modes)))
                   (zero-page-x-addressing (findf (lambda (el) (eq? (car el) 'zero-page-x)) (syntax->datum #'addressing-modes)))
                   (zero-page-y-addressing (findf (lambda (el) (eq? (car el) 'zero-page-y)) (syntax->datum #'addressing-modes)))
                   (indirect-x-addressing (findf (lambda (el) (eq? (car el) 'indirect-x)) (syntax->datum #'addressing-modes)))
                   (indirect-y-addressing (findf (lambda (el) (eq? (car el) 'indirect-y)) (syntax->datum #'addressing-modes)))
                   (immediate-addressing (findf (lambda (el) (eq? (car el) 'immediate)) (syntax->datum #'addressing-modes)))
                   (implicit-addressing (findf (lambda (el) (eq? (car el) 'implicit)) (syntax->datum #'addressing-modes)))
                   (nstx (make-id stx "~a" #'nstx)))
       (with-syntax ((acc-adr-p (datum->syntax stx (if (syntax->datum #'accumulator-addressing) #t #f)))
                     (acc-adr-opcode (datum->syntax stx (cdr (or (syntax->datum #'accumulator-addressing) '(0.0)))))
                     (zp-adr-p (datum->syntax stx (if (syntax->datum #'zero-page-addressing) #t #f)))
                     (zp-adr-opcode (datum->syntax stx (cdr (or (syntax->datum #'zero-page-addressing) '(0.0)))))
                     (rel-adr-p (datum->syntax stx (if (syntax->datum #'relative-addressing) #t #f)))
                     (rel-adr-opcode (datum->syntax stx (cdr (or (syntax->datum #'relative-addressing) '(0.0)))))
                     (abs-adr-p (datum->syntax stx (if (syntax->datum #'absolute-addressing) #t #f)))
                     (abs-adr-opcode (datum->syntax stx (cdr (or (syntax->datum #'absolute-addressing) '(0.0)))))
                     (absx-adr-p (datum->syntax stx (if (syntax->datum #'absolute-x-addressing) #t #f)))
                     (absx-adr-opcode (datum->syntax stx (cdr (or (syntax->datum #'absolute-x-addressing) '(0.0)))))
                     (absy-adr-p (datum->syntax stx (if (syntax->datum #'absolute-y-addressing) #t #f)))
                     (absy-adr-opcode (datum->syntax stx (cdr (or (syntax->datum #'absolute-y-addressing) '(0.0)))))
                     (zpx-adr-p (datum->syntax stx (if (syntax->datum #'zero-page-x-addressing) #t #f)))
                     (zpx-adr-opcode (datum->syntax stx (cdr (or (syntax->datum #'zero-page-x-addressing) '(0.0)))))
                     (zpy-adr-p (datum->syntax stx (if (syntax->datum #'zero-page-y-addressing) #t #f)))
                     (zpy-adr-opcode (datum->syntax stx (cdr (or (syntax->datum #'zero-page-y-addressing) '(0.0)))))
                     (indx-adr-p (datum->syntax stx (if (syntax->datum #'indirect-x-addressing) #t #f)))
                     (indx-adr-opcode (datum->syntax stx (cdr (or (syntax->datum #'indirect-x-addressing) '(0.0)))))
                     (indy-adr-p (datum->syntax stx (if (syntax->datum #'indirect-y-addressing) #t #f)))
                     (indy-adr-opcode (datum->syntax stx (cdr (or (syntax->datum #'indirect-y-addressing) '(0.0)))))
                     (imm-adr-p (datum->syntax stx (if (syntax->datum #'immediate-addressing) #t #f)))
                     (imm-adr-opcode (datum->syntax stx (cdr (or (syntax->datum #'immediate-addressing) '(0.0)))))
                     (impl-adr-p (datum->syntax stx (if (syntax->datum #'implicit-addressing) #t #f)))
                     (impl-adr-opcode (datum->syntax stx (cdr (or (syntax->datum #'implicit-addressing) '(0.0))))))
         #'(define-syntax (scoped-mnemonic nstx)
             (syntax-case nstx ()
               ([_]
                (cond [(and impl-adr-p)
                       #''('opcode impl-adr-opcode)]
                      [#t  (raise-syntax-error
                           'mnemonic
                           ;; report available addressing modes expected for one op
                           (format "implicit addressing mode is not defined for this opcode. line ~a:~a"
                                   (syntax-line nstx)
                                   (syntax-column nstx))
                           nstx)]))
               ([_ op]
                (cond [(and acc-adr-p
                          (eq? (syntax->datum #'op) 'A))
                       #''('opcode acc-adr-opcode)]
                      [(and zp-adr-p
                          (byte-operand? (syntax->datum #'op)))
                       (with-syntax ((op-num (byte-operand (syntax->datum #'op))))
                         #''('opcode zp-adr-opcode op-num))]
                      [(and rel-adr-p
                          (byte-operand? (syntax->datum #'op)))
                       (with-syntax ((op-num (byte-operand (syntax->datum #'op))))
                         #''('rel-opcode rel-adr-opcode op-num))]                      
                      [(and abs-adr-p
                          (word-operand? (syntax->datum #'op)))
                       (with-syntax ((op-low (low-byte (word-operand (syntax->datum #'op))))
                                     (op-high (high-byte (word-operand (syntax->datum #'op)))))
                         #''('opcode abs-adr-opcode op-low op-high))]
                      [(and imm-adr-p
                          (immediate-byte-operand? (syntax->datum #'op)))
                       (with-syntax ((op-num (immediate-byte-operand (syntax->datum #'op))))
                         #''('opcode imm-adr-opcode op-num))]
                      [(and indx-adr-p
                          (indirect-x-operand? (syntax->datum #'op)))
                       (with-syntax ((op-num (indirect-x-operand (syntax->datum #'op))))
                         #''('opcode indx-adr-opcode op-num))]
                      [#t (raise-syntax-error
                           'mnemonic
                           ;; report available addressing modes expected for one op
                           (format "addressing mode with one op not defined. line ~a:~a"
                                   (syntax-line nstx)
                                   (syntax-column nstx))
                           nstx)]))
               ([_ op1 op2]
                (cond [(and zpx-adr-p
                          (byte-operand? (syntax->datum #'op1))
                          (equal? (syntax->datum #'op2) '(unquote x)))
                       (with-syntax ((op-num (byte-operand (syntax->datum #'op1))))
                         #''('opcode zpx-adr-opcode op-num))]
                      [(and zpy-adr-p
                          (byte-operand? (syntax->datum #'op1))
                          (equal? (syntax->datum #'op2) '(unquote y)))
                       (with-syntax ((op-num (byte-operand (syntax->datum #'op1))))
                         #''('opcode zpy-adr-opcode op-num))]
                      [(and absx-adr-p
                          (word-operand? (syntax->datum #'op1))
                          (equal? (syntax->datum #'op2) '(unquote x)))
                       (with-syntax ((op-low (low-byte (word-operand (syntax->datum #'op1))))
                                     (op-high (high-byte (word-operand (syntax->datum #'op1)))))
                         #''('opcode absx-adr-opcode op-low op-high))]
                     [(and absy-adr-p
                          (word-operand? (syntax->datum #'op1))
                          (equal? (syntax->datum #'op2) '(unquote y)))
                       (with-syntax ((op-low (low-byte (word-operand (syntax->datum #'op1))))
                                     (op-high (high-byte (word-operand (syntax->datum #'op1)))))
                         #''('opcode absy-adr-opcode op-low op-high))]
                     [(and indy-adr-p
                         (indirect-y-operand? (syntax->datum #'op1))
                         (equal? (syntax->datum #'op2) '(unquote y)))
                      (with-syntax ((op-num (indirect-y-operand (syntax->datum #'op1))))
                         #''('opcode indy-adr-opcode op-num))]
                     [#t (raise-syntax-error
                          'mnemonic
                          ;; report available addressing modes expected for one op
                          (format "addressing mode with two op not defined. line ~a:~a"
                                  (syntax-line nstx)
                                  (syntax-column nstx))
                          nstx)])))))))))

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
                '('opcode #xe9 #x11))
  (check-equal? (SBC $10)
                '('opcode #xe5 #x10))
  (check-equal? (SBC $10,x)
                '('opcode #xf5 #x10))
  (check-equal? (SBC $FF10)
                '('opcode #xed #x10 #xff))
  (check-equal? (SBC $1112,x)
                '('opcode #xfd #x12 #x11))
  (check-equal? (SBC $1000,y)
                '('opcode #xf9 #x00 #x10))
  (check-equal? (SBC ($11,x))
                '('opcode #xe1 #x11))
  (check-equal? (SBC ($11),y)
                '('opcode #xf1 #x11)))

(define-opcode ASL
  ((accumulator . #x0a)
   (zero-page   . #x06)
   (zero-page-x . #x16)
   (absolute    . #x0e)
   (absolute-x  . #x1e)))

(module+ test #| ASL |#
  (check-equal? (ASL A)
                '('opcode #x0a))
  (check-equal? (ASL $10)
                '('opcode #x06 #x10))
  (check-equal? (ASL $10,x)
                '('opcode #x16 #x10))
  (check-equal? (ASL $1000)
                '('opcode #x0e #x00 #x10))
  (check-equal? (ASL $1000,x)
                '('opcode #x1e #x00 #x10)))

(define-opcode BRK ((implicit . #x00)))

(module+ test #| BRK |#
  (check-equal?  (BRK)
                 '('opcode #x00)))

(define-opcode BEQ ((relative . #xf0)))

(module+ test #| BEQ |#
  (check-equal? (BEQ $10)
                '('rel-opcode #xf0 #x10)))

;; idea: delayed decision about byte/word operand

;; '('decision-byte-word ":label" ('opcode #x00 #x00) ('opcode #x01 #x00 #x00)) ;; if :label is byte value use first opcode, else use second
;; is transformed to
;; '('opcode #x00 #x00) ;; in case label is a byte value
;; '('opcode #x01 #x00 #x00) ;; in case label is a word value



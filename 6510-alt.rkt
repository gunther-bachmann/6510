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
  '(opcode #xea))

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
            #''(opcode bytecode))))))))

(define-syntax (cm4a4 stx)
  (syntax-case stx  ()
    [(_ (pair ...))
     (with-syntax ([(transformed-pair ...)
                    ;; (map (lambda (lpair) (create-mnemonic-for-accumulator-pair-f stx (datum->syntax #'stx lpair))) (syntax->list #'(pair ...)))
                    ;; (map create-mnemonic-for-accumulator-pair-f2 (syntax->list #'(pair ...)))
                    (map (lambda (lpair) `(create-mnemonic-for-accumulator-pair ,(datum->syntax stx lpair))) (syntax->list #'(pair ...)))
                    ])
       (datum->syntax stx (syntax->datum #'(begin transformed-pair ...))))]))

(cm4a4 ((MNEMONIC1 . #x4a)
        (MNEMONIC2 . #x11)
        (MNEMONIC3 . #x12)
        (MNEMONIC4 . #x13)))

(module+ test #| cm4a4 |#
  (check-equal? (MNEMONIC1 A)
                '(opcode #x4a))
  (check-equal? (MNEMONIC2 A)
                '(opcode #x11))
  (check-equal? (MNEMONIC3 A)
                '(opcode #x12))
  (check-equal? (MNEMONIC4 A)
                '(opcode #x13)))

(define-for-syntax (addressing-mode sym addressing-modes)
  (findf (lambda (el) (eq? (car el) sym)) (syntax->datum addressing-modes)))

(define (addressing-opcode addressing-mode)
  (cdr (or (syntax->datum addressing-mode) '(0.0))))

;; add line information to opcode definition
;; enable labels instead of values, leaving byte/word length open

(define-syntax (define-opcode stx)
  (syntax-case stx ()
    ([_ mnemonic addressing-modes]
     (with-syntax ((scoped-mnemonic (datum->syntax stx (syntax->datum #'mnemonic)))
                   (accumulator-addressing (addressing-mode 'accumulator #'addressing-modes))
                   (zero-page-addressing (addressing-mode 'zero-page #'addressing-modes))
                   (relative-addressing (addressing-mode 'relative #'addressing-modes))
                   (absolute-addressing (addressing-mode 'absolute #'addressing-modes))
                   (absolute-x-addressing (addressing-mode 'absolute-x #'addressing-modes))
                   (absolute-y-addressing (addressing-mode 'absolute-y #'addressing-modes))
                   (zero-page-x-addressing (addressing-mode 'zero-page-x #'addressing-modes))
                   (zero-page-y-addressing (addressing-mode 'zero-page-y #'addressing-modes))
                   (indirect-x-addressing (addressing-mode 'indirect-x #'addressing-modes))
                   (indirect-y-addressing (addressing-mode 'indirect-y #'addressing-modes))
                   (immediate-addressing (addressing-mode 'immediate #'addressing-modes))
                   (implicit-addressing (addressing-mode 'implicit #'addressing-modes))
                   (nstx (make-id stx "~a" #'nstx)))
       #`(define-syntax (scoped-mnemonic nstx)
           (syntax-case nstx ()
             ([_]
              (cond [(pair? (syntax->datum #'implicit-addressing)) 
                     #'`(opcode ,(addressing-opcode #'implicit-addressing))]
                    [#t  (raise-syntax-error
                          'mnemonic
                          ;; report available addressing modes expected for one op
                          (format "implicit addressing mode is not defined for this opcode. line ~a:~a"
                                  (syntax-line nstx)
                                  (syntax-column nstx))
                          nstx)]))
             ([_ op]
              (cond [(and (pair? (syntax->datum #'accumulator-addressing))
                        (eq? (syntax->datum #'op) 'A))
                     #'`(opcode ,(addressing-opcode #'accumulator-addressing))]
                    [(and (pair? (syntax->datum #'zero-page-addressing))
                        (byte-operand? (syntax->datum #'op)))
                     (with-syntax ((op-num (byte-operand (syntax->datum #'op))))
                       #'`(opcode ,(addressing-opcode #'zero-page-addressing)  op-num))]
                    [(and (pair? (syntax->datum #'relative-addressing))
                        (byte-operand? (syntax->datum #'op)))
                     (with-syntax ((op-num (byte-operand (syntax->datum #'op))))
                       #'`(rel-opcode ,(addressing-opcode #'relative-addressing) op-num))]                      
                    [(and (pair? (syntax->datum #'absolute-addressing)) 
                        (word-operand? (syntax->datum #'op)))
                     (with-syntax ((op-low (low-byte (word-operand (syntax->datum #'op))))
                                   (op-high (high-byte (word-operand (syntax->datum #'op)))))
                       #'`(opcode ,(addressing-opcode #'absolute-addressing) op-low op-high))]
                    [(and (pair? (syntax->datum #'immediate-addressing))
                        (immediate-byte-operand? (syntax->datum #'op)))
                     (with-syntax ((op-num (immediate-byte-operand (syntax->datum #'op))))
                       #'`(opcode ,(addressing-opcode #'immediate-addressing) op-num))]
                    [(and (pair? (syntax->datum #'indirect-x-addressing)) 
                        (indirect-x-operand? (syntax->datum #'op)))
                     (with-syntax ((op-num (indirect-x-operand (syntax->datum #'op))))
                       #'`(opcode ,(addressing-opcode #'indirect-x-addressing) op-num))]
                    [#t (raise-syntax-error
                         'mnemonic
                         ;; report available addressing modes expected for one op
                         (format "addressing mode with one op not defined. line ~a:~a"
                                 (syntax-line nstx)
                                 (syntax-column nstx))
                         nstx)]))
             ([_ op1 op2]
              (cond [(and (pair? (syntax->datum #'zero-page-x-addressing))
                        (byte-operand? (syntax->datum #'op1))
                        (equal? (syntax->datum #'op2) '(unquote x)))
                     (with-syntax ((op-num (byte-operand (syntax->datum #'op1))))
                       #'`(opcode ,(addressing-opcode #'zero-page-x-addressing) op-num))]
                    [(and (pair? (syntax->datum #'zero-page-y-addressing))
                        (byte-operand? (syntax->datum #'op1))
                        (equal? (syntax->datum #'op2) '(unquote y)))
                     (with-syntax ((op-num (byte-operand (syntax->datum #'op1))))
                       #'`(opcode ,(addressing-opcode #'zero-page-y-addressing) op-num))]
                    [(and (pair? (syntax->datum #'absolute-x-addressing))
                        (word-operand? (syntax->datum #'op1))
                        (equal? (syntax->datum #'op2) '(unquote x)))
                     (with-syntax ((op-low (low-byte (word-operand (syntax->datum #'op1))))
                                   (op-high (high-byte (word-operand (syntax->datum #'op1)))))
                       #'`(opcode ,(addressing-opcode #'absolute-x-addressing) op-low op-high))]
                    [(and (pair? (syntax->datum #'absolute-y-addressing))
                        (word-operand? (syntax->datum #'op1))
                        (equal? (syntax->datum #'op2) '(unquote y)))
                     (with-syntax ((op-low (low-byte (word-operand (syntax->datum #'op1))))
                                   (op-high (high-byte (word-operand (syntax->datum #'op1)))))
                       #'`(opcode ,(addressing-opcode #'absolute-y-addressing) op-low op-high))]
                    [(and (pair? (syntax->datum #'indirect-y-addressing))
                        (indirect-y-operand? (syntax->datum #'op1))
                        (equal? (syntax->datum #'op2) '(unquote y)))
                     (with-syntax ((op-num (indirect-y-operand (syntax->datum #'op1))))
                       #'`(opcode ,(addressing-opcode #'indirect-y-addressing) op-num))]
                    [#t (raise-syntax-error
                         'mnemonic
                         ;; report available addressing modes expected for one op
                         (format "addressing mode with two op not defined. line ~a:~a"
                                 (syntax-line nstx)
                                 (syntax-column nstx))
                         nstx)]))))))))

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
  ;; (check-equal? (SBC some)
  ;;               '(decide (word . (opcode #xed "some:2"))
  ;;                        (byte . (opcode #xe5 "some:1"))))
  (check-equal? (SBC !$11)
                '(opcode #xe9 #x11))
  (check-equal? (SBC $10)
                '(opcode #xe5 #x10))
  (check-equal? (SBC $10,x)
                '(opcode #xf5 #x10))
  (check-equal? (SBC $FF10)
                '(opcode #xed #x10 #xff))
  (check-equal? (SBC $1112,x)
                '(opcode #xfd #x12 #x11))
  (check-equal? (SBC $1000,y)
                '(opcode #xf9 #x00 #x10))
  (check-equal? (SBC ($11,x))
                '(opcode #xe1 #x11))
  (check-equal? (SBC ($11),y)
                '(opcode #xf1 #x11)))

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

(define-opcode BRK ((implicit . #x00)))

(module+ test #| BRK |#
  (check-equal?  (BRK)
                 '(opcode #x00)))

(define-opcode BEQ ((relative . #xf0)))

(module+ test #| BEQ |#
  (check-equal? (BEQ $10)
                '(rel-opcode #xf0 #x10)))

;; idea: delayed decision about byte/word operand

;; '('decision-byte-word ":label" (opcode #x00 #x00) (opcode #x01 #x00 #x00)) ;; if :label is byte value use first opcode, else use second
;; is transformed to
;; '(opcode #x00 #x00) ;; in case label is a byte value
;; '(opcode #x01 #x00 #x00) ;; in case label is a word value



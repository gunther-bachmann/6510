#lang racket

(require (for-syntax "6510-syntax-utils.rkt"))

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


(define-syntax (define-opcode-function stx)
  (syntax-case stx ()
    [(_ mnemonic addressing-suffix code)
     (with-syntax ([function-name (make-id stx (format "~~a_~a" (syntax->datum #'addressing-suffix)) #'mnemonic)])
       #'(define (function-name) code))]))

(define-opcode-function ASL "acc" '('opcode #x0a))


;; (define-syntax (def-acc stx)
;;   (syntax-case stx ()
;;     [(_ mnemonic code)
;;      #''('opcode code)]))

;; (def-acc ASL #x0a)
;; (def-impl ADC #xaa)

(module+ test
  (check-equal? (ASL A)
                '('opcode #x0a)))

;;--------------------------------------------------------------------------------
(define (NOP)
  '('opcode #xea))

(require (for-syntax racket))

(define-for-syntax (twobytenum? num-str)
  (or (and (symbol? num-str)
        (twobytenum? (symbol->string num-str)))
     (and (string? num-str)
        (eq? 3 (string-length num-str))
        (string-prefix? num-str "$"))))

(define-for-syntax (twobytenum num-str)
  (if (symbol? num-str)
      (twobytenum (symbol->string num-str))
      (string->number (substring num-str 1) 16)))






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

;; actual syntax definition blueprint for commands --------------------------------------------------
(define-syntax (ASL stx)
  (syntax-case stx ()
    ([_]
     #''('opcode #x0a))
    ([_ op]
     (cond [(eq? (syntax->datum #'op) 'A)
            #''('opcode #x0a)]
           [(twobytenum? (syntax->datum #'op))
            (with-syntax ((op-num (twobytenum (syntax->datum #'op))))
              #''('opcode #x0b op-num))]
           ))
    ([_ op1 op2]
     (raise-syntax-error 'ASL (format "three operand addressing mode is not available line ~a:~a" (syntax-line stx) (syntax-column stx)))
     ;; #''('opcode #x00)
     )))

;; (ASL A B) ;; raises error

(define-syntax (define-opcode stx)
  (syntax-case stx ()
    ([_ mnemonic addressing-modes]
     (with-syntax ((scoped-mnemonic (datum->syntax stx (syntax->datum #'mnemonic)))
                    (accumulator-addressing (findf (lambda (el) (eq? (car el) 'accumulator)) (syntax->datum #'addressing-modes)))
                    (nstx (make-id stx "~a" #'nstx)))
       (with-syntax ((acc-adr-p (datum->syntax stx (if (syntax->datum #'accumulator-addressing) #t #f)))
                     (acc-adr-opcode (datum->syntax stx (cdr (or (syntax->datum #'accumulator-addressing) '(0.0))))))
         #'(define-syntax (scoped-mnemonic nstx)
             (syntax-case nstx ()
               ([_ op]
                (cond [(and acc-adr-p
                          (eq? (syntax->datum #'op) 'A))
                       #''('opcode acc-adr-opcode)]
                      [(twobytenum? (syntax->datum #'op)) ;; TODO check absolute addressing mode
                       (with-syntax ((op-num (twobytenum (syntax->datum #'op))))
                         #''('opcode #x0b op-num))]
                      [#t (raise-syntax-error
                           'ASL
                           ;; report available addressing modes expected for one op
                           (format "addressing mode with one op not defined. line ~a:~a"
                                   (syntax-line nstx)
                                   (syntax-column nstx))
                           nstx)])))))))))

;; wanted syntax, non wanted adressing modes are not available / produce errors
(define-opcode AXX ((accumulator . #x01)
                    (immediate    . #x02)
                    (zero-page-x  . #x03)))

;; (expand-once #'(define-opcode AXX ((accumulator . #x00)
;;                     (immediate    . #x01)
;;                     (zero-page-x  . #x02))))

(AXX A)

;; idea: delayed decision about byte/word operand

;; '('decision-byte-word ":label" ('opcode #x00 #x00) ('opcode #x01 #x00 #x00)) ;; if :label is byte value use first opcode, else use second
;; is transformed to
;; '('opcode #x00 #x00) ;; in case label is a byte value
;; '('opcode #x01 #x00 #x00) ;; in case label is a word value

(module+ test
  (check-equal? (ASL "$ab")
                '('opcode #x0b #xab))
  (check-equal? (ASL $ab)
                '('opcode #x0b #xab))
  (check-equal? (ASL)
                '('opcode #x0a))
  (check-equal? (ASL A)
                '('opcode #x0a)))

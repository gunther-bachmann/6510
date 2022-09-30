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

(define-syntax (create-mnemonic-for-accumulator-pair stx)
  (syntax-case stx ()
    ([_ pair ]
     (with-syntax ((mnemonic (datum->syntax stx (car (syntax->datum #'pair))))
                   (bytecode (datum->syntax stx (cdr (syntax->datum #'pair))))
                   (nstx (make-id stx "~a" #'nstx)))
       ;; (datum->syntax stx `(define-syntax (,#'mnemonic ,#'nstx)
       ;;                     (syntax-case ,#'nstx ()
       ;;                       ([_ op]
       ;;                        ;; (eq? (syntax->datum #'op) 'A) ;; will show error in editor when not eq
       ;;                        #''('opcode ,#'bytecode)))))

       #'(begin
           (define-syntax (mnemonic nstx)
             (syntax-case nstx ()
               ([_ op]
                ;; (eq? (syntax->datum #'op) 'A) ;; will show error in editor when not eq
                #''('opcode bytecode))))
           (mnemonic A))
       ))))

(define-syntax (create-mnemonic-for-accumulator-pair2 stx)
  (syntax-case stx ()
    ([_ ( mnemonic . bytecode) ]     
     #'(define-syntax (mnemonic stx)
         (syntax-case stx ()
           ([_ op]
            (eq? (syntax->datum #'op) 'A) ;; will show error in editor when not eq
            #''('opcode bytecode)))))))

(define-syntax (create-mnemonic-for-accumulator stx)
  (syntax-case stx ()
    ([_ mnemonic bytecode ]
     #'(define-syntax (mnemonic stx)
         (syntax-case stx ()
           ([_ op]
            (eq? (syntax->datum #'op) 'A) ;; will show error in editor when not eq
            #''('opcode bytecode)))))))




(require (for-syntax syntax/parse))

(define-syntax (cm4a5 stx)
  (syntax-case stx ()
    [(_ some)
     (datum->syntax stx '(create-mnemonic-for-accumulator-pair (LSX . 17)))]))

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

(expand-once #'(create-mnemonic-for-accumulator-pair (LSR . #x4a )))
(expand-once (expand-once (expand-once #'(cm4a4 ((LSX . #x11))))))
(expand-once (expand-once (expand-once #'(cm4a5 ((LSX . #x11))))))


;; (expand-once (expand-once (expand-once #'(cm4a5 ((LSX . #x11))))))
;; (cm4a5 ((LSX . #x11)))
(cm4a4 ((LSX . #x11)
        (LSY . #x12)))
(LSX A)
(LSY A)
(create-mnemonic-for-accumulator-pair (LSR . #x0a))


;; (begin
;;   (create-mnemonic-for-accumulator-pair (LSX . 17)))

;; (begin
;;   (define-syntax (LSX stx)
;;     (syntax-case stx ()
;;       ((_ op) #''('opcode 17))))
;;   (define-syntax (LSY stx)
;;     (syntax-case stx ()
;;       ((_ op) #''('opcode 18)))))

;; (LSX A)
;; (LSY A)

(module+ test
  (check-equal? (LSR A)
                '('opcode #x4a))
  )

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
           ))))

(module+ test
  (check-equal? (ASL "$ab")
                '('opcode #x0b #xab))
  (check-equal? (ASL $ab)
                '('opcode #x0b #xab))
  (check-equal? (ASL)
                '('opcode #x0a))
  (check-equal? (ASL A)
                '('opcode #x0a)))

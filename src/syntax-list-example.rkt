#lang racket

(require (for-syntax "6510-syntax-utils.rkt"))

(module+ test
  (require rackunit))


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

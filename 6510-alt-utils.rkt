#lang racket

(require "6510-utils.rkt")

(provide make-id
         byte-operand? byte-operand
         word-operand? word-operand
         indirect-x-operand? indirect-x-operand
         indirect-y-operand? indirect-y-operand
         immediate-byte-operand? immediate-byte-operand

         accumulator-addr-applies?
         opcode-for-addressing
         byte-addr-applies?
         word-addr-applies?
         relative-addr-applies?
         opcode-for-zero-page-addr
         opcode-for-indirect-x-addr
         opcode-for-relative-addr
         opcode-for-immediate-addr
         indirect-x-addr-applies?
         immediate-addr-applies?
         opcode-for-absolute-addr
         opcode-for-zero-page-indexed-addr
         zero-page-indexed-addr-applies?
         absolute-indexed-addr-applies?
         opcode-for-absolute-indexed-addr
         indirect-y-addr-applies?
         opcode-for-indirect-y-addr
         implicit-addr-applies?
         indirect-addr-applies?
         opcode-for-indirect-addr

         raise-addressing-error)

(module+ test
  (require rackunit))

(define (make-id stx id-template . ids)
  (let ([str (apply format id-template (map syntax->datum ids))])
    (datum->syntax stx (string->symbol str))))

(define (byte-operand? num-str)
  (or (and (symbol? num-str)
        (byte-operand? (symbol->string num-str)))
     (and (string? num-str)
        (eq? 3 (string-length num-str))
        (string-prefix? num-str "$"))))

(define (byte-operand num-str)
  (if (symbol? num-str)
      (byte-operand (symbol->string num-str))
      (string->number (substring num-str 1) 16)))

(define (word-operand num-str)
  (if (symbol? num-str)
      (word-operand (symbol->string num-str))
      (string->number (substring num-str 1) 16)))

(define (word-operand? num-str)
  (or (and (symbol? num-str)
        (word-operand? (symbol->string num-str)))
     (and (string? num-str)
        (eq? 5 (string-length num-str))
        (string-prefix? num-str "$"))))

(define (immediate-byte-operand? sym)
  (or (and (symbol? sym)
        (immediate-byte-operand? (symbol->string sym)))
     (and (string? sym)
        (string-prefix? sym "!")
        (byte-operand? (substring sym 1)))))

(define (immediate-byte-operand sym)
  (if (symbol? sym)
        (immediate-byte-operand (symbol->string sym))
        (byte-operand (substring sym 1))))

(define (indirect-x-operand? sym)
  (and (list? sym)
     (byte-operand? (car sym))
     (equal? (cadr sym) ',x)))

(define (indirect-y-operand? sym)
  (and (list? sym)
     (byte-operand? (car sym))))

(define (indirect-x-operand sym)
  (byte-operand (car sym)))

(define (indirect-y-operand sym)
  (byte-operand (car sym)))


(define (extract-def sym addressing-modes)
  (findf (lambda (el) (eq? (car el) sym)) addressing-modes))

(define (accumulator-addr-applies? addressing-modes-stx op-stx)
    (and (pair? (extract-def 'accumulator (syntax->datum addressing-modes-stx)))
     (eq?  (syntax->datum op-stx) 'A)))

(define (opcode-for-addressing addressing addressing-modes-stx)
  `(opcode ,(cdr (extract-def addressing (syntax->datum addressing-modes-stx)))))

(module+ test #| opcode-for-addressing |#
  (check-equal? (opcode-for-addressing 'implicit #'((accumulator . #x20) (implicit . #x10)))
                '(opcode #x10)))

(define (byte-addr-applies? addr-sym addressing-modes-stx op-stx)
  (and (pair? (extract-def addr-sym (syntax->datum addressing-modes-stx)))
     (byte-operand? (syntax->datum op-stx))))

(define (word-addr-applies? addr-sym addressing-modes-stx op-stx)
  (and (pair? (extract-def addr-sym (syntax->datum addressing-modes-stx)))
     (word-operand? (syntax->datum op-stx))))

(define (relative-addr-applies? addressing-modes-stx op-stx)
  (and (pair? (extract-def 'relative (syntax->datum addressing-modes-stx)))
     (byte-operand? (syntax->datum op-stx))))

(define (opcode-for-zero-page-addr addressing-modes-stx op-stx)
  `(opcode ,(cdr (extract-def 'zero-page (syntax->datum addressing-modes-stx))) ,(byte-operand (syntax->datum op-stx))))

(define (opcode-for-immediate-addr addressing-modes-stx op-stx)
  `(opcode ,(cdr (extract-def 'immediate (syntax->datum addressing-modes-stx))) ,(immediate-byte-operand (syntax->datum op-stx))))

(define (opcode-for-indirect-x-addr addressing-modes-stx op-stx)
  `(opcode ,(cdr (extract-def 'indirect-x (syntax->datum addressing-modes-stx)))
           ,(indirect-x-operand (syntax->datum op-stx))))

(define (opcode-for-relative-addr addressing-modes-stx op-stx)
  `(rel-opcode ,(cdr (extract-def 'relative (syntax->datum addressing-modes-stx))) ,(byte-operand (syntax->datum op-stx))))

(define (opcode-for-absolute-addr addressing-modes-stx op-stx)
  `(opcode ,(cdr (extract-def 'absolute (syntax->datum addressing-modes-stx)))
           ,(low-byte (word-operand (syntax->datum op-stx)))
           ,(high-byte (word-operand (syntax->datum op-stx)))))

(define (opcode-for-indirect-y-addr addressing-modes-stx op-stx)
    `(opcode ,(cdr (extract-def 'indirect-y (syntax->datum addressing-modes-stx)))
           ,(indirect-y-operand (syntax->datum op-stx))))

(define (opcode-for-zero-page-indexed-addr sym addressing-modes-stx op-stx)
    `(opcode ,(cdr (extract-def sym (syntax->datum addressing-modes-stx)))
             ,(byte-operand (syntax->datum op-stx))))

(define (opcode-for-indirect-addr addressing-modes-stx op-stx)
      `(opcode ,(cdr (extract-def 'indirect (syntax->datum addressing-modes-stx)))
             ,(low-byte (word-operand (car (syntax->datum op-stx))))
             ,(high-byte (word-operand (car (syntax->datum op-stx))))))

(define (immediate-addr-applies? addressing-modes-stx op-stx)
  (and (pair? (extract-def 'immediate (syntax->datum addressing-modes-stx)))
     (immediate-byte-operand? (syntax->datum op-stx))))

(define (implicit-addr-applies? addressing-modes-stx)
  (pair? (extract-def 'implicit (syntax->datum addressing-modes-stx))))

(define (indirect-addr-applies? addressing-modes-stx op-stx)
    (and (pair? (extract-def 'indirect (syntax->datum addressing-modes-stx)))
     (list? (syntax->datum op-stx))
     (word-operand (car (syntax->datum op-stx)))))

(define (indirect-x-addr-applies? addressing-modes-stx op-stx)
  (and (pair? (extract-def 'indirect-x (syntax->datum addressing-modes-stx)))
     (indirect-x-operand? (syntax->datum op-stx))))

(define (indirect-y-addr-applies? addressing-modes-stx op-stx1 op-stx2)
  (and (pair? (extract-def 'indirect-y (syntax->datum addressing-modes-stx)))
     (indirect-y-operand? (syntax->datum op-stx1))
     (equal? (syntax->datum op-stx2) ',y)))

(define (zero-page-indexed-addr-applies? sym op-sym addressing-modes-stx op1-stx op2-stx)
  (and (pair? (extract-def sym (syntax->datum addressing-modes-stx)))
     (byte-operand? (syntax->datum op1-stx))
     (equal? (syntax->datum op2-stx) op-sym)))

(define (absolute-indexed-addr-applies? sym op-sym addressing-modes-stx op1-stx op2-stx)
    (and (pair? (extract-def sym (syntax->datum addressing-modes-stx)))
     (word-operand? (syntax->datum op1-stx))
     (equal? (syntax->datum op2-stx) op-sym)))

(define (opcode-for-absolute-indexed-addr sym addressing-modes-stx op-stx)
    `(opcode ,(cdr (extract-def sym (syntax->datum addressing-modes-stx)))
           ,(low-byte (word-operand (syntax->datum op-stx)))
           ,(high-byte (word-operand (syntax->datum op-stx)))))

(define (raise-addressing-error stx addressing-modes-stx)
  (raise-syntax-error
   'mnemonic
   ;; report available addressing modes expected for one op
   (format "addressing mode not recognized.\nallowed addressing modes are ~a.\n  in line ~a:~a"
           (string-join (map (lambda (addr-pair) (symbol->string (car addr-pair))) (syntax->datum addressing-modes-stx))
                        ", ")
           (syntax-line stx)
           (syntax-column stx))
   stx))


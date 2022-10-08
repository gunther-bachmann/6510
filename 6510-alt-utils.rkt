#lang racket

(require "6510-utils.rkt")

(provide make-id
         
         absolute-indexed-addressing?
         accumulator-addressing?
         byte-addressing?
         immediate-addressing?
         implicit-addressing?
         indirect-addressing?
         indirect-x-addressing?
         indirect-y-addressing?
         relative-addressing?
         word-addressing?
         zero-page-indexed-addressing?

         absolute-opcode
         absolute-indexed-opcode
         immediate-opcode
         indirect-opcode
         indirect-x-opcode
         indirect-y-opcode
         relative-opcode
         zero-page-opcode
         zero-page-indexed-opcode
         no-operand-opcode
         raise-addressing-error)

(module+ test
  (require rackunit))

(define (make-id stx id-template . ids)
  (let ([str (apply format id-template (map syntax->datum ids))])
    (datum->syntax stx (string->symbol str))))

(define (byte-operand? any-num)
  (or (and (symbol? any-num)
        (byte-operand? (symbol->string any-num)))
     (and (number? any-num)
        (in-byte-range? any-num))
     (and (string? any-num)
        (or (and (6510-number-string? any-num)
              (in-byte-range? (parse-number-string any-num)))
           (byte-label? any-num)))))

(define (byte-label? str)
  (regexp-match #rx"^[><][a-zA-Z_-][a-zA-Z0-9_-]*$" str))

(module+ test #| byte-operand? |#
  (for ((byte '(10 0 255 "10" "0" "255" |10| |$10| |$FF| |%101|)))
    (check-true (byte-operand? byte) (format "~a not a byte" byte)))
  (for ((byte '(-1 256 "-1" "256" |-1| |$101|)))
    (check-false (byte-operand? byte) (format "~a is a byte" byte))))

(define (byte-operand any-num)
  (cond [(symbol? any-num)
         (byte-operand (symbol->string any-num))]
        [(number? any-num) any-num]
        [(6510-number-string? any-num) (parse-number-string any-num)]
        [(byte-label? any-num) `(resolve-byte ,any-num)]
        [#t (raise-syntax-error #f (format "unknown byte operand ~a" any-num))]))

(module+ test #| byte-operand |#
  (for ((byte-expectation
         '((10      . 10)
           (0       . 0)
           (255     . 255)
           ("10"    . 10)
           ("0"     . 0)
           ("255"   . 255)
           (|10|    . 10)
           (|$10|   . 16)
           (|$FF|   . 255)
           (|%101|  . 5)
           (">some" . (resolve-byte ">some")))))
    (check-equal? (byte-operand (car byte-expectation))
               (cdr byte-expectation)
               (format "expected: ~a == ~a"
                       (car byte-expectation)
                       (cdr byte-expectation)))))

(define (word-operand any-num)
  (cond [(symbol? any-num)
         (word-operand (symbol->string any-num))]
        [(number? any-num) any-num]
        [#t (parse-number-string any-num)]))

(module+ test #| word-operand |#
  (for ((word-expectation
         '((10       . 10)
           (0        . 0)
           (65535    . 65535)
           ("10"     . 10)
           ("0"      . 0)
           ("65535"  . 65535)
           (|10|     . 10)
           (|$10|    . 16)
           (|$FFff|  . 65535)
           (|%10001| . 17))))
    (check-eq? (word-operand (car word-expectation))
               (cdr word-expectation)
               (format "expected: ~a == ~a"
                       (car word-expectation)
                       (cdr word-expectation)))))

(define (word-operand? any-num)
  (or (and (symbol? any-num)
        (word-operand? (symbol->string any-num)))
     (and (number? any-num)
        (in-word-range? any-num))
     (and (string? any-num)
        (6510-number-string? any-num)
        (in-word-range? (parse-number-string any-num)))))

(module+ test #| word-operand? |#
  (for ((word '(10 0 65535 "10" "0" "65535" |10| |$10| |$FFFF| |%10001|)))
    (check-true (word-operand? word) (format "~a not a word" word)))
  (for ((word '(-1 65536 "-1" "65536" |-1| |$10001|)))
    (check-false (word-operand? word) (format "~a is a word" word))))

(define (immediate-byte-operand? sym)
  (or (and (symbol? sym)
        (immediate-byte-operand? (symbol->string sym)))
     (and (string? sym)
        (string-prefix? sym "!")
        (byte-operand? (substring sym 1)))))

(module+ test #| immediate-byte-operand? |#
  (for ((immediate-byte '("!10" "!0" "!255" |!10| |!$10| |!$FF| |!%101|)))
    (check-true (immediate-byte-operand? immediate-byte)
                (format "~a not an immediate byte" immediate-byte)))
  (for ((immediate-byte '("!-1" "!256" |!-1| |!$101|)))
    (check-false (immediate-byte-operand? immediate-byte)
                 (format "~a is a byte" immediate-byte))))

(define (immediate-byte-operand sym)
  (if (symbol? sym)
        (immediate-byte-operand (symbol->string sym))
        (byte-operand (substring sym 1))))

(module+ test #| immediate-byte-operand |#
  (for ((byte-expectation
         '(("!10"   . 10)
           ("!0"    . 0)
           ("!255"  . 255)
           (|!10|   . 10)
           (|!$10|  . 16)
           (|!$FF|  . 255)
           (|!%101| . 5))))
    (check-eq? (immediate-byte-operand (car byte-expectation))
               (cdr byte-expectation)
               (format "expected: ~a == ~a"
                       (car byte-expectation)
                       (cdr byte-expectation)))))

(define (find-addressing-mode sym addressing-modes)
  (findf (lambda (el) (and (pair? el) (eq? (car el) sym))) addressing-modes))

(module+ test #| find-addressing-mode |#
  (check-equal? (find-addressing-mode 'accumulator '((immediate . #x10) (accumulator . #x20)))
                '(accumulator . #x20)))

(define (has-addressing-mode? sym addressing-modes)
  (pair? (find-addressing-mode sym addressing-modes)))

(module+ test #| has-addressing-mode? |#
  (check-true (has-addressing-mode? 'accumulator '((immediate . #x10) (accumulator . #x20))))
  (check-false (has-addressing-mode? 'zero-page-x '((immediate . #x10) (accumulator . #x20)))))

(define (accumulator-addressing? addressing-modes-stx op-stx)
  (and (has-addressing-mode? 'accumulator (syntax->datum addressing-modes-stx)) 
     (eq? (syntax->datum op-stx) 'A)))

(module+ test #| accumulator-addressing? |#
  (check-true (accumulator-addressing? #'((accumulator . #x20)) #'A)))

(define (byte-addressing? addr-sym addressing-modes-stx op-stx)
  (and (has-addressing-mode? addr-sym (syntax->datum addressing-modes-stx))
     (byte-operand? (syntax->datum op-stx))))

(define (word-addressing? addr-sym addressing-modes-stx op-stx)
  (and (has-addressing-mode? addr-sym (syntax->datum addressing-modes-stx))
     (word-operand? (syntax->datum op-stx))))

(define (relative-addressing? addressing-modes-stx op-stx)
  (and (has-addressing-mode? 'relative (syntax->datum addressing-modes-stx))
     (byte-operand? (syntax->datum op-stx))))

(define (immediate-addressing? addressing-modes-stx op-stx)
  (and (has-addressing-mode? 'immediate (syntax->datum addressing-modes-stx))
     (immediate-byte-operand? (syntax->datum op-stx))))

(define (implicit-addressing? addressing-modes-stx)
  (has-addressing-mode? 'implicit (syntax->datum addressing-modes-stx)))

(define (indirect-addressing? addressing-modes-stx op-stx)
  (and (has-addressing-mode? 'indirect (syntax->datum addressing-modes-stx))
     (list? (syntax->datum op-stx))
     (word-operand (car (syntax->datum op-stx)))))

(define (indirect-x-addressing? addressing-modes-stx op-stx)
  (define op (syntax->datum op-stx))
  (and (has-addressing-mode? 'indirect-x (syntax->datum addressing-modes-stx))
     (list? op)
     (byte-operand? (car op))
     (pair? (cdr op))
     (equal? (cadr op) ',x)))

(define (indirect-y-addressing? addressing-modes-stx op-stx1 op-stx2)
  (and (has-addressing-mode? 'indirect-y (syntax->datum addressing-modes-stx))
     (list (syntax->datum op-stx1))
     (byte-operand? (car (syntax->datum op-stx1)))
     (equal? (syntax->datum op-stx2) ',y)))

(define (zero-page-indexed-addressing? sym op-sym addressing-modes-stx op1-stx op2-stx)
  (and (has-addressing-mode? sym (syntax->datum addressing-modes-stx))
     (byte-operand? (syntax->datum op1-stx))
     (equal? (syntax->datum op2-stx) op-sym)))

(define (absolute-indexed-addressing? sym op-sym addressing-modes-stx op1-stx op2-stx)
  (and (has-addressing-mode? sym (syntax->datum addressing-modes-stx))
     (word-operand? (syntax->datum op1-stx))
     (equal? (syntax->datum op2-stx) op-sym)))

(define (no-operand-opcode addressing addressing-modes)
  `(opcode ,(cdr (find-addressing-mode addressing addressing-modes))))

(module+ test #| opcode-without-operand |#
  (check-equal? (no-operand-opcode 'implicit '((accumulator . #x20) (implicit . #x10)))
                '(opcode #x10)))

(define (zero-page-opcode addressing-modes op)
  `(opcode ,(cdr (find-addressing-mode 'zero-page addressing-modes))
           ,(byte-operand  op)))

(define (zero-page-indexed-opcode sym addressing-modes op)
  `(opcode ,(cdr (find-addressing-mode sym addressing-modes))
           ,(byte-operand op)))

(define (immediate-opcode addressing-modes op)
  `(opcode ,(cdr (find-addressing-mode 'immediate addressing-modes))
           ,(immediate-byte-operand op)))

(define (indirect-x-opcode addressing-modes op)
  `(opcode ,(cdr (find-addressing-mode 'indirect-x addressing-modes))
           ,(byte-operand (car op))))

(define (relative-opcode addressing-modes op)
  `(rel-opcode ,(cdr (find-addressing-mode 'relative addressing-modes))
               ,(byte-operand op)))

(define (absolute-opcode addressing-modes op)
  `(opcode ,(cdr (find-addressing-mode 'absolute addressing-modes))
           ,(low-byte (word-operand  op))
           ,(high-byte (word-operand  op))))

(define (indirect-y-opcode addressing-modes op)
  `(opcode ,(cdr (find-addressing-mode 'indirect-y  addressing-modes))
           ,(byte-operand (car op))))

(define (indirect-opcode addressing-modes op)
  `(opcode ,(cdr (find-addressing-mode 'indirect addressing-modes))
           ,(low-byte (word-operand (car op)))
           ,(high-byte (word-operand (car op)))))

(define (absolute-indexed-opcode sym addressing-modes op)
  `(opcode ,(cdr (find-addressing-mode sym addressing-modes))
           ,(low-byte (word-operand op))
           ,(high-byte (word-operand op))))

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


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
         abs-or-zero-page-indexed-addressing?
         abs-or-zero-page-addressing?

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
         abs-or-zero-page-indexed-opcode
         abs-or-zero-page-opcode

         raise-addressing-error)

(module+ test
  (require rackunit))

(define (make-id stx id-template . ids)
  (let ([str (apply format id-template (map syntax->datum ids))])
    (datum->syntax stx (string->symbol str))))

(define (possibly-byte-operand? any-num)
  (or (and (symbol? any-num)
        (possibly-byte-operand? (symbol->string any-num)))
     (byte-operand? any-num)
     (ambiguous-operand? any-num)))

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

(define (label? str)
  (and (not (equal? str "A")) ;; reserved for accumulator addressing
     (regexp-match #rx"^[a-zA-Z_][a-zA-Z0-9_-]*$" str)))

(module+ test #| byte-operand? |#
  (for ((byte '(10 0 255 "10" "0" "255" |10| |$10| |$FF| |%101|)))
    (check-true (byte-operand? byte) (format "~a not a byte" byte)))
  (for ((byte '(-1 256 "-1" "256" |-1| |$101|)))
    (check-false (byte-operand? byte) (format "~a is a byte" byte))))

(define (byte-operand any-num (force true) (relative false))
  (cond [(symbol? any-num)
         (byte-operand (symbol->string any-num) force relative)]
        [(number? any-num) any-num]
        [(6510-number-string? any-num) (parse-number-string any-num)]
        [(byte-label? any-num) `(resolve-byte ,any-num)]
        [(and force
            (label? any-num)
            (not relative))
         `(resolve-byte ,any-num)]
        [(and force
            (label? any-num)
            relative)
         `(resolve-relative ,any-num)]
        [#t (raise-syntax-error #f (format "unknown byte operand ~a" any-num))]))

(module+ test #| byte-operand |#
  (check-equal? (byte-operand "some" #t)
                '(resolve-byte "some"))
  (check-equal? (byte-operand "some" #t #t)
                '(resolve-relative "some"))
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

(define (word-operand any-num (force #f))
  (cond [(symbol? any-num)
         (word-operand (symbol->string any-num) force)]
        [(number? any-num) any-num]
        [(6510-number-string? any-num) (parse-number-string any-num)]        
        [force `(resolve-word ,any-num)]
        [#t (raise-syntax-error 'word-operand (format "'~a' is no valid word operand" any-num))]))

(module+ test #| word-operand |#
  (for ((word-expectation
         '((10       . 10)
           (0        . 0)
           (65535    . 65535)
           ("10"     . 10)
           ("0"      . 0)
           ("65535"  . 65535)
           (|10|     . 10)
           ($10      . 16)
           ($FFff    . 65535)
           (%10001   . 17))))
    (check-eq? (word-operand (car word-expectation))
               (cdr word-expectation)
               (format "expected: ~a == ~a"
                       (car word-expectation)
                       (cdr word-expectation)))))

(define (possibly-word-operand? any-num)
  (or (and (symbol? any-num)
        (possibly-word-operand? (symbol->string any-num)))
     (word-operand? any-num)
     (ambiguous-operand? any-num)))

(define (ambiguous-operand? any-num)
  (or (and (symbol? any-num)
        (ambiguous-operand? (symbol->string any-num)))
     (and (string? any-num)
        (label? any-num))))

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
        (possibly-byte-operand? (substring sym 1)))))

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
        (byte-operand (substring sym 1) #t)))

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

(define (relative-addressing-operand? elem)
  (if (symbol? elem)
      (relative-addressing-operand? (symbol->string elem))
      (or (byte-operand? elem)
         (label? elem))))

(define (relative-addressing? addressing-modes-stx op-stx)
  (and (has-addressing-mode? 'relative (syntax->datum addressing-modes-stx))
     (relative-addressing-operand? (syntax->datum op-stx))))

(define (immediate-addressing? addressing-modes-stx op-stx)
  (and (has-addressing-mode? 'immediate (syntax->datum addressing-modes-stx))
     (immediate-byte-operand? (syntax->datum op-stx))))

(define (implicit-addressing? addressing-modes-stx)
  (has-addressing-mode? 'implicit (syntax->datum addressing-modes-stx)))

(define (indirect-addressing? addressing-modes-stx op-stx)
  (and (has-addressing-mode? 'indirect (syntax->datum addressing-modes-stx))
     (list? (syntax->datum op-stx))
     (possibly-word-operand? (car (syntax->datum op-stx)))))

(define (indirect-x-addressing? addressing-modes-stx op-stx)
  (define op (syntax->datum op-stx))
  (and (has-addressing-mode? 'indirect-x (syntax->datum addressing-modes-stx))
     (list? op)
     (possibly-byte-operand? (car op))
     (pair? (cdr op))
     (equal? (cadr op) ',x)))

(define (indirect-y-addressing? addressing-modes-stx op-stx1 op-stx2)
  (and (has-addressing-mode? 'indirect-y (syntax->datum addressing-modes-stx))
     (list (syntax->datum op-stx1))
     (possibly-byte-operand? (car (syntax->datum op-stx1)))
     (equal? (syntax->datum op-stx2) ',y)))

(define (abs-or-zero-page-indexed-addressing? addressing-sym-list addressing-modes-stx op1-stx op2-stx)
  (define possible-addressing-sym-pairs
    (filter (lambda (addressing-sym-pair)
              (has-addressing-mode? (car addressing-sym-pair)
                                    (syntax->datum addressing-modes-stx)))
            addressing-sym-list))
  (define possible-symbol-list
    (map (lambda (addressing-sym-pair) (cdr addressing-sym-pair))
         possible-addressing-sym-pairs))
  (and (not (empty? possible-addressing-sym-pairs))
     (ambiguous-operand? (syntax->datum op1-stx))
     (member (syntax->datum op2-stx) possible-symbol-list)
     #t))

(module+ test #| abs-or-zero-page-indexed-addressing? |#
  (check-true (abs-or-zero-page-indexed-addressing?
               '((zero-page-x . ,x)
                 (zero-page-y . ,y)
                 (absolute-x . ,x)
                 (absolute-y . ,y))
               #'((absolute-x . #x20)
                  (absolute-y . #x30))
               #'some
               #',x))
  (check-false (abs-or-zero-page-indexed-addressing?
                '((zero-page-x . ,x)
                  (zero-page-y . ,y)
                  (absolute-x . ,x)
                  (absolute-y . ,y))
                #'((absolute-x . #x20)
                   (zero-page-x . #x10))
                #'some
                #',y))
  (check-false (abs-or-zero-page-indexed-addressing?
                '((zero-page-x . ,x)
                  (zero-page-x . ,y)
                  (absolute-x . ,x)
                  (absolute-y . ,y))
                #'((absolute-x . #x20)
                   (absolute-y . #x30))
                #'$10
                #',x))
  (check-false (abs-or-zero-page-indexed-addressing?
                '((zero-page-x . ,x)
                  (zero-page-y . ,y)
                  (absolute-x . ,x)
                  (absolute-y . ,y))
                #'((absolute . #x20)
                   (zero-page . #x30))
                #'some
                #',x)))

(define (abs-or-zero-page-addressing? addressing-list addressing-modes-stx op-stx )
  (define possible-addressing-sym-pairs
    (filter (lambda (addressing)
              (has-addressing-mode? addressing (syntax->datum addressing-modes-stx)))
            addressing-list))
  (and (not (empty? possible-addressing-sym-pairs))
     (ambiguous-operand? (syntax->datum op-stx))
     #t))

(module+ test #| abs-or-zero-page-addressing? |#
  (check-true (abs-or-zero-page-addressing?
               '(absolute zero-page)
               #'((absolute . #x20)
                  (zero-page . #x30))
               #'some))
  (check-false (abs-or-zero-page-addressing?
                '(absolute zero-page)
                #'((absolute-x . #x20)
                   (zero-page-x . #x30))
                #'some)
               "there is no matching addresing mode available")
  (check-false (abs-or-zero-page-addressing?
                '(absolute zero-page)
                #'((absolute . #x20)
                   (zero-page . #x30))
                #'<some)
               "operand is definitively only byte"))

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
           ,(byte-operand (car op) #t)))

(define (relative-opcode addressing-modes op)
  `(rel-opcode ,(cdr (find-addressing-mode 'relative addressing-modes))
               ,(byte-operand op #t #t)))

(module+ test #| relative-opcode |#
  (check-equal? (relative-opcode '((relative . #x20)) '$10)
                '(rel-opcode #x20 #x10))
  (check-equal? (relative-opcode '((relative . #x20)) 'some)
                '(rel-opcode #x20 (resolve-relative "some"))))

(define (absolute-opcode addressing-modes op)
  `(opcode ,(cdr (find-addressing-mode 'absolute addressing-modes))
           ,(low-byte (word-operand  op))
           ,(high-byte (word-operand  op))))

(module+ test #| absolute-opcode |#
  (check-equal? (absolute-opcode '((absolute . #x20)) '$1000)
                '(opcode #x20 #x00 #x10)))

(define (indirect-y-opcode addressing-modes op)
  `(opcode ,(cdr (find-addressing-mode 'indirect-y  addressing-modes))
           ,(byte-operand (car op) #t)))

(module+ test #| indirect-y-opcode |#
  (check-equal? (indirect-y-opcode '((indirect-y . #x20)) '($10))
                '(opcode #x20 #x10)))

(define (indirect-opcode addressing-modes op)
  (let ((word (word-operand (car op) #t)))
    (if (number? word)
        `(opcode ,(cdr (find-addressing-mode 'indirect addressing-modes))                                 
                 ,(low-byte word)
                 ,(high-byte word))
        `(opcode ,(cdr (find-addressing-mode 'indirect addressing-modes))
                 ,word))))

(module+ test #| indirect-opcode |#
  (check-equal? (indirect-opcode '((indirect . #x20)) '($1000))
                '(opcode #x20 #x00 #x10)))

(define (absolute-indexed-opcode sym addressing-modes op)
  `(opcode ,(cdr (find-addressing-mode sym addressing-modes))
           ,(low-byte (word-operand op))
           ,(high-byte (word-operand op))))

(module+ test #| absolute-indexed-opcode |#
  (check-equal? (absolute-indexed-opcode 'absolute-x '((absolute-x . #x20)) '$1000)
                '(opcode #x20 0 16)))

(define (possible-addressings addressing-sym-list addressing-modes op1 op2)
  (map (lambda (addressing-sym-pair) (car addressing-sym-pair))
       (filter (lambda (addressing-sym-pair)
                 (and (has-addressing-mode? (car addressing-sym-pair) addressing-modes)
                    (equal? op2 (cdr addressing-sym-pair))))
               addressing-sym-list)))

(module+ test #|  possible-addressings |#
  (check-equal? (possible-addressings
                 '((zero-page-x . ,x)
                   (absolute-x . ,x)
                   (zero-page-y . ,y))
                 '((absolute-x . #x20)
                   (zero-page-y . #x30))
                 'some
                 ',x)
                '(absolute-x))
  (check-equal? (possible-addressings
                 '((zero-page-x . ,x)
                   (zero-page-y . ,y))
                 '((zero-page-y . #x30))
                 'some
                 ',x)
                '())
  (check-equal? (possible-addressings
                 '((zero-page-x . ,x)
                   (absolute-x . ,x)
                   (zero-page-y . ,y))
                 '((absolute-x . #x20)
                   (zero-page-x . #x30)
                   (accumulator . #x40))
                 'some
                 ',x)
                '(zero-page-x absolute-x)))

(define (ambiguous-addressing-opcode possible-addressing-modes op)
  (cond [(empty? possible-addressing-modes)
         (raise-syntax-error 'ambiguous-addressing "no possible addressing mode found")]
        [(= 1 (length possible-addressing-modes))
         `(opcode ,(cdar possible-addressing-modes)
                  (,(hash-ref address-mode-definitions (caar possible-addressing-modes))
                   ,(->string op)))]
        [(< 1 (length possible-addressing-modes))
         `(decide ,(map (lambda (addressing-mode)
                          `((,(hash-ref address-mode-definitions (car addressing-mode))
                             ,(->string op)) . (opcode ,(cdr addressing-mode))))
                        possible-addressing-modes) )]
        [#t (raise-syntax-error 'ambiguous-addressing "no option found for ambiguous address resolution")]))

(define (abs-or-zero-page-indexed-opcode addressing-sym-list addressing-modes op1 op2)
  (define pos-addressings (possible-addressings addressing-sym-list addressing-modes op1 op2))
  (define possible-addressing-modes
    (filter (λ (addressing-mode)
              (member (car addressing-mode) pos-addressings))
            addressing-modes))
  (ambiguous-addressing-opcode possible-addressing-modes op1))

(module+ test #| abs-or-zero-page-indexed-opcode |#
  (check-equal? (abs-or-zero-page-indexed-opcode
                 '((zero-page-x . ,x)
                   (zero-page-y . ,y)
                   (absolute-x . ,x)
                   (absolute-y . ,y))
                 '((absolute-x . #x20)
                   (absolute-y . #x30))
                 'some
                 ',x)
                '(opcode #x20 (resolve-word "some"))))

(define (abs-or-zero-page-opcode addressing-list addressing-modes op)
  (define pos-addressings
    (filter (lambda (addressing)
              (has-addressing-mode? addressing addressing-modes))
            addressing-list))
  (define possible-addressing-modes
    (filter (λ (addressing-mode)
              (member (car addressing-mode) pos-addressings))
            addressing-modes))
  (ambiguous-addressing-opcode possible-addressing-modes op))

(define (->string el)
  (cond [(string? el) el]
        [(symbol? el) (symbol->string el)]))

(module+ test #| ->string |#
    (check-equal? (->string 'some)
                  "some")
    (check-equal? (->string "some")
                  "some"))

(define address-mode-definitions
  (hash 'absolute 'resolve-word
        'absolute-x 'resolve-word
        'absolute-y 'resolve-word
        'zero-page 'resolve-byte
        'zero-page-x 'resolve-byte
        'zero-page-y 'resolve-byte))

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

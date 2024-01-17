#lang racket

(require "../6510-utils.rkt")
(require "../ast/6510-command.rkt")
(require (rename-in racket/contract [define/contract define/c]))

(provide ;; all addressing mode checks
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

         ;; opcode transformers 
         absolute-opcode
         absolute-opcode-w-meta
         absolute-indexed-opcode
         immediate-opcode
         immediate-opcode-w-meta
         indirect-opcode
         indirect-opcode-w-meta
         indirect-x-opcode
         indirect-x-opcode-w-meta
         indirect-y-opcode
         relative-opcode
         relative-opcode-w-meta
         zero-page-opcode
         zero-page-opcode-w-meta
         zero-page-indexed-opcode
         zero-page-indexed-opcode-w-meta
         no-operand-opcode
         no-operand-opcode-w-meta
         abs-or-zero-page-indexed-opcode
         abs-or-zero-page-indexed-opcode-w-meta
         abs-or-zero-page-opcode
         abs-or-zero-page-opcode-w-meta
         
         raise-addressing-error)

;; map addressing mode to the respective resolve-<width> 
(define address-mode-to-resolve-map
  (hash 'absolute 'resolve-word
        'absolute-x 'resolve-word
        'absolute-y 'resolve-word
        'zero-page 'resolve-byte
        'zero-page-x 'resolve-byte
        'zero-page-y 'resolve-byte))

(module+ test
  (require rackunit))

;; is the given operand possibly of byte length?
(define/c (possibly-byte-operand? any-num)
  (-> (or/c symbol? number? string?) (or/c (listof string?) boolean?))
  (or (and (symbol? any-num)
        (possibly-byte-operand? (symbol->string any-num)))
     (byte-operand? any-num)
     (ambiguous-operand? any-num)))

;; is the given operand definitively of byte length?
(define/c (byte-operand? any-num)
  (-> any/c boolean?)
  (or (and (symbol? any-num)
        (byte-operand? (symbol->string any-num)))
     (and (number? any-num)
        (in-byte-range? any-num))
     (and (string? any-num)
        (or (and (6510-number-string? any-num)
              (in-byte-range? (parse-number-string any-num)))
           (byte-label? any-num)))))

(module+ test #| byte-operand? |#
  (for ((byte `(10 0 255 "10" "0" "255" "%101" ,(string->symbol "10") |10| |$10| |$FF| |%101| ">some" "<other")))
    (check-true (byte-operand? byte) (format "~a not a byte" byte)))
  (for ((no-byte '(-1 256 "-1" "256" |-1| |$101| "some" "other")))
    (check-false (byte-operand? no-byte) (format "~a is a byte" no-byte))))

;; is the given string a byte label?
(define/c (byte-label? str)
  (-> string? boolean?)
  (and (regexp-match #rx"^[><][a-zA-Z_-][a-zA-Z0-9_-]*$" str) #t))

;; is the given string a label
(define/c (label? str)
  (-> string? boolean?)
  (and (not (equal? str "A")) ;; reserved for accumulator addressing
     (regexp-match #rx"^[a-zA-Z_][a-zA-Z0-9_-]*$" str)
     #t))

;; resolve the given operand to a byte or to an open to resolve-to-byte ast command
(define/c (byte-operand any-num (force true) (relative false))
  (->* ((or/c symbol? number? string?)) (boolean? boolean?) (or/c number? ast-resolve-byte-scmd?))
  (cond [(symbol? any-num)
         (byte-operand (symbol->string any-num) force relative)]
        [(number? any-num) any-num]
        [(6510-number-string? any-num) (parse-number-string any-num)]
        [(byte-label? any-num) (ast-resolve-byte-scmd (base-label-str any-num) (label->byte-resolve-mode any-num))]
        [(and force
            (label? any-num)
            (not relative))
         (ast-resolve-byte-scmd (base-label-str any-num) (label->byte-resolve-mode any-num))]
        [(and force
            (label? any-num)
            relative)
         (ast-resolve-byte-scmd any-num 'relative)]
        [#t (raise-syntax-error #f (format "unknown byte operand ~a" any-num))]))

(module+ test #| byte-operand |#
  (check-equal? (byte-operand "some" #t)
                (ast-resolve-byte-scmd "some" 'low-byte))
  (check-equal? (byte-operand "some" #t #t)
                (ast-resolve-byte-scmd "some" 'relative))
  (for ((byte-expectation
         `((10      . 10)
           (0       . 0)
           (255     . 255)
           ("10"    . 10)
           ("0"     . 0)
           ("255"   . 255)
           (|10|    . 10)
           (|$10|   . 16)
           (|$FF|   . 255)
           (|%101|  . 5)
           (">some" . ,(ast-resolve-byte-scmd "some" 'high-byte)))))
    (check-equal? (byte-operand (car byte-expectation))
                  (cdr byte-expectation)
                  (format "expected: ~a == ~a"
                          (car byte-expectation)
                          (cdr byte-expectation)))))

;; resolve the given operand to a word or an open to resolve-to-word ast command
(define/c (word-operand any-num (force #f))
  (->* ((or/c symbol? number? string?)) (boolean?) (or/c number? ast-resolve-sub-cmd?))
  (cond [(symbol? any-num)
         (word-operand (symbol->string any-num) force)]
        [(number? any-num) any-num]
        [(6510-number-string? any-num) (parse-number-string any-num)]        
        [force (ast-resolve-word-scmd any-num)]
        [#t (raise-syntax-error 'word-operand (format "'~a' is no valid word operand" any-num))]))

(module+ test #| word-operand |#
  (check-equal? (word-operand "some" #t)
                (ast-resolve-word-scmd "some"))
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

;; it the given operator possibly a word operand?
(define/c (possibly-word-operand? any-num)
  (-> (or/c symbol? number? string?) boolean?)
  (or (and (symbol? any-num)
        (possibly-word-operand? (symbol->string any-num)))
     (word-operand? any-num)
     (ambiguous-operand? any-num)))

;; the given operand is neither definitively a byte nor definitively a word operand?
;; it could be one of both!
(define/c (ambiguous-operand? any-num)
  (-> any/c boolean?)
  (or (and (symbol? any-num)
        (ambiguous-operand? (symbol->string any-num)))
     (and (string? any-num)
        (label? any-num)
        (not (byte-label? any-num)))))

;; is the given operand definitively a word operand?
;; (it could also be a byte operand if it is number and in byte range, too)
(define/c (word-operand? any-num)
  (-> any/c boolean?)
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

;; is this operand an immediate byte value operand?
;; immediate values are always of byte length, so if a prefix ! is given, even a label must be of byte length
(define/c (immediate-byte-operand? sym)
  (-> any/c (or/c (listof string?) boolean?))
  (or (and (symbol? sym)
        (immediate-byte-operand? (symbol->string sym)))
     (and (string? sym)
        (string-prefix? sym "!")
        (possibly-byte-operand? (substring sym 1)))))

(module+ test #| immediate-byte-operand? |#
  (for ((immediate-byte '("!10" "!0" "!255" |!10| |!$10| |!$FF| |!%101| "!<some" "!some")))
    (check-true (immediate-byte-operand? immediate-byte)
                (format "~a not an immediate byte" immediate-byte)))
  (for ((immediate-byte '("!-1" "!256" |!-1| |!$101|)))
    (check-false (immediate-byte-operand? immediate-byte)
                 (format "~a is a byte" immediate-byte))))

;; resolve immediate operand to a byte or a resolve to byte ast command
(define/c (immediate-byte-operand sym)
  (-> (or/c symbol? string?) (or/c number? ast-resolve-sub-cmd?))
  (if (symbol? sym)
      (immediate-byte-operand (symbol->string sym))
      (byte-operand (substring sym 1) #t)))

(module+ test #| immediate-byte-operand |#
  (for ((byte-expectation
         `(("!10"   . 10)
           ("!0"    . 0)
           ("!255"  . 255)
           (|!10|   . 10)
           (|!$10|  . 16)
           (|!$FF|  . 255)
           (|!%101| . 5)
           ("!>some" . ,(ast-resolve-byte-scmd "some" 'high-byte)))))
    (check-equal? (immediate-byte-operand (car byte-expectation))
                  (cdr byte-expectation)
                  (format "expected: ~a == ~a"
                          (car byte-expectation)
                          (cdr byte-expectation)))))

;; mode-element is an addressing-mode definition
;; of the form  ''
(define/c (addressing-mode? mode-el)
  (-> any/c boolean?)
  (and (pair? mode-el)
     (symbol? (car mode-el))
     (byte? (cdr mode-el))))

;; get the first addressing mode that matches the sym
(define/c (find-addressing-mode sym addressing-modes)
  (-> any/c (listof addressing-mode?) (or/c addressing-mode? #f))
  (findf (lambda (el) (and (pair? el) (eq? (car el) sym))) addressing-modes))

(module+ test #| find-addressing-mode |#
  (check-equal? (find-addressing-mode 'accumulator '((immediate . #x10) (accumulator . #x20)))
                '(accumulator . #x20)))

;; is the given symbol in the addressing-modes?
(define/c (has-addressing-mode? sym addressing-modes)
  (-> any/c (listof addressing-mode?) boolean?)
  (pair? (find-addressing-mode sym addressing-modes)))

(module+ test #| has-addressing-mode? |#
  (check-true (has-addressing-mode? 'accumulator '((immediate . #x10) (accumulator . #x20))))
  (check-false (has-addressing-mode? 'zero-page-x '((immediate . #x10) (accumulator . #x20)))))

;; is the given operand syntax of accumulator addressing and is accumulator addressing part of the 
;; addressing modes?
(define/c (accumulator-addressing? addressing-modes-stx op-stx)
  (-> any/c syntax? boolean?)
  (and (has-addressing-mode? 'accumulator (syntax->datum addressing-modes-stx)) 
     (eq? (syntax->datum op-stx) 'A)))

(module+ test #| accumulator-addressing? |#
  (check-true (accumulator-addressing? #'((accumulator . #x20)) #'A)))

;; is the given operand syntax of byte-addressing
;; and the addressing-modes contains the addr-sym?
(define/c (byte-addressing? addr-sym addressing-modes-stx op-stx)
  (-> any/c syntax? syntax? boolean?)
  (and (has-addressing-mode? addr-sym (syntax->datum addressing-modes-stx))
     (byte-operand? (syntax->datum op-stx))))

;; is the given operand syntax of word-addressing
;; and the addressing-modes contains the addr-sym?
(define/c (word-addressing? addr-sym addressing-modes-stx op-stx)
  (-> any/c syntax? syntax? boolean?)
  (and (has-addressing-mode? addr-sym (syntax->datum addressing-modes-stx))
     (word-operand? (syntax->datum op-stx))))

;; is the given operand symbol a relative addressing operand?
(define/c (relative-addressing-operand? elem)
  (-> any/c boolean?)
  (and
   (if (symbol? elem)
       (relative-addressing-operand? (symbol->string elem))
       (or (byte-operand? elem)
          (label? elem)))
   #t))

;; is the given operand syntax of relative addressing
;; and is relative addressing in the given addressing-modes-stx?
(define/c (relative-addressing? addressing-modes-stx op-stx)
  (-> syntax? syntax? boolean?)
  (and (has-addressing-mode? 'relative (syntax->datum addressing-modes-stx))
     (relative-addressing-operand? (syntax->datum op-stx))))

;; is the given operand syntax of immediate addressing
;; and is immediate addressing in the addressing-modes-stx?
(define/c (immediate-addressing? addressing-modes-stx op-stx)
  (-> syntax? syntax? boolean?)
  (and (has-addressing-mode? 'immediate (syntax->datum addressing-modes-stx))
     (immediate-byte-operand? (syntax->datum op-stx))))

;; is this (without operand) of implicit addressing (yes)
;; and is implicit addressing in the addressing-modes-stx?
(define/c (implicit-addressing? addressing-modes-stx)
  (-> syntax? boolean?)
  (has-addressing-mode? 'implicit (syntax->datum addressing-modes-stx)))

(define/c (indirect-addressing? addressing-modes-stx op-stx)
  (-> syntax? syntax? boolean?)
  (and (has-addressing-mode? 'indirect (syntax->datum addressing-modes-stx))
     (list? (syntax->datum op-stx))
     (possibly-word-operand? (car (syntax->datum op-stx)))))

(define/c (indirect-x-addressing? addressing-modes-stx op-stx)
  (-> syntax? syntax? boolean?)
  (define op (syntax->datum op-stx))
  (and (has-addressing-mode? 'indirect-x (syntax->datum addressing-modes-stx))
     (list? op)
     (possibly-byte-operand? (car op))
     (pair? (cdr op))
     (equal? (cadr op) ',x)))

(define/c (indirect-y-addressing? addressing-modes-stx op-stx1 op-stx2)
  (-> syntax? syntax? syntax? boolean?)
  (and (has-addressing-mode? 'indirect-y (syntax->datum addressing-modes-stx))
     (list (syntax->datum op-stx1))
     (possibly-byte-operand? (car (syntax->datum op-stx1)))
     (equal? (syntax->datum op-stx2) ',y)))

(define/c (abs-or-zero-page-indexed-addressing? addressing-sym-list addressing-modes-stx op1-stx op2-stx)
  (-> (listof (cons/c symbol? any/c)) syntax? syntax? syntax? boolean?)
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

(define/c (abs-or-zero-page-addressing? addressing-list addressing-modes-stx op-stx )
  (-> (listof symbol?) syntax? syntax? boolean?)
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

(define/c (zero-page-indexed-addressing? sym op-sym addressing-modes-stx op1-stx op2-stx)
  (-> any/c any/c syntax? syntax? syntax? boolean?)
  (and (has-addressing-mode? sym (syntax->datum addressing-modes-stx))
     (byte-operand? (syntax->datum op1-stx))
     (equal? (syntax->datum op2-stx) op-sym)))

(define/c (absolute-indexed-addressing? sym op-sym addressing-modes-stx op1-stx op2-stx)
  (-> any/c any/c syntax? syntax? syntax? boolean?)
  (and (has-addressing-mode? sym (syntax->datum addressing-modes-stx))
     (word-operand? (syntax->datum op1-stx))
     (equal? (syntax->datum op2-stx) op-sym)))

(define/c (no-operand-opcode addressing addressing-modes)
  (-> symbol? (listof addressing-mode?) ast-opcode-cmd?)
  (ast-opcode-cmd '() (list (cdr (find-addressing-mode addressing addressing-modes)))))

(define/c (no-operand-opcode-w-meta addressing addressing-modes meta)
  (-> symbol? (listof addressing-mode?) list? ast-opcode-cmd?)
  (ast-opcode-cmd meta (list (cdr (find-addressing-mode addressing addressing-modes)))))

(module+ test #| opcode-without-operand |#
  (check-equal? (no-operand-opcode 'implicit '((accumulator . #x20) (implicit . #x10)))
                (ast-opcode-cmd '() '(#x10))))

(module+ test #| no-operand-opcode-w-meta |#
  (check-equal? (no-operand-opcode-w-meta 'implicit '((accumulator . #x20) (implicit . #x10)) '(#:line 17 #:some))
                (ast-opcode-cmd '(#:line 17 #:some) '(#x10))))

(define/c (zero-page-opcode addressing-modes op)
  (-> (listof addressing-mode?) any/c (or/c ast-opcode-cmd? ast-unresolved-opcode-cmd?))
  (define operand (byte-operand op))
  (if (number? operand)
      (ast-opcode-cmd '() (list (cdr (find-addressing-mode 'zero-page addressing-modes))
                            operand))
      (ast-unresolved-opcode-cmd '() (list (cdr (find-addressing-mode 'zero-page addressing-modes)))
                                 operand)))

(define/c (zero-page-opcode-w-meta addressing-modes op meta)
  (-> (listof addressing-mode?) any/c list? (or/c ast-opcode-cmd? ast-unresolved-opcode-cmd?))
  (define operand (byte-operand op))
  (if (number? operand)
      (ast-opcode-cmd meta (list (cdr (find-addressing-mode 'zero-page addressing-modes))
                            operand))
      (ast-unresolved-opcode-cmd meta (list (cdr (find-addressing-mode 'zero-page addressing-modes)))
                                 operand)))

(define/c (zero-page-indexed-opcode sym addressing-modes op)
  (-> any/c (listof addressing-mode?) any/c (or/c ast-opcode-cmd? ast-unresolved-opcode-cmd?))
  (define operand (byte-operand op))
  (if (number? operand)
      (ast-opcode-cmd '() (list (cdr (find-addressing-mode sym addressing-modes))
                            operand))
      (ast-unresolved-opcode-cmd '() (list (cdr (find-addressing-mode sym addressing-modes)))
                                 operand)))

(define/c (zero-page-indexed-opcode-w-meta sym addressing-modes op meta)
  (-> any/c (listof addressing-mode?) any/c list? (or/c ast-opcode-cmd? ast-unresolved-opcode-cmd?))
  (define operand (byte-operand op))
  (if (number? operand)
      (ast-opcode-cmd meta (list (cdr (find-addressing-mode sym addressing-modes))
                            operand))
      (ast-unresolved-opcode-cmd meta (list (cdr (find-addressing-mode sym addressing-modes)))
                                 operand)))

(define/c (immediate-opcode addressing-modes op)
  (-> (listof addressing-mode?) any/c (or/c ast-opcode-cmd? ast-unresolved-opcode-cmd?))
  (define operand (immediate-byte-operand op))
  (if (number? operand)
      (ast-opcode-cmd '() (list (cdr (find-addressing-mode 'immediate addressing-modes))
                              operand))
      (ast-unresolved-opcode-cmd '() (list (cdr (find-addressing-mode 'immediate addressing-modes)))
                                 operand)))

(define/c (immediate-opcode-w-meta addressing-modes op meta)
  (-> (listof addressing-mode?) any/c list? (or/c ast-opcode-cmd? ast-unresolved-opcode-cmd?))
  (define operand (immediate-byte-operand op))
  (if (number? operand)
      (ast-opcode-cmd meta (list (cdr (find-addressing-mode 'immediate addressing-modes))
                            operand))
      (ast-unresolved-opcode-cmd meta (list (cdr (find-addressing-mode 'immediate addressing-modes)))
                                 operand)))

(module+ test #| immediate opcode |#
  (check-equal? (immediate-opcode '((immediate . #x33)) "!$20")
                (ast-opcode-cmd '() '(#x33 #x20)))
  (check-equal? (immediate-opcode '((immediate . #x33)) "!<some")
                (ast-unresolved-opcode-cmd '() '(#x33) (ast-resolve-byte-scmd "some" 'low-byte)))
  (check-equal? (immediate-opcode '((immediate . #x33)) "!>some")
                (ast-unresolved-opcode-cmd '() '(#x33) (ast-resolve-byte-scmd "some" 'high-byte))))

(define/c (indirect-x-opcode addressing-modes op)
  (-> (listof addressing-mode?) any/c (or/c ast-opcode-cmd? ast-unresolved-opcode-cmd?))
  (define operand (byte-operand (car op) #t))
  (if (number? operand)
      (ast-opcode-cmd '() (list (cdr (find-addressing-mode 'indirect-x addressing-modes))
                            operand))
      (ast-unresolved-opcode-cmd '() (list (cdr (find-addressing-mode 'indirect-x addressing-modes)))
                                 operand)))

(define/c (indirect-x-opcode-w-meta addressing-modes op meta)
  (-> (listof addressing-mode?) any/c list? (or/c ast-opcode-cmd? ast-unresolved-opcode-cmd?))
  (define operand (byte-operand (car op) #t))
  (if (number? operand)
      (ast-opcode-cmd meta (list (cdr (find-addressing-mode 'indirect-x addressing-modes))
                            operand))
      (ast-unresolved-opcode-cmd meta (list (cdr (find-addressing-mode 'indirect-x addressing-modes)))
                                 operand)))

(define/c (relative-opcode addressing-modes op)
  (-> (listof addressing-mode?) any/c (or/c ast-rel-opcode-cmd? ast-unresolved-rel-opcode-cmd?))
  (let ((operand (byte-operand op #t #t))
        (opcode  (cdr (find-addressing-mode 'relative addressing-modes))))
    (if (number? operand)
        (ast-rel-opcode-cmd '() (list opcode operand))
        (ast-unresolved-rel-opcode-cmd
         '()
         (list opcode)
         (ast-resolve-byte-scmd (ast-resolve-sub-cmd-label operand) 'relative)))))

(define/c (relative-opcode-w-meta addressing-modes op meta)
  (-> (listof addressing-mode?) any/c list? (or/c ast-rel-opcode-cmd? ast-unresolved-rel-opcode-cmd?))
  (let ([operand (byte-operand op #t #t)]
        [opcode  (cdr (find-addressing-mode 'relative addressing-modes))])
    (if (number? operand)
        (ast-rel-opcode-cmd meta (list opcode operand))
        (ast-unresolved-rel-opcode-cmd
         meta
         (list opcode)
         (ast-resolve-byte-scmd (ast-resolve-sub-cmd-label operand) 'relative)))))

(module+ test #| relative-opcode |#
  (check-equal? (relative-opcode '((relative . #x20)) '$10)
                (ast-rel-opcode-cmd '() '(#x20 #x10)))
  (check-equal? (relative-opcode '((relative . #x20)) 'some)
                (ast-unresolved-rel-opcode-cmd '() '(#x20) (ast-resolve-byte-scmd "some" 'relative))))

(define/c (absolute-opcode addressing-modes op)
  (-> (listof addressing-mode?) any/c ast-opcode-cmd?)
  (ast-opcode-cmd '() (list (cdr (find-addressing-mode 'absolute addressing-modes))
                        (low-byte (word-operand  op))
                        (high-byte (word-operand  op)))))

(define/c (absolute-opcode-w-meta addressing-modes op meta)
  (-> (listof addressing-mode?) any/c list? ast-opcode-cmd?)
  (ast-opcode-cmd meta (list (cdr (find-addressing-mode 'absolute addressing-modes))
                        (low-byte (word-operand  op))
                        (high-byte (word-operand  op)))))

(module+ test #| absolute-opcode |#
  (check-equal? (absolute-opcode '((absolute . #x20)) '$1000)
                (ast-opcode-cmd '() '(#x20 #x00 #x10))))

(define/c (indirect-y-opcode addressing-modes op)
  (-> (listof addressing-mode?) any/c (or/c ast-opcode-cmd? ast-unresolved-opcode-cmd?))
  (define operand (byte-operand (car op) #t))
  (if (number? operand)
      (ast-opcode-cmd '() (list (cdr (find-addressing-mode 'indirect-y  addressing-modes))
                            operand))
      (ast-unresolved-opcode-cmd '() (list (cdr (find-addressing-mode 'indirect-y  addressing-modes)))
                                 operand)))

(module+ test #| indirect-y-opcode |#
  (check-equal? (indirect-y-opcode '((indirect-y . #x20)) '($10))
                (ast-opcode-cmd '() '(#x20 #x10))))

(define/c (indirect-opcode addressing-modes op)
  (-> (listof addressing-mode?) any/c (or/c ast-opcode-cmd? ast-unresolved-opcode-cmd?))
  (let ((word (word-operand (car op) #t)))
    absolute-opcode-w-meta
    (if (number? word)
        (ast-opcode-cmd
         '()
         (list (cdr (find-addressing-mode 'indirect addressing-modes))
               (low-byte word)
               (high-byte word)))
        (ast-unresolved-opcode-cmd '() (list (cdr (find-addressing-mode 'indirect addressing-modes)))
                                   word))))

(define/c (indirect-opcode-w-meta addressing-modes op meta)
  (-> (listof addressing-mode?) any/c list? (or/c ast-opcode-cmd? ast-unresolved-opcode-cmd?))
  (let ([word (word-operand (car op) #t)])
    absolute-opcode-w-meta
    (if (number? word)
        (ast-opcode-cmd
         meta
         (list (cdr (find-addressing-mode 'indirect addressing-modes))
               (low-byte word)
               (high-byte word)))
        (ast-unresolved-opcode-cmd meta (list (cdr (find-addressing-mode 'indirect addressing-modes)))
                                   word))))

(module+ test #| indirect-opcode |#
  (check-equal? (indirect-opcode '((indirect . #x20)) '($1000))
                (ast-opcode-cmd '() '(#x20 #x00 #x10))))

(define/c (absolute-indexed-opcode sym addressing-modes op)
  (-> any/c (listof addressing-mode?) any/c (or/c ast-opcode-cmd? ast-unresolved-opcode-cmd?))
  (ast-opcode-cmd '() (list (cdr (find-addressing-mode sym addressing-modes))
                        (low-byte (word-operand op))
                        (high-byte (word-operand op)))))

(module+ test #| absolute-indexed-opcode |#
  (check-equal? (absolute-indexed-opcode 'absolute-x '((absolute-x . #x20)) '$1000)
                (ast-opcode-cmd '() '(#x20 0 16))))

(define (possible-addressings addressing-sym-list addressing-modes op1 op2)
  (-> (listof (cons/c symbol? any/c))
     (listof addressing-mode?)
     any/c
     any/c
     (listof (cons/c symbol? any/c)))
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

(define/c (ambiguous-addressing-opcode possible-addressing-modes op)
  (-> (listof (cons/c symbol? any/c)) any/c (or/c ast-unresolved-opcode-cmd? ast-decide-cmd?))
  (cond [(empty? possible-addressing-modes)
         (raise-syntax-error 'ambiguous-addressing "no possible addressing mode found")]
        [(= 1 (length possible-addressing-modes))
         (let ((res (hash-ref address-mode-to-resolve-map (caar possible-addressing-modes))))
           (ast-unresolved-opcode-cmd '()
                                      (list (cdar possible-addressing-modes))
                                      (if (eq? res 'resolve-word)
                                          (ast-resolve-word-scmd (->string op))
                                          (ast-resolve-byte-scmd (->string op) 'low-byte))))]
        [(< 1 (length possible-addressing-modes))
         (ast-decide-cmd '() (map (lambda (addressing-mode)
                                (let ((resolve-strategy (hash-ref address-mode-to-resolve-map (car addressing-mode))))
                                  (ast-unresolved-opcode-cmd '()
                                   (list (cdr addressing-mode))
                                   (cond [(eq? resolve-strategy 'resolve-word)
                                          (ast-resolve-word-scmd (->string op))]
                                         [#t (ast-resolve-byte-scmd (->string op) 'low-byte)]))))
                              possible-addressing-modes) )]
        [#t (raise-syntax-error 'ambiguous-addressing "no option found for ambiguous address resolution")]))

(define/c (ambiguous-addressing-opcode-w-meta possible-addressing-modes op meta)
  (-> (listof (cons/c symbol? any/c)) any/c list? (or/c ast-unresolved-opcode-cmd? ast-decide-cmd?))
  (cond [(empty? possible-addressing-modes)
         (raise-syntax-error 'ambiguous-addressing "no possible addressing mode found")]
        [(= 1 (length possible-addressing-modes))
         (let ([res (hash-ref address-mode-to-resolve-map (caar possible-addressing-modes))])
           (ast-unresolved-opcode-cmd meta
                                      (list (cdar possible-addressing-modes))
                                      (if (eq? res 'resolve-word)
                                          (ast-resolve-word-scmd (->string op))
                                          (ast-resolve-byte-scmd (->string op) 'low-byte))))]
        [(< 1 (length possible-addressing-modes))
         (ast-decide-cmd meta (map (lambda (addressing-mode)
                                (let ([resolve-strategy (hash-ref address-mode-to-resolve-map (car addressing-mode))])
                                  (ast-unresolved-opcode-cmd
                                   meta
                                   (list (cdr addressing-mode))
                                   (cond [(eq? resolve-strategy 'resolve-word)
                                          (ast-resolve-word-scmd (->string op))]
                                         [else (ast-resolve-byte-scmd (->string op) 'low-byte)]))))
                              possible-addressing-modes) )]
        [else (raise-syntax-error 'ambiguous-addressing "no option found for ambiguous address resolution")]))

(define/c (abs-or-zero-page-indexed-opcode addressing-sym-list addressing-modes op1 op2)
  (-> (listof (cons/c symbol? any/c)) (listof addressing-mode?) any/c any/c (or/c ast-unresolved-opcode-cmd? ast-decide-cmd?))
  (define pos-addressings (possible-addressings addressing-sym-list addressing-modes op1 op2))
  (define possible-addressing-modes
    (filter (λ (addressing-mode)
              (member (car addressing-mode) pos-addressings))
            addressing-modes))
  (ambiguous-addressing-opcode possible-addressing-modes op1))

(define/c (abs-or-zero-page-indexed-opcode-w-meta addressing-sym-list addressing-modes op1 op2 meta)
  (-> (listof (cons/c symbol? any/c)) (listof addressing-mode?) any/c any/c list? (or/c ast-unresolved-opcode-cmd? ast-decide-cmd?))
  (define pos-addressings (possible-addressings addressing-sym-list addressing-modes op1 op2))
  (define possible-addressing-modes
    (filter (λ (addressing-mode)
              (member (car addressing-mode) pos-addressings))
            addressing-modes))
  (ambiguous-addressing-opcode-w-meta possible-addressing-modes op1 meta))

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
                (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-word-scmd "some")))
  (check-equal? (abs-or-zero-page-indexed-opcode
                 '((zero-page-x . ,x)
                   (absolute-x . ,x))
                 '((absolute-x . #x20)
                   (zero-page-x . #x30))
                 'some
                 ',x)
                (ast-decide-cmd
                 '()
                 (list
                  (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-word-scmd "some"))
                  (ast-unresolved-opcode-cmd '()'(#x30) (ast-resolve-byte-scmd "some" 'low-byte))))))

(define/c (abs-or-zero-page-opcode addressing-list addressing-modes op)
  (-> (listof symbol?) (listof addressing-mode?) any/c (or/c ast-unresolved-opcode-cmd? ast-decide-cmd?))
  (define pos-addressings
    (filter (lambda (addressing)
              (has-addressing-mode? addressing addressing-modes))
            addressing-list))
  (define possible-addressing-modes
    (filter (λ (addressing-mode)
              (member (car addressing-mode) pos-addressings))
            addressing-modes))
  (ambiguous-addressing-opcode possible-addressing-modes op))

(define/c (abs-or-zero-page-opcode-w-meta addressing-list addressing-modes op meta)
  (-> (listof symbol?) (listof addressing-mode?) any/c list? (or/c ast-unresolved-opcode-cmd? ast-decide-cmd?))
  (define pos-addressings
    (filter (lambda (addressing)
              (has-addressing-mode? addressing addressing-modes))
            addressing-list))
  (define possible-addressing-modes
    (filter (λ (addressing-mode)
              (member (car addressing-mode) pos-addressings))
            addressing-modes))
  (ambiguous-addressing-opcode-w-meta possible-addressing-modes op meta))

(define/c (raise-addressing-error stx addressing-modes-stx num)
  (-> syntax? syntax? exact-integer? syntax?)
  (raise-syntax-error
   'mnemonic
   ;; report available addressing modes expected for one op
   (format "addressing mode not recognized.\nexpecting ~a operand(s).\nallowed addressing modes are ~a.\n  in line ~a:~a"
           num
           (string-join (map (lambda (addr-pair) (symbol->string (car addr-pair))) (syntax->datum addressing-modes-stx))
                        ", ")
           (syntax-line stx)
           (syntax-column stx))
   stx))


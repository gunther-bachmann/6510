#lang racket

(provide (all-from-out racket/list)
         (all-from-out "6510-utils.rkt")
         (all-from-out "ast/6510-command.rkt")
         (all-from-out "ops/6510.arithmetic-ops.rkt")
         (all-from-out "ops/6510.branch-ops.rkt")
         (all-from-out "ops/6510.compare-ops.rkt")
         (all-from-out "ops/6510.flag-ops.rkt")
         (all-from-out "ops/6510.increment-ops.rkt")
         (all-from-out "ops/6510.logic-ops.rkt")
         (all-from-out "ops/6510.memory-ops.rkt")
         (all-from-out "ops/6510.misc-ops.rkt")
         (all-from-out "ops/6510.shift-ops.rkt")
         (all-from-out "ops/6510.stack-ops.rkt")
         (all-from-out "ops/6510.subroutine-ops.rkt")
         (all-from-out "ops/6510.transfer-ops.rkt")
         (all-from-out "scheme-asm/6510-addressing-utils.rkt")
         (all-from-out "tools/data-tools.rkt")
         asc                            ;; define string memory bytes
         byte                           ;; define memory bytes
         byte-const                     ;; define assembler byte constant
         byte-ref                       ;; define assembler byte reference
         label                          ;; define label
         org                            ;; define origin of code
         org-align                      ;; define origin by alignment to current position
         provide-byte                   ;; provide byte for linker of other files
         provide-word                   ;; provide word for linker of other files
         require-byte                   ;; require byte from linking other files
         require-word                   ;; require word from linking other files
         word                           ;; define memory word
         word-const                     ;; define assembler word constant
         word-ref)                      ;; define an assembler word reference

#|

 provide all functions necessary to write 6510 assembler code

 central entry for syntax transformation rules for the translation from 6510 dsl code
 into 6510 byte/assembler code.

 |#

(require (for-syntax "tools/data-tools.rkt")
         (for-syntax "6510-utils.rkt")
         (for-syntax "ast/6510-command.rkt")
         (for-syntax "scheme-asm/6510-syntax-utils.rkt")
         (only-in racket/list flatten)
         "6510-utils.rkt"
         "ast/6510-command.rkt"
         "ops/6510.arithmetic-ops.rkt"
         "ops/6510.branch-ops.rkt"
         "ops/6510.compare-ops.rkt"
         "ops/6510.flag-ops.rkt"
         "ops/6510.increment-ops.rkt"
         "ops/6510.logic-ops.rkt"
         "ops/6510.memory-ops.rkt"
         "ops/6510.misc-ops.rkt"
         "ops/6510.shift-ops.rkt"
         "ops/6510.stack-ops.rkt"
         "ops/6510.subroutine-ops.rkt"
         "ops/6510.transfer-ops.rkt"
         "scheme-asm/6510-addressing-utils.rkt"
         "tools/data-tools.rkt")

(module+ test #| require test utils |#
  (require "6510-test-utils.rkt"))

;;--------------------------------------------------------------------------------
;; https://docs.racket-lang.org/reference/syntax-util.html
;; (format-id ...)

;; https://blog.racket-lang.org/2011/04/writing-syntax-case-macros.html
;;--------------------------------------------------------------------------------

(define-syntax (org stx)
  (syntax-case stx ()
    ([_ str]
     #'(ast-org-command '() (syntax->datum #'str)))))

(define-syntax (org-align stx)
  (syntax-case stx ()
    ([_ str]
     #'(ast-org-align-command '() (syntax->datum #'str)))))

(define-syntax (label stx)
  (syntax-case stx ()
    ([_ str]
     #'(ast-label-def-cmd '() (->string #'str)))))

(module+ test #| label |#
  (check-equal?
   (label some)
   (ast-label-def-cmd '() "some"))
  (check-equal?
   (label "some")
   (ast-label-def-cmd '() "some")))

(define-syntax (byte-const stx)
  (syntax-case stx ()
    ([_ label byte]
     (and (6510-number-string? (->string #'byte))
        (in-byte-range? (parse-number-string (->string #'byte))))
     #'(ast-const-byte-cmd
        '()
        (->string #'label)
         (parse-number-string (->string #'byte))))))

(module+ test #| byte-const |#
  (check-equal?
   (byte-const some 10)
   (ast-const-byte-cmd '() "some" 10))
  (check-equal?
   (byte-const some %10)
   (ast-const-byte-cmd '() "some" #b10))
  (check-equal?
   (byte-const some $10)
   (ast-const-byte-cmd '() "some" #x10))
  (check-exn exn:fail:syntax? (λ () (expand #'(byte-const some $100)))))

(define-syntax (word-const stx)
  (syntax-case stx ()
    ([_ label word]
     (and (6510-number-string? (->string #'word))
        (in-word-range? (parse-number-string (->string #'word))))
     #'(ast-const-word-cmd
        '()
        (->string #'label)
        (parse-number-string (->string #'word))))))

(module+ test #| word-const |#
  (check-equal?
   (word-const some 1000)
   (ast-const-word-cmd '() "some" 1000))
  (check-equal?
   (word-const some %100010001000)
   (ast-const-word-cmd '() "some" #b100010001000))
  (check-equal?
   (word-const some $2000)
   (ast-const-word-cmd '() "some" #x2000))
  (check-exn exn:fail:syntax? (λ () (expand #'(word-const some $10000)))))

(define-for-syntax (byte-number-check val)
  (let ([str-val (->string val)])
    (and (6510-number-string? str-val)
       (in-byte-range? (parse-number-string str-val)))))

(define-syntax (byte stx)
  (syntax-case stx ()
    ([_ meta-info byte ...]
     (cond [(and (meta-info? #'meta-info)
               (with-syntax (((is-byte-number ...)
                              (map byte-number-check (syntax->list #'(byte ...)))))
                 (all #'(is-byte-number ...))))
            #'(ast-bytes-cmd meta-info (list (parse-number-string (->string #'byte)) ...))]
           [(and (6510-number-string? (->string #'meta-info))
               (in-byte-range? (parse-number-string (->string #'meta-info)))
               (with-syntax (((is-byte-number ...)
                              (map byte-number-check (syntax->list #'(byte ...)))))
                 (all #'(is-byte-number ...))))
            #'(ast-bytes-cmd '() (list (parse-number-string (->string #'meta-info)) (parse-number-string (->string #'byte)) ...))]
           [else (raise-syntax-error 'byte-constant "byte cannot be parsed")]))))

(module+ test #| byte |#
  (check-equal? (byte '(#:line 17) $10 $FF $D2 %10010000 %11111111)
                (ast-bytes-cmd '(#:line 17) '(#x10 #xFF #xD2 #b10010000 #b11111111)))
  (check-equal? (byte "$10" "$FF" "$D2" "%10010000" "%11111111")
                (ast-bytes-cmd '() '(#x10 #xFF #xD2 #b10010000 #b11111111)))
  (check-equal? (byte "$10" "$FF" "$D2" "%10010000" "%11111111")
                (ast-bytes-cmd '() '(#x10 #xFF #xD2 #b10010000 #b11111111)))
  (check-equal? (byte "$10")
                (ast-bytes-cmd '() '(#x10)))
  (check-equal? (byte '(#:line 23) "$10")
                (ast-bytes-cmd '(#:line 23) '(#x10)))
  (check-exn exn:fail:syntax? (λ () (expand #'(byte $100))))
  (check-exn exn:fail:syntax? (λ () (expand #'(byte '(#:line 17) $100))))
  (check-exn exn:fail:syntax? (λ () (expand #'(byte '(#:line 17) $10 $100))))
  (check-exn exn:fail:syntax? (λ () (expand #'(byte $10 $100)))))

(define-syntax (provide-word stx)
  (syntax-case stx ()
    ([_ label]
     #'(ast-provide-word-cmd '() (->string #'label)))))

(module+ test #| |#
  (check-equal? (provide-word "some")
                (ast-provide-word-cmd '() "some"))
  (check-equal? (provide-word some)
                (ast-provide-word-cmd '()  "some")))

(define-syntax (provide-byte stx)
  (syntax-case stx ()
    ([_ label]
     #'(ast-provide-byte-cmd '() (->string #'label)))))

(module+ test #| |#
  (check-equal? (provide-byte "some")
                (ast-provide-byte-cmd '() "some"))
  (check-equal? (provide-byte some)
                (ast-provide-byte-cmd '() "some")))

(define-syntax (require-byte stx)
  (syntax-case stx ()
    ([_ label]
     #'(ast-require-byte-cmd '() (->string #'label)))))

(module+ test #| |#
  (check-equal? (require-byte "some")
                (ast-require-byte-cmd '() "some"))
  (check-equal? (require-byte some)
                (ast-require-byte-cmd '() "some")))

(define-syntax (require-word stx)
  (syntax-case stx ()
    ([_ label]
     #'(ast-require-word-cmd '() (->string #'label)))))

(module+ test #| |#
  (check-equal? (require-word "some")
                (ast-require-word-cmd '() "some"))
  (check-equal? (require-word some)
                (ast-require-word-cmd '() "some")))

(define-for-syntax (parse-syntax-number val-stx)
  (parse-number-string (->string val-stx)))

(define-for-syntax (all stx-list)
  (foldl (λ (l r) (and (syntax->datum l) r))
         #t
         (syntax->list stx-list)))

(define-syntax (word-ref stx)
  (syntax-case stx ()
    ([_ ref ...]
     #'(list (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd (->string #'ref))) ...))))

(module+ test #| word-ref |#
  (check-equal? (word-ref some+4)
                (list (ast-unresolved-bytes-cmd '() '()  (ast-resolve-word-scmd "some+4"))))
  (check-equal? (word-ref some+4 some+2)
                (list (ast-unresolved-bytes-cmd '() '()  (ast-resolve-word-scmd "some+4"))
                      (ast-unresolved-bytes-cmd '() '()  (ast-resolve-word-scmd "some+2")))))

(define-syntax (byte-ref stx)
  (syntax-case stx ()
    ([_ ref ...]
     #'(list (ast-unresolved-bytes-cmd
              '() '()
              (ast-resolve-byte-scmd (string-replace (->string #'ref) #rx"^[><]" "")
                                     (cond [(string-prefix? (->string #'ref) ">") 'high-byte]
                                           [else 'low-byte]))) ...))))

(module+ test #| byte-ref |#
  (check-equal? (byte-ref >some)
                (list (ast-unresolved-bytes-cmd '() '() (ast-resolve-byte-scmd "some" 'high-byte))))
  (check-equal? (byte-ref <some)
                (list (ast-unresolved-bytes-cmd '() '() (ast-resolve-byte-scmd "some" 'low-byte))))
  (check-equal? (byte-ref some)
                (list (ast-unresolved-bytes-cmd '() '() (ast-resolve-byte-scmd "some" 'low-byte)))))

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
       #'(ast-bytes-cmd '() (map syntax->datum
                             (flatten (map list (syntax->list #'(low-bytes ...))
                                           (syntax->list #'(high-bytes ...))))))))))

(module+ test #| word |#
  (check-equal? (word $1000 $FFD2 %1001000011111111)
                (ast-bytes-cmd '() '(#x00 #x10 #xD2 #xFF #b11111111 #b10010000)))
  (check-exn exn:fail:syntax? (λ () (expand #'(word $1000 $10000)))))

(define (c64-char->byte char)
  (char->integer char))

(define-syntax (asc stx)
  (syntax-case stx ()
    ([_ str]
     (string? (syntax->datum #'str))
     #'(ast-bytes-cmd '()  (map c64-char->byte (string->list (->string #'str)))))))

(module+ test #| asc |#
  (check-equal? (asc "some")
                (ast-bytes-cmd '() '(115 111 109 101))))

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

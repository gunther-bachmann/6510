#lang racket

;; central entry for syntax transformation rules for the translation from 6510 dsl code
;; into 6510 byte/assembler code.

(require "6510-addressing-utils.rkt")
(require "6510-utils.rkt")
(require (for-syntax "6510-utils.rkt"))

(require (for-syntax "ast/6510-command.rkt"))
(require "ast/6510-command.rkt")

(require "ops/6510.logic-ops.rkt")
(require "ops/6510.branch-ops.rkt")
(require "ops/6510.arithmetic-ops.rkt")
(require "ops/6510.increment-ops.rkt")
(require "ops/6510.flag-ops.rkt")
(require "ops/6510.memory-ops.rkt")
(require "ops/6510.transfer-ops.rkt")
(require "ops/6510.shift-ops.rkt")
(require "ops/6510.misc-ops.rkt")
(require "ops/6510.compare-ops.rkt")
(require "ops/6510.subroutine-ops.rkt")
(require "ops/6510.stack-ops.rkt")

(provide (all-from-out "ast/6510-command.rkt"))

(provide (all-from-out "ops/6510.logic-ops.rkt"))
(provide (all-from-out "ops/6510.branch-ops.rkt"))
(provide (all-from-out "ops/6510.arithmetic-ops.rkt"))
(provide (all-from-out "ops/6510.increment-ops.rkt"))
(provide (all-from-out "ops/6510.flag-ops.rkt"))
(provide (all-from-out "ops/6510.memory-ops.rkt"))
(provide (all-from-out "ops/6510.transfer-ops.rkt"))
(provide (all-from-out "ops/6510.shift-ops.rkt"))
(provide (all-from-out "ops/6510.misc-ops.rkt"))
(provide (all-from-out "ops/6510.compare-ops.rkt"))
(provide (all-from-out "ops/6510.subroutine-ops.rkt"))
(provide (all-from-out "ops/6510.stack-ops.rkt"))

(provide label word-const byte-const byte word asc provide-byte provide-word require-byte require-word) ;; meta commands

(provide (all-from-out "6510-addressing-utils.rkt"))

(module+ test
  (require "6510-test-utils.rkt"))

;;--------------------------------------------------------------------------------
;; https://docs.racket-lang.org/reference/syntax-util.html
;; (format-id ...)

;; https://blog.racket-lang.org/2011/04/writing-syntax-case-macros.html
;;--------------------------------------------------------------------------------

(define-syntax (label stx)
  (syntax-case stx ()
    ([_ str]
     #'(ast-label-def-cmd (->string #'str)))))

(module+ test #| label |#
  (check-equal?
   (label some)
   (ast-label-def-cmd "some"))
  (check-equal?
   (label "some")
   (ast-label-def-cmd "some")))

(define-syntax (byte-const stx)
  (syntax-case stx ()
    ([_ label byte]
     (and (6510-number-string? (->string #'byte))
        (in-byte-range? (parse-number-string (->string #'byte))))
     #'(ast-const-byte-cmd
         (->string #'label)
         (parse-number-string (->string #'byte))))))

(module+ test #| byte-const |#
  (check-equal?
   (byte-const some 10)
   (ast-const-byte-cmd "some" 10))
  (check-equal?
   (byte-const some %10)
   (ast-const-byte-cmd "some" #b10))
  (check-equal?
   (byte-const some $10)
   (ast-const-byte-cmd "some" #x10))
  (check-exn exn:fail:syntax? (λ () (expand #'(byte-const some $100)))))

(define-syntax (word-const stx)
  (syntax-case stx ()
    ([_ label word]
     (and (6510-number-string? (->string #'word))
        (in-word-range? (parse-number-string (->string #'word))))
     #'(ast-const-word-cmd
        (->string #'label)
        (parse-number-string (->string #'word))))))

(module+ test #| word-const |#
  (check-equal?
   (word-const some 1000)
   (ast-const-word-cmd "some" 1000))
  (check-equal?
   (word-const some %100010001000)
   (ast-const-word-cmd "some" #b100010001000))
  (check-equal?
   (word-const some $2000)
   (ast-const-word-cmd "some" #x2000))
  (check-exn exn:fail:syntax? (λ () (expand #'(word-const some $10000)))))

(define-syntax (byte stx)
  (syntax-case stx ()
    ([_ byte ...]
     (with-syntax (((is-byte-number ...)
                    (map (λ (val)
                           (let ((str-val (->string val)))
                             (and (6510-number-string? str-val)
                                (in-byte-range? (parse-number-string str-val)))))
                         (syntax->list #'(byte ...)))))
       (all #'(is-byte-number ...)))
     #'(ast-bytes-cmd (list (parse-number-string (->string #'byte)) ...)))))

(module+ test #| byte |#
  (check-equal? (byte $10 $FF $D2 %10010000 %11111111)
                (ast-bytes-cmd '(#x10 #xFF #xD2 #b10010000 #b11111111)))
  (check-equal? (byte "$10" "$FF" "$D2" "%10010000" "%11111111")
                (ast-bytes-cmd '(#x10 #xFF #xD2 #b10010000 #b11111111)))
  (check-exn exn:fail:syntax? (λ () (expand #'(byte $10 $100)))))

(define-syntax (provide-word stx)
  (syntax-case stx ()
    ([_ label]
     #'(ast-provide-word-cmd (->string #'label)))))

(module+ test #| |#
  (check-equal? (provide-word "some")
                (ast-provide-word-cmd "some"))
  (check-equal? (provide-word some)
                (ast-provide-word-cmd "some")))

(define-syntax (provide-byte stx)
  (syntax-case stx ()
    ([_ label]
     #'(ast-provide-byte-cmd (->string #'label)))))

(module+ test #| |#
  (check-equal? (provide-byte "some")
                (ast-provide-byte-cmd "some"))
  (check-equal? (provide-byte some)
                (ast-provide-byte-cmd "some")))

(define-syntax (require-byte stx)
  (syntax-case stx ()
    ([_ label]
     #'(ast-require-byte-cmd (->string #'label)))))

(module+ test #| |#
  (check-equal? (require-byte "some")
                (ast-require-byte-cmd "some"))
  (check-equal? (require-byte some)
                (ast-require-byte-cmd "some")))

(define-syntax (require-word stx)
  (syntax-case stx ()
    ([_ label]
     #'(ast-require-word-cmd (->string #'label)))))

(module+ test #| |#
  (check-equal? (require-word "some")
                (ast-require-word-cmd "some"))
  (check-equal? (require-word some)
                (ast-require-word-cmd "some")))

(define-for-syntax (parse-syntax-number val-stx)
  (parse-number-string (->string val-stx)))

(define-for-syntax (all stx-list)
  (foldl (λ (l r) (and (syntax->datum l) r))
         #t
         (syntax->list stx-list)))

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
       #'(ast-bytes-cmd (map syntax->datum
                             (flatten (map list (syntax->list #'(low-bytes ...))
                                           (syntax->list #'(high-bytes ...))))))))))

(module+ test #| word |#
  (check-equal? (word $1000 $FFD2 %1001000011111111)
                (ast-bytes-cmd '(#x00 #x10 #xD2 #xFF #b11111111 #b10010000)))
  (check-exn exn:fail:syntax? (λ () (expand #'(word $1000 $10000)))))

(define (c64-char->byte char)
  (char->integer char))

(define-syntax (asc stx)
  (syntax-case stx ()
    ([_ str]
     (string? (syntax->datum #'str))
     #'(ast-bytes-cmd (map c64-char->byte (string->list (->string #'str)))))))

(module+ test #| asc |#
  (check-equal? (asc "some")
                (ast-bytes-cmd '(115 111 109 101))))

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

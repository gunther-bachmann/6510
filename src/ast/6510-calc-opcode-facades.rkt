#lang racket

(require "../6510.rkt")

(provide label-def
         BNE-relative
         CMP-immediate
         JMP-absolute
         JSR-absolute
         LDA-immediate
         LDX-immediate)

(module+ test #| setup |#
  (require "../6510-test-utils.rkt"))


(define/contract (-pass-expression command-symbol expression)
  (-> symbol? any/c ast-command?)
  (with-syntax ((e expression)
                (s command-symbol))
    (eval-syntax #'(s e))))

(define/contract (label-def target)
  (-> symbol? ast-label-def-cmd?)
  (with-syntax ((e target))
    (eval-syntax #'(label e))))

(module+ test #| label-def |#
  (check-equal? (label-def 'some)
                (label some)))

(define/contract (BNE-relative target)
  (-> symbol? ast-command?)
  (-pass-expression 'BNE target))

(module+ test #| BNE-relative |#
  (check-equal? (BNE-relative 'some)
                (BNE some)))

(define/contract (CMP-immediate expr)
  (-> exact-nonnegative-integer? ast-opcode-cmd?)
  (-pass-expression 'CMP (string-append "!" (number->string expr))))

(module+ test #| CMP-immediate |#
  (check-equal? (CMP-immediate (+ 1 2))
                (CMP !3)))

(define/contract (JMP-absolute target)
  (-> symbol? ast-command?)
  (-pass-expression 'JMP target))

(module+ test #| JMP-absolute |#
  (check-equal? (JMP-absolute 'some)
                (JMP some)))

(define/contract (JSR-absolute target)
  (-> symbol? ast-opcode-cmd?)
  (-pass-expression 'JSR target))

(module+ test #| JSR-absolute |#
  (check-equal? (JSR-absolute (string->symbol "some"))
                (JSR some)))

;; (define-syntax (LDA-gen stx)
;;   (syntax-case stx ()
;;     ([_ fun] #'(define (LDA-immediate expr) (-pass-expression 'LDA (fun expr))))))

;; (LDA-gen (lambda (expr) (string-append "!" (number->string expr))))

;; (define (mnemonic-gen addressing-modes)
;;   (if (empty? addressing-modes)
;;       (void)
;;       (begin
;;         (mnemonic-gen (cdr addressing-modes)))))

(define/contract (LDA-immediate expr)
  (-> exact-nonnegative-integer? ast-opcode-cmd?)
  (-pass-expression 'LDA (string-append "!" (number->string expr))))

(module+ test #| LDA-immediate |#
  (define locally-defined 2)
  (check-equal? (LDA-immediate (+ 1 locally-defined))
                (ast-opcode-cmd '() (list 169 3))))

(define/contract (LDX-immediate expr)
  (-> exact-nonnegative-integer? ast-opcode-cmd?)
  (-pass-expression 'LDX (string-append "!" (number->string expr))))

#lang racket

(require "6510-alt-command.rkt")

(module+ test
  (require "6510-test-utils.rkt"))

(provide ->resolved-decisions label-instructions resolve-word? resolve-byte?)

(define (resolve-word? res)
  (and (list? res)
     (eq? (car res) 'resolve-word)))

(define (resolve-byte? res)
  (and (list? res)
     (eq? (car res) 'resolve-byte)))

(define label-types '(byte word))
;; (define label-definitions
;;   '(
;;     label-def
;;     byte-const-def
;;     word-const-def
;;     ))

(define (is-byte-label? instruction)
  (ast-const-byte-cmd? instruction))

(define (is-word-label? instruction)
  (or (ast-const-word-cmd? instruction)
     (ast-label-def-cmd? instruction)))

(module+ test #| is-word-label? |#
  (check-true (is-word-label? (ast-label-def-cmd "some")))
  (check-true (is-word-label? (ast-const-word-cmd "some" #x2000)))
  (check-false (is-word-label? (ast-const-byte-cmd "some" #x20))))

(define (first-word-label-in label program)
  (findf (λ (instruction)
           (define instruction-word-label
             (cond [(ast-const-word-cmd? instruction)
                    (ast-const-word-cmd-label instruction)]
                   [(ast-label-def-cmd? instruction)
                    (ast-label-def-cmd-label instruction)]
                   [#t '()]))
           (equal? label instruction-word-label))
         program))

(module+ test #| is-word-label? |#
  (check-equal? (first-word-label-in "some" (list (ast-label-def-cmd "some")))
                (ast-label-def-cmd "some"))
  (check-equal? (first-word-label-in "other" (list (ast-label-def-cmd "some")))
                #f)
  (check-equal? (first-word-label-in "some" (list (ast-label-def-cmd "other")
                                              (ast-const-word-cmd "some" #x3000)))
                (ast-const-word-cmd "some" #x3000)))

(define (first-label-in label program)
  (findf (λ (instruction)
           (define label-str
             (cond   [(ast-label-def-cmd? instruction)
                      (ast-label-def-cmd-label instruction)]
                     [(ast-const-word-cmd? instruction)
                      (ast-const-word-cmd-label instruction)]
                     [(ast-const-byte-cmd? instruction)
                      (ast-const-byte-cmd-label instruction)]))
           (equal? label label-str))
         program))

(define (is-label-instruction? instruction)
  (or (ast-label-def-cmd? instruction)
     (ast-const-word-cmd? instruction)
     (ast-const-byte-cmd? instruction)))

(module+ test #| is-label-instruction? |#
  (check-not-false (is-label-instruction? (ast-label-def-cmd "some")))
  (check-not-false (is-label-instruction? (ast-const-word-cmd "some" #x2000)))
  (check-not-false (is-label-instruction? (ast-const-byte-cmd "some" #x20)))
  (check-false (is-label-instruction? '(opcode #xea))))

(define (label-instructions program)
  (filter is-label-instruction? program))

(define (matching-decide-option labels decide-options)
  (findf (λ (decide-option)
           (match-let (((list (list resolver-tag label) _ ...) decide-option))
             (define label-entry (first-label-in label labels))
             (if label-entry
                 (cond [(eq? 'resolve-byte resolver-tag)
                        (is-byte-label? label-entry)]
                       [(eq? 'resolve-word resolver-tag)
                        (is-word-label? label-entry)]
                       [#t #f])
                 #f)))
         decide-options))

(module+ test #| matching-decide-option |#
  (check-equal? (matching-decide-option (list (ast-label-def-cmd "hello"))
                                        '(((resolve-byte "hello") opcode 166)
                                          ((resolve-word "hello") opcode 174)))
                '((resolve-word "hello") opcode 174))
  (check-equal? (matching-decide-option (list (ast-const-word-cmd "hello" #x2000))
                                        '(((resolve-byte "hello") opcode 166)
                                          ((resolve-word "hello") opcode 174)))
                '((resolve-word "hello") opcode 174))
  (check-equal? (matching-decide-option (list (ast-const-byte-cmd "hello" #x20))
                                        '(((resolve-byte "hello") opcode 166)
                                          ((resolve-word "hello") opcode 174)))
                '((resolve-byte "hello") opcode 166)))

(define (decide-option->instruction decide-option)
  (append (cdr decide-option) (list (car decide-option))))

(define (->resolved-decisions labels program)
  (if (empty? program)
      '()
      (let* ((instruction (car program))
             (tag (car instruction))
             (default-result-f (λ () (cons instruction (->resolved-decisions labels (cdr program))))))
        (cond [(eq? tag 'decide)
               (let* ((options (cadr instruction))
                      (moption (matching-decide-option labels options)))
                 (if moption
                     (cons (decide-option->instruction moption) (->resolved-decisions labels (cdr program)))
                     (default-result-f)))]
              [#t
               (default-result-f)]))))

(module+ test #| ->resolved-decisions |#
  (check-equal? (->resolved-decisions (list (ast-label-def-cmd "hello"))
                                    '((decide (((resolve-byte "hello") opcode 166)
                                               ((resolve-word "hello") opcode 174)))))
                '((opcode 174 (resolve-word "hello"))))
  (check-equal? (->resolved-decisions (list (ast-const-byte-cmd "hello" #x20))
                                    '((decide (((resolve-byte "hello") opcode 166)
                                               ((resolve-word "hello") opcode 174)))))
                '((opcode 166 (resolve-byte "hello"))))
  (check-equal? (->resolved-decisions (list (ast-const-word-cmd "hello" #x2000))
                                    '((decide (((resolve-byte "hello") opcode 166)
                                               ((resolve-word "hello") opcode 174)))))
                '((opcode 174 (resolve-word "hello")))
                "decide for word if referencing word label")
  (check-equal? (->resolved-decisions (list (ast-label-def-cmd "not-found"))
                                    '((decide (((resolve-byte "hello") opcode 166)
                                               ((resolve-word "hello") opcode 174)))))
                '((decide (((resolve-byte "hello") opcode 166)
                           ((resolve-word "hello") opcode 174))))
                "nothing is decided if label is not found"))

#lang racket

(require "6510-alt-command.rkt")

(module+ test
  (require "6510-test-utils.rkt"))

(provide ->resolved-decisions label-instructions)

(define (is-byte-label-cmd? instruction)
  (ast-const-byte-cmd? instruction))

(define (is-word-label-cmd? instruction)
  (or (ast-const-word-cmd? instruction)
     (ast-label-def-cmd? instruction)))

(module+ test #| is-word-label? |#
  (check-true (is-word-label-cmd? (ast-label-def-cmd "some")))
  (check-true (is-word-label-cmd? (ast-const-word-cmd "some" #x2000)))
  (check-false (is-word-label-cmd? (ast-const-byte-cmd "some" #x20))))

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

(module+ test #| first-label-in |#
  (check-equal? (first-label-in "some" (list (ast-label-def-cmd "hello")))
                #f)
  (check-equal? (first-label-in "hello" (list (ast-label-def-cmd "hello")))
                (ast-label-def-cmd "hello")))

(define (is-label-instruction? instruction)
  (or (ast-label-def-cmd? instruction)
     (ast-const-word-cmd? instruction)
     (ast-const-byte-cmd? instruction)))

(module+ test #| is-label-instruction? |#
  (check-not-false (is-label-instruction? (ast-label-def-cmd "some")))
  (check-not-false (is-label-instruction? (ast-const-word-cmd "some" #x2000)))
  (check-not-false (is-label-instruction? (ast-const-byte-cmd "some" #x20)))
  (check-false (is-label-instruction? (ast-opcode-cmd '(#xea)))))

(define (label-instructions program)
  (filter is-label-instruction? program))

(define (matching-decide-option labels decide-options)
  (findf (λ (decide-option)
           (define resolve-scmd (ast-unresolved-opcode-cmd-resolve-sub-command decide-option))
           (define label (ast-resolve-sub-cmd-label resolve-scmd))
           (define label-entry (first-label-in label labels))
           (if label-entry
               (cond [(ast-resolve-byte-scmd? resolve-scmd)
                      (is-byte-label-cmd? label-entry)]
                     [(ast-resolve-word-scmd? resolve-scmd)
                      (is-word-label-cmd? label-entry)]
                     [#t #f])
               #f))
         decide-options))

(module+ test #| matching-decide-option |#
  (check-equal? (matching-decide-option
                 (list (ast-label-def-cmd "hello"))
                 (list (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                       (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello"))))
                (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello")))
  (check-equal? (matching-decide-option
                 (list (ast-const-word-cmd "hello" #x2000))
                 (list (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                       (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello"))))
                (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello")))
  (check-equal? (matching-decide-option
                 (list (ast-const-byte-cmd "hello" #x20))
                 (list (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                       (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello"))))
                (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))))

(define (->resolved-decisions labels program)
  (if (empty? program)
      '()
      (let* ((instruction (car program))
             (default-result-f (λ () (cons instruction (->resolved-decisions labels (cdr program))))))
        (cond [(ast-decide-cmd? instruction)
               (let* ((options (ast-decide-cmd-options instruction))
                      (moption (matching-decide-option labels options)))
                 (if moption
                     (cons moption (->resolved-decisions labels (cdr program)))
                     (default-result-f)))]
              [#t
               (default-result-f)]))))

(module+ test #| ->resolved-decisions |#
  (check-equal? (->resolved-decisions
                 (list (ast-label-def-cmd "hello"))
                 (list
                  (ast-decide-cmd
                   (list (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                         (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello"))))))
                (list (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello"))))
  (check-equal? (->resolved-decisions
                 (list (ast-const-byte-cmd "hello" #x20))
                 (list
                  (ast-decide-cmd
                   (list (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                         (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello"))))))
                (list (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))))
  (check-equal? (->resolved-decisions
                 (list (ast-const-word-cmd "hello" #x2000))
                 (list
                  (ast-decide-cmd
                   (list (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                         (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello"))))))
                (list (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello"))))
  (check-equal? (->resolved-decisions
                 (list (ast-label-def-cmd "not-found"))
                 (list
                  (ast-decide-cmd
                   (list (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                         (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello"))))))
                (list
                 (ast-decide-cmd
                  (list (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                        (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello")))))
                "nothing is decided if label is not found"))

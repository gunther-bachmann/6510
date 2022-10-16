#lang racket

(module+ test
  (require "6510-test-utils.rkt"))

(provide ->resolved-decisions label-instructions)

(define label-types '(byte word))
(define label-definitions
  '(
    label-def
    byte-const-def
    word-const-def
    ))

(define (is-byte-label? instruction)
  (define tag (car instruction))
  (or (eq? tag 'byte-const-def)))

(define (is-word-label? instruction)
  (define tag (car instruction))
  (or (eq? tag 'label-def)
     (eq? tag 'word-const-def)))

(module+ test #| is-word-label? |#
  (check-true (is-word-label? '(label-def "some")))
  (check-true (is-word-label? '(word-const-def "some" #x2000)))
  (check-false (is-word-label? '(byte-const-def "some" #x20)))
  (for ((label-def-symbol label-definitions))
    (check-not-eq? (is-byte-label? (list label-def-symbol "some"))
                   (is-word-label? (list label-def-symbol "some")))))

(define (first-word-label-in label program)
  (findf (λ (instruction)
           (and (is-word-label? instruction)
              (equal? label (second instruction))))
         program))

(module+ test #| is-word-label? |#
  (check-equal? (first-word-label-in "some" '((label-def "some")))
                '(label-def "some"))
  (check-equal? (first-word-label-in "other" '((label-def "some")))
                #f)
  (check-equal? (first-word-label-in "some" '((label-def "other")
                                              (word-const-def "some" #x3000)))
                '(word-const-def "some" #x3000)))

(define (first-label-in label-str program)
  (findf (λ (instruction)
           (and (is-label-instruction? instruction)
              (equal? label-str (second instruction))))
         program))

(define (is-label-instruction? instruction)
  (define tag (car instruction))
  (memq tag label-definitions))

(module+ test #| is-label-instruction? |#
  (check-not-false (is-label-instruction? '(label-def "some")))
  (check-not-false (is-label-instruction? '(word-const-def "some" #x2000)))
  (check-not-false (is-label-instruction? '(byte-const-def "some" #x20)))
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
  (check-equal? (matching-decide-option '((label-def "hello"))
                                        '(((resolve-byte "hello") opcode 166)
                                          ((resolve-word "hello") opcode 174)))
                '((resolve-word "hello") opcode 174))
  (check-equal? (matching-decide-option '((word-const-def "hello" #x2000))
                                        '(((resolve-byte "hello") opcode 166)
                                          ((resolve-word "hello") opcode 174)))
                '((resolve-word "hello") opcode 174))
  (check-equal? (matching-decide-option '((byte-const-def "hello" #x200))
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
  (check-equal? (->resolved-decisions '((label-def "hello"))
                                    '((decide (((resolve-byte "hello") opcode 166)
                                               ((resolve-word "hello") opcode 174)))))
                '((opcode 174 (resolve-word "hello"))))
  (check-equal? (->resolved-decisions '((byte-const-def "hello" #x20))
                                    '((decide (((resolve-byte "hello") opcode 166)
                                               ((resolve-word "hello") opcode 174)))))
                '((opcode 166 (resolve-byte "hello"))))
  (check-equal? (->resolved-decisions '((word-const-def "hello" #x2000))
                                    '((decide (((resolve-byte "hello") opcode 166)
                                               ((resolve-word "hello") opcode 174)))))
                '((opcode 174 (resolve-word "hello")))
                "decide for word if referencing word label")
  (check-equal? (->resolved-decisions '((label-def "not-found"))
                                    '((decide (((resolve-byte "hello") opcode 166)
                                               ((resolve-word "hello") opcode 174)))))
                '((decide (((resolve-byte "hello") opcode 166)
                           ((resolve-word "hello") opcode 174))))
                "nothing is decided if label is not found"))

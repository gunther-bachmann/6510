#lang racket

(require "6510-test-utils.rkt")
(require "6510-utils.rkt")

(module+ test #| resolve-constants |#
  (check-equal? (resolve-constants '() '#hash(("some" . #x30))
                                   '((opcode #x20 (resolve-byte "some"))))
                '((opcode #x20 #x30)))
  (check-equal? (resolve-constants '() '#hash(("some" . #x3010))
                                   '((opcode #x20 (resolve-byte ">some"))))
                '((opcode #x20 #x30)))
  (check-equal? (resolve-constants '() '#hash(("some" . #x3010))
                                   '((opcode #x20 (resolve-byte "<some"))))
                '((opcode #x20 #x10)))
  (check-equal? (resolve-constants '() '#hash(("some" . #x3010))
                                   '((opcode #x20 (resolve-word "some"))))
                '((opcode #x20 #x10 #x30)))
  (check-equal? (resolve-constants '() '#hash(("other" . #x3010))
                                   '((opcode #x20 (resolve-word "some"))))
                '((opcode #x20 (resolve-word "some")))))

(define (resolve-constants result constants commands)
  (if (empty? commands)
      result
      (let* ((instruction (car commands))
             (res (last instruction)))
        (cond [(and (list? res)
                  (eq? (car res) 'resolve-word))
               (let* ((label (cadr res))
                      (value (hash-ref constants label #f)))
                 (if value
                     (resolve-constants
                      (append
                       result
                       (list
                        (append (takef instruction (λ (el) (not (list? el))))
                                (list (low-byte value)
                                      (high-byte value)))))
                      constants
                      (cdr commands))
                     (resolve-constants (append result (list instruction)) constants (cdr commands))))]
              [(and (list? res)
                  (eq? (car res) 'resolve-byte))
               (let* ((label (cadr res))
                      (base-label (base-label-str label))
                      (value (hash-ref constants base-label #f)))
                 (cond [(and value (eq? #\> (string-ref label 0)))
                        (resolve-constants
                         (list
                          (append
                           result
                           (append (takef instruction (λ (el) (not (list? el))))
                                   (list (high-byte value)))))
                         constants
                         (cdr commands))]
                       [(and value (eq? #\< (string-ref label 0)))
                        (resolve-constants
                         (list
                          (append
                           result
                           (append (takef instruction (λ (el) (not (list? el))))
                                   (list (low-byte value)))))
                         constants
                         (cdr commands))]
                       [(and value)
                        (resolve-constants
                         (list
                          (append
                           result
                           (append (takef instruction (λ (el) (not (list? el))))
                                   (list (byte value)))))
                         constants
                         (cdr commands))]
                       [#t
                        (resolve-constants result constants (cdr commands))]))]
              [#t (resolve-constants (append result (list instruction)) result constants (cdr commands))]))))

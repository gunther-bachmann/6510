#lang racket

(require "6510-test-utils.rkt")
(require "6510-utils.rkt")

(provide constant-definitions resolve-constants)

(define (take-until-resolve-info instruction)
  (takef instruction (λ (el) (not (list? el)))))

(define (resolve-word-constant-instruction instruction value)
  (append (take-until-resolve-info instruction)
          (list (low-byte value)
                (high-byte value))))

(define (resolve-hibyte-constant-instruction instruction value)
  (append (take-until-resolve-info instruction)
          (list (high-byte value))))

(define (resolve-lobyte-constant-instruction instruction value)
  (append (take-until-resolve-info instruction)
          (list (low-byte value))))

(define (resolved-word-instruction label constants instruction)
  (let* ((value (hash-ref constants label #f)))
    (if value
        (resolve-word-constant-instruction instruction value)
        instruction)))

(define (resolved-byte-instruction label constants instruction)  
  (let* ((base-label (base-label-str label))
         (value (hash-ref constants base-label #f)))
    (cond [(and value (eq? #\> (string-ref label 0)))
           (resolve-hibyte-constant-instruction instruction value)]
          [(and value)
           (resolve-lobyte-constant-instruction instruction value)]
          [#t instruction])))

(define (constant-definitions commands)
  (let ((def-commands
          (filter
           (λ (command)
             (let ((tag (car command)))
               (or (eq? tag 'word-const-def)
                  (eq? tag 'byte-const-def))))
           commands)))
    (foldl (λ (command hash)
            (hash-set hash (second command) (third command)))
           (hash)
           def-commands)))

(module+ test #| constant-definitions |#
  (check-equal? (constant-definitions '((byte-const-def "some" #x30)))
                '#hash(("some" . #x30)))
  (check-equal? (constant-definitions '((byte-const-def "some" #x30)
                                        (word-const-def "other" #x3020)))
                '#hash(("some" . #x30)
                       ("other" . #x3020))))

(define (resolve-constants result constants commands)
  (if (empty? commands)
      result
      (let* ((instruction (car commands))
             (res         (last instruction))
             (next-result
              (cond [(and (list? res)
                        (eq? (car res) 'resolve-word))
                     (resolved-word-instruction (cadr res) constants instruction)]
                    [(and (list? res)
                        (eq? (car res) 'resolve-byte))
                     (resolved-byte-instruction (cadr res) constants instruction)]
                    [#t instruction])))
        (resolve-constants (append result (list next-result)) constants (cdr commands)))))

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

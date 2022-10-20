#lang racket

(require "6510-test-utils.rkt")
(require "6510-utils.rkt")
(require "6510-alt-command.rkt")
(require (only-in "6510-alt-resolver.rkt" resolve-word? resolve-byte?))

(provide constant-definitions-hash resolve-constants)

(define (take-until-resolve-info command)
  (takef command
         (λ (el) (not (or (resolve-word? el)
                       (resolve-byte? el))))))

(define (word-constant->command command value)
  (append (take-until-resolve-info command)
          (list (low-byte value)
                (high-byte value))))

(define (hibyte-constant->command command value)
  (append (take-until-resolve-info command)
          (list (high-byte value))))

(define (lobyte-constant->command command value)
  (append (take-until-resolve-info command)
          (list (low-byte value))))

(define (resolve-known-word->command label constants command)
  (let* ((value (hash-ref constants label #f)))
    (if value
        (word-constant->command command value)
        command)))

(define (resolve-known-byte->command label constants command)  
  (let* ((base-label (base-label-str label))
         (value      (hash-ref constants base-label #f)))
    (cond [(and value (string-prefix? label ">"))
           (hibyte-constant->command command value)]
          [value
           (lobyte-constant->command command value)]
          [#t command])))

(define (constant-definition-commands commands)
  (filter
   (λ (command) (or (ast-const-word-cmd? command)
                   (ast-const-byte-cmd? command)))
   commands))

(define (constant-definitions-hash commands)
  (foldl (λ (command hash)
           (define-values (label value)
             (cond [(ast-const-word-cmd? command)
                    (values (ast-const-word-cmd-label command)
                            (ast-const-word-cmd-word command))]
                   [(ast-const-byte-cmd? command)
                    (values (ast-const-byte-cmd-label command)
                            (ast-const-byte-cmd-byte command))]))
           (hash-set hash label value))
         (hash)
         (constant-definition-commands commands)))

(module+ test #| constant-definitions |#
  (check-equal? (constant-definitions-hash (list (ast-const-byte-cmd "some" #x30)))
                '#hash(("some" . #x30)))
  (check-equal? (constant-definitions-hash (list (ast-const-byte-cmd "some" #x30)
                                             (ast-const-word-cmd "other" #x3020)))
                '#hash(("some" . #x30)
                       ("other" . #x3020))))

(define (resolve-constants result constants commands)
  (if (empty? commands)
      result
      (let* ((command (car commands))
             (res         (last command))
             (next-result
              (cond [(resolve-word? res)
                     (resolve-known-word->command (cadr res) constants command)]
                    [(resolve-byte? res)
                     (resolve-known-byte->command (cadr res) constants command)]
                    [#t command])))
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

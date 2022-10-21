#lang racket

(require "6510-test-utils.rkt")
(require "6510-utils.rkt")
(require "6510-alt-command.rkt")
(require (only-in "6510-alt-resolver.rkt" resolve-word? resolve-byte?))

(provide constant-definitions-hash resolve-constants)

(define (constant->command command res-byte-list)
  (cond [(ast-unresolved-command? command)
         (ast-opcode-cmd (append (ast-opcode-cmd-bytes command) res-byte-list))]
        [#t (raise-user-error "unknown unresolved command")]))

(define (word-constant->command command value)
  (constant->command command (list (low-byte value) (high-byte value))))

(define (hibyte-constant->command command value)
  (constant->command command (list (high-byte value))))

(define (lobyte-constant->command command value)
  (constant->command command (list (low-byte value))))

(define (resolve-known-word->command label constants command)
  (let* ((value (hash-ref constants label #f)))
    (if value
        (word-constant->command command value)
        command)))

(define (resolve-known-byte->command label hilo-ind constants command)  
  (let* ((value (hash-ref constants label #f)))
    (cond [(and value (eq? hilo-ind 'high-byte))
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

(define (ast-unresolved-command-res command)
  (cond [(ast-unresolved-opcode-cmd? command)
         (ast-unresolved-opcode-cmd-resolve-sub-command command)]
        [(ast-unresolved-rel-opcode-cmd? command)
         (ast-unresolved-rel-opcode-cmd-resolve-sub-command command)]
        [#t (raise-user-error "unknown unresolved command")]))

(define (resolve-constants result constants commands)
  (if (empty? commands)
      result
      (let* ((command (car commands))
             (res     (if (ast-unresolved-command? command)
                          (ast-unresolved-command-res command)
                          #t))
             (next-result
              (cond [(ast-resolve-word-scmd? res)
                     (resolve-known-word->command (ast-resolve-sub-cmd-label res) constants command)]
                    [(ast-resolve-byte-scmd? res)
                     (resolve-known-byte->command
                      (ast-resolve-sub-cmd-label res)
                      (ast-resolve-byte-scmd-mode res)
                      constants
                      command)]
                    [#t command])))
        (resolve-constants (append result (list next-result)) constants (cdr commands)))))

(module+ test #| resolve-constants |#
  (check-equal? (resolve-constants '() '#hash(("some" . #x30))
                                   (list (ast-unresolved-opcode-cmd
                                          '(#x20)
                                          (ast-resolve-byte-scmd "some" 'low-byte))))
                (list (ast-opcode-cmd '(#x20 #x30))))
  (check-equal? (resolve-constants '() '#hash(("some" . #x3010))
                                   (list (ast-unresolved-opcode-cmd
                                          '(#x20)
                                          (ast-resolve-byte-scmd "some" 'high-byte))))
                (list (ast-opcode-cmd '(#x20 #x30))))
  (check-equal? (resolve-constants '() '#hash(("some" . #x3010))
                                   (list (ast-unresolved-opcode-cmd
                                          '(#x20)
                                          (ast-resolve-byte-scmd "some" 'low-byte))))
                (list (ast-opcode-cmd '(#x20 #x10))))
  (check-equal? (resolve-constants '() '#hash(("some" . #x3010))
                                   (list (ast-unresolved-opcode-cmd
                                          '(#x20)
                                          (ast-resolve-word-scmd "some"))))
                (list (ast-opcode-cmd '(#x20 #x10 #x30))))
  (check-equal? (resolve-constants '() '#hash(("other" . #x3010))
                                   (list (ast-unresolved-opcode-cmd
                                          '(#x20)
                                          (ast-resolve-word-scmd "some"))))
                (list (ast-unresolved-opcode-cmd '(#x20) (ast-resolve-word-scmd "some")))))

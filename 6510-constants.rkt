#lang racket

;; allow constant definitions in opcodes and resolve these constants 
;; accordingly. e.g.
;;   (word-const label #xFFD2) =>
;;     (ast-const-word-cmd "label" #xFFD2)
;;
;;   (byte-const label #x20) =>
;;     (ast-const-byte-cmd "label" #x20)
;;
;; label can then be used anywhere a value is expected
;; e.g.:
;;   (jsr label)
;;   (lda #<label)

(require "6510-command.rkt")
(require (only-in "6510-utils.rkt" high-byte low-byte byte/c word/c))
(require (rename-in  racket/contract [define/contract define/c]))

(provide constant-definitions-hash resolve-constants)

(module+ test
  (require "6510-test-utils.rkt"))

;; use the byte-code of the command, appending the res-byte-list
(define/c (constant->command command res-byte-list)
  (-> ast-command? (listof byte/c) ast-command?)
  (cond [(ast-unresolved-opcode-cmd? command)
         (ast-opcode-cmd (append (ast-opcode-cmd-bytes command) res-byte-list))]
        [(ast-unresolved-rel-opcode-cmd? command)
         (ast-rel-opcode-cmd (append (ast-rel-opcode-cmd-bytes command) res-byte-list))]
        [#t (raise-user-error "unknown unresolved command")]))

;; append the low and highbyte of value to the command opcode
(define/c (word-constant->command command value)
  (-> ast-command? word/c ast-command?)
  (constant->command command (list (low-byte value) (high-byte value))))

;; append the high byte of the value to the opcode command
(define/c (hibyte-constant->command command value)
  (-> ast-command? word/c ast-command?)
  (constant->command command (list (high-byte value))))

;; append the low byte of the value to the opcode command
(define/c (lobyte-constant->command command value)
  (-> ast-command? word/c ast-command?)
  (constant->command command (list (low-byte value))))

;; resolve the label in constants appending the word value to the opcode command
(define/c (resolve-known-word->command label constants command)
  (-> string? hash? ast-command? ast-command?)
  (let* ((value (hash-ref constants label #f)))
    (if value
        (word-constant->command command value)
        command)))

;; resolve a single label in constants using hilo-ind in this single command
(define/c (resolve-known-byte->command label hilo-ind constants command)
  (-> string? (or/c 'high-byte 'low-byte) hash? ast-command? ast-command?)
  (let* ((value (hash-ref constants label #f)))
    (cond [(and value (eq? hilo-ind 'high-byte))
           (hibyte-constant->command command value)]
          [value
           (lobyte-constant->command command value)]
          [#t command])))

;; get all constant defining commands from the list
(define/c (constant-definition-commands commands)
  (-> (listof ast-command?) (listof (or/c ast-const-word-cmd? ast-const-byte-cmd?)))
  (filter
   (λ (command) (or (ast-const-word-cmd? command)
                   (ast-const-byte-cmd? command)))
   commands))

;; create a hash-table label->value for all constant definitions
(define/c (constant-definitions-hash commands)
  (-> (listof ast-command?) hash?)
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

;; get the sub command from an unresolved (rel) opcode command
(define/c (ast-unresolved-command-res command)
  (-> ast-command? ast-resolve-sub-cmd?)
  (cond [(ast-unresolved-opcode-cmd? command)
         (ast-unresolved-opcode-cmd-resolve-sub-command command)]
        [(ast-unresolved-rel-opcode-cmd? command)
         (ast-unresolved-rel-opcode-cmd-resolve-sub-command command)]
        [#t (raise-user-error "unknown unresolved command")]))

;; resolve all constants in commands into result using the constants hash
(define/c (-resolve-constants result constants commands)
  (-> (listof ast-command?) hash? (listof ast-command?) (listof ast-command?))
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
        (-resolve-constants (append result (list next-result)) constants (cdr commands)))))

;; resolve all constants in commands using the constants hash
(define/c (resolve-constants constants commands)
  (-> hash? (listof ast-command?) (listof ast-command?))
  (-resolve-constants '() constants commands))

(module+ test #| resolve-constants |#
  (check-equal? (resolve-constants '#hash(("some" . #x30))
                                   (list (ast-unresolved-opcode-cmd
                                          '(#x20)
                                          (ast-resolve-byte-scmd "some" 'low-byte))))
                (list (ast-opcode-cmd '(#x20 #x30))))
  (check-equal? (resolve-constants '#hash(("some" . #x3010))
                                   (list (ast-unresolved-opcode-cmd
                                          '(#x20)
                                          (ast-resolve-byte-scmd "some" 'high-byte))))
                (list (ast-opcode-cmd '(#x20 #x30))))
  (check-equal? (resolve-constants '#hash(("some" . #x3010))
                                   (list (ast-unresolved-opcode-cmd
                                          '(#x20)
                                          (ast-resolve-byte-scmd "some" 'low-byte))))
                (list (ast-opcode-cmd '(#x20 #x10))))
  (check-equal? (resolve-constants '#hash(("some" . #x3010))
                                   (list (ast-unresolved-opcode-cmd
                                          '(#x20)
                                          (ast-resolve-word-scmd "some"))))
                (list (ast-opcode-cmd '(#x20 #x10 #x30))))
  (check-equal? (resolve-constants '#hash(("other" . #x3010))
                                   (list (ast-unresolved-opcode-cmd
                                          '(#x20)
                                          (ast-resolve-word-scmd "some"))))
                (list (ast-unresolved-opcode-cmd '(#x20) (ast-resolve-word-scmd "some")))))

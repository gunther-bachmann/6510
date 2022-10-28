#lang racket

(require (only-in "6510-utils.rkt" word/c byte/c))
(require (rename-in  racket/contract [define/contract define/c]))

(provide (struct-out ast-program)
         (struct-out ast-command)
         (struct-out ast-decide-cmd)
         (struct-out ast-resolve-sub-cmd)
         (struct-out ast-resolve-word-scmd)
         (struct-out ast-resolve-byte-scmd)
         (struct-out ast-opcode-cmd)
         (struct-out ast-rel-opcode-cmd)
         (struct-out ast-unresolved-opcode-cmd)
         (struct-out ast-unresolved-rel-opcode-cmd)
         ast-unresolved-command?
         label->byte-resolve-mode
         (struct-out ast-bytes-cmd)
         (struct-out ast-label-def-cmd)
         (struct-out ast-require-byte-cmd)
         (struct-out ast-require-word-cmd)
         (struct-out ast-provide-byte-cmd)
         (struct-out ast-provide-word-cmd)
         (struct-out ast-const-byte-cmd)
         (struct-out ast-const-word-cmd))

(module+ test
  (require "6510-test-utils.rkt"))

(define (ast-unresolved-command? command)
  (or (ast-unresolved-rel-opcode-cmd? command)
     (ast-unresolved-opcode-cmd? command)))

;; generic root of all ast commands
(struct ast-command
  ()
  #:transparent)

;; a complete program (module) of 6510 assembler code
(struct ast-program
  (commands)
  #:transparent
  #:guard (struct-guard/c (listof ast-command?)))

;; options depending on e.g. word/byte addressing
(struct ast-decide-cmd ast-command
  (options)
  #:transparent
  #:guard (struct-guard/c (listof ast-unresolved-command?)))

;; a resolve sub command, referencing this label (use word/byte sub structure)
(struct ast-resolve-sub-cmd
  (label)
  #:transparent
  #:guard (struct-guard/c string?))

;; resolving to a word
(struct ast-resolve-word-scmd ast-resolve-sub-cmd
  ()
  #:transparent)

(define (byte-resolve-mode? mode)
  (or (eq? mode 'high-byte)
     (eq? mode 'low-byte)
     (eq? mode 'relative)))

(define (label->byte-resolve-mode label)
  (cond [(string-prefix? label ">") 'high-byte]
        [#t 'low-byte]))

;; resolving to a byte
(struct ast-resolve-byte-scmd ast-resolve-sub-cmd
  (mode)
  #:transparent
  #:guard (struct-guard/c string? byte-resolve-mode?))

;; resolved opcode (just bytes)
(struct ast-opcode-cmd ast-command
  (bytes)
  #:transparent
  #:guard (struct-guard/c
           (listof byte?)))

(struct ast-rel-opcode-cmd ast-command
  (bytes)
  #:transparent
  #:guard (struct-guard/c
           (listof byte?)))

;; bytes (usually data)
(struct ast-bytes-cmd ast-command
  (bytes)
  #:transparent
  #:guard (struct-guard/c
           (listof byte?)))

;; opcode including unresolved resolve subcommand
(struct ast-unresolved-opcode-cmd ast-opcode-cmd
  (resolve-sub-command)
  #:transparent
  #:guard (struct-guard/c
           (listof byte?)
           ast-resolve-sub-cmd?))

;; opcode including unresolved resolve subcommand
(struct ast-unresolved-rel-opcode-cmd ast-rel-opcode-cmd
  (resolve-sub-command)
  #:transparent
  #:guard (struct-guard/c
           (listof byte?)
           ast-resolve-byte-scmd?))

;; label definition
(struct ast-label-def-cmd ast-command
  (label)
  #:transparent
  #:guard (struct-guard/c string?))

;; import word definition
(struct ast-require-word-cmd ast-command
  (label)
  #:transparent
  #:guard (struct-guard/c string?))

;; import byte definition
(struct ast-require-byte-cmd ast-command
  (label)
  #:transparent
  #:guard (struct-guard/c string?))

;; export word definition
(struct ast-provide-word-cmd ast-command
  (label)
  #:transparent
  #:guard (struct-guard/c string?))

;; export byte definition
(struct ast-provide-byte-cmd ast-command
  (label)
  #:transparent
  #:guard (struct-guard/c string?))

;; constant byte definition
(struct ast-const-byte-cmd ast-command
  (label byte)
  #:transparent
  #:guard (struct-guard/c string? byte?))

;; constant word definition
(struct ast-const-word-cmd ast-command
  (label word)
  #:transparent
  #:guard (struct-guard/c string? word/c))

(module+ test #| struct type |#
  (define opcode-example (ast-unresolved-opcode-cmd '(#x20) (ast-resolve-word-scmd "label")))
  (define decide-example (ast-decide-cmd (list opcode-example)))
  (define opcode-resolved (ast-opcode-cmd '(#x20 #xd2 #xff)))

  (check-true (cond [(ast-opcode-cmd? opcode-example)
                     #t]
                    [(ast-decide-cmd? opcode-example)
                     #f]))
  (check-true (cond [(ast-opcode-cmd? opcode-resolved)
                     #t]
                    [(ast-decide-cmd? opcode-resolved)
                     #f]))
  (check-true (cond [(ast-unresolved-opcode-cmd? opcode-example)
                     #t]
                    [(ast-unresolved-opcode-cmd? opcode-resolved)
                     #f]))
  (check-true (cond [(ast-opcode-cmd? decide-example)
                     #f]
                    [(ast-decide-cmd? decide-example)
                     #t]))
  (check-equal? (ast-decide-cmd-options decide-example)
                (list opcode-example)))

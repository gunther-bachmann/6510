#lang racket
#|

 decide how to resolve 6510 commands.
 decision is made depending on the width of a resolved label, for example

 |#

(require (rename-in  racket/contract [define/contract define/c]))
(require "6510-command.rkt")
(require (only-in "../6510-utils.rkt" byte/c low-byte high-byte word/c two-complement-of))
(require (only-in "6510-relocator.rkt" command-len label-string-offsets))

(module+ test
  (require "../6510-test-utils.rkt"))

(provide ->resolved-decisions label-instructions ->resolve-labels resolved-program->bytes commands->bytes)

;; is this instruction introducing a label referencing a byte value (e.g. constant def)?
(define/c (byte-label-cmd? instruction)
  (-> ast-command? boolean?)
  (or (ast-const-byte-cmd? instruction)
     (ast-require-byte-cmd? instruction)))

;; is this instruction introducing a label referencing a word value (e.g. constant or code label)?
(define/c (word-label-cmd? instruction)
  (-> ast-command? boolean?)
  (or (ast-const-word-cmd? instruction)
     (ast-label-def-cmd? instruction)
     (ast-require-word-cmd? instruction)))

(module+ test #| is-word-label? |#
  (check-true (word-label-cmd? (ast-label-def-cmd "some")))
  (check-true (word-label-cmd? (ast-const-word-cmd "some" #x2000)))
  (check-false (word-label-cmd? (ast-const-byte-cmd "some" #x20))))

;; first instruction that introduces the given label as word
(define/c (first-word-label-in label program)
  (-> string?
     (listof ast-command?)
     (or/c ast-const-word-cmd?
           ast-label-def-cmd?
           #f))
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

;; first instruction that introduces the given label
(define/c (first-label-in label program)
  (-> string?
     (listof ast-command?)
     (or/c ast-const-word-cmd?
           ast-const-byte-cmd?
           ast-label-def-cmd?
           ast-require-byte-cmd?
           ast-require-word-cmd?
           #f))
  (findf (λ (instruction)
           (define label-str
             (cond   [(ast-label-def-cmd? instruction)
                      (ast-label-def-cmd-label instruction)]
                     [(ast-const-word-cmd? instruction)
                      (ast-const-word-cmd-label instruction)]
                     [(ast-const-byte-cmd? instruction)
                      (ast-const-byte-cmd-label instruction)]
                     [(ast-require-word-cmd? instruction)
                      (ast-require-word-cmd-label instruction)]
                     [(ast-require-byte-cmd? instruction)
                      (ast-require-byte-cmd-label instruction)]))
           (equal? label label-str))
         program))

(module+ test #| first-label-in |#
  (check-equal? (first-label-in "some" (list (ast-label-def-cmd "hello")))
                #f)
  (check-equal? (first-label-in "hello" (list (ast-label-def-cmd "hello")))
                (ast-label-def-cmd "hello")))

;; is the given instruction introducing a label?
(define/c (label-instruction? instruction)
  (-> ast-command? boolean?)
  (or (ast-label-def-cmd? instruction)
     (ast-const-word-cmd? instruction)
     (ast-const-byte-cmd? instruction)
     (ast-require-word-cmd? instruction)
     (ast-require-byte-cmd? instruction)))

(module+ test #| is-label-instruction? |#
  (check-not-false (label-instruction? (ast-label-def-cmd "some")))
  (check-not-false (label-instruction? (ast-const-word-cmd "some" #x2000)))
  (check-not-false (label-instruction? (ast-const-byte-cmd "some" #x20)))
  (check-false (label-instruction? (ast-opcode-cmd '(#xea)))))

;; all instructions that introduce labels
(define/c (label-instructions program)
  (-> (listof ast-command?) (listof label-instruction?))
  (filter label-instruction? program))

;; find first unresolved command that contains an option to decide on that may be resolved by the first matching label
(define/c (matching-decide-option labels decide-options)
  (-> (listof label-instruction?) (listof ast-unresolved-command?) (or/c ast-unresolved-command? #f))
  (findf (λ (decide-option)
           (define resolve-scmd (ast-unresolved-opcode-cmd-resolve-sub-command decide-option))
           (define label        (ast-resolve-sub-cmd-label resolve-scmd))
           (define label-entry  (first-label-in label labels))
           (if label-entry
               (cond [(ast-resolve-byte-scmd? resolve-scmd)
                      (byte-label-cmd? label-entry)]
                     [(ast-resolve-word-scmd? resolve-scmd)
                      (word-label-cmd? label-entry)]
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
                (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte)))
  (check-equal? (matching-decide-option
                 (list (ast-require-word-cmd "hello"))
                 (list (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                       (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello"))))
                (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello")))
  (check-equal? (matching-decide-option
                 (list (ast-require-byte-cmd "hello"))
                 (list (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                       (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello"))))
                (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))))

;; given program with all decisions resolved that can be resolved with the given list of labels
(define/c (->resolved-decisions labels program)
  (-> (listof label-instruction?) (listof ast-command?) (listof ast-command?))
  (if (empty? program)
      '()
      (let* ((instruction      (car program))
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
                 (list (ast-require-word-cmd "hello"))
                 (list
                  (ast-decide-cmd
                   (list (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                         (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello"))))))
                (list (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello"))))
  (check-equal? (->resolved-decisions
                 (list (ast-require-byte-cmd "hello"))
                 (list
                  (ast-decide-cmd
                   (list (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                         (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "hello"))))))
                (list (ast-unresolved-opcode-cmd '(166) (ast-resolve-byte-scmd "hello" 'low-byte))))
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

;; encode this regular opcode cmd adding bytes to the command
(define/c (encode-opcode-cmd instruction . bytes)
  (->* (ast-unresolved-opcode-cmd?) (listof byte/c) ast-opcode-cmd?)
  (ast-opcode-cmd (append (ast-opcode-cmd-bytes instruction) bytes)))

;; encode this opcode cmd adding bytes (low, high byte) of the word value
(define/c (encode-label-word-value instruction value)
  (-> ast-unresolved-opcode-cmd? word/c ast-opcode-cmd?)
  (encode-opcode-cmd instruction (low-byte value) (high-byte value)))

;; encode this opcode cmd adding high byte of the word value
(define/c (encode-label-hbyte-value instruction value)
  (-> ast-unresolved-opcode-cmd? word/c ast-opcode-cmd?)
  (encode-opcode-cmd instruction (high-byte value)))

;; encode this opcode cmd adding low byte of the word value
(define/c (encode-label-lbyte-value instruction value)
  (-> ast-unresolved-opcode-cmd? word/c ast-opcode-cmd?)
  (encode-opcode-cmd instruction (low-byte value)))

;; encode this opcode cmd adding relative byte value
(define/c (encode-label-rel-value instruction rel-value)
  (-> ast-unresolved-rel-opcode-cmd? byte/c ast-rel-opcode-cmd?)
  (ast-rel-opcode-cmd (append (ast-rel-opcode-cmd-bytes instruction) (list rel-value))))

;; resolve this regular opcode (if applicable) 
(define/c (resolve-opcode-cmd instruction labels)
  (-> ast-unresolved-opcode-cmd? hash? ast-opcode-cmd?)
  (let* ((subcmd (ast-unresolved-opcode-cmd-resolve-sub-command instruction))
         (label  (ast-resolve-sub-cmd-label subcmd))
         (value  (hash-ref labels label #f)))
    (if value
        (cond [(ast-resolve-word-scmd? subcmd)
               (encode-label-word-value instruction value)]
              [(and (ast-resolve-byte-scmd? subcmd)
                  (eq? 'high-byte (ast-resolve-byte-scmd-mode subcmd)))
               (encode-label-hbyte-value instruction value)]
              [(and (ast-resolve-byte-scmd? subcmd)
                  (eq? 'low-byte (ast-resolve-byte-scmd-mode subcmd)))
               (encode-label-lbyte-value instruction value)])
        instruction)))

;; resolve this relative opcode command (if applicable) using the given current offset of the code
(define/c (resolve-rel-opcode-cmd instruction offset labels)
  (-> ast-unresolved-rel-opcode-cmd? word/c hash? ast-rel-opcode-cmd?)
  (let* ((subcmd (ast-unresolved-rel-opcode-cmd-resolve-sub-command instruction))
         (label  (ast-resolve-sub-cmd-label subcmd))
         (value  (hash-ref labels label #f)))
    (if value
        (let ((rel-value (two-complement-of (- value (+ offset 2)))))
          (encode-label-rel-value instruction rel-value))
        instruction)))

;; resolve labels to bytes in the given program, using offset as absolute program start
(define/c (->resolve-labels offset labels program resolved-program)
  (-> word/c hash? (listof ast-command?) (listof ast-command?) (listof ast-command?)) 
  (if (empty? program)
      resolved-program
      (let* ((instruction  (car program))
             (next-offset  (+ offset (command-len instruction)))
             (resolved-cmd (cond [(ast-unresolved-opcode-cmd? instruction)
                                  (resolve-opcode-cmd instruction labels)]
                                 [(ast-unresolved-rel-opcode-cmd? instruction)
                                  (resolve-rel-opcode-cmd instruction offset labels)]
                                 [#t instruction]))
             (new-res-prg  (append resolved-program (list resolved-cmd))))        
        (->resolve-labels next-offset labels (cdr program) new-res-prg))))

(module+ test #| ->resolve-labels |#
  (check-equal?
   (->resolve-labels 0 '#hash(("some" . #x10) ("other" . #x0) ("sowo" . #xFFD2))
                    (list (ast-unresolved-opcode-cmd '(30) (ast-resolve-word-scmd "some"))
                          (ast-unresolved-rel-opcode-cmd '(40) (ast-resolve-byte-scmd "some" 'low-byte))
                          (ast-unresolved-rel-opcode-cmd '(40) (ast-resolve-byte-scmd "other" 'low-byte))
                          (ast-unresolved-opcode-cmd '(30) (ast-resolve-byte-scmd "sowo" 'low-byte))
                          (ast-unresolved-opcode-cmd '(30) (ast-resolve-byte-scmd "sowo" 'high-byte))
                          (ast-unresolved-opcode-cmd '(30) (ast-resolve-byte-scmd "unknown" 'high-byte))
                          (ast-unresolved-opcode-cmd '(30) (ast-resolve-word-scmd "unknown")))
                    '())
   (list (ast-opcode-cmd '(30 #x10 #x00))
         (ast-rel-opcode-cmd '(40 11))
         (ast-rel-opcode-cmd '(40 249))
         (ast-opcode-cmd '(30 #xD2))
         (ast-opcode-cmd '(30 #xFF))
         (ast-unresolved-opcode-cmd '(30) (ast-resolve-byte-scmd "unknown" 'high-byte))
         (ast-unresolved-opcode-cmd '(30) (ast-resolve-word-scmd "unknown"))))
  (check-equal?
   (->resolve-labels 0 '#hash(("sout" . 3))
                    (list (ast-unresolved-opcode-cmd '(174) (ast-resolve-word-scmd "sout"))
                          (ast-label-def-cmd "sout")
                          (ast-unresolved-opcode-cmd '(189) (ast-resolve-word-scmd "sout"))
                          (ast-opcode-cmd '(#x20 #xd2 #xff))
                          (ast-opcode-cmd '(202))
                          (ast-unresolved-rel-opcode-cmd '(208) (ast-resolve-byte-scmd "sout" 'low-byte)))
                    '())
   (list (ast-opcode-cmd '(174 3 0))
         (ast-label-def-cmd "sout")
         (ast-opcode-cmd '(189 3 0) )
         (ast-opcode-cmd '(#x20 #xd2 #xff))
         (ast-opcode-cmd '(202))
         (ast-rel-opcode-cmd '(208 247)))))

(define/c (-resolved-program->bytes program resolved)
  (-> (listof ast-command?) (listof byte/c) (listof byte/c))
  (if (empty? program)
      resolved
      (let* ((instruction (car program))
             (bytes       (cond [(ast-opcode-cmd? instruction)
                                 (ast-opcode-cmd-bytes instruction)]
                                [(ast-rel-opcode-cmd? instruction)
                                 (ast-rel-opcode-cmd-bytes instruction)]
                                [(ast-bytes-cmd? instruction)
                                 (ast-bytes-cmd-bytes instruction)]
                                [#t '()])))
        (-resolved-program->bytes (cdr program) (append resolved bytes)))))

;; transform a resolved PROGRAM into a list of bytes
(define/c (resolved-program->bytes program)
  (-> (listof ast-command?) (listof byte/c))
  (-resolved-program->bytes program '()))

(module+ test #| resolve-program->bytes |#
  (check-equal? (resolved-program->bytes
                 (list
                  (ast-opcode-cmd '(174 59 8))
                  (ast-label-def-cmd "sout")
                  (ast-opcode-cmd '(189 59 8))
                  (ast-opcode-cmd '(32 210 255))
                  (ast-opcode-cmd '(202))
                  (ast-rel-opcode-cmd '(208 247))
                  (ast-opcode-cmd '(24))
                  (ast-opcode-cmd '(162 5))
                  (ast-label-def-cmd "some")
                  (ast-opcode-cmd '(169 65))
                  (ast-opcode-cmd '(32 55 8))
                  (ast-opcode-cmd '(105 1))
                  (ast-opcode-cmd '(32 55 8))
                  (ast-opcode-cmd '(105 1))
                  (ast-opcode-cmd '(32 55 8))
                  (ast-opcode-cmd '(169 13))
                  (ast-opcode-cmd '(32 55 8))
                  (ast-label-def-cmd "end")
                  (ast-opcode-cmd '(202))
                  (ast-rel-opcode-cmd '(208 233))
                  (ast-opcode-cmd '(96))
                  (ast-label-def-cmd "cout")
                  (ast-opcode-cmd '(32 210 255))
                  (ast-opcode-cmd '(96))
                  (ast-label-def-cmd "hello")
                  (ast-bytes-cmd '(18))
                  (ast-bytes-cmd '(13))
                  (ast-bytes-cmd '(33 68 76 82 79 119 32 87 69 78 32 79 76 76 69 104))
                  (ast-bytes-cmd '(14))
                  ))
                '(174 59 8
                  189 59 8
                  32 210 255
                  202
                  208 247
                  24
                  162 5
                  169 65
                  32 55 8
                  105 1
                  32 55 8
                  105 1
                  32 55 8
                  169 13
                  32 55 8
                  202
                  208 233
                  96
                  32 210 255
                  96
                  18
                  13
                  33 68 76 82 79 119 32 87 69 78 32 79 76 76 69 104
                  14)))

;; translate raw program COMMANDS with possibly undecided decisions
;; to resolved program bytes at MEMORY-ADDRESS
(define/c (commands->bytes memory-address commands )
  (-> word/c (listof ast-command?) (listof byte/c))
  (define resolved-decisions (->resolved-decisions (label-instructions commands) commands))
  (define resolved-labels
    (->resolve-labels memory-address
                     (label-string-offsets memory-address resolved-decisions)
                     resolved-decisions
                     '()))
  (resolved-program->bytes resolved-labels))

(module+ test #| commands->bytes |#
  (check-equal? (commands->bytes #xa000 (list (ast-opcode-cmd '(20 #xff #xa0))))
                '(20 #xff #xa0))
  (check-equal? (commands->bytes #xa000 (list (ast-unresolved-opcode-cmd '(#x14) (ast-resolve-word-scmd "absadr"))
                                         (ast-label-def-cmd "absadr")))
                '(20 #x03 #xa0))
  (check-equal? (commands->bytes #xa000 (list (ast-label-def-cmd "before")
                                             (ast-unresolved-rel-opcode-cmd '(#xd0) (ast-resolve-byte-scmd "before" 'low-byte))))
                '(#xd0 #xfe)))

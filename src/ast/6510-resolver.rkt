#lang racket

(provide resolved-instruction->bytes
         ->resolved-decisions
         ->resolved-decisions-new
         label-instructions
         constant-instructions
         ->resolve-labels
         resolved-program->bytes
         commands->bytes
         label-label
         label-offset
         add-label-suffix
         replace-labels
         resolved-program-length        ;; get the length of bytes of the given program (all references resolved)
         resolved-instruction-length    ;; get the number of bytes of the given instruction (all references resolved)
         )

#|

 decide how to resolve 6510 commands.
 decision is made depending on the width of a resolved label, for example

 |#

(require
 (rename-in  racket/contract [define/contract define/c])
 "6510-command.rkt"
 (only-in "../6510-utils.rkt" byte/c low-byte high-byte word/c two-complement-of in-word-range? in-byte-range?)
 (only-in "6510-relocator.rkt" command-len label-string-offsets))

(module+ test #| require |#
  (require "../6510-test-utils.rkt"))

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
  (check-true (word-label-cmd? (ast-label-def-cmd '() "some")))
  (check-true (word-label-cmd? (ast-const-word-cmd '() "some" #x2000)))
  (check-false (word-label-cmd? (ast-const-byte-cmd '() "some" #x20))))

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
                   [else '()]))
           (equal? label instruction-word-label))
         program))

(module+ test #| is-word-label? |#
  (check-equal? (first-word-label-in "some" (list (ast-label-def-cmd '() "some")))
                (ast-label-def-cmd '() "some"))
  (check-equal? (first-word-label-in "other" (list (ast-label-def-cmd '() "some")))
                #f)
  (check-equal? (first-word-label-in "some" (list (ast-label-def-cmd '() "other")
                                                  (ast-const-word-cmd '() "some" #x3000)))
                (ast-const-word-cmd '() "some" #x3000)))

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
                      (ast-require-byte-cmd-label instruction)]
                     [else '()]))
           (equal? label label-str))
         program))

(module+ test #| first-label-in |#
  (check-equal? (first-label-in "some" (list (ast-label-def-cmd '()  "hello")))
                #f)
  (check-equal? (first-label-in "hello" (list (ast-label-def-cmd '() "hello")))
                (ast-label-def-cmd '() "hello")))

;; is the given instruction introducing a label?
(define/c (label-instruction? instruction)
  (-> ast-command? boolean?)
  (or (ast-label-def-cmd? instruction)
     (ast-const-word-cmd? instruction)
     (ast-const-byte-cmd? instruction)
     (ast-require-word-cmd? instruction)
     (ast-require-byte-cmd? instruction)))

(module+ test #| is-label-instruction? |#
  (check-not-false (label-instruction? (ast-label-def-cmd '() "some")))
  (check-not-false (label-instruction? (ast-const-word-cmd '() "some" #x2000)))
  (check-not-false (label-instruction? (ast-const-byte-cmd '() "some" #x20)))
  (check-false (label-instruction? (ast-opcode-cmd '() '(#xea)))))

;; all instructions that introduce labels
(define/c (label-instructions program)
  (-> (listof ast-command?) (listof label-instruction?))
  (filter label-instruction? program))

(define/c (constant-instructions program)
  (-> (listof ast-command?) (listof label-instruction?))
  (filter (lambda (instruction) (or (ast-const-word-cmd? instruction)
                              (ast-const-byte-cmd? instruction)))
          program))

;; find first unresolved command that contains an option to decide on that may be resolved by the first matching label
(define/c (matching-decide-option labels decide-options)
  (-> (listof label-instruction?) (listof ast-unresolved-command?) (or/c ast-unresolved-command? #f))
  (findf (λ (decide-option)
           (define resolve-scmd (ast-unresolved-opcode-cmd-resolve-sub-command decide-option))
           (define label        (label-label (ast-resolve-sub-cmd-label resolve-scmd)))
           (define label-entry  (first-label-in label labels))
           (if label-entry
               (cond [(ast-resolve-byte-scmd? resolve-scmd)
                      (byte-label-cmd? label-entry)]
                     [(ast-resolve-word-scmd? resolve-scmd)
                      (word-label-cmd? label-entry)]
                     [else #f])
               #f))
         decide-options))

;; find first unresolved command that contains an option to decide on that may be resolved by the first matching label
(define/c (matching-decide-option-new labels label-commands decide-options)
  (-> (hash/c string? integer?)(listof label-instruction?) (listof ast-unresolved-command?) (or/c ast-unresolved-command? #f))
  (findf (λ (decide-option)
           (define resolve-scmd    (ast-unresolved-opcode-cmd-resolve-sub-command decide-option))
           (define label           (label-label (ast-resolve-sub-cmd-label resolve-scmd)))
           (define label-entry     (hash-ref labels label #f))
           (define label-cmd-entry (first-label-in label label-commands))
           ;; (displayln (format "resolve label: ~a, found entry: ~a, found label-cmd: ~a" label label-entry label-cmd-entry))
           (if (or label-entry label-cmd-entry)
               (cond [(ast-resolve-byte-scmd? resolve-scmd)
                      (if label-entry
                          (in-byte-range? label-entry)
                          (byte-label-cmd? label-cmd-entry))]
                     [(ast-resolve-word-scmd? resolve-scmd)
                      (if label-entry
                          (in-word-range? label-entry)
                          (word-label-cmd? label-cmd-entry))]
                     [else #f])
               #f))
         decide-options))

(module+ test #| matching-decide-option |#
  (check-equal? (matching-decide-option
                 (list (ast-label-def-cmd '() "hello"))
                 (list (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                       (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello"))))
                (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello")))
  (check-equal? (matching-decide-option
                 (list (ast-const-word-cmd '() "hello" #x2000))
                 (list (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                       (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello"))))
                (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello")))
  (check-equal? (matching-decide-option
                 (list (ast-const-byte-cmd '() "hello" #x20))
                 (list (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                       (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello"))))
                (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte)))
  (check-equal? (matching-decide-option
                 (list (ast-require-word-cmd '() "hello"))
                 (list (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                       (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello"))))
                (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello")))
  (check-equal? (matching-decide-option
                 (list (ast-require-byte-cmd '() "hello"))
                 (list (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                       (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello"))))
                (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))))

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
              [else
               (default-result-f)]))))

;; given program with all decisions resolved that can be resolved with the given list of labels
(define/c (->resolved-decisions-new labels label-commands program)
  (-> (hash/c string? integer?) (listof label-instruction?) (listof ast-command?) (listof ast-command?))
  (if (empty? program)
      '()
      (let* ((instruction      (car program))
             (default-result-f (λ () (cons instruction (->resolved-decisions-new labels label-commands (cdr program))))))
        (cond [(ast-decide-cmd? instruction)
               (let* ((options (ast-decide-cmd-options instruction))
                      (moption (matching-decide-option-new labels label-commands options)))
                 (if moption
                     (cons moption (->resolved-decisions-new labels label-commands (cdr program)))
                     (default-result-f)))]
              [else
               (default-result-f)]))))

(module+ test #| ->resolved-decisions |#
  (check-equal? (->resolved-decisions
                 (list (ast-require-word-cmd '() "hello"))
                 (list
                  (ast-decide-cmd
                   '()
                   (list (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                         (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello"))))))
                (list (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello"))))
  (check-equal? (->resolved-decisions
                 (list (ast-require-byte-cmd '() "hello"))
                 (list
                  (ast-decide-cmd
                   '()
                   (list (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                         (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello"))))))
                (list (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))))
  (check-equal? (->resolved-decisions
                 (list (ast-label-def-cmd '() "hello"))
                 (list
                  (ast-decide-cmd
                   '()
                   (list (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                         (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello"))))))
                (list (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello"))))
  (check-equal? (->resolved-decisions
                 (list (ast-const-byte-cmd '() "hello" #x20))
                 (list
                  (ast-decide-cmd
                   '()
                   (list (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                         (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello"))))))
                (list (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))))
  (check-equal? (->resolved-decisions
                 (list (ast-const-word-cmd '() "hello" #x2000))
                 (list
                  (ast-decide-cmd
                   '()
                   (list (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                         (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello"))))))
                (list (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello"))))
  (check-equal? (->resolved-decisions
                 (list (ast-label-def-cmd '() "not-found"))
                 (list
                  (ast-decide-cmd
                   '()
                   (list (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                         (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello"))))))
                (list
                 (ast-decide-cmd
                  '()
                  (list (ast-unresolved-opcode-cmd '() '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
                        (ast-unresolved-opcode-cmd '() '(174) (ast-resolve-word-scmd "hello")))))
                "nothing is decided if label is not found"))

;; encode this regular opcode cmd adding bytes to the command
(define/c (encode-opcode-cmd instruction . bytes)
  (->* (ast-unresolved-opcode-cmd?) (listof byte/c) ast-opcode-cmd?)
  (ast-opcode-cmd (ast-command-meta-information instruction) (append (ast-opcode-cmd-bytes instruction) bytes)))

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
  (ast-rel-opcode-cmd (ast-command-meta-information instruction) (append (ast-rel-opcode-cmd-bytes instruction) (list rel-value))))

(define/c (label-offset label-expression)
  (-> string? fixnum?)
  (define offset (regexp-match #rx"[+-][0-9]+$" label-expression))
  (if offset
      (string->number (car offset))
      0))

(define/c (label-label label-expression)
  (-> string? string?)
  (define str (regexp-match #rx"^[a-zA-Z0-9_]+" label-expression))
  (if str
      (car str)
      (raise-user-error (format "\"~a\" is not a label-expression" label-expression))))

(module+ test #| label-offset |#
  (check-equal? (label-offset "abc+4")
                4)
  (check-equal? (label-offset "_abc7+4")
                4)
  (check-equal? (label-offset "abc")
                0)
  (check-equal? (label-offset "abc-3")
                -3))

;; resolve this regular opcode (if applicable) 
(define/c (resolve-opcode-cmd instruction labels)
  (-> ast-unresolved-opcode-cmd? hash? ast-opcode-cmd?)
  (let* ((subcmd (ast-unresolved-opcode-cmd-resolve-sub-command instruction))
         (label  (ast-resolve-sub-cmd-label subcmd))
         (offset (label-offset label))
         (value  (hash-ref labels (label-label label) #f)))
    (if value
        (cond [(ast-resolve-word-scmd? subcmd)
               (encode-label-word-value instruction (+ offset value))]
              [(and (ast-resolve-byte-scmd? subcmd)
                  (eq? 'high-byte (ast-resolve-byte-scmd-mode subcmd)))
               (encode-label-hbyte-value instruction (+ offset value))]
              [(and (ast-resolve-byte-scmd? subcmd)
                  (eq? 'low-byte (ast-resolve-byte-scmd-mode subcmd)))
               (encode-label-lbyte-value instruction (+ offset value))]
              [else (raise-user-error "unknown subcommand ~a" subcmd) ])
        instruction)))

(define/c (resolve-bytes-cmd instruction labels)
  (-> ast-unresolved-bytes-cmd? hash? ast-bytes-cmd?)
  (let* ((subcmd (ast-unresolved-bytes-cmd-resolve-sub-command instruction))
         (label  (ast-resolve-sub-cmd-label subcmd))
         (ex-offset (label-offset label))
         (value  (hash-ref labels (label-label label) #f)))
    (if value
        (cond
          [(ast-resolve-word-scmd? subcmd)
           (ast-bytes-cmd '() (list (low-byte (+ ex-offset value)) (high-byte (+ ex-offset value))))]
          [(and (ast-resolve-byte-scmd? subcmd)
              (eq? 'high-byte (ast-resolve-byte-scmd-mode subcmd)))
           (ast-bytes-cmd '() (list (high-byte (+ ex-offset value))))]
          [(and (ast-resolve-byte-scmd? subcmd)
              (eq? 'low-byte (ast-resolve-byte-scmd-mode subcmd)))
           (ast-bytes-cmd '() (list (low-byte (+ ex-offset value))))]
          [else (raise-user-error (format "unknown subcommand ~a" subcmd)) ])
        instruction)))

;; resolve this relative opcode command (if applicable) using the given current offset of the code
(define/c (resolve-rel-opcode-cmd instruction offset labels)
  (-> ast-unresolved-rel-opcode-cmd? word/c hash? ast-rel-opcode-cmd?)

  (let* ((subcmd (ast-unresolved-rel-opcode-cmd-resolve-sub-command instruction))
         (label  (ast-resolve-sub-cmd-label subcmd))
         (ex-offset (label-offset label))
         (value  (hash-ref labels (label-label label) #f)))
    (with-handlers ((exn:fail? (lambda (exception)
                                 (display (format "instr: ~a\ncur-offset: ~a\nlabel-value: ~a\nlabel: ~a"
                                                  instruction offset value label))
                                 (raise exception ))))
      (cond [value
             (let ([rel-value (two-complement-of (+ ex-offset (- value (+ offset 2))))])
               (unless (byte? rel-value)
                 (raise-user-error (format "label ~a produces non byte offset ~a in rel opcode ~a"
                                           (label-label label)
                                           value
                                           instruction)))
               (encode-label-rel-value instruction rel-value))]
            [else instruction]))))

;; resolve labels to bytes in the given program, using offset as absolute program start
(define/c (->resolve-labels offset labels program resolved-program)
  (-> word/c hash? (listof ast-command?) (listof ast-command?) (listof ast-command?)) 
  (if (empty? program)
      resolved-program
      (let* ((instruction  (car program))
             (next-offset (cond
                            [(ast-org-command? instruction)
                             (ast-org-command-org instruction)]
                            [(ast-org-align-command? instruction)
                             (define al (ast-org-align-command-org-alignment instruction))
                             (+ offset (- al (bitwise-and offset (sub1 al))))]
                            [else
                             (+ offset (command-len instruction))]))
             (resolved-cmd (cond [(ast-unresolved-opcode-cmd? instruction)
                                  (resolve-opcode-cmd instruction labels)]
                                 [(ast-unresolved-rel-opcode-cmd? instruction)
                                  (resolve-rel-opcode-cmd instruction offset labels)]
                                 [(ast-unresolved-bytes-cmd? instruction)
                                  (resolve-bytes-cmd instruction labels)]
                                 [else instruction]))
             (new-res-prg  (append resolved-program (list resolved-cmd))))        
        (->resolve-labels next-offset labels (cdr program) new-res-prg))))

(module+ test #| ->resolve-labels |#
  (check-equal?
   (->resolve-labels 0 '#hash(("some" . #x10) ("other" . #x0) ("sowo" . #xFFD2))
                    (list (ast-unresolved-opcode-cmd '(#:test) '(30) (ast-resolve-word-scmd "some"))
                          (ast-unresolved-rel-opcode-cmd '(#:test) '(40) (ast-resolve-byte-scmd "some" 'low-byte))
                          (ast-unresolved-rel-opcode-cmd '(#:test) '(40) (ast-resolve-byte-scmd "other" 'low-byte))
                          (ast-unresolved-rel-opcode-cmd '(#:test) '(40) (ast-resolve-byte-scmd "some-3" 'low-byte))
                          (ast-unresolved-rel-opcode-cmd '(#:test) '(40) (ast-resolve-byte-scmd "other+2" 'low-byte))
                          (ast-unresolved-opcode-cmd '(#:test) '(30) (ast-resolve-byte-scmd "sowo" 'low-byte))
                          (ast-unresolved-opcode-cmd '(#:test) '(30) (ast-resolve-byte-scmd "sowo" 'high-byte))
                          (ast-unresolved-opcode-cmd '(#:test) '(30) (ast-resolve-byte-scmd "unknown" 'high-byte))
                          (ast-unresolved-opcode-cmd '(#:test) '(30) (ast-resolve-word-scmd "unknown"))
                          (ast-unresolved-opcode-cmd '(#:test) '(30) (ast-resolve-word-scmd "some+3"))
                          (ast-unresolved-opcode-cmd '(#:test) '(30) (ast-resolve-word-scmd "some-1"))
)
                    '())
   (list (ast-opcode-cmd '(#:test) '(30 #x10 #x00))
         (ast-rel-opcode-cmd '(#:test) '(40 11))
         (ast-rel-opcode-cmd '(#:test) '(40 249))
         (ast-rel-opcode-cmd '(#:test) '(40 04))
         (ast-rel-opcode-cmd '(#:test) '(40 247))
         (ast-opcode-cmd '(#:test) '(30 #xD2))
         (ast-opcode-cmd '(#:test) '(30 #xFF))
         (ast-unresolved-opcode-cmd '(#:test) '(30) (ast-resolve-byte-scmd "unknown" 'high-byte))
         (ast-unresolved-opcode-cmd '(#:test) '(30) (ast-resolve-word-scmd "unknown"))
         (ast-opcode-cmd '(#:test) '(30 #x13 #x00))
         (ast-opcode-cmd '(#:test) '(30 #x0F #x00))))

  (check-equal?
   (->resolve-labels 0 '#hash(("sout" . 3))
                    (list (ast-unresolved-opcode-cmd '(#:test) '(174) (ast-resolve-word-scmd "sout"))
                          (ast-label-def-cmd '(#:test) "sout")
                          (ast-unresolved-opcode-cmd '(#:test) '(189) (ast-resolve-word-scmd "sout"))
                          (ast-opcode-cmd '(#:test) '(#x20 #xd2 #xff))
                          (ast-opcode-cmd '(#:test) '(202))
                          (ast-unresolved-rel-opcode-cmd '(#:test) '(208) (ast-resolve-byte-scmd "sout" 'low-byte)))
                    '())
   (list (ast-opcode-cmd '(#:test) '(174 3 0))
         (ast-label-def-cmd '(#:test) "sout")
         (ast-opcode-cmd '(#:test) '(189 3 0) )
         (ast-opcode-cmd '(#:test) '(#x20 #xd2 #xff))
         (ast-opcode-cmd '(#:test) '(202))
         (ast-rel-opcode-cmd '(#:test) '(208 247))))

  (check-equal?
   (->resolve-labels 0 '#hash(("sout" . 3)) ;; sout is at position 3
                    (list (ast-unresolved-opcode-cmd '(#:test) '(174) (ast-resolve-word-scmd "sout"))
                          (ast-label-def-cmd '(#:test) "sout")
                          (ast-org-command '() 10)
                          (ast-unresolved-rel-opcode-cmd '(#:test) '(208) (ast-resolve-byte-scmd "sout" 'low-byte))
                          (ast-org-align-command '() #x10)
                          (ast-unresolved-rel-opcode-cmd '(#:test) '(208) (ast-resolve-byte-scmd "sout" 'low-byte)))
                    '())
   (list (ast-opcode-cmd '(#:test) '(174 3 0))
         (ast-label-def-cmd '(#:test) "sout")
         (ast-org-command '() 10)
         (ast-rel-opcode-cmd '(#:test) '(208 247))
         (ast-org-align-command '() 16)
         (ast-rel-opcode-cmd '(#:test) '(208 241)))
   "the last relative opcode command needs to jump over to the previous code section (org-command)"))


;; get the number of bytes of the given instruction (all references resolved)
(define/c (resolved-instruction-length instruction)
  (-> ast-command? nonnegative-integer?)
  (length (resolved-instruction->bytes instruction)))

(define/c (resolved-instruction->bytes instruction)
  (-> ast-command? (listof byte/c))
  (cond
    [(ast-unresolved-bytes-cmd? instruction)
     (raise-user-error (format "cannot resolve unresolved bytes command to bytes ~a" instruction))]
    [(ast-unresolved-command? instruction)
     (raise-user-error (format "cannot resolve unresolved command to bytes ~a" instruction))]
    [(ast-decide-cmd? instruction)
     (raise-user-error (format "cannot resolve undecided command to bytes ~a" instruction))]
    [(ast-opcode-cmd? instruction)
     (ast-opcode-cmd-bytes instruction)]
    [(ast-rel-opcode-cmd? instruction)
     (ast-rel-opcode-cmd-bytes instruction)]
    [(ast-bytes-cmd? instruction)
     (ast-bytes-cmd-bytes instruction)]
    [(ast-const? instruction) '()]
    [(ast-require? instruction) '()]
    [(ast-provide? instruction) '()]
    [(ast-label-def-cmd? instruction) '()]
    [else (raise-user-error (format "cannot resolve instruction to bytes ~a" instruction))]))

(define/c (-resolved-program->bytes program resolved)
  (-> (listof ast-command?) (listof byte/c) (listof byte/c))
  (if (empty? program)
      resolved
      (let* ((instruction (car program))
             (bytes       (resolved-instruction->bytes instruction)))
        (-resolved-program->bytes (cdr program) (append resolved bytes)))))

;; get the length of bytes of the given program (all references resolved)
(define/c (resolved-program-length program)
  (-> (listof ast-command?) nonnegative-integer?)
  (length (resolved-program->bytes program)))

;; transform a resolved PROGRAM into a list of bytes
(define/c (resolved-program->bytes program)
  (-> (listof ast-command?) (listof byte/c))
  (-resolved-program->bytes program '()))

(module+ test #| resolve-program->bytes |#
  (check-equal? (resolved-program->bytes
                 (list
                  (ast-opcode-cmd '() '(174 59 8))
                  (ast-label-def-cmd '() "sout")
                  (ast-opcode-cmd '() '(189 59 8))
                  (ast-opcode-cmd '() '(32 210 255))
                  (ast-opcode-cmd '() '(202))
                  (ast-rel-opcode-cmd '() '(208 247))
                  (ast-opcode-cmd '() '(24))
                  (ast-opcode-cmd '() '(162 5))
                  (ast-label-def-cmd '() "some")
                  (ast-opcode-cmd '() '(169 65))
                  (ast-opcode-cmd '() '(32 55 8))
                  (ast-opcode-cmd '() '(105 1))
                  (ast-opcode-cmd '() '(32 55 8))
                  (ast-opcode-cmd '() '(105 1))
                  (ast-opcode-cmd '() '(32 55 8))
                  (ast-opcode-cmd '() '(169 13))
                  (ast-opcode-cmd '() '(32 55 8))
                  (ast-label-def-cmd '() "end")
                  (ast-opcode-cmd '() '(202))
                  (ast-rel-opcode-cmd '() '(208 233))
                  (ast-opcode-cmd '() '(96))
                  (ast-label-def-cmd '() "cout")
                  (ast-opcode-cmd '() '(32 210 255))
                  (ast-opcode-cmd '() '(96))
                  (ast-label-def-cmd '() "hello")
                  (ast-bytes-cmd '() '(18))
                  (ast-bytes-cmd '() '(13))
                  (ast-bytes-cmd '() '(33 68 76 82 79 119 32 87 69 78 32 79 76 76 69 104))
                  (ast-bytes-cmd '() '(14))
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
;; this step does not resolve any constants (see assemble in 6510-assembler.rkt)
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
  (check-equal? (commands->bytes #xa000 (list (ast-opcode-cmd '() '(20 #xff #xa0))))
                '(20 #xff #xa0))
  (check-equal? (commands->bytes #xa000 (list (ast-unresolved-opcode-cmd '() '(#x14) (ast-resolve-word-scmd "absadr"))
                                         (ast-label-def-cmd '() "absadr")))
                '(20 #x03 #xa0))
  (check-equal? (commands->bytes #xa000 (list (ast-label-def-cmd '() "before")
                                             (ast-unresolved-rel-opcode-cmd '() '(#xd0) (ast-resolve-byte-scmd "before" 'low-byte))))
                '(#xd0 #xfe)))

(define/c (string-suffix-w-offset? str suffix)
  (-> string? string? boolean?)
  (regexp-match? (regexp (string-join (list (regexp-quote suffix) "([+-][0-9a-f]+)?$") "")) str))

(module+ test #| string-suffix-w-offset? |#
  (check-true (string-suffix-w-offset? "some__+1" "__"))
  (check-true (string-suffix-w-offset? "some__+15" "__"))
  (check-true (string-suffix-w-offset? "some__+1a" "__"))
  (check-true (string-suffix-w-offset? "some__-1" "__"))
  (check-true (string-suffix-w-offset? "some__-15" "__"))
  (check-true (string-suffix-w-offset? "some__-1a" "__"))
  (check-true (string-suffix-w-offset? "some__" "__"))
  (check-false (string-suffix-w-offset? "some__+" "__"))
  (check-false (string-suffix-w-offset? "some__+1a_" "__"))
  (check-false (string-suffix-w-offset? "some__-" "__"))
  (check-false (string-suffix-w-offset? "some__-1a_" "__"))
  (check-false (string-suffix-w-offset? "some__a" "__"))
  (check-false (string-suffix-w-offset? "some" "__")))

;; replace the given suffix in str (if present)
(define/c (string-replace-suffix str suffix with)
  (-> string? string? string? string?)
  (regexp-replace (regexp (string-join (list (regexp-quote suffix) "([+-][0-9a-f]+)?$") ""))
                  str
                  (string-join (list with "\\1") "")))

(module+ test #| string-replace-suffix |#
  (check-equal? (string-replace-suffix "some_function__" "__" "__suffix")
                "some_function__suffix")
  (check-equal? (string-replace-suffix "some__+1" "__" "__suffix")
                "some__suffix+1")
  (check-equal? (string-replace-suffix "some__+15" "__" "__suffix")
                "some__suffix+15")
  (check-equal? (string-replace-suffix "some__+1a" "__" "__suffix")
                "some__suffix+1a")
  (check-equal? (string-replace-suffix "some__-1" "__" "__suffix")
                "some__suffix-1")
  (check-equal? (string-replace-suffix "some__-15" "__" "__suffix")
                "some__suffix-15")
  (check-equal? (string-replace-suffix "some__-1a" "__" "__suffix")
                "some__suffix-1a")
  (check-equal? (string-replace-suffix "some__" "__" "__suffix")
                "some__suffix")

  (check-equal? (string-replace-suffix "some_function__o" "__" "__suffix")
                "some_function__o"))

(define/c (hash-replace replacement-map str)
  (-> (hash/c string? string?) string? string?)
  (define expr-tail "([^+-]*)([+-][0-9a-f]+)$")
  (cond [(regexp-match expr-tail str)
         (match-let (((list _ pure-label expr-suffix) (regexp-match expr-tail str)))
           (format "~a~a" (hash-ref replacement-map pure-label pure-label) expr-suffix))]
        [else (hash-ref replacement-map str str)]))

(module+ test #| hash-replace |#
  (check-equal? (hash-replace (hash "label" "label_new") "label")
                "label_new")
  (check-equal? (hash-replace (hash "label" "label_new") "label+0")
                "label_new+0")
  (check-equal? (hash-replace (hash "label" "label_new") "label-15")
                "label_new-15")
  (check-equal? (hash-replace (hash "label" "label_new") "xlabel-15")
                "xlabel-15")
  (check-equal? (hash-replace (hash "label" "label_new") "xlabel")
                "xlabel"))

(define/c (string-replace-in-sub-cmd cmd replacement-map)
  (-> (or/c ast-resolve-sub-cmd? ast-resolve-byte-scmd? ast-resolve-word-scmd?) (hash/c string? string?) ast-resolve-sub-cmd?)
  (cond [(ast-resolve-byte-scmd? cmd)
         (struct-copy ast-resolve-byte-scmd cmd
                      [label #:parent ast-resolve-sub-cmd
                             (hash-replace replacement-map (ast-resolve-sub-cmd-label cmd))])]
        [(ast-resolve-word-scmd? cmd)
         (struct-copy ast-resolve-word-scmd cmd
                      [label #:parent ast-resolve-sub-cmd
                             (hash-replace replacement-map (ast-resolve-sub-cmd-label cmd))])]
        [(ast-resolve-sub-cmd? cmd)
         (struct-copy ast-resolve-sub-cmd cmd
                      [label
                       (hash-replace replacement-map (ast-resolve-sub-cmd-label cmd))])]
        [else (raise-user-error (format "subcommand ~a unexpected here"))]))

(module+ test #| string-replace-in-sub-cmd |#
  (check-equal? (string-replace-in-sub-cmd (ast-resolve-sub-cmd "label")
                                                  (hash "label" "label_new"))
                (ast-resolve-sub-cmd "label_new"))
  (check-equal? (string-replace-in-sub-cmd (ast-resolve-byte-scmd "label" 'relative)
                                                  (hash "label" "label_new"))
                (ast-resolve-byte-scmd "label_new" 'relative))
  (check-equal? (string-replace-in-sub-cmd (ast-resolve-word-scmd "label")
                                                  (hash "label" "label_new"))
                (ast-resolve-word-scmd "label_new"))
  (check-equal? (string-replace-in-sub-cmd (ast-resolve-sub-cmd "xlabel")
                                                  (hash "label" "label_new"))
                (ast-resolve-sub-cmd "xlabel"))
  (check-equal? (string-replace-in-sub-cmd (ast-resolve-byte-scmd "xlabel" 'relative)
                                                  (hash "label" "label_new"))
                (ast-resolve-byte-scmd "xlabel" 'relative))
  (check-equal? (string-replace-in-sub-cmd (ast-resolve-word-scmd "xlabel")
                                                  (hash "label" "label_new"))
                (ast-resolve-word-scmd "xlabel")))

;; replace suffix in label of ast-resolve-sub-cmd
(define/c (string-replace-suffix-in-sub-cmd cmd suffix with)
  (-> (or/c ast-resolve-sub-cmd? ast-resolve-byte-scmd? ast-resolve-word-scmd?) string? string? ast-resolve-sub-cmd?)
  (cond [(ast-resolve-byte-scmd? cmd)
         (struct-copy ast-resolve-byte-scmd cmd
                      [label #:parent ast-resolve-sub-cmd
                       (string-replace-suffix (ast-resolve-sub-cmd-label cmd) suffix with)])]
        [(ast-resolve-word-scmd? cmd)
         (struct-copy ast-resolve-word-scmd cmd
                      [label #:parent ast-resolve-sub-cmd
                       (string-replace-suffix (ast-resolve-sub-cmd-label cmd) suffix with)])]
        [(ast-resolve-sub-cmd? cmd)
         (struct-copy ast-resolve-sub-cmd cmd
                      [label
                       (string-replace-suffix (ast-resolve-sub-cmd-label cmd) suffix with)])]
        [else (raise-user-error (format "subcommand ~a unexpected here"))]))

(module+ test #| string-replace-suffix-in-sub-cmd |#
  (check-equal? (string-replace-suffix-in-sub-cmd (ast-resolve-sub-cmd "label__")
                                                  "__"
                                                  "__suffix")
                (ast-resolve-sub-cmd "label__suffix"))
  (check-equal? (string-replace-suffix-in-sub-cmd (ast-resolve-byte-scmd "label__" 'relative)
                                                  "__"
                                                  "__suffix")
                (ast-resolve-byte-scmd "label__suffix" 'relative))
  (check-equal? (string-replace-suffix-in-sub-cmd (ast-resolve-word-scmd "label__")
                                                  "__"
                                                  "__suffix")
                (ast-resolve-word-scmd "label__suffix")))

(define/c (replace-label cmd replacement-map)
  (-> ast-command? (hash/c string? string?) ast-command?)
  (cond [(ast-label-def-cmd? cmd)
         (struct-copy ast-label-def-cmd cmd
                      [label (hash-replace replacement-map (ast-label-def-cmd-label cmd))])]
        [(ast-unresolved-rel-opcode-cmd? cmd)
         (struct-copy ast-unresolved-rel-opcode-cmd cmd
                      [resolve-sub-command (string-replace-in-sub-cmd
                                            (ast-unresolved-rel-opcode-cmd-resolve-sub-command cmd)
                                            replacement-map)])]
        [(ast-unresolved-opcode-cmd? cmd)
         (struct-copy ast-unresolved-opcode-cmd cmd
                      [resolve-sub-command (string-replace-in-sub-cmd
                                            (ast-unresolved-opcode-cmd-resolve-sub-command cmd)
                                            replacement-map)])]
        [(ast-unresolved-bytes-cmd? cmd)
         (struct-copy ast-unresolved-bytes-cmd cmd
                      [resolve-sub-command (string-replace-in-sub-cmd
                                            (ast-unresolved-bytes-cmd-resolve-sub-command cmd)
                                            replacement-map)])]
        [(ast-decide-cmd? cmd)
         (define new-options (map (lambda (option) (replace-label option replacement-map))
                                  (ast-decide-cmd-options cmd)))
         (struct-copy ast-decide-cmd cmd
                      [options new-options])]
        [else cmd]))

(module+ test #| |#
  (check-equal? (replace-label
                 (ast-label-def-cmd '() "label")
                 (hash "label" "label_new"))
                (ast-label-def-cmd '() "label_new"))
  (check-equal? (replace-label
                 (ast-unresolved-bytes-cmd '() '() (ast-resolve-sub-cmd "label"))
                 (hash "label" "label_new"))
                (ast-unresolved-bytes-cmd '() '() (ast-resolve-sub-cmd "label_new")))
  (check-equal? (replace-label
                 (ast-unresolved-rel-opcode-cmd '() '() (ast-resolve-byte-scmd "label" 'relative ))
                 (hash "label" "label_new"))
                (ast-unresolved-rel-opcode-cmd '() '() (ast-resolve-byte-scmd "label_new" 'relative )))
  (check-equal? (replace-label
                 (ast-unresolved-opcode-cmd '() '() (ast-resolve-sub-cmd "label"))
                 (hash "label" "label_new"))
                (ast-unresolved-opcode-cmd '() '() (ast-resolve-sub-cmd "label_new")))
  (check-equal? (replace-label
                 (ast-decide-cmd
                  '()
                  (list
                   (ast-unresolved-opcode-cmd '() '(133) (ast-resolve-byte-scmd "label" 'low-byte))
                   (ast-unresolved-opcode-cmd '() '(141) (ast-resolve-word-scmd "label"))))
                 (hash "label" "label_new"))
                (ast-decide-cmd
                  '()
                  (list
                   (ast-unresolved-opcode-cmd '() '(133) (ast-resolve-byte-scmd "label_new" 'low-byte))
                   (ast-unresolved-opcode-cmd '() '(141) (ast-resolve-word-scmd "label_new"))))))

;; replace this suffix in label of unresolved command
(define/c (replace-label-suffix cmd suffix new-suffix)
  (-> ast-command? string? string? ast-command?)
  (cond [(ast-label-def-cmd? cmd)
         (struct-copy ast-label-def-cmd cmd
                      [label (string-replace-suffix
                              (ast-label-def-cmd-label cmd)
                              suffix
                              new-suffix)])]
        [(ast-unresolved-rel-opcode-cmd? cmd)
         (struct-copy ast-unresolved-rel-opcode-cmd cmd
                      [resolve-sub-command (string-replace-suffix-in-sub-cmd
                                            (ast-unresolved-rel-opcode-cmd-resolve-sub-command cmd)
                                            suffix
                                            new-suffix)])]
        [(ast-unresolved-opcode-cmd? cmd)
         (struct-copy ast-unresolved-opcode-cmd cmd
                      [resolve-sub-command (string-replace-suffix-in-sub-cmd
                                            (ast-unresolved-opcode-cmd-resolve-sub-command cmd)
                                            suffix
                                            new-suffix)])]
        [(ast-unresolved-bytes-cmd? cmd)
         (struct-copy ast-unresolved-bytes-cmd cmd
                      [resolve-sub-command (string-replace-suffix-in-sub-cmd
                                            (ast-unresolved-bytes-cmd-resolve-sub-command cmd)
                                            suffix
                                            new-suffix)])]
        [(ast-decide-cmd? cmd)
         (define new-options (map (lambda (option) (replace-label-suffix option suffix new-suffix))
                                  (ast-decide-cmd-options cmd)))
         (struct-copy ast-decide-cmd cmd
                      [options new-options])]
        [else cmd]))

(module+ test #| |#
  (check-equal? (replace-label-suffix
                 (ast-label-def-cmd '() "some__")
                 "__" "__suffix")
                (ast-label-def-cmd '() "some__suffix"))
  (check-equal? (replace-label-suffix
                 (ast-unresolved-bytes-cmd '() '() (ast-resolve-sub-cmd "some__"))
                 "__" "__suffix")
                (ast-unresolved-bytes-cmd '() '() (ast-resolve-sub-cmd "some__suffix")))
  (check-equal? (replace-label-suffix
                 (ast-unresolved-rel-opcode-cmd '() '() (ast-resolve-byte-scmd "some__" 'relative ))
                 "__" "__suffix")
                (ast-unresolved-rel-opcode-cmd '() '() (ast-resolve-byte-scmd "some__suffix" 'relative )))
  (check-equal? (replace-label-suffix
                 (ast-unresolved-opcode-cmd '() '() (ast-resolve-sub-cmd "some__"))
                 "__" "__suffix")
                (ast-unresolved-opcode-cmd '() '() (ast-resolve-sub-cmd "some__suffix")))
  (check-equal? (replace-label-suffix
                 (ast-decide-cmd
                  '()
                  (list
                   (ast-unresolved-opcode-cmd '() '(133) (ast-resolve-byte-scmd "some__" 'low-byte))
                   (ast-unresolved-opcode-cmd '() '(141) (ast-resolve-word-scmd "some__"))))
                 "__" "__suffix")
                (ast-decide-cmd
                  '()
                  (list
                   (ast-unresolved-opcode-cmd '() '(133) (ast-resolve-byte-scmd "some__suffix" 'low-byte))
                   (ast-unresolved-opcode-cmd '() '(141) (ast-resolve-word-scmd "some__suffix"))))))

;; replace suffix in label of all unresolved commands
(define/c (add-label-suffix suffix-to-replace new-suffix commands (transformed-commands '()))
  (->* [string? string? (listof ast-command?)] [(listof ast-command?)] (listof ast-command?))
  (cond
    [(empty? commands) (reverse transformed-commands)]
    [else
     (define cmd (car commands))
     (add-label-suffix
      suffix-to-replace
      new-suffix
      (cdr commands)
      (cons (replace-label-suffix cmd suffix-to-replace new-suffix)
            transformed-commands))]))

(module+ test #| add-label-suffix |#
  (check-equal? (add-label-suffix "__" "__suffix"
                                  (list (ast-label-def-cmd '() "some")
                                        (ast-unresolved-rel-opcode-cmd '() '() (ast-resolve-byte-scmd "some__" 'relative))
                                        (ast-label-def-cmd '() "some__")
                                        (ast-unresolved-bytes-cmd '() '() (ast-resolve-sub-cmd "some__+1"))
                                        (ast-unresolved-opcode-cmd '() '() (ast-resolve-sub-cmd "some__"))
                                        (ast-bytes-cmd '() '())))
                (list (ast-label-def-cmd '() "some")
                      (ast-unresolved-rel-opcode-cmd '() '() (ast-resolve-byte-scmd "some__suffix" 'relative))
                      (ast-label-def-cmd '() "some__suffix")
                      (ast-unresolved-bytes-cmd '() '() (ast-resolve-sub-cmd "some__suffix+1"))
                      (ast-unresolved-opcode-cmd '() '() (ast-resolve-sub-cmd "some__suffix"))
                      (ast-bytes-cmd '() '()))))

;; replace suffix in label of all unresolved commands
(define/c (replace-labels replacement-map commands (transformed-commands '()))
  (->* [(hash/c string? string?) (listof ast-command?)] [(listof ast-command?)] (listof ast-command?))
  (cond
    [(empty? commands) (reverse transformed-commands)]
    [else
     (define cmd (car commands))
     (replace-labels
      replacement-map
      (cdr commands)
      (cons (replace-label cmd replacement-map)
            transformed-commands))]))

(module+ test #| replace-labels |#
  (check-equal? (replace-labels (hash "label" "label_new")
                                  (list (ast-label-def-cmd '() "some")
                                        (ast-unresolved-rel-opcode-cmd '() '() (ast-resolve-byte-scmd "label" 'relative))
                                        (ast-label-def-cmd '() "label")
                                        (ast-unresolved-bytes-cmd '() '() (ast-resolve-sub-cmd "label+1"))
                                        (ast-unresolved-opcode-cmd '() '() (ast-resolve-sub-cmd "label"))
                                        (ast-bytes-cmd '() '())))
                (list (ast-label-def-cmd '() "some")
                      (ast-unresolved-rel-opcode-cmd '() '() (ast-resolve-byte-scmd "label_new" 'relative))
                      (ast-label-def-cmd '() "label_new")
                      (ast-unresolved-bytes-cmd '() '() (ast-resolve-sub-cmd "label_new+1"))
                      (ast-unresolved-opcode-cmd '() '() (ast-resolve-sub-cmd "label_new"))
                      (ast-bytes-cmd '() '()))))

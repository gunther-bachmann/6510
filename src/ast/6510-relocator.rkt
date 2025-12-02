#lang racket
#|

 provide functions for code (re)location, based on resolved/calculated command len

 |#

(require (rename-in racket/contract
                    [define/contract define/c])
         (only-in racket/fixnum
                  fx+)
         (only-in "../6510-utils.rkt"
                  byte/c
                  word/c
                  low-byte
                  high-byte
                  absolute
                  word)
         "6510-command.rkt")

(provide
 label-string-offsets
 code-len
 command-len
 label->hilo-indicator
 )

(module+ test
  (require "../6510-test-utils.rkt"))

(define command/c (or/c ast-command? (listof any/c)))

(define/c (code-len code)
  (-> (listof command/c) nonnegative-integer?)
  (foldl + 0 (map command-len code)))

(module+ test #|code-len|#
  (check-equal? (code-len
                 (list))
                0
                "empty list has code len 0")

  (check-equal? (code-len
                 (list
                  (ast-label-def-cmd '() "some")
                  (ast-label-def-cmd '() "other")))
                0
                "just labels have together code len 0")

  (check-equal? (code-len
                 (list
                  (ast-label-def-cmd '() "some")
                  (ast-opcode-cmd '() (list 1 2 3))
                  (ast-label-def-cmd '() "other")))
                3
                "mix code has the right code len"))

(define/c (command-len command)
  (-> command/c nonnegative-integer?)
  (cond
    [(ast-decide-cmd? command)
     (apply max
      (map command-len (ast-decide-cmd-options command)))] ;; just estimated, dependes on decisions to be made
    [(ast-org-command? command) 0]
    [(ast-org-align-command? command) 0]
    [(ast-unresolved-bytes-cmd? command)
     (if (ast-resolve-word-scmd? (ast-unresolved-bytes-cmd-resolve-sub-command command))
         2
         1)]
    [(ast-bytes-cmd? command)
     (length (ast-bytes-cmd-bytes command))]
    [(ast-rel-opcode-cmd? command)
     2]
    [(ast-unresolved-opcode-cmd? command)
     (+ (length (ast-opcode-cmd-bytes command))
        (if (ast-resolve-word-scmd?
             (ast-unresolved-opcode-cmd-resolve-sub-command command))
            2
            1))]
    [(ast-unresolved-rel-opcode-cmd? command) 2]
    ;; order is important here!
    [(ast-opcode-cmd? command)
     (length (ast-opcode-cmd-bytes command))]
    [(ast-label-def-cmd? command) 0]
    [(ast-const? command) 0]
    [(ast-provide? command) 0]
    [(ast-require? command) 0]
    [else (raise-user-error (format "unknown command for command len ~a" command))]))

(module+ test #| command-len |#
  (check-equal? (command-len (ast-opcode-cmd '() '(100)))
                1)
  (check-equal? (command-len (ast-unresolved-bytes-cmd '() '() (ast-resolve-word-scmd "some")))
                2)
  (check-equal? (command-len (ast-label-def-cmd '() "some"))
                0)
  (check-equal? (command-len (ast-const-word-cmd '() "some" #x2000))
                0)
  (check-equal? (command-len (ast-const-byte-cmd '() "some" #x20))
                0)
  (check-equal? (command-len (ast-bytes-cmd '() '(#xd2 #xff)))
                2)
  (check-equal? (command-len (ast-opcode-cmd '() '(#x20 #xff #xd2)))
                3)
  (check-equal? (command-len (ast-rel-opcode-cmd '() '(#x20 #xff)))
                2)
  (check-equal? (command-len (ast-unresolved-rel-opcode-cmd '() '(#x20) (ast-resolve-byte-scmd "some" 'relative)))
                2)
  (check-equal? (command-len (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-word-scmd "some")))
                3)
  (check-equal? (command-len (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-byte-scmd "some" 'low-byte)))
                2))

(define/c (label-string-offsets offset commands)
  (-> nonnegative-integer? (listof command/c) hash?)
  (-label-string-offsets offset (hash) commands))

;; collect all labels with their respective offset into the given hash
;; make sure that the commands do NOT contain any decisions (they must be resolved)
(define/c (-label-string-offsets offset collected-results commands)
  (-> nonnegative-integer? hash? (listof command/c) hash?)
  (if (empty? commands)
      collected-results
      (let* ((command (car commands))
             (next-results
              (cond
                [(ast-label-def-cmd? command)
                 (define label (ast-label-def-cmd-label command))
                 (when (hash-has-key? collected-results label)
                   (raise-user-error (format "duplicate label found '~a' at ~a, now again at ~a"
                                             label
                                             (hash-ref collected-results label)
                                             offset)))
                 (hash-set collected-results label offset)]
                [else collected-results])))
        (cond
          [(ast-org-command? command)
           (-label-string-offsets (ast-org-command-org command) next-results (cdr commands))]
          [(ast-org-align-command? command)
           (define al (ast-org-align-command-org-alignment command))
           (-label-string-offsets (+ offset (- al (bitwise-and offset (sub1 al)))) next-results (cdr commands))]
          [else
           (-label-string-offsets (+ offset (command-len command)) next-results (cdr commands))]))))

(module+ test #| collect-label-offsets |#
  (check-equal? (label-string-offsets 10 (list (ast-label-def-cmd '() "some")))
                '#hash(("some" . 10)))
  (check-equal? (label-string-offsets 10 (list (ast-opcode-cmd '() '(#x20 #xd2 #xff))
                                               (ast-label-def-cmd '() "some")))
                '#hash(("some" . 13)))
  (check-equal? (label-string-offsets 10 (list (ast-label-def-cmd '() "hello")
                                               (ast-opcode-cmd '() '(#x20 #xd2 #xff))
                                               (ast-label-def-cmd '() "some")
                                               (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-word-scmd "hello"))
                                               (ast-label-def-cmd '() "other")
                                               (ast-unresolved-opcode-cmd '() '(#xA2) (ast-resolve-byte-scmd "hello" 'low-byte))
                                               (ast-label-def-cmd '() "after-other")))
                '#hash(("some" . 13)
                       ("hello" . 10)
                       ("other" . 16)
                       ("after-other" . 18)))
  (check-equal? (label-string-offsets 10 (list (ast-label-def-cmd '() "hello")
                                               (ast-opcode-cmd '() '(#x20 #xd2 #xff))
                                               (ast-label-def-cmd '() "some")
                                               (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-word-scmd "hello"))
                                               (ast-org-command '() #x40)
                                               (ast-label-def-cmd '() "other")
                                               (ast-unresolved-opcode-cmd '() '(#xA2) (ast-resolve-byte-scmd "hello" 'low-byte))
                                               (ast-label-def-cmd '() "after-other")
                                               (ast-org-align-command '() #x10)
                                               (ast-label-def-cmd '() "after-align")))
                '#hash(("some" . 13)
                       ("hello" . 10)
                       ("other" . 64)
                       ("after-other" . 66)
                       ("after-align" . 80))))

(define/c (label->hilo-indicator full-label)
  (-> string? byte/c)
  (cond [(string-prefix? full-label ">") 1]
        [(string-prefix? full-label "<") 0]
        [else (raise-user-error (format "full-label ~a has no hi/low prefix" full-label))]))

;; relocation table format
;; offset       data
;; 0            rel-position-low, rel-position-high, : position where the value has to be written to
;; 2            width (byte), (if width = 1 0:lowbyte 1:highbyte)?,
;; 3/4          rel-value-low, rel-value-high        : (val + origin) is the value to be written
(define/c (reloc-entry-bytes offset rel-offset . bytes)
  (->* (word/c word/c) (listof byte/c) (listof byte/c))
  (append (list (low-byte (+ 1 offset))
                (high-byte (+ 1 offset)))
          bytes
          (list (low-byte rel-offset)
                (high-byte rel-offset))))

(define/c (resolve-word->reloc-bytes resolve label-offsets offset)
  (-> ast-resolve-sub-cmd? hash? word/c (listof byte/c))
  (let ([rel-offset (hash-ref label-offsets (ast-resolve-sub-cmd-label resolve))])
    (reloc-entry-bytes offset rel-offset 2)))

(module+ test #| resolve-word->reloc-bytes |#
  (check-equal? (resolve-word->reloc-bytes (ast-resolve-word-scmd "some")
                                          '#hash(("some" . #x01d2))
                                          #xc040)
                '(#x41 #xc0 2 #xd2 #x01)))

(define/c (resolve-byte->reloc-bytes resolve label-offsets offset)
  (-> ast-resolve-sub-cmd? hash? word/c (listof byte/c))
  (let* ((label (ast-resolve-sub-cmd-label resolve))
         (rel-offset (hash-ref label-offsets label)))
    (reloc-entry-bytes offset rel-offset 1 (if (eq? 'high-byte (ast-resolve-byte-scmd-mode resolve)) 1 0))))

(module+ test #| resolve-byte->reloc-bytes |#
  (check-equal? (resolve-byte->reloc-bytes (ast-resolve-byte-scmd "some" 'high-byte)
                                          '#hash(("some" . #x01fa))
                                          #x0005)
                '(#x06 #x00 1 1 #xfa #x01))
  (check-equal? (resolve-byte->reloc-bytes (ast-resolve-byte-scmd "some" 'low-byte)
                                          '#hash(("some" . #x01fa))
                                          #x0005)
                '(#x06 #x00 1 0 #xfa #x01)))

(define/c (reloc-table-bytes offset collected-entries label-offsets commands)
  (-> word/c (listof byte/c) hash? (listof ast-command?) (listof byte/c))
  (if (empty? commands)
      collected-entries
      (let* ((command      (car commands))
             (res          (and (ast-unresolved-opcode-cmd? command)
                              (ast-unresolved-opcode-cmd-resolve-sub-command command)))
             (next-offset  (+ offset (command-len command)))
             (entry        (cond [(ast-resolve-word-scmd? res)
                                  (resolve-word->reloc-bytes res label-offsets offset)]
                                 [(ast-resolve-byte-scmd? res)
                                  (resolve-byte->reloc-bytes res label-offsets offset)]
                                 [else '()]))
             (next-entries (append collected-entries entry)))
        (reloc-table-bytes next-offset next-entries label-offsets (cdr commands)))))

(module+ test #| reloc-table-bytes |#
  (check-equal? (reloc-table-bytes #xc040 '() '#hash(("some" . #x01d2)("other" . #x01d9))
                                   (list (ast-unresolved-opcode-cmd '() '(20) (ast-resolve-byte-scmd "some" 'high-byte))
                                         (ast-unresolved-opcode-cmd '() '(20) (ast-resolve-word-scmd "other"))
                                         (ast-unresolved-opcode-cmd '() '(20) (ast-resolve-byte-scmd "some" 'low-byte))))
                '(#x41 #xc0 1 1 #xd2 #x01
                  #x43 #xc0 2 #xd9 #x01
                  #x46 #xc0 1 0 #xd2 #x01)))

(define/c (value-by-hilo-ind hilo-ind value)
  (-> byte/c word/c byte/c)
  (cond [(= 1 hilo-ind) (high-byte value)]
        [else (low-byte value)]))

(module+ test #| value-by-hilo-ind |#
  (check-eq? (value-by-hilo-ind 0 #xffd2)
             #xd2)
  (check-eq? (value-by-hilo-ind 1 #xffd2)
             #xff))

(define/c (relocated-bytes width reloc-table delta)
  (-> byte/c (listof byte/c) word/c (listof byte/c))
  (cond [(= 1 width)
         (match-let* (((list _ _ _ hilo-ind low high) (take reloc-table 6))
                      (value (word (fx+ delta (absolute high low)))))
           (list (value-by-hilo-ind hilo-ind value)))]
        [(= 2 width)
         (match-let* (((list _ _ _ low high) (take reloc-table 5))
                      (value (word (fx+ delta (absolute high low)))))
           (list (low-byte value) (high-byte value)))]
        [else (raise-user-error (format "relocated-bytes width ~a is unknown" width))]))

(define/c (relocate-program delta reloc-table bytes)
  (-> word/c  (listof byte/c) (listof byte/c) (listof byte/c))
  (-relocate-program delta 0 reloc-table bytes '()))

(define/c (-relocate-program delta offset reloc-table bytes result)
  (-> word/c word/c (listof byte/c) (listof byte/c) (listof byte/c) (listof byte/c))
  (cond
    [(empty? bytes)       result]
    [(empty? reloc-table) (append result bytes)]
    [else
     (match-let*
         (((list offset-low offset-high width) (take reloc-table 3))
          (reloc-applies (eq? offset (absolute offset-high offset-low)))
          ((list offset-inc rem-reloc rem-bytes reloc-bytes)
           (if reloc-applies
               (list width
                     (drop reloc-table (- 7 width))
                     (drop bytes width)
                     (relocated-bytes width reloc-table delta))
               (list 1 reloc-table (cdr bytes) (take bytes 1)))))
       (-relocate-program delta (+ offset offset-inc) rem-reloc rem-bytes (append result reloc-bytes)))]))

(module+ test #| relocate-program |#
  (check-equal? (relocate-program +10 '(#x01 #x00 1 1 #xfe #x34) '(#x20 #xc1 #xc2 #x09))
                '(#x20 #x35 #xc2 #x09))
  (check-equal? (relocate-program +10 '(#x01 #x00 1 0 #xfe #x34) '(#x20 #xc1 #xc2 #x09))
                '(#x20 #x08 #xc2 #x09))
  (check-equal? (relocate-program +10 '(#x01 #x00 2 #xfe #x34) '(#x20 #xc1 #xc2 #x09))
                '(#x20 #x08 #x35 #x09)))

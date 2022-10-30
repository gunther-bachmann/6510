#lang racket

(require (rename-in  racket/contract [define/contract define/c]))
(require (only-in "6510-utils.rkt" byte/c word/c low-byte high-byte absolute))
(require "6510-command.rkt")

(provide label-string-offsets command-len label->hilo-indicator)

(module+ test
  (require "6510-test-utils.rkt"))

(define command/c (or/c ast-command? (listof any/c)))

(define/c (command-len command)
  (-> command/c nonnegative-integer?)
  (cond
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
    ;; order is important here!
    [(ast-opcode-cmd? command)
     (length (ast-opcode-cmd-bytes command))]
    [#t 0]))

(module+ test #| command-len |#
  (check-equal? (command-len (ast-opcode-cmd '(100)))
                1)
  (check-equal? (command-len (ast-label-def-cmd "some"))
                0)
  (check-equal? (command-len (ast-const-word-cmd "some" #x2000))
                0)
  (check-equal? (command-len (ast-const-byte-cmd "some" #x20))
                0)
  (check-equal? (command-len (ast-bytes-cmd '(#xd2 #xff)))
                2)
  (check-equal? (command-len (ast-opcode-cmd '(#x20 #xff #xd2)))
                3)
  (check-equal? (command-len (ast-rel-opcode-cmd '(#x20 #xff)))
                2)
  (check-equal? (command-len (ast-unresolved-rel-opcode-cmd '(#x20) (ast-resolve-byte-scmd "some" 'relative)))
                2)
  (check-equal? (command-len (ast-unresolved-opcode-cmd '(#x20) (ast-resolve-word-scmd "some")))
                3)
  (check-equal? (command-len (ast-unresolved-opcode-cmd '(#x20) (ast-resolve-byte-scmd "some" 'low-byte)))
                2))

(define/c (label-string-offsets offset collected-results commands)
  (-> nonnegative-integer? hash? (listof command/c) hash?)
  (if (empty? commands)
      collected-results
      (let* ((command (car commands))
             (next-results
              (cond
                [(ast-label-def-cmd? command)
                 (hash-set collected-results (ast-label-def-cmd-label command) offset)]
                [#t collected-results])))
        (label-string-offsets (+ offset (command-len command)) next-results (cdr commands)))))

(module+ test #| collect-label-offsets |#
  (check-equal? (label-string-offsets 10 (hash) (list (ast-label-def-cmd "some")))
                '#hash(("some" . 10)))
  (check-equal? (label-string-offsets 10 (hash) (list (ast-opcode-cmd '(#x20 #xd2 #xff))
                                                      (ast-label-def-cmd "some")))
                '#hash(("some" . 13)))
  (check-equal? (label-string-offsets 10 (hash) (list (ast-label-def-cmd "hello")
                                                      (ast-opcode-cmd '(#x20 #xd2 #xff))
                                                      (ast-label-def-cmd "some")
                                                      (ast-unresolved-opcode-cmd '(#x20) (ast-resolve-word-scmd "hello"))))
                '#hash(("some" . 13)
                       ("hello" . 10))))

(define/c (label->hilo-indicator full-label)
  (-> string? byte/c)
  (cond [(string-prefix? full-label ">") 1]
        [(string-prefix? full-label "<") 0]
        [#t (raise-user-error (format "full-label ~a has no hi/low prefix" full-label))]))

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
  (let ((rel-offset (hash-ref label-offsets (ast-resolve-sub-cmd-label resolve))))                 
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
                                 [#t '()]))
             (next-entries (append collected-entries entry)))
        (reloc-table-bytes next-offset next-entries label-offsets (cdr commands)))))

(module+ test #| reloc-table-bytes |#
  (check-equal? (reloc-table-bytes #xc040 '() '#hash(("some" . #x01d2)("other" . #x01d9))
                                   (list (ast-unresolved-opcode-cmd '(20) (ast-resolve-byte-scmd "some" 'high-byte))
                                         (ast-unresolved-opcode-cmd '(20) (ast-resolve-word-scmd "other"))
                                         (ast-unresolved-opcode-cmd '(20) (ast-resolve-byte-scmd "some" 'low-byte))))
                '(#x41 #xc0 1 1 #xd2 #x01
                  #x43 #xc0 2 #xd9 #x01
                  #x46 #xc0 1 0 #xd2 #x01)))

(define/c (value-by-hilo-ind hilo-ind value)
  (-> byte/c word/c byte/c)
  (cond [(= 1 hilo-ind) (high-byte value)]
        [#t (low-byte value)]))

(module+ test #| value-by-hilo-ind |#
  (check-eq? (value-by-hilo-ind 0 #xffd2)
             #xd2)
  (check-eq? (value-by-hilo-ind 1 #xffd2)
             #xff))

(define/c (relocate-program delta offset reloc-table bytes result)
  (-> word/c word/c (listof byte/c) (listof byte/c) (listof byte/c) (listof byte/c))
  (if (empty? bytes)
      result      
      (if (empty? reloc-table)
          (relocate-program delta (+ offset (length bytes)) reloc-table '() (append result bytes))
          (match-let (((list offset-low offset-high width _ ...) reloc-table))
            (if (not (eq? offset (absolute offset-high offset-low)))
                (relocate-program delta (+ offset 1) reloc-table (cdr bytes) (append result (list (car bytes))))
                (cond [(= 1 width)
                       (match-let* (((list _ _ _ hilo-ind low high) reloc-table)
                                    (value (+ delta (absolute high low))))
                         (relocate-program delta (+ offset 1) (drop reloc-table 6) (cdr bytes) (append result (list (value-by-hilo-ind hilo-ind value)))))]
                      [(= 2 width)
                       (match-let* (((list _ _ _ low high) reloc-table)
                                    (value (+ delta (absolute high low))))
                         (relocate-program delta (+ offset 2) (drop reloc-table 5) (cddr bytes) (append result (list (low-byte value) (high-byte value)))))]
                      [#t (raise-user-error "some")]))))))

(module+ test #| relocate-program |#
  (check-equal? (relocate-program +10 0 '(#x01 #x00 1 1 #xfe #x34) '(#x20 #xc1 #xc2 #x09) '())
                '(#x20 #x35 #xc2 #x09))
  (check-equal? (relocate-program +10 0 '(#x01 #x00 1 0 #xfe #x34) '(#x20 #xc1 #xc2 #x09) '())
                '(#x20 #x08 #xc2 #x09))
  (check-equal? (relocate-program +10 0 '(#x01 #x00 2 #xfe #x34) '(#x20 #xc1 #xc2 #x09) '())
                '(#x20 #x08 #x35 #x09)))

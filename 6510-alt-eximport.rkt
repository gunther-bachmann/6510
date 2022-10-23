#lang racket

(require (rename-in  racket/contract [define/contract define/c]))

(require "6510-alt-resolver.rkt")
(require "6510-alt-relocator.rkt")

(require "6510-test-utils.rkt")
(require "6510-utils.rkt")
(require "6510-alt-command.rkt")

(provide import-table-bytes export-table-bytes)

;; constants hash label->const
;; labels hash label->rel-pos to load-pos

(define/c (string->bytes string)
  (-> string? (listof byte/c))
  (cons
   (low-byte (string-length string))
   (map char->integer (string->list string))))

(define/c (encode-word-const const-value label)
  (-> word/c string? (listof byte/c))
  (append (list 2 0
                (low-byte const-value)
                (high-byte const-value))
          (string->bytes label)))

(define/c (encode-byte-const const-value label)
  (-> byte/c string? (listof byte/c))
  (append (list 1 const-value)
          (string->bytes label)))

(define/c (encode-word-relative rel-label label)
  (-> word/c string? (listof byte/c))
  (append (list 2 1
                (low-byte rel-label)
                (high-byte rel-label))
          (string->bytes label)))

(define/c (ast-provide-cmd-label command)
  (-> (or/c ast-provide-word-cmd? ast-provide-byte-cmd?) string?)
  (or (and (ast-provide-word-cmd? command)
        (ast-provide-word-cmd-label command))
     (and (ast-provide-byte-cmd? command)
        (ast-provide-byte-cmd-label command))))

(define/c (provide-entry provide-command labels constants)
  (-> (or/c ast-provide-word-cmd? ast-provide-byte-cmd?) hash? hash? (listof byte/c))
  (let* ((label       (ast-provide-cmd-label provide-command))
         (const-value (hash-ref constants label #f))
         (rel-label   (hash-ref labels label #f)))
    (cond [(and const-value (ast-provide-word-cmd? provide-command))
           (encode-word-const const-value label)]
          [(and const-value (ast-provide-byte-cmd? provide-command))
           (encode-byte-const const-value label)]
          [(and rel-label (ast-provide-word-cmd? provide-command))
           (encode-word-relative rel-label label)]
          [#t (raise-user-error "cannot be converted")])))

(module+ test #| provide-entry |#
  (check-equal? (provide-entry (ast-provide-word-cmd "some")
                               (hash)
                               '#hash(("some" . #xFFD2)))
                 '(2 0 #xD2 #xFF 4 115 111 109 101))
  (check-equal? (provide-entry (ast-provide-byte-cmd "some")
                               (hash)
                               '#hash(("some" . #xD2)))
                 '(1 #xD2 4 115 111 109 101))
  (check-equal? (provide-entry (ast-provide-word-cmd "some")
                               '#hash(("some" . #x01D2))
                               (hash))
                '(2 1 #xD2 #x01 4 115 111 109 101)))

;; export table format
;; offset           data
;; 0                width (byte), (if width = 2 0: absolute-const, 1:relative-load-address)?.
;; 1/2              value-low|rel-position-low (optional value-high|rel-position-high)
;; 1/2+width        strlen (byte)
;; 2/3+width        string
;; 1/2+width+strlen ... next entry

(define (export-table-bytes collected-bytes labels constants commands)
  (if (empty? commands)
      collected-bytes
      (let* ((command (car commands))
             (bytes   (if (or (ast-provide-byte-cmd? command)
                             (ast-provide-word-cmd? command))
                          (provide-entry command labels constants)
                          '())))        
        (export-table-bytes (append collected-bytes bytes)
                            labels constants (cdr commands)))))

(module+ test #| export-table-bytes |#
  (check-equal? (export-table-bytes
                 '()
                 '#hash(("none" . #x01E0))
                 '#hash(("some" . #xFFD2)
                        ("other" . #xC0))
                 (list (ast-provide-word-cmd "some")
                       (ast-provide-word-cmd "none")
                       (ast-provide-byte-cmd "other")))
                 '(2 0 #xD2 #xFF 4 115 111 109 101
                   2 1 #xE0 #x01 4 110 111 110 101
                   1 #xC0 5 111 116 104 101 114)))

;; import table format
;; offset           data
;; 0                rel-position-low, rel-position-high, : position where the value has to be written to
;; 2                width (byte), (if width = 1 0:lowbyte 1:highbyte)?,
;; 3/4              strlen (byte)
;; 4/5              string 
;; 4/5+strlen       ...next entry

(define (encode-import-word-entry offset label)
  (append
   (list (low-byte offset)
         (high-byte offset)
         2)
   (string->bytes label)))

(define (encode-import-byte-entry offset hilo-ind label)
  (append
   (list (low-byte offset)
         (high-byte offset)
         1 hilo-ind)
   (string->bytes label)))

(define (import-word-entry-bytes label req-hashes offset)
  (let* ((value (hash-ref req-hashes label)))
    (if (eq? 'word value)
        (encode-import-word-entry offset label)
        (encode-import-byte-entry offset 0 label))))

(define (import-byte-entry-bytes label req-hashes offset hilo-ind)
  (let* ((value      (hash-ref req-hashes label)))
    (if (eq? 'word value)
        (encode-import-byte-entry offset (if (eq? 'high-byte hilo-ind) 1 0) label)
        (encode-import-byte-entry offset 0 label))))

(define (import-table-bytes offset collected-entries req-hashes commands)
  (if (empty? commands)
      collected-entries
      (let* ((command  (car commands))
             (res      (and (ast-unresolved-opcode-cmd? command)
                          (ast-unresolved-opcode-cmd-resolve-sub-command command)))
             (offset+1 (+ offset 1))
             (new-entry 
              (cond [(ast-resolve-word-scmd? res)
                     (import-word-entry-bytes (ast-resolve-sub-cmd-label res) req-hashes offset+1)]
                    [(ast-resolve-byte-scmd? res)
                     (import-byte-entry-bytes (ast-resolve-sub-cmd-label res) req-hashes offset+1
                                              (ast-resolve-byte-scmd-mode res))]
                    [#t '()])))
        (import-table-bytes (+ offset (command-len command))
                            (append collected-entries new-entry)
                            req-hashes
                            (cdr commands)))))

(module+ test #| import-table-bytes |#
  (check-equal? (import-table-bytes #xfe '() '#hash(("some" . word) ("other" . byte))
                                    (list(ast-unresolved-opcode-cmd '(#x20) (ast-resolve-word-scmd "some"))
                                         (ast-unresolved-opcode-cmd '(#xea) (ast-resolve-byte-scmd "other" 'low-byte))
                                         (ast-unresolved-opcode-cmd '(#xea) (ast-resolve-byte-scmd "some" 'low-byte))
                                         (ast-unresolved-opcode-cmd '(#xea) (ast-resolve-byte-scmd "some" 'high-byte))))
                '(#xff #x00 2 4 115 111 109 101
                  #x02 #x01 1 0 5 111 116 104 101 114
                  #x04 #x01 1 0 4 115 111 109 101
                  #x06 #x01 1 1 4 115 111 109 101)))

(define (require-hash command hash)
  (cond [(ast-require-word-cmd? command)
         (hash-set hash (ast-require-word-cmd-label command) 'word)]
        [(ast-require-byte-cmd? command)
         (hash-set hash (ast-require-byte-cmd-label command) 'byte)]
        [#t hash]))

(define (require-hashes commands)
  (foldl require-hash (hash) commands))

(module+ test #| require-hashes |#
  (check-equal? (require-hashes (list (ast-require-word-cmd "some")
                                      (ast-require-byte-cmd "other")
                                      (ast-opcode-cmd '(#x20))))
                '#hash(("some" . word)
                       ("other" . byte))))

(define providing-program
  '((provide-word aout)
    (provide-byte char)
    (byte-const char $65)
    (label aout)
    (LDA !char)
    (JMP $FFD2)))

(define requiring-program
  '((require-word aout)
    (require-byte char)
    (LDX !$05)
    (label repeat)
    (JSR aout)
    (DEX)
    (BNE repeat)
    (CLC)
    (LDA !char)
    (ADC !#01)
    (JSR $FFD2)
    (BRK)))

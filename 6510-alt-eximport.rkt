#lang racket

(require "6510-alt-resolver.rkt")
(require "6510-alt-relocator.rkt")

(require "6510-test-utils.rkt")
(require "6510-utils.rkt")

(provide import-table-bytes export-table-bytes)

;; constants hash label->const
;; labels hash label->rel-pos to load-pos

(define (string->bytes string)
  (cons
    (low-byte (string-length string))
    (map char->integer (string->list string))))

(define (encode-word-const const-value label)
  (append (list 2 0
                (low-byte const-value)
                (high-byte const-value))
          (string->bytes label)))

(define (encode-byte-const const-value label)
  (append (list 1
                (low-byte const-value))
          (string->bytes label)))

(define (encode-word-relative rel-label label)
  (append (list 2 1
                (low-byte rel-label)
                (high-byte rel-label))
          (string->bytes label)))

(define (provide-entry provide-command labels constants)
  (match-let* (((list tag label) provide-command)
               (const-value      (hash-ref constants label #f))
               (rel-label        (hash-ref labels label #f)))
    (cond [(and const-value (eq? tag 'provide-word))
           (encode-word-const const-value label)]
          [(and const-value (eq? tag 'provide-byte))
           (encode-byte-const const-value label)]
          [(and rel-label (eq? tag 'provide-word))
           (encode-word-relative rel-label label)]
          [#t (raise-user-error "cannot be converted")])))

(module+ test #| provide-entry |#
  (check-equal? (provide-entry '(provide-word "some")
                               (hash)
                               '#hash(("some" . #xFFD2)))
                 '(2 0 #xD2 #xFF 4 115 111 109 101))
  (check-equal? (provide-entry '(provide-byte "some")
                               (hash)
                               '#hash(("some" . #xD2)))
                 '(1 #xD2 4 115 111 109 101))
  (check-equal? (provide-entry '(provide-word "some")
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
             (tag     (car command))
             (bytes   (if (or (eq? tag 'provide-byte)
                             (eq? tag 'provide-word))
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
                 '((provide-word "some")
                   (provide-word "none")
                   (provide-byte "other")))
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

(define (import-byte-entry-bytes label req-hashes offset)
  (let* ((base-label (base-label-str label))
         (value      (hash-ref req-hashes base-label)))
    (if (eq? 'word value)
        (encode-import-byte-entry offset (label->hilo-indicator label) base-label)
        (encode-import-byte-entry offset 0 base-label))))

(define (import-table-bytes offset collected-entries req-hashes commands)
  (if (empty? commands)
      collected-entries
      (let* ((command  (car commands))
             (last-el  (last command))
             (offset+1 (+ offset 1))
             (new-entry 
              (cond [(resolve-word? last-el)
                     (import-word-entry-bytes (cadr last-el) req-hashes offset+1)]
                    [(resolve-byte? last-el)
                     (import-byte-entry-bytes (cadr last-el) req-hashes offset+1)]
                    [#t '()])))
        (import-table-bytes (+ offset (command-len command))
                            (append collected-entries new-entry)
                            req-hashes
                            (cdr commands)))))

(module+ test #| import-table-bytes |#
  (check-equal? (import-table-bytes #xfe '() '#hash(("some" . word) ("other" . byte))
                                    '((opcode #x20 (resolve-word "some"))
                                      (opcode #xea (resolve-byte "other"))
                                      (opcode #xea (resolve-byte "<some"))
                                      (opcode #xea (resolve-byte ">some"))))
                '(#xff #x00 2 4 115 111 109 101
                  #x02 #x01 1 0 5 111 116 104 101 114
                  #x04 #x01 1 0 4 115 111 109 101
                  #x06 #x01 1 1 4 115 111 109 101)))

(define (require-command? command)
  (and (list? command)
     (or (eq? 'require-word (car command))
        (eq? 'require-byte (car command)))))

(define (require-hash command hash)
  (if (require-command? command)
      (hash-set hash (last command)
                (if (eq? 'require-word (car command))
                    'word
                    'byte))
      hash))

(define (require-hashes commands)
  (foldl require-hash (hash) commands))

(module+ test #| require-hashes |#
  (check-equal? (require-hashes '((require-word "some")
                                  (require-byte "other")
                                  (opcode #x20)))
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

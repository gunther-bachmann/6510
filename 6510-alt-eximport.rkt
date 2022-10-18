#lang racket

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

(define (import-table-bytes offset collected-entries label-offsets commands)
  '())

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

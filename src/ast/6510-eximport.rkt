#lang racket

#|

 create export table bytes and import table bytes of a list of ast commands

 |#

(require (rename-in  racket/contract [define/contract define/c]))
(require (only-in "6510-relocator.rkt" command-len label-string-offsets))
(require (only-in "../6510-utils.rkt" low-byte high-byte byte/c word/c absolute))
(require (only-in threading ~>>))
(require "6510-command.rkt")
(require (only-in "6510-resolver.rkt" ->resolve-labels ->resolved-decisions label-instructions resolved-program->bytes))
(require (only-in "6510-constants.rkt" resolve-constants constant-definitions-hash))

(require "../6510.rkt")

(provide import-table-bytes export-table-bytes)

(module+ test
  (require "../6510-test-utils.rkt"))

;; constants hash label->const
;; labels hash label->rel-pos to load-pos

;; string of length <= 255, that can be encoded 
(define byte-len-string? (and/c string? (lambda (str) (< (string-length str) 256))))

;; encode a string into bytes
(define/c (string->bytes string)
  (-> byte-len-string? (listof byte/c))
  (cons
   (low-byte (string-length string))
   (map char->integer (string->list string))))

(module+ test #| string->bytes |#
  (check-exn exn:fail:contract? (lambda () (string->bytes (make-string 256 #\a))))
  (check-not-exn (lambda () (string->bytes (make-string 255 #\a))))
  (check-equal? (string->bytes "A")
                (list 1 65))
  (check-equal? (string->bytes "AB")
                (list 2 65 66))
  (check-equal? (string->bytes "")
                (list 0)))

(define/c (bytes->string bytes)
  (-> (listof byte/c) byte-len-string?)
  (define len (car bytes))
  (~>> (drop bytes 1)
     (take _ len)
     (map integer->char)
     (apply string)))

(module+ test #|bytes->string|#
  (check-equal? (bytes->string (list 2 65 66 67))
                "AB")
  (check-equal? (bytes->string (list 1 65 66))
                "A")
  (check-equal? (bytes->string (list 0 1 2))
                "")
  (check-equal? (bytes->string (string->bytes "Hello World"))
                "Hello World"))

;; encode entry for word constant
(define/c (encode-word-const const-value label)
  (-> word/c string? (listof byte/c))
  (append (list 2 0
                (low-byte const-value)
                (high-byte const-value))
          (string->bytes label)))

;; check whether the next bytes indicate an encoded word constant
(define/c (encoded-word-const? bytes)
  (-> (listof byte/c) boolean?)
  (equal? (take bytes 2)
       (list 2 0)))

;; decode entry of word constant
(define/c (decode-word-const bytes)
  (-> (listof byte/c) (values word/c string?))
  (define hi (list-ref bytes 3))
  (define lo (list-ref bytes 2))
  (values (+ lo (* 256 hi)) (bytes->string (drop bytes 4))))

(module+ test #| encode-word/decode-word |#
  (check-equal? (encode-word-const #xC010 "ABC")
                (list 2 0 #x10 #xc0 3 65 66 67))

  (let-values (((address string) (decode-word-const (list 2 0 #x20 #xd1 2 65 67))))
    (check-equal? address #xd120)
    (check-equal? string "AC"))  

  (let-values (((address string) (decode-word-const (encode-word-const #xC010 "some"))))
    (check-equal? address #xc010)
    (check-equal? string "some")))

;; encode entry for byte constant
(define/c (encode-byte-const const-value label)
  (-> byte/c string? (listof byte/c))
  (append (list 1 const-value)
          (string->bytes label)))

(define/c (encoded-byte-const? bytes)
  (-> (listof byte/c) boolean?)
  (eq? (car bytes)
       1))

(define/c (decode-byte-const bytes)
  (-> (listof byte/c) (values byte/c string?))
  (values (cadr bytes) (bytes->string (drop bytes 2))))

(module+ test #| encode/decode-byte-const |#
  (let-values (((value label) (decode-byte-const (list 1 #xf8 3 65 66 67))))
    (check-equal? value #xf8)
    (check-equal? label "ABC"))

  (check-equal? (encode-byte-const #x21 "AC")
                (list 1 #x21 2 65 67))

  (let-values (((value label) (decode-byte-const (encode-byte-const #x17 "label"))))
    (check-equal? value #x17)
    (check-equal? label "label"))
  )

;; encode entry for a relative word reference
(define/c (encode-word-relative rel-label label)
  (-> word/c string? (listof byte/c))
  (append (list 2 1
                (low-byte rel-label)
                (high-byte rel-label))
          (string->bytes label)))

;; check whether the next bytes indicate an encoded word relative
(define/c (encoded-word-relative? bytes)
  (-> (listof byte/c) boolean?)
  (equal? (take bytes 2)
       (list 2 1)))

;; decode entry of word relative
(define/c (decode-word-relative bytes)
  (-> (listof byte/c) (values word/c string?))
  (define hi (list-ref bytes 3))
  (define lo (list-ref bytes 2))
  (values (+ lo (* 256 hi)) (bytes->string (drop bytes 4))))

(module+ test #| encode-word/decode-word |#
  (check-equal? (encode-word-relative #xC010 "ABC")
                (list 2 1 #x10 #xc0 3 65 66 67))

  (let-values (((address string) (decode-word-relative (list 2 1 #x20 #xd1 2 65 67))))
    (check-equal? address #xd120)
    (check-equal? string "AC"))

  (let-values (((address string) (decode-word-relative (encode-word-relative #xC010 "some"))))
    (check-equal? address #xc010)
    (check-equal? string "some")))

;; get the label of the provide command (both word and byte commands supported)
(define/c (ast-provide-cmd-label command)
  (-> (or/c ast-provide-word-cmd? ast-provide-byte-cmd?) string?)
  (or (and (ast-provide-word-cmd? command)
        (ast-provide-word-cmd-label command))
     (and (ast-provide-byte-cmd? command)
        (ast-provide-byte-cmd-label command))))

(module+ test #| ast-provide-cmd-label |#
  (check-equal? (ast-provide-cmd-label (ast-provide-byte-cmd '() "some-byte"))
                "some-byte")
  (check-equal? (ast-provide-cmd-label (ast-provide-word-cmd '() "some-word"))
                "some-word"))

;; encode one entry of the export table for the provide-command
;;   using label -> word-value hash-map
;;   and constant -> word|byte-value hash-map
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
          [else (raise-user-error "cannot be converted")])))

(module+ test #| provide-entry |#
  (check-equal? (provide-entry (ast-provide-word-cmd '() "some")
                               (hash)
                               '#hash(("some" . #xFFD2)))
                 '(2 0 #xD2 #xFF 4 115 111 109 101))
  (check-equal? (provide-entry (ast-provide-byte-cmd '() "some")
                               (hash)
                               '#hash(("some" . #xD2)))
                 '(1 #xD2 4 115 111 109 101))
  (check-equal? (provide-entry (ast-provide-word-cmd '() "some")
                               '#hash(("some" . #x01D2))
                               (hash))
                '(2 1 #xD2 #x01 4 115 111 109 101)))

(define/c (retrieve-entry bytes labels constants)
  (-> (listof byte/c) hash? hash? (values exact-nonnegative-integer? hash? hash?))
  (cond [(encoded-word-const? bytes)
         (let-values (((const label) (decode-word-const bytes)))
           (values (+ 5 (string-length label)) labels (hash-set constants label const)))]
        [(encoded-byte-const? bytes)
         (let-values (((const label) (decode-byte-const bytes)))
           (values (+ 3 (string-length label)) labels (hash-set constants label const)))]
        [(encoded-word-relative? bytes)
         (let-values (((rel label) (decode-word-const bytes)))
           (values (+ 5 (string-length label)) (hash-set labels label rel) constants))]
        [else (raise-user-error "cannot be converted")]))

(module+ test #| provide-entry retrieve-entry |#
  (let-values (((len labels constants)
                (retrieve-entry (provide-entry (ast-provide-word-cmd '() "some")
                                               (hash)
                                               '#hash(("some" . #xFFD2)))
                                #hash() #hash())))
    (check-equal? len 9)
    (check-equal? (hash-ref constants "some")
                  #xffd2))
  (let-values (((len labels constants)
                (retrieve-entry (provide-entry (ast-provide-byte-cmd '() "some")
                                               (hash)
                                               '#hash(("some" . #xD2)))
                                #hash() #hash())))
    (check-equal? len 7)
    (check-equal? (hash-ref constants "some")
                  #xd2))
  (let-values (((len labels constants)
                (retrieve-entry (provide-entry (ast-provide-word-cmd '() "some")
                                               '#hash(("some" . #x01D2))
                                               (hash))
                                #hash() #hash())))
    (check-equal? len 9)
    (check-equal? (hash-ref labels "some")
                  #x01d2))

  (let-values (((len labels constants) (retrieve-entry '(2 0 #xD2 #xFF 4 115 111 109 101) #hash() #hash())))
    (check-equal? len 9)
    (check-equal? (hash-ref constants "some")
                  #xffd2))
  (let-values (((len labels constants) (retrieve-entry '(1 #xD2 4 115 111 109 101) #hash() #hash())))
    (check-equal? len 7)
    (check-equal? (hash-ref constants "some")
                  #xd2))
  (let-values (((len labels constants) (retrieve-entry '(2 1 #xD2 #x01 4 115 111 109 101) #hash() #hash())))
    (check-equal? len 9)
    (check-equal? (hash-ref labels "some")
                  #x01d2)))

;; export table format
;; offset           data
;; 0                width (byte), (if width = 2 0: absolute-const, 1:relative-load-address)?.
;; 1/2              value-low|rel-position-low (optional value-high|rel-position-high)
;; 1/2+width        strlen (byte)
;; 2/3+width        string
;; 1/2+width+strlen ... next entry

;; encode export table bytes into collected-bytes
;;   using a hash for label -> word-value, and constants -> word|byte-value hash maps
;;   reading through all commands
(define/c (export-table-bytes collected-bytes labels constants commands)
  (-> (listof byte/c) hash? hash? (listof ast-command?) (listof byte/c))
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
                 (list (ast-provide-word-cmd '()  "some")
                       (ast-provide-word-cmd '()  "none")
                       (ast-provide-byte-cmd '()  "other")))
                 '(2 0 #xD2 #xFF 4 115 111 109 101
                   2 1 #xE0 #x01 4 110 111 110 101
                   1 #xC0 5 111 116 104 101 114)))

;; decode the bytes of an export table again in two hashes,
;;   the label hash: label->rel-word-value
;;   and the constants hash map: constant-name->byte|word-value
(define/c (decode-export-table-bytes bytes labels constants)
  (-> (listof byte/c) hash? hash? (values hash? hash?))
  (if (empty? bytes)
      (values labels constants)
      (let-values (((len new-labels new-constants) (retrieve-entry bytes labels constants)))
        (decode-export-table-bytes (drop bytes len) new-labels new-constants))))

(module+ test #| decode-export-table-bytes |#
  (let-values (((labels constants)
                (decode-export-table-bytes
                 '(2 0 #xD2 #xFF 4 115 111 109 101
                     2 1 #xE0 #x01 4 110 111 110 101
                     1 #xC0 5 111 116 104 101 114)
                 #hash() #hash())))
    (check-equal? labels #hash(("none"  . #x01e0)))
    (check-equal? constants #hash(("some"  . #xffd2)
                                  ("other" . #xc0)))))

;; import table format
;; offset           data
;; 0                rel-position-low, rel-position-high, : position where the value has to be written to
;; 2                width (byte), (if width = 1 0:lowbyte 1:highbyte)?,
;; 3/4              strlen (byte)
;; 4/5              string 
;; 4/5+strlen       ...next entry

(define/c (encode-import-word-entry offset label)
  (-> word/c string? (listof byte/c))
  (append
   (list (low-byte offset)
         (high-byte offset)
         2)
   (string->bytes label)))

(define/c (encode-import-byte-entry offset hilo-ind label)
  (-> word/c byte/c string? (listof byte/c))
  (append
   (list (low-byte offset)
         (high-byte offset)
         1 hilo-ind)
   (string->bytes label)))

(define/c (import-word-entry-bytes label req-hashes offset)
  (-> string? hash? word/c (listof byte/c))
  (let* ((value (hash-ref req-hashes label)))
    (if (eq? 'word value)
        (encode-import-word-entry offset label)
        (encode-import-byte-entry offset 0 label))))

(define/c (import-byte-entry-bytes label req-hashes offset hilo-ind)
  (-> string? hash? word/c (or/c 'high-byte 'low-byte) (listof byte/c))
  (let* ((value (hash-ref req-hashes label)))
    (if (eq? 'word value)
        (encode-import-byte-entry offset (if (eq? 'high-byte hilo-ind) 1 0) label)
        (encode-import-byte-entry offset 0 label))))

;; write the import table based on unresolved ast commands encoded into bytes
(define/c (import-table-bytes offset collected-entries req-hashes commands)
  (-> word/c (listof byte/c) hash? (listof ast-command?) (listof byte/c))
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
                    [else '()])))
        (import-table-bytes (+ offset (command-len command))
                            (append collected-entries new-entry)
                            req-hashes
                            (cdr commands)))))

(module+ test #| import-table-bytes |#
  (check-equal? (import-table-bytes #xfe '() '#hash(("some" . word) ("other" . byte))
                                    (list(ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-word-scmd "some"))
                                         (ast-unresolved-opcode-cmd '() '(#xea) (ast-resolve-byte-scmd "other" 'low-byte))
                                         (ast-unresolved-opcode-cmd '() '(#xea) (ast-resolve-byte-scmd "some" 'low-byte))
                                         (ast-unresolved-opcode-cmd '() '(#xea) (ast-resolve-byte-scmd "some" 'high-byte))))
                '(#xff #x00 2 4 115 111 109 101
                  #x02 #x01 1 0 5 111 116 104 101 114
                  #x04 #x01 1 0 4 115 111 109 101
                  #x06 #x01 1 1 4 115 111 109 101)))

(define/c (collect-import-hash commands collected)
  (-> (listof ast-command?) hash? hash?)
  (cond [(empty? commands) collected]
        [else
         (let ([command (first commands)])
           (collect-import-hash
            (cdr commands)
            (cond [(ast-require-byte-cmd? command)
                   (hash-set collected (ast-require-byte-cmd-label command) 'byte)]
                  [(ast-require-word-cmd? command)
                   (hash-set collected (ast-require-word-cmd-label command) 'word)]
                  [else collected])))]))

(module+ test #| collect-import-hash |#
  (check-equal? (collect-import-hash (list (ast-require-byte-cmd '() "some")) #hash())
                #hash(("some" . byte)))
  (check-equal? (collect-import-hash (list) #hash())
                #hash())
  (check-equal? (collect-import-hash (list (ast-require-word-cmd '() "some")) #hash())
                #hash(("some" . word)))
  (check-equal? (collect-import-hash
                 (list (ast-require-byte-cmd '() "some")
                       (ast-opcode-cmd '() '(0 1 2))
                       (ast-require-word-cmd '() "other")
                       (ast-label-def-cmd '() "label"))
                 #hash())
                #hash(("some" . byte)
                      ("other" . word))))

;; decode values of a byte import in encoded import table
(define/c (decode-import-table-byte-entry-values bytes)
  (-> (listof byte/c) (values byte/c string? symbol?))
  (let* ((hilo-ind     (fourth bytes))
         (label        (bytes->string (drop bytes 4)))
         (entry-len    (+ 5 (string-length label)))
         (hilo-ind-sym (if (eq? 1 hilo-ind) 'high-byte 'low-byte)))
    (values entry-len label hilo-ind-sym)))

;; decode values of a word import in encoded import table
(define/c (decode-import-table-word-entry-values bytes)
  (-> (listof byte/c) (values byte/c string? symbol?))
  (let* ((label     (bytes->string (drop bytes 3)))
         (entry-len (+ 4 (string-length label))))
    (values entry-len label 'word)))

(define import-entry/c (list/c string? word/c symbol?))

;; collect a list of label, address and hilo/word indicator from decoding the import table bytes
(define/c (decode-import-table-bytes bytes import-entries)
  (-> (listof byte/c) (listof import-entry/c) (listof import-entry/c))
  (if (empty? bytes)
      import-entries
      (let* ((lo    (first bytes))
             (hi    (second bytes))
             (addr  (absolute hi lo))
             (width (third bytes)))
        (let-values
            (((entry-len label sym)
              (cond [(eq? width 2) (decode-import-table-word-entry-values bytes)]
                    [(eq? width 1) (decode-import-table-byte-entry-values bytes)]
                    [else (raise-user-error "illegal width (neither 1 nor 2)")])))
          (decode-import-table-bytes
           (drop bytes entry-len)
           (cons (list label addr sym)
                 import-entries))))))

(module+ test #| decode-import-table-bytes |#
  (check-not-false (member (decode-import-table-bytes
                            '(#xff #x00 2 4 115 111 109 101
                              #x02 #x01 1 0 5 111 116 104 101 114
                              #x04 #x01 1 0 4 115 111 109 101
                              #x06 #x01 1 1 4 115 111 109 101)
                            '())
                           (permutations '(("some"  #x0106 high-byte)
                                           ("some"  #x0104 low-byte)
                                           ("other" #x0102 low-byte)
                                           ("some"  #x00ff word)))))
  (check-exn exn:fail?
             (lambda () (decode-import-table-bytes
                            '(#xff #x00 0 4 115 111 109 101)
                            '())))
  (check-exn exn:fail?
             (lambda () (decode-import-table-bytes
                            '(#xff #x00 2 4 115 111 109 101
                              #x02 #x01 3 0 5 111 116 104 101 114)
                            '()))))

;; extend the given hash-map of required label -> 'word | 'byte from this command if applicable
(define/c (require-hash command hash)
  (-> ast-command? hash? hash?)
  (cond [(ast-require-word-cmd? command)
         (hash-set hash (ast-require-word-cmd-label command) 'word)]
        [(ast-require-byte-cmd? command)
         (hash-set hash (ast-require-byte-cmd-label command) 'byte)]
        [else hash]))

;; get the complete hash of label -> 'word | 'byte that are required by the given commands
(define/c (require-hashes commands)
  (-> (listof ast-command?) hash?)
  (foldl require-hash (hash) commands))

(module+ test #| require-hashes |#
  (check-equal? (require-hashes (list (ast-require-word-cmd '()  "some")
                                      (ast-require-byte-cmd '() "other")
                                      (ast-opcode-cmd '() '(#x20))))
                '#hash(("some" . word)
                       ("other" . byte))))

;; is the given value resolvable given the imports, labels and constants?
(define/c (-resolvable-cmd? imports-hash labels constants-hash command)
  (-> hash? (listof string?) hash? ast-command? boolean?)
  (cond [(ast-unresolved-command? command)
         (let ([unresolved-label (ast-resolve-sub-cmd-label (ast-unresolved-command-resolve-sub-command command))])
           (or (and (hash-ref imports-hash unresolved-label #f) #t)
              (and (hash-ref constants-hash unresolved-label #f) #t)
              (and (member unresolved-label labels) #t)))]
        [else #t]))

(module+ test #| -resolvable-cmd |#
  (check-true (-resolvable-cmd? #hash() '() #hash() (ast-label-def-cmd '() "sout")))
  (check-true (-resolvable-cmd? #hash() '() #hash() (ast-opcode-cmd '() '(#x20 #x10 #x00))))
  (check-false (-resolvable-cmd? #hash() '() #hash() (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-word-scmd "label"))))
  (check-true (-resolvable-cmd? #hash(("label" . 'byte)) '() #hash() (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-word-scmd "label"))))
  (check-true (-resolvable-cmd? #hash() '() #hash(("label" . 'byte)) (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-word-scmd "label"))))
  (check-true (-resolvable-cmd? #hash() '("label") #hash() (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-word-scmd "label")
                                                            )))
  (check-true (-resolvable-cmd? #hash() (list (symbol->string 'label)) #hash() (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-word-scmd "label")))))

;; are the ast-commands resolvable using the given imports, labels and constants
(define/c (-resolvable? imports-hash labels constants-hash ast-commands)
  (-> hash? (listof string?) hash? (listof ast-command?) boolean?)
  (if (empty? ast-commands)
      #t
      (and (-resolvable-cmd? imports-hash labels constants-hash (car ast-commands))
         (-resolvable? imports-hash labels constants-hash (cdr ast-commands)))))

;; are all values used in the commands are either known/defined or imported?
(define/c (resolvable? ast-commands)
  (-> (listof ast-command?) boolean?)
  (-resolvable? (collect-import-hash ast-commands #hash())
                (map (lambda (label-cmd) (ast-label-def-cmd-label label-cmd))
                     (filter ast-label-def-cmd? ast-commands))
                (constant-definitions-hash ast-commands)
                ast-commands))

(define/c (-unresolved-commands imports-hash labels constants-hash ast-commands collected-unresolved)
  (-> hash? (listof string?) hash? (listof ast-command?) (listof ast-command?) (listof ast-command?))
  (if (empty? ast-commands)
      collected-unresolved
      (let* ((command       (car ast-commands))
             (new-collected (if (-resolvable-cmd? imports-hash labels constants-hash command)
                                collected-unresolved
                                (cons command collected-unresolved))))
        (-unresolved-commands imports-hash labels constants-hash (cdr ast-commands) new-collected))))

;; list all commands that could not be resolved through imports, labels or constants
(define/c (unresolved-commands ast-commands)
  (-> (listof ast-command?) (listof ast-command?))
  (-unresolved-commands (collect-import-hash ast-commands #hash())
                        (map (lambda (label-cmd) (ast-label-def-cmd-label label-cmd))
                     (filter ast-label-def-cmd? ast-commands))
                (constant-definitions-hash ast-commands)
                ast-commands
                (list)))

;; all ast commands are resolved
(define/c (resolved? ast-commands)
  (-> (listof ast-command?) boolean?)
  (empty? (filter ast-unresolved-command? ast-commands)))

(module+ test #| resolvable? |#
  (check-true (resolvable? (list)))
  (check-false (resolvable? (list (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-word-scmd "label")))))
  (check-false (resolvable? (list (ast-unresolved-rel-opcode-cmd '() '(#xA9) (ast-resolve-byte-scmd "sout" 'relative)))))
  (check-true (resolvable?
               (list (ast-label-def-cmd '() "sout")
                     (ast-unresolved-rel-opcode-cmd '() '(#xA9) (ast-resolve-byte-scmd "sout" 'relative)))))
  (check-true (resolvable?
               (list (ast-label-def-cmd '() "sout")
                     (ast-const-byte-cmd '() "my-byte" 65)
                     (ast-const-word-cmd '() "my-word" #xffd2)
                     (require-word "req-word")
                     (require-byte "req-byte")
                     (ast-unresolved-rel-opcode-cmd '() '(#xA9) (ast-resolve-byte-scmd "sout" 'relative))
                     (ast-unresolved-opcode-cmd '() '(#xa9) (ast-resolve-byte-scmd "my-byte" 'low-byte))
                     (ast-unresolved-opcode-cmd '() '(#xa5) (ast-resolve-word-scmd "my-word"))
                     (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-sub-cmd "req-word"))
                     (ast-unresolved-opcode-cmd '() '(#xa1) (ast-resolve-byte-scmd "req-byte" 'low-byte)))))
  (check-equal? (unresolved-commands
                 (list (ast-label-def-cmd '(0) "repeat")
                       (ast-const-byte-cmd '() "my-byte" 65)
                       (ast-const-word-cmd '() "my-word" #xffd2)
                       (require-word req-word)
                       (require-byte "req-byte")
                       (ast-unresolved-rel-opcode-cmd '() '(#xA9) (ast-resolve-byte-scmd "repeat" 'relative))
                       (ast-unresolved-opcode-cmd '() '(#xa9) (ast-resolve-byte-scmd "my-byte" 'low-byte))
                       (ast-unresolved-opcode-cmd '() '(#xa5) (ast-resolve-word-scmd "my-word"))
                       (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-sub-cmd "req-word"))
                       (ast-unresolved-opcode-cmd '() '(#xa1) (ast-resolve-byte-scmd "req-byte" 'low-byte))))
                (list))
  (check-equal? (unresolved-commands
                 (list (ast-label-def-cmd '() "sout")
                       (ast-const-word-cmd '() "my-word" #xffd2)
                       (require-word "req-word")
                       (require-byte "req-byte")
                       (ast-unresolved-rel-opcode-cmd '() '(#xA9) (ast-resolve-byte-scmd "sout" 'relative))
                       (ast-unresolved-opcode-cmd '() '(#xa9) (ast-resolve-byte-scmd "my-byte" 'low-byte))
                       (ast-unresolved-opcode-cmd '() '(#xa5) (ast-resolve-word-scmd "my-word"))
                       (ast-unresolved-opcode-cmd '() '(#x20) (ast-resolve-sub-cmd "req-word"))
                       (ast-unresolved-opcode-cmd '() '(#xa1) (ast-resolve-byte-scmd "req-byte" 'low-byte))))
                (list
                 (ast-unresolved-opcode-cmd '() '(#xa9) (ast-resolve-byte-scmd "my-byte" 'low-byte)))))

(module+ test #| linking two programs together|#

  (define providing-program
    (list
      (provide-word aout)
      (provide-byte char)
      (provide-word clong)
      (byte-const "char" "$65")
      (word-const clong "$FFD2")
      (LDA !char)
      (label aout)
      (JMP clong)))

  (define requiring-program
    (list
      (require-word aout)
      (require-byte char)
      (byte-const "bc" "$65")
      (LDX !$05)
      (label repeat)
      (JSR aout)
      (DEX)
      (BNE repeat)
      (CLC)
      (LDA !char)
      (ADC !$01)
      (JSR $FFD2)
      (BRK)
      (LDA "aout")
      (LDA ">aout")
      (LDA "char")))

  (define providing-program-p1 (->resolved-decisions (label-instructions providing-program) providing-program))
  (define providing-program-p2 (->resolve-labels 0 (label-string-offsets 0 providing-program-p1) providing-program-p1 '()))
  (define providing-program-p3 (resolve-constants (constant-definitions-hash providing-program-p1) providing-program-p2))
  (define providing-raw-bytes (resolved-program->bytes providing-program-p3))

  (define providing-label-offsets (label-string-offsets 0 providing-program-p3))
  (define providing-constants (constant-definitions-hash providing-program-p3))
  (define providing-export-table (export-table-bytes '() providing-label-offsets providing-constants providing-program-p3))
  (define-values (providing-decoded-export-labels providing-decoded-export-constants) (decode-export-table-bytes providing-export-table #hash() #hash()))
  ;; simply add to the providing-decoded-export-labels (string->offset) org to the offset, where org is the origin of the providing program

  (define requiring-import-hash (collect-import-hash requiring-program #hash()))
  (define requiring-program-labels (map (lambda (label-cmd) (ast-label-def-cmd-label label-cmd))
                                        (filter ast-label-def-cmd? requiring-program)))
  (define requiring-import-table-bytes (import-table-bytes 0 '() requiring-import-hash requiring-program))
  (define requiring-import-table-decoded (decode-import-table-bytes requiring-import-table-bytes '()))
  ;; in requiring-import-table-decoded a list of "label", offset, indicator, the code is "patched"
  ;; by setting the next word, low-byte or high-byte (indicator) at the given offset to the value
  ;; retrieved by the label

  (define requiring-program-p1 (->resolved-decisions (label-instructions requiring-program) requiring-program))
  (define requiring-program-p2 (->resolve-labels 0 (label-string-offsets 0 requiring-program-p1) requiring-program-p1 '()))

  ;; the following step fills the data bytes accordingly, these values however are overwritten, once
  ;; the actual linking takes place
  (define requiring-program-p2-1 (->resolve-labels 0 providing-decoded-export-labels requiring-program-p2 '()))
  (define requiring-program-constants (constant-definitions-hash requiring-program-p2-1))
  (define requiring-program-p3 (resolve-constants requiring-program-constants requiring-program-p2-1))
  (define requiring-program-p3-1 (resolve-constants providing-decoded-export-constants requiring-program-p3))
  ;; currently no placeholders are encoded into the raw bytes -> this function works only on already resolved programs
  (define requiring-raw-bytes (resolved-program->bytes requiring-program-p3-1))

  (check-true (resolvable? requiring-program-p3))
  (check-false (resolved? requiring-program-p3))
  (check-true (resolved? providing-program-p3))
  (check-equal? (unresolved-commands requiring-program-p3)
                (list))
  ;; bin filelayout:
  ;; 0:    word: total size = 8 + export table size + import table size + code size
  ;; 2:    word: ioff = # bytes to first relocatable code = 6 + export table size
  ;; 4:    word: coff = # bytes to first import table byte = 6 + export table size + import table size
  ;; 6:    export table bytes
  ;; ioff: import table bytes
  ;; coff: relocatable code bytes

  (define providing-program-bin (list)) ;; relocatable code + import + export table
  (define requiring-program-bin (list))


  (skip ": linking programs is not supported yet"
        (check-equal? (link-programs (list providing-program-bin requiring-program-bin))
                      (list #x00))))

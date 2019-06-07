#lang racket

;; todo: add possibility to use labels instead of values (in all addressing modes)
;; todo: add method descriptions (scrbl)
;; planned: realize with typed racket

(require (only-in racket/format ~a))
(require (only-in threading ~>))
(require (only-in rnrs/base-6 div mod))
(require (for-syntax (only-in racket/list second empty?)))

(require (for-syntax "6510-utils.rkt"))
(require (for-syntax (rename-in "6510-syntax-utils.rkt"
                                (one-arg-adr-modes-relative? relative?)
                                (one-arg-adr-modes-accumulator? accumulator?)
                                (one-arg-adr-modes-immediate? immediate?)
                                (one-arg-adr-modes-absolute? absolute?)
                                (one-arg-adr-modes-zero-page? zero-page?)
                                (ind-arg-adr-modes-indirect-x? indirect-x?)
                                (ind-arg-adr-modes-indirect-y? indirect-y?)
                                (idx-arg-adr-modes-absolute-x? absolute-x?)
                                (idx-arg-adr-modes-absolute-y? absolute-y?)
                                (idx-arg-adr-modes-zero-page-x? zero-page-x?) )))
(require "6510-utils.rkt")
(require "6510-interpreter.rkt")

(module+ test
  (require rackunit))

(provide parse-number-string replace-labels commands->bytes create-prg run-emulator pretty-print-program
         ADC ASL BCC BCS BEQ BMI BNE BPL BRK BVC BVS DEC INC LDA JSR RTS STA
         LABEL BYTES)


;; ================================================================================ address resolution

(define (6510-byte-length command)
  (case (first command)
    [('rel-opcode) (if (6510-label-string? (last command))
                       2
                       (- (length command) 1))]
    [('opcode) (if (6510-label-string? (last command))
                   3
                   (- (length command) 1))]
    [('bytes) (length (last command))]
    [('label) 0]
    [else (error "uknown command" (first command))]))

(module+ test
  (check-match (6510-byte-length '('opcode 1 ":some"))
               3)

  (check-match (6510-byte-length '('opcode 1 ":other"))
               3)

  (check-match (6510-byte-length '('bytes (1 2 3)))
               3)

  (check-match (6510-byte-length '('opcode 1 2 3))
               3)

  (check-match (6510-byte-length '('label ":test"))
               0))

(define (lo-sums list current-sum)
  (if (empty? list)
      '()
      (let* ([first-num (first list)]
             [new-sum (+ current-sum first-num)])
        (append `((,first-num ,current-sum)) (lo-sums (drop list 1) new-sum)))))

(define (collect-label-offset-map commands-bytes-list)
  (let* ([labels-bytes-list (filter (lambda (command-byte-pair)
                                      (case (first (first command-byte-pair))
                                        [('label) #t]
                                        [else #f])) commands-bytes-list)])
    labels-bytes-list))

(module+ test
  (check-match (collect-label-offset-map '((('opcode 1 2) (2 0))
                                           (('label ":some") (0 2))
                                           (('opcode 1 -2) (2 2))
                                           (('label ":other") (0 4))
                                           (('opcode 5 ":some") (3 4))
                                           (('label ":end") (0 7))))
               '((('label ":some") (0 2)) (('label ":other") (0 4)) (('label ":end") (0 7)))))

(define (get-label-offset labels-byte-list label)
  (let ([filtered (filter (lambda (label-byte-pair)
                            (equal? label (last (first label-byte-pair))))
                          labels-byte-list)])
    (when (empty? filtered)
      (error "label not found in list" label filtered))
    (last (last (last filtered)))))

(module+ test
  (check-match (get-label-offset '((('label ":some") (0 2))
                                   (('label ":other") (0 4))
                                   (('label ":end") (0 7)))
                                 ":some")
               2)

  (check-match (get-label-offset '((('label ":some") (0 2))
                                   (('label ":other") (0 4))
                                   (('label ":end") (0 7)))
                                 ":other")
               4)

  (check-exn
   exn:fail?
   (lambda () (get-label-offset '((('label ":some") (0 2))
                             (('label ":other") (0 4))
                             (('label ":end") (0 7)))
                           ":unknown"))))

(define (commands-bytes-list commands)
  (let* ([byte-lengths (map 6510-byte-length commands)]
         [byte-lengths/w-offset (lo-sums byte-lengths 0)])
    (map list commands byte-lengths/w-offset)))

(module+ test

  (check-match (commands-bytes-list '(('opcode 10 10 10)
                                      ('label ":some")
                                      ('opcode 0)
                                      ('opcode 10 10)))
               '((('opcode 10 10 10) (3 0))
                 (('label ":some") (0 3))
                 (('opcode 0) (1 3))
                 (('opcode 10 10) (2 4)))))

(define (replace-label command-byte-pair labels-bytes-list address)
  (let* ([command (first command-byte-pair)]
         [current-offset (last (last command-byte-pair))]
         [command-length (first (last command-byte-pair))])
    (if (>= 1 (length command))
        command-byte-pair
        (if (and (not (equal? ''label (first command))) (6510-label-string? (last command)))
            (let* ([label-offset (+ address (get-label-offset labels-bytes-list (last command)))]
                   [rel-label-offset (- (get-label-offset labels-bytes-list (last command)) current-offset command-length)])
              (case (first command)
                [('rel-opcode)
                 (list (append (drop-right command 1)
                               (list (low-byte rel-label-offset)))
                       (last command-byte-pair))]
                [('opcode)
                 (list (append (drop-right command 1)
                               (list (low-byte label-offset) (high-byte label-offset)))
                       (last command-byte-pair))]
                [else (error (string-append "unknown label reference in opcode" (symbol->string (first (command)))))])
              )
            command-byte-pair))))

(module+ test
  (check-match (replace-label '(('opcode 20 ":some") (3 10))
                              '((('label ":some") (0 8)))
                              100)
               '(('opcode 20 108 0) (3 10)))

  (check-match (replace-label '(('rel-opcode #xf0 ":some") (2 10))
                              '((('label ":some") (0 8)))
                              100)
               '(('rel-opcode #xf0 #xfc) (2 10)))

  (check-match (replace-label '(('rel-opcode #xf0 ":some") (2 10))
                              '((('label ":some") (0 20)))
                              100)
               '(('rel-opcode #xf0 8) (2 10)))

  (check-match (replace-label '(('opcode 20 30 80) (3 10))
                              '((('label ":some") (0 8)))
                              100)
               '(('opcode 20 30 80) (3 10))))

(define (replace-labels commands address)
  (let* ([commands-bytes-list (commands-bytes-list commands)]
         [labels-bytes-list (collect-label-offset-map commands-bytes-list)])
    (map first
         (map (lambda (command-byte-pair) (replace-label command-byte-pair labels-bytes-list address))
              commands-bytes-list))))

(module+ test
  (check-match (replace-labels '(('opcode 1 2)
                                 ('label ":some")
                                 ('rel-opcode #xf0 ":end")
                                 ('opcode 1 ":some")
                                 ('label ":other")
                                 ('opcode 5 ":other")
                                 ('label ":end"))
                               10)
               '(('opcode 1 2)
                 ('label ":some")
                 ('rel-opcode #xf0 6)
                 ('opcode 1 12 0)
                 ('label ":other")
                 ('opcode 5 17 0)
                 ('label ":end")))

  (check-match (replace-labels '(((quote label) ":some")
                                 ((quote opcode) 169 65)
                                 ((quote rel-opcode) #xf0 ":some")
                                 ((quote opcode) 32 ":some")
                                 ((quote opcode) 0)) 10)
               '(((quote label) ":some")
                 ('opcode 169 65)
                 ('rel-opcode #xf0 #xfc)
                 ('opcode 32 10 0)
                 ('opcode 0))))

(define (resolve-statements commands)
  (let* [(label-offsets (collect-label-offset-map commands))]
    (replace-labels label-offsets commands)))

(define (command-is-label? command)
  (case (first command) [('label) #t] [else #f]))

(define (remove-resolved-statements commands)
  (filter-not command-is-label?
              (map (lambda (command)
                     (case (first command)
                       [('opcode) (drop command 1)]
                       [('bytes) (drop command 1)]
                       [('rel-opcode) (drop command 1)]
                       [else command]))
                   commands)))

(module+ test
  (check-match (remove-resolved-statements '((1 2 3)
                                             ('opcode 2 3 4)
                                             ('rel-opcode #xF0 12)
                                             ('bytes (1 2 3))
                                             (0)))
               '((1 2 3)
                 (2 3 4)
                 (#xF0 12)
                 ((1 2 3))
                 (0))))

(define (commands->bytes memory-address commands )
  (flatten (~>  (replace-labels commands memory-address)
               remove-resolved-statements)))

(define (LABEL_s label) (list ''label label))

(define-syntax (LABEL stx)
  (syntax-case stx ()
    [(LABEL op)
     #'(LABEL_s op)]))

;; ================================================================================ opcode definition helper

(define-for-syntax (accumulator-mode opcode operand)
  (with-syntax ([operand-value (syntax->datum operand)]
                [symbol-acc (symbol-append opcode '_acc)])
    (when (equal? 'A (syntax->datum #'operand-value))
      #'(symbol-acc))))

(define-for-syntax (immediate-mode opcode operand)
  (with-syntax ([operand-value (syntax->datum operand)]
                [symbol-i (symbol-append opcode '_i)])
    (when (is-immediate-number? (syntax->datum #'operand-value))
      #'(symbol-i (parse-number-string (substring operand-value 1))))))

(define-for-syntax (zero-page-mode opcode operand)
  (with-syntax ([operand-value (syntax->datum operand)])
    (when (6510-number-string? (syntax->datum #'operand-value))
      (with-syntax ([op-number (parse-number-string (syntax->datum operand))]
                    [symbol-zp (symbol-append opcode '_zp)])
        (when (> 256 (syntax->datum #'op-number))
          #'(symbol-zp (parse-number-string operand-value)))))))

(define-for-syntax (absolute-mode opcode operand)
  (with-syntax ([operand-value (syntax->datum operand)])
    (when (6510-number-string? (syntax->datum #'operand-value))
      (with-syntax ([op-number (parse-number-string (syntax->datum operand))]
                    [symbol-abs (symbol-append opcode '_abs)])
        (when (<= 256 (syntax->datum #'op-number))
          #'(symbol-abs op-number))))))

(define-for-syntax (indirect-x-mode opcode open operand close-or-x close-or-y)
  (with-syntax ([symbol-indx (symbol-append opcode '_indx)]
                [op-number (parse-number-string (syntax->datum operand))]
                [x-idx (syntax->datum close-or-x)])
    (when (or (equal? (syntax->datum #'x-idx) '(unquote x))
              (equal? (syntax->datum #'x-idx) 'x))
      #'(symbol-indx op-number))))

(define-for-syntax (indirect-y-mode opcode open operand close-or-x close-or-y)
  (with-syntax ([symbol-indy (symbol-append opcode '_indy)]
                [op-number (parse-number-string (syntax->datum operand))]
                [y-idx (syntax->datum close-or-y)])
    (when (or (equal? (syntax->datum #'y-idx) '(unquote y))
              (equal? (syntax->datum #'y-idx) 'y))
      #'(symbol-indy op-number))))

(define-for-syntax (absolute-x-mode opcode operand idx)
  (with-syntax ([symbol-absx (symbol-append opcode '_absx)]
                [op-number (parse-number-string (syntax->datum operand))]
                [x-idx (syntax->datum idx)])
    (when (and (< 255 (syntax->datum #'op-number))
               (equal? (syntax->datum #'x-idx) 'x))
      #'(symbol-absx op-number))))

(define-for-syntax (absolute-y-mode opcode operand idx)
  (with-syntax ([symbol-absy (symbol-append opcode '_absy)]
                [op-number (parse-number-string (syntax->datum operand))]
                [y-idx (syntax->datum idx)])
    (when (and (< 255 (syntax->datum #'op-number))
               (equal? (syntax->datum #'y-idx) 'y))
      #'(symbol-absy op-number))))

(define-for-syntax (relative-mode opcode operand)
  (with-syntax ([operand-value (syntax->datum operand)]
                [symbol-rel (symbol-append opcode '_rel)])
    (if (6510-label-string? (syntax->datum #'operand-value))
        #'(symbol-rel operand-value)
        (when (and (6510-number-string? (syntax->datum #'operand-value))
                   (> 256 (parse-number-string (syntax->datum #'operand-value))))
          (with-syntax ([op-number (parse-number-string (syntax->datum operand))])
            #'(symbol-rel op-number)))
        )))

(define-for-syntax (zeropage-x-mode opcode operand idx)
  (with-syntax ([symbol-zpx (symbol-append opcode '_zpx)]
                [op-number (parse-number-string (syntax->datum operand))]
                [x-idx (syntax->datum idx)])
    (when (and (> 256 (syntax->datum #'op-number))
               (equal? (syntax->datum #'x-idx) 'x))
      #'(symbol-zpx op-number))))

(define-for-syntax (error-string/indirect adr-modes opcode-string)
  (string-append
   "invalid syntax.\n"
   (if (not (or (indirect-x? adr-modes) (indirect-y? adr-modes))) (string-append opcode-string " cannot be used with indirect addressing\n") "expected:\n")
   (if (indirect-x? adr-modes) (string-append "  (" opcode-string  " \"($1000,x)\") # indirect x addressing mode\n") "")
   (if (indirect-y? adr-modes) (string-append "  (" opcode-string  " \"($1000),y\") # indirect y addressing mode\n") "")
   "got: "))

(define-for-syntax (error-string/indexed adr-modes opcode-string)
  (string-append
   "invalid syntax.\n"
   (if (not (or (absolute-x? adr-modes) (absolute-y? adr-modes) (zero-page-x? adr-modes)))
       (string-append opcode-string " cannot use index\n") "expected:\n")
   (if (absolute-x? adr-modes) (string-append "  (" opcode-string " \"$1000\",x) # absolute x addressing mode\n") "")
   (if (absolute-y? adr-modes) (string-append "  (" opcode-string " \"$1000\",y) # absolute y addressing mode\n") "")
   (if (zero-page-x? adr-modes) (string-append "  (" opcode-string  " \"$10\",x)   # zero page x addressing mode\n") "")
   "got: "))

(define-for-syntax (error-string/single adr-modes opcode-string)
  (string-append
   "invalid syntax.\n"
   (if (not (or (relative? adr-modes) (accumulator? adr-modes) (immediate? adr-modes) (zero-page? adr-modes) (absolute? adr-modes)))
       (string-append opcode-string " cannot have one operand\n") "expected:\n")
   (if (relative? adr-modes) (string-append "  (" opcode-string " \":label\") # relative mode\n") "")
   (if (accumulator? adr-modes) (string-append "  (" opcode-string " A) # accumulator mode\n") "")
   (if (immediate? adr-modes) (string-append "  (" opcode-string " \"#$10\") # immediate addressing mode\n") "")
   (if (zero-page? adr-modes) (string-append "  (" opcode-string  " \"$10\") # zeropage addressing mode\n") "")
   (if (absolute? adr-modes) (string-append "  (" opcode-string " \"$1000\") # absolute addressing.\n") "")
   "got: "))

(define-for-syntax (opcode-with-addressing/single adr-modes opcode op stx)
  (with-syntax ([ires (when (immediate? adr-modes) (immediate-mode opcode op))]
                [zpres (when (zero-page? adr-modes) (zero-page-mode opcode op))]
                [absres (when (absolute? adr-modes) (absolute-mode opcode op))]
                [accres (when (accumulator? adr-modes) (accumulator-mode opcode op))]
                [relres (when (relative? adr-modes) (relative-mode opcode op))])
    (let ([res (foldl discard-void-syntax-object #'()  (list #'relres #'accres #'ires #'zpres #'absres))]
          [opcode-string (symbol->string (syntax->datum opcode))])
      (if (equal? '() (syntax->datum res))
          (error (error-string/single adr-modes opcode-string)
                 (syntax->datum stx))
          res))))

(define-for-syntax (opcode-with-addressing/indirect adr-modes opcode open op close-or-x close-or-y stx)
  (with-syntax ([indxres (when (indirect-x? adr-modes) (indirect-x-mode opcode open op close-or-x close-or-y))]
                [indyres (when (indirect-y? adr-modes) (indirect-y-mode opcode open op close-or-x close-or-y))])
    (let ([res (foldl discard-void-syntax-object #'()  (list #'indxres #'indyres))]
          [opcode-string (symbol->string (syntax->datum opcode))])
      (if (equal? '() (syntax->datum res))
          (error (error-string/indirect adr-modes opcode-string)
                 (syntax->datum stx))
          res))))

(define-for-syntax (opcode-with-addressing/indexed adr-modes opcode op idx stx)
  (with-syntax ([absxres (when (absolute-x? adr-modes) (absolute-x-mode opcode op idx))]
                [absyres (when (absolute-y? adr-modes) (absolute-y-mode opcode op idx))]
                [zpxres (when (zero-page-x? adr-modes) (zeropage-x-mode opcode op idx))])
    (let ([res (foldl discard-void-syntax-object #'()  (list #'zpxres #'absxres #'absyres))]
          [opcode-string (symbol->string (syntax->datum opcode))])
      (if (equal? '() (syntax->datum res))
          (error (error-string/indexed adr-modes opcode-string)
                 (syntax->datum stx))
          res))))

(define-for-syntax (list->one-arg-adr-modes option-list)
  (one-arg-adr-modes (member 'relative option-list)
                     (member 'accumulator option-list)
                     (member 'immediate option-list)
                     (member 'zero-page option-list)
                     (member 'absolute option-list)))

(define-for-syntax (list->ind-arg-adr-modes option-list)
  (ind-arg-adr-modes (member 'indirect-x option-list)
                     (member 'indirect-y option-list)))

(define-for-syntax (list->idx-arg-adr-modes option-list)
  (idx-arg-adr-modes (member 'absolute-x option-list)
                     (member 'absolute-y option-list)
                     (member 'zero-page-x option-list)))

(define-syntax-rule (opcode-with-addressing opcode option-list)
  (define-syntax (opcode stx)
    (syntax-case stx ()
      [(opcode op)
       (opcode-with-addressing/single (list->one-arg-adr-modes option-list) #'opcode #'op stx)]
      [(opcode open op close-or-x close-or-y)
       (opcode-with-addressing/indirect (list->ind-arg-adr-modes option-list) #'opcode #'open #'op #'close-or-x #'close-or-y stx)]
      [(opcode op, idx)
       (opcode-with-addressing/indexed (list->idx-arg-adr-modes option-list) #'opcode #'op #'idx stx)]
      [(opcode op idx)
       (opcode-with-addressing/indexed (list->idx-arg-adr-modes option-list) #'opcode #'op #'idx stx)])))

(define-syntax (define-opcode-functions/macro stx)
  (syntax-case stx ()
    [(_ op option-list bytecode-list mode ext param body)
     (let* ([option-list-clean (second (syntax->datum #'option-list))]
            [bytecode-list-clean (second (syntax->datum #'bytecode-list))]
            [option-list-length (length option-list-clean)]
            [mode? (member `,(syntax->datum #'mode) option-list-clean)]
            [byte-code (when mode? (list-ref bytecode-list-clean (- option-list-length (length mode?))))])
       (with-syntax ([func-name (datum->syntax #'op (symbol-append #'op (syntax->datum #'ext)))]
                     [byte-code-sy byte-code])
         (with-syntax ([op-function
                        (when mode?
                          (if (equal? 'empty (syntax->datum #'param))
                              #'(define (func-name) (map (lambda (elt) (if (equal? elt 'byte-code-place) byte-code-sy elt)) body))
                              #'(define (func-name param) (map (lambda (elt) (if (equal? elt 'byte-code-place) byte-code-sy elt)) body))))])
           #'op-function)))]))

(define-syntax (define-opcode-functions stx)
  (syntax-case stx ()
    [(_ op option-list bytecode-list)
     #'(begin
         (define-opcode-functions/macro op option-list bytecode-list relative "_rel" value (list ''rel-opcode 'byte-code-place value))
         (define-opcode-functions/macro op option-list bytecode-list accumulator "_acc" empty (list ''opcode 'byte-code-place))
         (define-opcode-functions/macro op option-list bytecode-list immediate "_i" value (list ''opcode 'byte-code-place value))
         (define-opcode-functions/macro op option-list bytecode-list zero-page "_zp" value (list ''opcode 'byte-code-place value))
         (define-opcode-functions/macro op option-list bytecode-list zero-page-x "_zpx" value (list ''opcode 'byte-code-place value))
         (define-opcode-functions/macro op option-list bytecode-list absolute "_abs" value (list ''opcode 'byte-code-place (low-byte value) (high-byte value)))
         (define-opcode-functions/macro op option-list bytecode-list absolute-x "_absx" value (list ''opcode 'byte-code-place (low-byte value) (high-byte value)))
         (define-opcode-functions/macro op option-list bytecode-list absolute-y "_absy" value (list ''opcode 'byte-code-place (low-byte value) (high-byte value)))
         (define-opcode-functions/macro op option-list bytecode-list indirect-x "_indx" value (list ''opcode 'byte-code-place (low-byte value) (high-byte value)))
         (define-opcode-functions/macro op option-list bytecode-list indirect-y "_indy" value (list ''opcode 'byte-code-place (low-byte value) (high-byte value)))
         (opcode-with-addressing op option-list)
         )]))


;; ================================================================================ opcode definition

(define-syntax (BYTES stx)
  (syntax-case stx ()
    [(BYTES bytes)
     #'(BYTES_list (quote bytes))]))


(define (BYTES_list bytes)
  (list ''bytes bytes))

(define-opcode-functions ADC
  '(immediate zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y)
  '(#x69      #x65      #x75        #x6D     #x7D       #x79       #x61       #x71))

(module+ test
  (check-match (ADC "%10",x)
               '('opcode #x75 2))

  (check-match (ADC "$1237",y)
               '('opcode #x79 #x37 #x12))

  (check-match (ADC "#100")
               '('opcode #x69 100))

  (check-match (ADC "#$FF")
               '('opcode #x69 #xFF))

  (check-match (ADC "$FF")
               '('opcode #x65 #xFF))

  (check-match (ADC "$FFFF")
               '('opcode #x6d #xff #xff))

  (check-match (ADC < "$FFFF" > ,y)
               '('opcode #x71 #xff #xff))

  (check-match (ADC < "$FFFF" ,x >)
               '('opcode #x61 #xff #xff)))

(define-opcode-functions ASL
  '(accumulator zero-page zero-page-x absolute absolute-x)
  '(#x0a        #x06      #x16        #x0e     #x1e))

(module+ test
  ;; TODO add tests
  (check-match (ASL A)
               '('opcode #x0a))

  (check-match (ASL "$10")
               '('opcode #x06 #x10))

  (check-match (ASL "$10",x)
               '('opcode #x16 #x10))

  (check-match (ASL "$1000")
               '('opcode #x0e #x00 #x10))

  (check-match (ASL "$1000",x)
               '('opcode #x1e #x00 #x10)))

(define-opcode-functions BCC '(relative) '(#x90))
(define-opcode-functions BCS '(relative) '(#xb0))

(define-opcode-functions BEQ
  '(relative)
  '(#xf0))

(module+ test
  (check-match (BEQ "$FC")
               '('rel-opcode #xF0 #xfc))
  (check-match (BEQ ":some")
               '('rel-opcode #xF0 ":some")))

(define-opcode-functions BMI '(relative) '(#x30))
(define-opcode-functions BNE '(relative) '(#xd0))
(define-opcode-functions BPL '(relative) '(#x10))

(define (BRK) (list ''opcode #x00))

(define-opcode-functions BVC '(relative) '(#x50))
(define-opcode-functions BVS '(relative) '(#x70))

(module+ test
  (check-match (BRK)
               '('opcode #x00)))

(define-opcode-functions DEC
  '(zero-page absolute absolute-x zero-page-x)
  '(#xc6      #xce     #xde       #xd6))

(define-opcode-functions INC
  '(zero-page absolute absolute-x zero-page-x)
  '(#xe6      #xee     #xfe       #xf6))

(module+ test
  (check-match (INC "$10")
               '('opcode #xE6 #x10))

  (check-match (INC "$10",x)
               '('opcode #xF6 #x10))

  (check-match (INC "$1000")
               '('opcode #xEE #x00 #x10))

  (check-match (INC "$1000",x)
               '('opcode #xFE #x00 #x10)))

(define (JMP_abs absolute)
  (list ''opcode #x4C (high-byte absolute) (low-byte absolute)))

(define (JSR_abs_label str)
  (list ''opcode #x20 str))

(define (JSR_abs absolute)
  (list ''opcode #x20 (low-byte absolute) (high-byte absolute)))

(define-syntax (JSR stx)
  (syntax-case stx ()
    [(JSR op)
     (if (6510-label-string? (syntax->datum #'op))
         #'(JSR_abs_label op)
         #'(JSR_abs (parse-number-string op)))]))

(define-opcode-functions LDA
  '(immediate zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y)
  '(#xA9      #xA5      #xB5        #xAD     #xBD       #xB9       #xA1       #xB1))

(module+ test
  (check-match (LDA "#$10")
               '('opcode #xA9 16))

  (check-match (LDA "$17")
               '('opcode #xa5 #x17))

  (check-match (LDA "$178F")
               '('opcode #xad #x8F #x17))

  (check-match (LDA "$10",x)
               '('opcode #xB5 16))

  (check-match (LDA "$A000",x)
               '('opcode #xBD #x00 #xA0))

  (check-match (LDA "$A000",y)
               '('opcode #xB9 #x00 #xA0))

  (check-match (LDA < "$A000" >,y )
               '('opcode #xB1 #x00 #xA0))

  (check-match (LDA < "$A000", x > )
               '('opcode #xA1 #x00 #xA0)))

(define (RTS)
  (list ''opcode #x60))

(define-opcode-functions STA
  '(zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y)
  '(#x85      #x95        #x8D     #x9D       #x99       #x81       #x91))

(module+ test
  (check-match (STA "$17")
               '('opcode #x85 23))

  (check-match (STA "$1728")
               '('opcode #x8d #x28 #x17))

  (check-match (STA < "$1728" ,x >)
               '('opcode #x81 #x28 #x17))

  (check-match (STA < "$1728" > y)
               '('opcode #x91 #x28 #x17))

  (check-match (STA < "$1728" > ,y)
               '('opcode #x91 #x28 #x17))

  (check-match (STA "$1728" ,x)
               '('opcode #x9d #x28 #x17))

  (check-match (STA "$1728" ,y)
               '('opcode #x99 #x28 #x17))

  (check-match (STA "$28" ,x)
               '('opcode #x95 #x28))

  (check-match (STA "$1728" 'x)
               '('opcode #x9d #x28 #x17))

  (check-match (STA "$1728" 'y)
               '('opcode #x99 #x28 #x17))

  (check-match (STA "$28" 'x)
               '('opcode #x95 #x28))

  (check-match (STA "$1728" x)
               '('opcode #x9d #x28 #x17))

  (check-match (STA "$1728" y)
               '('opcode #x99 #x28 #x17))

  (check-match (STA "$28" x)
               '('opcode #x95 #x28)))


;; ================================================================================ whole program functions

; (run (assembler-program (initialize-cpu) 0 (list (LDA_i #x41) (JSR_abs #xFFFF) (BRK))))

(define (create-prg program org file-name)
  (display-to-file (list->bytes (append `(,(low-byte org) ,(high-byte org)) program))
                   file-name
                   #:mode 'binary
                   #:exists 'replace))

(define (run-emulator file-name)
  (system (string-append "x64 " file-name)))

;; ======================================== pretty print

(define (hex-format-any a-number-str)
  (let ([parsed-number (parse-number-string a-number-str)])
    (if (> parsed-number 255)
        (string-append "$" (hex-format (high-byte parsed-number)) (hex-format (low-byte parsed-number)))
        (string-append "$" (hex-format parsed-number)))))

(define (hex-format a-number)
  (define digits "0123456789ABCDEF")
  (string (string-ref digits (div a-number 16))
          (string-ref digits (mod a-number 16))))

(define (format-raw-line slst)
  (string-join (map (lambda (element)
                      (cond [(symbol? element) (symbol->string element)]
                            [(6510-number-string? element) (hex-format-any element)]
                            [(is-immediate-number? element) (string-append "#" (hex-format-any (substring element 1)))]
                            [else  element])) slst) " "))

(define (pretty-print-line line)
  (let* ([opcodes (first line)]
         [syntax (last line)]
         [compiled
          (case (first opcodes)
            [('opcode 'rel-opcode)
             (~a  (string-join (map hex-format (drop opcodes 1)) " ")
                  #:min-width 12)]
            [('label) (last opcodes)]
            [('bytes) (~a (string-join  (map hex-format (last opcodes)) " "))]
            [else "?"])]
         [syntax-str
          (case (first opcodes)
            [('opcode 'rel-opcode) (format-raw-line syntax)]
            [('bytes) "   ; raw bytes"]
            [else ""])])
    (string-append compiled syntax-str)))

(define (pretty-print-program resolved-program raw-program)
  (let ([interleaved (map list resolved-program raw-program)])
    (string-join (map pretty-print-line interleaved) "\n")))

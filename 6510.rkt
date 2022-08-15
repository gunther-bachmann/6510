#lang racket

;; todo: implement increment x and increment y and compare
;; todo: make sure to expand label to 2 bytes (for absolutes) and 1 byte (for zero page) opcodes! (e.g. adc (:cout,x))!!
;; ongoing: add possibility to use labels instead of values (in all addressing modes)
;; todo: add method descriptions (scrbl)
;; todo: get more information about syntax object context, to allow propagation of syntax context of megaparsack to transformations in 6510.rkt
;; todo: check whether ebnf + some is a dsl that could be used instead of programmatically constructing parsers with megaparsack (dsl to generate that)
;; planned: realize with typed racket

(require (only-in racket/format ~a))
(require (only-in threading ~>))
(require (only-in rnrs/base-6 div mod))
(require (for-syntax (only-in racket/list second empty? first)))

(require (for-syntax "6510-utils.rkt"))
(require (for-syntax (rename-in "6510-syntax-utils.rkt"
                                (one-arg-adr-modes-relative? relative?)
                                (one-arg-adr-modes-accumulator? accumulator?)
                                (one-arg-adr-modes-immediate? immediate?)
                                (one-arg-adr-modes-absolute? absolute?)
                                (one-arg-adr-modes-zero-page? zero-page?)
                                (ind-arg-adr-modes-indirect? indirect?)
                                (ind-arg-adr-modes-indirect-x? indirect-x?)
                                (ind-arg-adr-modes-indirect-y? indirect-y?)
                                (idx-arg-adr-modes-absolute-x? absolute-x?)
                                (idx-arg-adr-modes-absolute-y? absolute-y?)
                                (idx-arg-adr-modes-zero-page-x? zero-page-x?)
                                (idx-arg-adr-modes-zero-page-y? zero-page-y?))))
(require "6510-utils.rkt")
(require "6510-interpreter.rkt")
(require (rename-in  racket/contract [define/contract define/c]))

(module+ test
  (require rackunit)
  (begin-for-syntax
    (require rackunit)))

(provide parse-number-string replace-labels commands->bytes create-prg run-emulator pretty-print-program create-image-with-program
         ADC AND ASL
         BCC BCS BEQ BIT BMI BNE BPL BRK BVC BVS
         CLC CLD CLI CLV CMP CPX CPY
         DEC DEX DEY 
         EOR
         INC INX INY
         JMP JSR         
         LDA LDX LDY LSR
         NOP
         ORA
         PHA PHP PLA PLP
         ROL ROR RTI RTS
         SBC SEC SED SEI STA STX STY
         TAX TAY TSX TXA TXS TYA        
         LABEL BYTES)


;; -------------------------------------------------------------------------------- address resolution

(define-for-syntax (drop-meta-info line)
  (filter (lambda (element) (not (and (list? element) (keyword? (first element))))) line))

(define (drop-meta-info line)
  (filter (lambda (element) (not (and (list? element) (keyword? (first element))))) line))

(module+ test
  (check-match (drop-meta-info '(ADC (#:line 5 #:org-cmd "some") "#$10"))
               '(ADC "#$10")))

(define/c (6510-byte-length command)
  (-> (listof any/c) exact-nonnegative-integer?)
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

  (check-match (6510-byte-length '('bytes () (1 2 3)))
               3)

  (check-match (6510-byte-length '('opcode 1 2 3))
               3)

  (check-match (6510-byte-length '('label () ":test"))
               0))

;; translates a list of command lengths into a list of pairs, each holding one command length and its absolute position (relative to current-sum)
(define/c (lo-sums list current-sum)
  (-> (listof any/c) exact-nonnegative-integer? (listof (listof exact-nonnegative-integer?)))
  (if (empty? list)
      '()
      (let* ([first-num (first list)]
             [new-sum (+ current-sum first-num)])
        (append `((,first-num ,current-sum)) (lo-sums (drop list 1) new-sum)))))

(module+ test #| lo-sums |#
  (check-match (lo-sums '(1 2 2 0) 0)
               '((1 0) (2 1) (2 3) (0 5)))
  (check-match (lo-sums '() 0)
               '())
  (check-match (lo-sums '(1 2 2 0) 8)
               '((1 8) (2 9) (2 11) (0 13))))

;; collect from the list of pairs of command definition and command-len-absolute-offset pairs, only the labels (and their lenght-offset pairs)
(define (collect-label-offset-map commands-bytes-list)
  (filter (lambda (command-byte-pair)
              (case (first (first command-byte-pair))
                [('label) #t]
                [else #f])) commands-bytes-list))

(module+ test #| collect-label-offset-map |#
  (check-match (collect-label-offset-map '((('opcode 1 2) (2 0))
                                           (('label ":some") (0 2))
                                           (('opcode 1 -2) (2 2))
                                           (('label ":other") (0 4))
                                           (('opcode 5 ":some") (3 4))
                                           (('label ":end") (0 7))))
               '((('label ":some") (0 2)) (('label ":other") (0 4)) (('label ":end") (0 7)))))

;; get the offset of a given label
(define (get-label-offset labels-byte-list label)
  (let ([filtered (filter (lambda (label-byte-pair)
                            (equal? label (last (first label-byte-pair))))
                          labels-byte-list)])
    (when (empty? filtered)
      (error "label not found in list" label filtered))
    (last (last (last filtered)))))

(module+ test #| get-label-offset |#
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

;; construct a list of commands and their command-length-absolute-offset pairs
(define (commands-bytes-list commands)
  (let* ([byte-lengths (map 6510-byte-length commands)]
         [byte-lengths/w-offset (lo-sums byte-lengths 0)])
    (map list commands byte-lengths/w-offset)))

(module+ test #| commands-byte-list |#

  (check-match (commands-bytes-list '(('opcode 10 10 10)
                                      ('label ":some")
                                      ('opcode 0)
                                      ('opcode 10 10)))
               '((('opcode 10 10 10) (3 0))
                 (('label ":some") (0 3))
                 (('opcode 0) (1 3))
                 (('opcode 10 10) (2 4)))))

(define label-high-byte-suffix "-H")
(define label-low-byte-suffix "-L")
(define (main-label-string label-string)
  (regexp-replace #rx"#?(:[a-zA-Z][a-zA-Z0-9_]*)(-H|-L)?" label-string "\\1"))

(module+ test #| main-label-string |#
  (check-equal? (main-label-string ":some")
                ":some")
  (check-equal? (main-label-string ":some-H")
                ":some")
  (check-equal? (main-label-string ":some-L")
                ":some")
  (check-equal? (main-label-string "#:some-L")
                ":some"))

;; replace label references in opcode list command-len-absolute-offset pairs with their absolute / relative offsets
(define (replace-label command-byte-pair labels-bytes-list address)
  (let* ([command (first command-byte-pair)]
         [current-offset (last (last command-byte-pair))]
         [command-length (first (last command-byte-pair))])
    (if (>= 1 (length command))
        command-byte-pair
        (if (and (not (equal? ''label (first command))) (or (6510-label-string? (last command))
                                                       (6510-label-byte-string? (last command))
                                                       (6510-label-immediate-byte-string? (last command))))
            (let* ([label-string (last command)]
                   [main-label-string (main-label-string label-string)]
                   [label-offset (+ address (get-label-offset labels-bytes-list main-label-string))]
                   [rel-label-offset (- (get-label-offset labels-bytes-list main-label-string) current-offset command-length)])
              (case (first command)
                [('rel-opcode)
                 (list (append (drop-right command 1)
                               (list (low-byte rel-label-offset)))
                       (last command-byte-pair))]
                [('opcode)
                 (cond ((string-suffix? label-string label-low-byte-suffix)
                        (list (append (drop-right command 1)
                                      (list (low-byte label-offset)))
                              (last command-byte-pair)))
                       ((string-suffix? label-string label-high-byte-suffix)
                        (list (append (drop-right command 1)
                                      (list (high-byte label-offset)))
                              (last command-byte-pair)))
                       (else
                        (list (append (drop-right command 1)
                                      (list (low-byte label-offset) (high-byte label-offset)))
                              (last command-byte-pair))))]
                [else (error (string-append "unknown label reference in opcode" (symbol->string (first (command)))))])
              )
            command-byte-pair))))

(module+ test
  (check-match (replace-label '(('opcode 20 ":some") (3 10))
                              '((('label ":some") (0 8)))
                              100)
               '(('opcode 20 108 0) (3 10)))

  (check-match (replace-label '(('opcode #xa9 ":some-L") (2 10))
                              '((('label ":some") (0 8)))
                              100)
               '(('opcode #xa9 108) (2 10)))

  (check-match (replace-label '(('opcode #xa9 ":some-H") (2 10))
                              '((('label ":some") (0 8)))
                              #xc000)
               '(('opcode #xa9 #xc0) (2 10)))

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

;; replace labels in the opcode list (using intermediate rep with commandlen-absolute-offset- pairs)
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

;; is the given command a label (placeholder)
(define (command-is-label? command)
  (case (first command) [('label) #t] [else #f]))

;; remove information from command list (leaving just a list of byte lists for each command line)
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

;; translate commands to bytes
(define (commands->bytes memory-address commands )
  (flatten (~>  (replace-labels commands memory-address)
               remove-resolved-statements)))

(define (LABEL_s label) (list ''label label))

(define-syntax (LABEL stx)
  (syntax-case stx ()
    [(LABEL meta op)
     #'(LABEL_s op)]))

;; ================================================================================ opcode definition helper
(define-for-syntax (accumulator-mode opcode operand)
  (with-syntax ([operand-value (syntax->datum operand)]
                [symbol-acc (symbol-append opcode '_acc)])
    (when (equal? 'A (syntax->datum #'operand-value))
      #'(symbol-acc))))

(module+ test
  (begin-for-syntax
    (check-match (syntax->datum (accumulator-mode #'LDA #'A))
                 '(LDA_acc))
    (check-eq? (accumulator-mode #'LDA #'B)
               (void))
    (check-eq? (accumulator-mode #'LDA #'"$10")
               (void))))

(define-for-syntax (immediate-mode opcode operand)
  (with-syntax ([operand-value (syntax->datum operand)]
                [symbol-i (symbol-append opcode '_i)])
    (if (is-immediate-number? (syntax->datum #'operand-value))                     
      (with-syntax ([op-number (parse-number-string (substring (syntax->datum operand) 1))])
        #'(symbol-i op-number))
      (when (6510-label-immediate-byte-string? (syntax->datum #'operand-value))
        #'(symbol-i operand-value)))))

(module+ test
  (begin-for-syntax
    (check-match (syntax->datum (immediate-mode #'LDA #'"#$10"))
                 '(LDA_i 16))
    (check-eq? (immediate-mode #'LDA #'"$10")
               (void))
    (check-eq? (immediate-mode #'LDA #'"#A0")
               (void))
    (check-eq? (immediate-mode #'LDA #'"$10A0")
               (void))
    (check-match (syntax->datum (immediate-mode #'LDA #'"#$A0"))
                 '(LDA_i 160))))

(define-for-syntax (zero-page-mode opcode operand)
  (with-syntax ([operand-value (syntax->datum operand)]
                [symbol-zp (symbol-append opcode '_zp)])
    (if (6510-number-string? (syntax->datum operand))
          (with-syntax ([op-number (parse-number-string (syntax->datum operand))])
            (when (> 256 (parse-number-string (syntax->datum #'operand-value)))
              #'(symbol-zp op-number)))
          (when (6510-label-byte-string? (syntax->datum #'operand-value))
            #'(symbol-zp operand-value)))))

(module+ test
  (begin-for-syntax
    (check-match (syntax->datum (zero-page-mode #'LDA #'"$10"))
                 '(LDA_zp 16))
    (check-eq? (zero-page-mode #'LDA #'"#$10")
               (void))
    (check-eq? (zero-page-mode #'LDA #'"$Q0")
               (void))
    (check-eq? (zero-page-mode #'LDA #'"$10A0")
               (void))
    (check-eq? (zero-page-mode #'LDA #'":out")
               (void))
    (check-match (syntax->datum (zero-page-mode #'LDA #'"$A0"))
                 '(LDA_zp 160))
    (check-match (syntax->datum (zero-page-mode #'LDA #'":out-L"))
                 '(LDA_zp ":out-L"))))

(define-for-syntax (absolute-mode opcode operand)
  (with-syntax ([operand-value (syntax->datum operand)]
                [symbol-abs (symbol-append opcode '_abs)])
    (if (6510-number-string? (syntax->datum #'operand-value))
        (with-syntax ([op-number (parse-number-string (syntax->datum operand))])
          (when (<= 256 (syntax->datum #'op-number))
            #'(symbol-abs op-number)))
        (when (6510-label-string? (syntax->datum #'operand-value))
          #'(symbol-abs operand-value)))))

(module+ test
  (begin-for-syntax
    (check-match (syntax->datum (absolute-mode #'JSR #'"$1000"))
                 '(JSR_abs 4096))
    (check-eq? (absolute-mode #'LDA #'"$10")
               (void))
    (check-match (syntax->datum (absolute-mode #'JSR #'":out"))
                 '(JSR_abs ":out"))))

(define-for-syntax (indirect-mode opcode open operand close)
  (with-syntax ([operand-value (syntax->datum operand)]
                [symbol-ind (symbol-append opcode '_ind)])    
    (if (6510-number-string? (syntax->datum #'operand-value))
        (with-syntax ([op-number (parse-number-string (syntax->datum operand))])
          (when (<= 256 (syntax->datum #'op-number))
            #'(symbol-ind op-number)))
        (when (6510-label-string? (syntax->datum #'operand-value))
          #'(symbol-ind operand-value)))))

(module+ test #| indirect-mode |#
  (begin-for-syntax
    (check-match (syntax->datum (indirect-mode #'JMP #'#\( #'"$1011" #'#\)))
                 '(JMP_ind 4113))

    (check-match (syntax->datum (indirect-mode #'JMP #'#\( #'":out" #'#\)))
                 '(JMP_ind ":out"))))

(define-for-syntax (indirect-x-mode opcode open operand close-or-x close-or-y)
  (with-syntax ([symbol-indx (symbol-append opcode '_indx)]
                [x-idx (syntax->datum close-or-x)])
    (when (or (equal? (syntax->datum #'x-idx) '(unquote x))
             (equal? (syntax->datum #'x-idx) 'x))
      (if (6510-number-string? (syntax->datum operand))
          (with-syntax ([op-number (parse-number-string (syntax->datum operand))])
            (when (>= 255 (syntax->datum #'op-number))
              #'(symbol-indx op-number)))
          (with-syntax ([op operand])
            #'(symbol-indx op))))))

(module+ test #| indirect-x-mode |#
  (begin-for-syntax
    (check-match (syntax->datum (indirect-x-mode #'LDA #'#\( #'"$10" #'x #'#\)))
                 '(LDA_indx 16))
    (check-eq? (indirect-x-mode #'LDA #'#\( #'"$1000" #'x #'#\))
               (void))
    (check-match (syntax->datum (indirect-x-mode #'LDA #'#\( #'":out" #'x #'#\)))
                 '(LDA_indx ":out"))))

(define-for-syntax (indirect-y-mode opcode open operand close-or-x close-or-y)
  (with-syntax ([symbol-indy (symbol-append opcode '_indy)]
                [y-idx (syntax->datum close-or-y)])
    (when (or (equal? (syntax->datum #'y-idx) '(unquote y))
              (equal? (syntax->datum #'y-idx) 'y))
      (if (6510-number-string? (syntax->datum operand))
          (with-syntax ([op-number (parse-number-string (syntax->datum operand))])
            (when (>= 255 (syntax->datum #'op-number))
              #'(symbol-indy op-number)))
          (with-syntax ([op operand])
            #'(symbol-indy op))))))

(module+ test
  (begin-for-syntax
    (check-match (syntax->datum (indirect-y-mode #'LDA #'#\( #'"$10" #'#\) #'y))
                 '(LDA_indy 16))
    (check-eq? (indirect-y-mode #'LDA #'#\( #'"$1000" #'#\) #'y)
               (void))
    (check-match (syntax->datum (indirect-y-mode #'LDA #'#\( #'":out" #'#\) #'y))
                 '(LDA_indy ":out"))))

(define-for-syntax (absolute-x-mode opcode operand idx)
  (with-syntax ([symbol-absx (symbol-append opcode '_absx)]
                [x-idx (syntax->datum idx)])
    (when (equal? (syntax->datum #'x-idx) 'x)
      (if (6510-number-string? (syntax->datum operand))
          (with-syntax ([op-number (parse-number-string (syntax->datum operand))])
            (when (< 255 (syntax->datum #'op-number))
              #'(symbol-absx op-number)))
          (when (6510-label-string? (syntax->datum operand))
            (with-syntax ([op operand])
              #'(symbol-absx op)))))))

(module+ test
  (begin-for-syntax
    (check-match (syntax->datum (absolute-x-mode #'LDA #'"$1000" #'x))
                 '(LDA_absx 4096))
    (check-eq? (absolute-x-mode #'LDA #'"$10" #'x)
               (void))
    (check-match (syntax->datum (absolute-x-mode #'LDA #'":out" #'x))
                 '(LDA_absx ":out"))))

(define-for-syntax (absolute-y-mode opcode operand idx)
  (with-syntax ([symbol-absy (symbol-append opcode '_absy)]
                [y-idx (syntax->datum idx)])
    (when (equal? (syntax->datum #'y-idx) 'y)
      (if (6510-number-string? (syntax->datum operand))
          (with-syntax ([op-number (parse-number-string (syntax->datum operand))])
            (when (< 255 (syntax->datum #'op-number))
              #'(symbol-absy op-number)))
          (when (6510-label-string? (syntax->datum operand))
            (with-syntax ([op operand])
              #'(symbol-absy op)))))))

(module+ test
  (begin-for-syntax
    (check-match (syntax->datum (absolute-y-mode #'LDA #'"$1000" #'y))
                 '(LDA_absy 4096))
    (check-eq? (absolute-y-mode #'LDA #'"$10" #'y)
               (void))
    (check-match (syntax->datum (absolute-y-mode #'LDA #'":out" #'y))
                 '(LDA_absy ":out"))))

(define-for-syntax (relative-mode opcode operand)
  (with-syntax ([operand-value (syntax->datum operand)]
                [symbol-rel (symbol-append opcode '_rel)])
    (if (6510-label-string? (syntax->datum #'operand-value))
        #'(symbol-rel operand-value)
        (when (and (6510-number-string? (syntax->datum #'operand-value))
                   (> 256 (parse-number-string (syntax->datum #'operand-value))))
          (with-syntax ([op-number (parse-number-string (syntax->datum operand))])
            #'(symbol-rel op-number))))))

(module+ test
  (begin-for-syntax
    (check-match (syntax->datum (relative-mode #'BEQ #'"$10"))
                 '(BEQ_rel 16))
    (check-eq? (relative-mode #'BEQ #'"$1000")
               (void))
    (check-match (syntax->datum (relative-mode #'BEQ #'":out"))
                 '(BEQ_rel ":out"))))

(define-for-syntax (zeropage-x-mode opcode operand idx)
  (with-syntax ([symbol-zpx (symbol-append opcode '_zpx)]
                [operand-value (syntax->datum operand)]
                [x-idx (syntax->datum idx)])
    (when (equal? (syntax->datum #'x-idx) 'x)
      (if (6510-number-string? (syntax->datum operand))
          (with-syntax ([op-number (parse-number-string (syntax->datum operand))])
            (when (> 256 (syntax->datum #'op-number))
              #'(symbol-zpx op-number)))
          #'(symbol-zpx operand-value)))))

(define-for-syntax (zeropage-y-mode opcode operand idx)
  (with-syntax ([symbol-zpy (symbol-append opcode '_zpy)]
                [operand-value (syntax->datum operand)]
                [y-idx (syntax->datum idx)])
    (when (equal? (syntax->datum #'y-idx) 'y)
      (if (6510-number-string? (syntax->datum operand))
          (with-syntax ([op-number (parse-number-string (syntax->datum operand))])
            (when (> 256 (syntax->datum #'op-number))
              #'(symbol-zpy op-number)))          
          #'(symbol-zpy operand-value)))))

(module+ test
  (begin-for-syntax
    (check-match (syntax->datum (zeropage-x-mode #'LDA #'"$10" #'x))
                 '(LDA_zpx 16))
    (check-eq? (zeropage-x-mode #'LDA #'"$1000" #'x)
               (void))
    (check-match (syntax->datum (zeropage-x-mode #'LDA #'":out" #'x))
                 '(LDA_zpx ":out"))))

(define-for-syntax (construct-example opcode argument qargument description within-non-racket-syntax)
  (if within-non-racket-syntax
      (string-append "  " opcode " " argument "  ; " description "\n")
      (string-append "  (" opcode " " qargument  ") # " description "\n")))

(define-for-syntax (string-append-l list-of-strings)
  (foldr string-append "" (filter (lambda (elt) (not (void? elt))) list-of-strings)))

(module+ test
  (begin-for-syntax
    (check-equal? (string-append-l `("a" ,(void)  "b"))
                  "ab")
    (check-equal? (string-append-l `("a" ,(when (eq? "a" "b") "some") "b"))
                  "ab")))

(define-for-syntax (error-string/indirect adr-modes opcode-string within-non-racket-syntax)
  (string-append-l
   (list
    "invalid syntax.\n"
    (if (or (indirect-x? adr-modes) (indirect-y? adr-modes) (indirect? adr-modes))
        "expected:\n"
        (string-append opcode-string " cannot be used with indirect addressing\n"))
    (when (indirect-x? adr-modes)
      (construct-example opcode-string "($10,x)" "< \"$10\",x >" "indirect x addressing mode" within-non-racket-syntax))
    (when (indirect-y? adr-modes)
      (construct-example opcode-string "($10),y" "< \"$10\" >,y" "indirect y addressing mode" within-non-racket-syntax))
    (when (indirect? adr-modes)
      (construct-example opcode-string "($1000)" "< \"($1000)\" >" "indirect addressing mode" within-non-racket-syntax))
    "got: ")))

(define-for-syntax (error-string/indexed adr-modes opcode-string within-non-racket-syntax)
  (string-append-l
   (list
    "invalid syntax.\n"
    (if (or (absolute-x? adr-modes) (absolute-y? adr-modes) (zero-page-x? adr-modes))
        "expected:\n"
        (string-append opcode-string " cannot use index\n"))
    (when (absolute-x? adr-modes)
      (construct-example opcode-string "$1000,x" "\"$1000\",x" "absolute x addressing mode" within-non-racket-syntax))
    (when (absolute-y? adr-modes)
      (construct-example opcode-string "$1000,y" "\"$1000\",y" "absolute y addressing mode" within-non-racket-syntax))
    (when (zero-page-x? adr-modes)
      (construct-example opcode-string  "$10,x" "\"$10\",x" "zero page x addressing mode" within-non-racket-syntax))
    (when (zero-page-y? adr-modes)
      (construct-example opcode-string  "$10,y" "\"$10\",y" "zero page y addressing mode" within-non-racket-syntax))
    "got: ")))

(define-for-syntax (error-string/single adr-modes opcode-string within-non-racket-syntax)
  (string-append-l
   (list
    "invalid syntax.\n"
    (if (or (relative? adr-modes) (accumulator? adr-modes) (immediate? adr-modes) (zero-page? adr-modes) (absolute? adr-modes))
        "expected:\n"
        (string-append opcode-string " cannot have one operand\n"))
    (when (relative? adr-modes)
      (string-append "  (" opcode-string " \":label\") # relative mode\n"))
    (when (accumulator? adr-modes)
      (construct-example opcode-string "A" "A" "accumulator mode" within-non-racket-syntax))
    (when (immediate? adr-modes)
      (construct-example opcode-string "#$10" "\"#$10\"" "immediate addressing mode" within-non-racket-syntax))
    (when (zero-page? adr-modes)
      (construct-example opcode-string "$10" "\"$10\"" "zeropage addressing mode" within-non-racket-syntax))
    (when (absolute? adr-modes)
      (construct-example opcode-string "$1000" "\"$1000\"" "absolute addressing mode" within-non-racket-syntax))
    "got: ")))

(define-for-syntax (raise-syntax-error error-string opcode non-racket-syn org-string stx)
  (error error-string
         (if non-racket-syn
             (syntax->datum org-string)
             (drop-meta-info (syntax->datum stx)))
         'in 'line (if non-racket-syn
                       (syntax->datum non-racket-syn)
                       (syntax-line opcode))))

(define-for-syntax (collect-syntax-result list-of-syntax-objects)
  (foldl discard-void-syntax-object #'()  list-of-syntax-objects))

(define-for-syntax (opcode-with-addressing/single adr-modes opcode op stx non-racket-syn org-string)
  (with-syntax ([ires (when (immediate? adr-modes) (immediate-mode opcode op))]
                [zpres (when (zero-page? adr-modes) (zero-page-mode opcode op))]
                [absres (when (absolute? adr-modes) (absolute-mode opcode op))]
                [accres (when (accumulator? adr-modes) (accumulator-mode opcode op))]
                [relres (when (relative? adr-modes) (relative-mode opcode op))])
    (let* ([res (collect-syntax-result (list #'relres #'accres #'ires #'zpres #'absres))]
           [opcode-string (symbol->string (syntax->datum opcode))]
           [error-string (error-string/single adr-modes opcode-string non-racket-syn)])
      (if (empty? (syntax->datum res))
          (raise-syntax-error error-string opcode non-racket-syn org-string stx)
          res))))

(module+ test
  (begin-for-syntax
    (check-match (syntax->datum (opcode-with-addressing/single (list->one-arg-adr-modes '(zero-page)) #'LDA #'"$10" #'() #'5 #'"lda $10"))
                 '(LDA_zp 16))
    (check-exn #rx"invalid syntax.\nexpected.*LDA \\$10  ;.*got:  \"lda #\\$10\" 'in 'line 8"
               (lambda () (opcode-with-addressing/single (list->one-arg-adr-modes '(zero-page)) #'LDA #'"#$10" #'() #'8 #'"lda #$10")))
    (check-match (syntax->datum (opcode-with-addressing/single (list->one-arg-adr-modes '(zero-page immediate)) #'LDA #'"#$10" #'() #f #'""))
                 '(LDA_i 16))))

(define-for-syntax (opcode-with-addressing/just-indirect adr-modes opcode open op close stx non-racket-syn org-string)
  (with-syntax ([ind (when (indirect? adr-modes) (indirect-mode opcode open op close))])
    (let* ([res (collect-syntax-result (list #'ind))]
           [opcode-string (symbol->string (syntax->datum opcode))]
           [error-string (error-string/indirect adr-modes opcode-string non-racket-syn)])
      (if (equal? '() (syntax->datum res))
          (raise-syntax-error error-string opcode non-racket-syn org-string stx)
          res))))

(module+ test
  (begin-for-syntax
    (check-match (syntax->datum (opcode-with-addressing/just-indirect (list->ind-arg-adr-modes '(indirect)) #'JMP #'#\( #'"$1011" #'#\) #'() #f #'""))
                 '(JMP_ind 4113))))

(define-for-syntax (opcode-with-addressing/indirect adr-modes opcode open op close-or-x close-or-y stx non-racket-syn org-string)
  (with-syntax ([indxres (when (indirect-x? adr-modes) (indirect-x-mode opcode open op close-or-x close-or-y))]
                [indyres (when (indirect-y? adr-modes) (indirect-y-mode opcode open op close-or-x close-or-y))])
    (let* ([res (collect-syntax-result (list #'indxres #'indyres))]
           [opcode-string (symbol->string (syntax->datum opcode))]
           [error-string (error-string/indirect adr-modes opcode-string non-racket-syn)])
      (if (equal? '() (syntax->datum res))
          (raise-syntax-error error-string opcode non-racket-syn org-string stx)
          res))))

(module+ test
  (begin-for-syntax
    (check-match (syntax->datum (opcode-with-addressing/indirect (list->ind-arg-adr-modes '(indirect-x)) #'LDA #'#\( #'"$10" #'x #'#\) #'() #f #'""))
                 '(LDA_indx 16))
    (check-exn #rx"invalid syntax.\nexpected.*\\(LDA < \"\\$10\",x >\\) #.*got:  '\\(some\\) 'in 'line "
               (lambda () (opcode-with-addressing/indirect (list->ind-arg-adr-modes '(indirect-x)) #'LDA #'#\( #'"$10" #'#\) #'y #'(some) #f #'"")))
    (check-match (syntax->datum (opcode-with-addressing/indirect (list->ind-arg-adr-modes '(indirect-x indirect-y)) #'LDA #'#\( #'"$10" #'#\) #'y #'() #f #'""))
                 '(LDA_indy 16))))

(define-for-syntax (opcode-with-addressing/indexed adr-modes opcode op idx stx non-racket-syn org-string)
  (with-syntax ([zpxres (when (zero-page-x? adr-modes) (zeropage-x-mode opcode op idx))]
                [zpyres (when (zero-page-y? adr-modes) (zeropage-y-mode opcode op idx))]
                [absxres (when (absolute-x? adr-modes) (absolute-x-mode opcode op idx))]
                [absyres (when (absolute-y? adr-modes) (absolute-y-mode opcode op idx))])
    (let* ([res (collect-syntax-result (list #'zpxres #'zpyres #'absxres #'absyres))]
           [opcode-string (symbol->string (syntax->datum opcode))]
           [error-string (error-string/indexed adr-modes opcode-string non-racket-syn)])
      (if (equal? '() (syntax->datum res))
          (raise-syntax-error error-string opcode non-racket-syn org-string stx)
          res))))

(module+ test
  (begin-for-syntax
    (check-match (syntax->datum (opcode-with-addressing/indexed (list->idx-arg-adr-modes '(absolute-x)) #'LDA #'"$1000" #'x #'() #f #'""))
                 '(LDA_absx 4096))
    (check-exn
     #rx"invalid syntax.\nexpected.*\\(LDA \"\\$1000\",x\\) #.*got:  '\\(LDA \"\\$1000\" y\\) 'in 'line"
     (lambda () (opcode-with-addressing/indexed (list->idx-arg-adr-modes '(absolute-x)) #'LDA #'"$1000" #'y #'(LDA "$1000" y) #f #'""))
     "check error message to use racket syntax")
    (check-exn
     #rx"invalid syntax.\nexpected.*LDA \\$1000,x  ;.*got:  \"org string\" 'in 'line"
     (lambda () (opcode-with-addressing/indexed (list->idx-arg-adr-modes '(absolute-x)) #'LDA #'"$1000" #'y #'(LDA "$1000" y) #'some #'"org string"))
     "check error message to use free syntax")
    (check-match (syntax->datum (opcode-with-addressing/single (list->one-arg-adr-modes '(zero-page)) #'LDA  #'":some-L" #'() #f #'""))
                 '(LDA_zp ":some-L"))
    (check-match (syntax->datum (opcode-with-addressing/indexed (list->idx-arg-adr-modes '(absolute-x absolute-y)) #'LDA  #'"$1000" #'y #'() #f #'""))
                 '(LDA_absy 4096))))

(define-for-syntax (list->one-arg-adr-modes option-list)
  (one-arg-adr-modes (member 'relative option-list)
                     (member 'accumulator option-list)
                     (member 'immediate option-list)
                     (member 'zero-page option-list)
                     (member 'absolute option-list)))

(define-for-syntax (list->ind-arg-adr-modes option-list)
  (ind-arg-adr-modes (member 'indirect-x option-list)
                     (member 'indirect-y option-list)
                     (member 'indirect option-list)))

(define-for-syntax (list->idx-arg-adr-modes option-list)
  (idx-arg-adr-modes (member 'absolute-x option-list)
                     (member 'absolute-y option-list)
                     (member 'zero-page-x option-list)
                     (member 'zero-page-y option-list)))

(define-for-syntax (operands-indirect-y? open op close y)
  (let ([open-i (syntax->datum open)]
        [close-i (syntax->datum close)]
        [y-i (syntax->datum y)])
    (and (equal? open-i '<) (equal? close-i '>) (or (equal? y-i 'y) (equal? y-i '(unquote y))))))

(define-for-syntax (operands-indirect-x? open op x close)
  (let ([open-i (syntax->datum open)]
        [x-i (syntax->datum x)]
        [close-i (syntax->datum close)])
    (and (equal? open-i '<) (equal? close-i '>) (or  (equal? x-i 'x) (equal? x-i '(unquote x))))))

(define-for-syntax (operands-indirect? open op close)
  (let ([open-i (syntax->datum open)]
        [close-i (syntax->datum close)])
    (and (equal? open-i '<) (equal? close-i '>))))

(define-syntax-rule (opcode-with-addressing opcode option-list)
  (define-syntax (opcode stx)
    (syntax-case stx ()
      [(opcode)
       (with-syntax ([symbol (symbol-append #'opcode (syntax->datum #'_impl))]) #'(symbol))]
      [(opcode op)
       (opcode-with-addressing/single (list->one-arg-adr-modes option-list) #'opcode #'op stx #f #'"")]
      [(opcode (#:line line-number #:org-cmd org-string) op)
       (opcode-with-addressing/single (list->one-arg-adr-modes option-list) #'opcode #'op stx #'line-number #'org-string)]
      [(opcode open op close y)
       (operands-indirect-y? #'open #'op #'close #'y)
       (opcode-with-addressing/indirect (list->ind-arg-adr-modes option-list) #'opcode #'open #'op #'close #'y stx #f #'"")]
      [(opcode (#:line line-number #:org-cmd org-string) open op close y)
       (operands-indirect-y? #'open #'op #'close #'y)
       (opcode-with-addressing/indirect (list->ind-arg-adr-modes option-list) #'opcode #'open #'op #'close #'y stx #'line-number #'org-string)]
      [(opcode open op x close)
       (operands-indirect-x? #'open #'op #'x #'close)
       (opcode-with-addressing/indirect (list->ind-arg-adr-modes option-list) #'opcode #'open #'op #'x #'close stx #f #'"")]
      [(opcode (#:line line-number #:org-cmd org-string) open op x close)
       (operands-indirect-x? #'open #'op #'x #'close)
       (opcode-with-addressing/indirect (list->ind-arg-adr-modes option-list) #'opcode #'open #'op #'x #'close stx #'line-number #'org-string)]
      [(opcode open op close)
       (operands-indirect? #'open #'op #'close)
       (opcode-with-addressing/just-indirect (list->ind-arg-adr-modes option-list) #'opcode #'open #'op #'close stx #f #'"")]
      [(opcode (#:line line-number #:org-cmd org-string) open op close)
       (operands-indirect? #'open #'op #'close)
       (opcode-with-addressing/just-indirect (list->ind-arg-adr-modes option-list) #'opcode #'open #'op #'close stx #'line-number #'org-string)]      
      [(opcode op, idx)
       (opcode-with-addressing/indexed (list->idx-arg-adr-modes option-list) #'opcode #'op #'idx stx #f #'"")]
      [(opcode (#:line line-number #:org-cmd org-string) op, idx)
       (opcode-with-addressing/indexed (list->idx-arg-adr-modes option-list) #'opcode #'op #'idx stx #'line-number #'org-string)]
      [(opcode op idx)
       (opcode-with-addressing/indexed (list->idx-arg-adr-modes option-list) #'opcode #'op #'idx stx #f #'"")]
      [(opcode (#:line line-number #:org-cmd org-string) op idx)
       (opcode-with-addressing/indexed (list->idx-arg-adr-modes option-list) #'opcode #'op #'idx stx #'line-number #'org-string)])))

(define (test-FANTASTIC_abs ops) 'ok)
(module+ test
  (opcode-with-addressing test-FANTASTIC '(absolute))
  (check-match (test-FANTASTIC "$1000")
               'ok))

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
         (define-opcode-functions/macro op option-list bytecode-list no-arg "" empty (list ''opcode 'byte-code-place))
         (define-opcode-functions/macro op option-list bytecode-list implicit "_impl" empty (list ''opcode 'byte-code-place))
         (define-opcode-functions/macro op option-list bytecode-list relative "_rel" value (list ''rel-opcode 'byte-code-place value))
         (define-opcode-functions/macro op option-list bytecode-list accumulator "_acc" empty (list ''opcode 'byte-code-place))
         (define-opcode-functions/macro op option-list bytecode-list immediate "_i" value (list ''opcode 'byte-code-place value))
         (define-opcode-functions/macro op option-list bytecode-list zero-page "_zp" value (list ''opcode 'byte-code-place value))
         (define-opcode-functions/macro op option-list bytecode-list zero-page-x "_zpx" value (list ''opcode 'byte-code-place value))
         (define-opcode-functions/macro op option-list bytecode-list zero-page-y "_zpy" value (list ''opcode 'byte-code-place value))
         (define-opcode-functions/macro op option-list bytecode-list absolute "_abs" value (append (list ''opcode 'byte-code-place) (if (number? value) (list (low-byte value) (high-byte value)) (list value))))
         (define-opcode-functions/macro op option-list bytecode-list absolute-x "_absx" value (append (list ''opcode 'byte-code-place) (if (number? value) (list (low-byte value) (high-byte value)) (list value))))
         (define-opcode-functions/macro op option-list bytecode-list absolute-y "_absy" value (append (list ''opcode 'byte-code-place) (if (number? value) (list (low-byte value) (high-byte value)) (list value))))
         (define-opcode-functions/macro op option-list bytecode-list indirect "_ind" value (append (list ''opcode 'byte-code-place) (if (number? value) (list (low-byte value) (high-byte value)) (list value))))
         (define-opcode-functions/macro op option-list bytecode-list indirect-x "_indx" value (append (list ''opcode 'byte-code-place) (if (number? value) (list (byte value)) (list value))))
         (define-opcode-functions/macro op option-list bytecode-list indirect-y "_indy" value (append (list ''opcode 'byte-code-place) (if (number? value) (list (byte value)) (list value))))
         (opcode-with-addressing op option-list)
         )]))


;; -------------------------------------------------------------------------------- opcode definition

(define-syntax (BYTES stx)
  (syntax-case stx ()
    [(BYTES meta bytes)
     #'(BYTES_list (quote bytes))]))

(define (BYTES_list bytes)
  (list ''bytes bytes))

(module+ test
  (check-match (BYTES '() '(10 20 30))
               '('bytes '(10 20 30))))

(define-opcode-functions ADC
  '(immediate zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y)
  '(#x69      #x65      #x75        #x6D     #x7D       #x79       #x61       #x71))

(module+ test #| ADC |#
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

  (check-match (ADC < "$FF" > ,y)
               '('opcode #x71 #xff))

  (check-match (ADC < "$FF" ,x >)
               '('opcode #x61 #xff)))

(define-opcode-functions AND
  '(indirect-x zero-page immediate absolute indirect-y zero-page-x absolute-y absolute-x)
  '(#x21       #x25      #x29      #x2d     #x31       #x35        #x39       #x3d))

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
(define-opcode-functions BEQ '(relative) '(#xf0))

(define-opcode-functions BIT
  '(zero-page absolute)
  '(#x24      #x2c))

(module+ test
  (check-match (BEQ "$FC")
               '('rel-opcode #xF0 #xfc))
  (check-match (BEQ ":some")
               '('rel-opcode #xF0 ":some")))

(define-opcode-functions BMI '(relative) '(#x30))
(define-opcode-functions BNE '(relative) '(#xd0))
(define-opcode-functions BPL '(relative) '(#x10))

(define-opcode-functions BRK '(implicit) '(#x00))

(define-opcode-functions BVC '(relative) '(#x50))
(define-opcode-functions BVS '(relative) '(#x70))

(define-opcode-functions CLC '(implicit) '(#x18))

(define-opcode-functions CLD '(implicit) '(#xd8))

(define-opcode-functions CLI '(implicit) '(#x58))

(define-opcode-functions CLV '(implicit) '(#xb8))

(define-opcode-functions CMP
  '(indirect-x zero-page immediate absolute indirect-y zero-page-x absolute-y absolute-x)
  '(#xc1       #xc5      #xc9      #xcd     #xd1       #xd5        #xd9       #xdd))

(define-opcode-functions CPX
  '(immediate zero-page absolute )
  '(#xe0      #xe4      #xec))

(define-opcode-functions CPY
  '(immediate zero-page absolute )
  '(#xc0      #xc4      #xcc))

(module+ test
  (check-match (BRK)
               '('opcode #x00)))

(define-opcode-functions DEC
  '(zero-page absolute absolute-x zero-page-x)
  '(#xc6      #xce     #xde       #xd6))

(define-opcode-functions DEX '(implicit) '(#xCA))

(define-opcode-functions DEY '(implicit) '(#x88))

(define-opcode-functions EOR
  '(indirect-x zero-page immediate absolute indirect-y zero-page-x absolute-y absolute-x)
  '(#x41       #x45      #x49      #x4d     #x51       #x55        #x59       #x5d))

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

(define-opcode-functions INX '(implicit) '(#xe8))

(define-opcode-functions INY '(implicit) '(#xc8))

(define-opcode-functions JMP
  '(absolute indirect)
  '(#x4C     #x6c))

(module+ test #| jmp |#
  (check-match (JMP < "$1011" >)
               '('opcode #x6c #x11 #x10))
  (check-match (JMP "$1011")
               '('opcode #x4c #x11 #x10)))

(define-opcode-functions JSR '(absolute) '(#x20))

(define-opcode-functions LDA
  '(immediate zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y)
  '(#xA9      #xA5      #xB5        #xAD     #xBD       #xB9       #xA1       #xB1))

(module+ test #| lda |#
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

  (check-match (LDA < "$A0" >,y )
               '('opcode #xB1 #xA0))

  (check-match (LDA < "$A0", x > )
               '('opcode #xA1 #xA0)))

(define-opcode-functions LDX
  '(immediate zero-page zero-page-y absolute absolute-y)
  '(#xA2      #xA6      #xB6        #xAE     #xBE))

(define-opcode-functions LDY
  '(immediate zero-page zero-page-x absolute absolute-x)
  '(#xA0      #xA4      #xB4        #xAC     #xBC))

(module+ test #| ldx |#
  (check-match (LDX "#$10")
               '('opcode #xA2 16)))

(define-opcode-functions LSR
  '(zero-page implicit absolute zero-page-x absolute-x)
  '(#x46      #x4a     #x4e     #x56        #x5e))

(define-opcode-functions NOP '(implicit) '(#xea))

(define-opcode-functions ORA
  '(indirect-x zero-page immediate absolute indirect-y zero-page-x absolute-y absolute-x )
  '(#x01       #x05      #x09      #x0d     #x11       #x15        #x19       #x1d))

(module+ test #| ora |#
    (check-match (ORA < "$10" ,x >)
                 '('opcode #x01 #x10))
    (check-match (ORA "$10")
                 '('opcode #x05 #x10))
    (check-match (ORA "#$10")
                 '('opcode #x09 #x10))
    (check-match (ORA "$1011")
                 '('opcode #x0d #x11 #x10))
    (check-match (ORA <"$10">,y)
                 '('opcode #x11 #x10))
    (check-match (ORA "$10",x)
                 '('opcode #x15 #x10))
    (check-match (ORA "$1011",y)
                 '('opcode #x19 #x11 #x10))
    (check-match (ORA "$1011",x)
                 '('opcode #x1d #x11 #x10)))

(define-opcode-functions PHA '(implicit) '(#x48))

(define-opcode-functions PHP '(implicit) '(#x08))

(define-opcode-functions PLA '(implicit) '(#x68))

(define-opcode-functions PLP '(implicit) '(#x28))

(define-opcode-functions ROL
  '(zero-page implicit absolute zero-page-x absolute-x)
  '(#x26      #x2a     #x2e     #x36        #x3e))

(define-opcode-functions ROR
  '(zero-page implicit absolute zero-page-x absolute-x)
  '(#x66      #x6a     #x6e     #x76        #x7e))

(define-opcode-functions RTI '(implicit) '(#x40))

(define-opcode-functions RTS '(implicit) '(#x60))

(define-opcode-functions SBC
  '(immediate zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y)
  '(#xe9      #xe5      #xf5        #xed     #xfd       #xf9       #xe1       #xf1))

(define-opcode-functions SEC '(implicit) '(#x38))

(define-opcode-functions SED '(implicit) '(#xf8))

(define-opcode-functions SEI '(implicit) '(#x78))

(define-opcode-functions STA
  '(zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y)
  '(#x85      #x95        #x8D     #x9D       #x99       #x81       #x91))

(module+ test
  (check-match (STA "$17")
               '('opcode #x85 23))

  (check-match (STA "$1728")
               '('opcode #x8d #x28 #x17))

  (check-match (STA < "$17" ,x >)
               '('opcode #x81 #x17))

  (check-match (STA < "$28" > y)
               '('opcode #x91 #x28))

  (check-match (STA < "$17" > ,y)
               '('opcode #x91 #x17))

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

(define-opcode-functions STX
  '(zero-page absolute zero-page-y)
  '(#x86      #x8e     #x96))

(define-opcode-functions STY
  '(zero-page absolute zero-page-x)
  '(#x84      #x8c     #x94))

(define-opcode-functions TAX '(implicit) '(#xaa))

(define-opcode-functions TAY '(implicit) '(#xa8))

(define-opcode-functions TSX '(implicit) '(#xba))

(define-opcode-functions TXA '(implicit) '(#x8a))

(define-opcode-functions TXS '(implicit) '(#x9a))

(define-opcode-functions TYA '(implicit) '(#x98))

;; -------------------------------------------------------------------------------- whole program functions

; (run (assembler-program (initialize-cpu) 0 (list (LDA_i #x41) (JSR_abs #xFFFF) (BRK))))

(define (create-prg program org file-name)
  (display-to-file (list->bytes (append `(,(low-byte org) ,(high-byte org)) program))
                   file-name
                   #:mode 'binary
                   #:exists 'replace))

(define (run-emulator file-name)
  (system (string-append "LD_LIBRARY_PATH=  x64sc -8 " file-name)))

(define (create-d64 name)
  (system (string-append "c1541 -format \"" name ",01\" d64 " name)))

(define (add-prg-to-d64 prg-file-name d64 target-name)
  (system (string-append "c1541 -attach " d64 " -write " prg-file-name " " target-name)))

(define (create-image-with-program program org file-name d64 target-name)
  (create-prg program org file-name)
  (create-d64 d64)
  (add-prg-to-d64 file-name d64 target-name))

;; -------------------------------------------------- pretty print

(define (hex-format-any a-number-str)
  (let ([parsed-number (parse-number-string a-number-str)])
    (if (> parsed-number 255)
        (string-append "$" (word->hex-string parsed-number) )
        (string-append "$" (byte->hex-string parsed-number)))))

(define (format-raw-line slst)
  (string-join (map (lambda (element)
                      (cond [(symbol? element) (symbol->string element)]
                            [(6510-number-string? element) (hex-format-any element)]
                            [(is-immediate-number? element) (string-append "#" (hex-format-any (substring element 1)))]
                            [else  element])) slst) " "))


(define (pretty-print-line line)
  (let* ([opcodes (drop-meta-info (first line))]
         [syntax (drop-meta-info (last line))]
         [compiled
          (case (first opcodes)
            [('opcode 'rel-opcode)
             (~a  (string-join (map byte->hex-string (drop opcodes 1)) " ")
                  #:min-width 12)]
            [('label) (last opcodes)]
            [('bytes) (~a (string-join  (map byte->hex-string (last opcodes)) " "))]
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

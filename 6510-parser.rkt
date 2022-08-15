#lang racket
(require syntax/strip-context)
(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)
(require "6510.rkt")
(require "6510-interpreter.rkt")
;; (require "6510-debugger.rkt")
(require (rename-in  racket/contract [define/contract define/c]))

; usage:
; - create file with content
;     #lang reader "6510-reader.rkt"
;     *=$C000
;     lda #$40
; - C-c to open repl for this source file
; - follow the hints printed

; naming convention:
;   .../p is or returns a parser

(module+ test
  (require rackunit)
    (begin-for-syntax
    (require rackunit))

  (define (parsed-string-result syntax string)
    (syntax->datum (parse-result! (parse-string (syntax/p syntax) string)))))

(provide compile-opcode 6510-program/p list->values)

(define space-or-tab/p
  (or/p (char/p #\space) (char/p #\tab)))

;; is the given char a hexadecimal number digit?
(define/c (char-hex? char)
  (-> char? boolean?)
  (let ([char-int (char->integer char)])
    (or (and (>= char-int 48) (<= char-int 57))
       (and (>= char-int 65) (<= char-int 70))
       (and (>= char-int 97) (<= char-int 102)))))

(define hex-digit/p
  (label/p "hex-number" (satisfy/p char-hex?)))

(define hex-string/p
  (do [digits <- (many+/p hex-digit/p)]
      (pure (list->string digits))))

(define ml-whitespace/p
  (many/p (or/p (char/p #\newline)
                space-or-tab/p
                (do (char/p #\;)
                    (many/p (char-not/p #\newline))))))

(define hex-integer/p
  (do (char/p #\$)
      [x <-  hex-string/p]
    (pure (parse-number-string (string-append "$" x)))))

;; is the given char a binary number digit?
(define/c (char-bin? char)
  (-> char? boolean?)
  (or (eq? char #\0)
     (eq? char #\1)))

(define bin-digit/p
  (label/p "bin-number" (satisfy/p char-bin?)))

(define bin-string/p
  (do [digits <- (many+/p bin-digit/p)]
      (pure (list->string digits))))

(define bin-integer/p
  (do (char/p #\%)
      [x <- bin-string/p]
    (pure (parse-number-string (string-append "%" x)))))

(module+ test
  (check-true (char-bin? #\1))

  (check-match (parsed-string-result bin-string/p "10")
               "10")

  (check-match (parsed-string-result bin-integer/p "%10")
               2))

(define 6510-integer/p
  (or/p integer/p hex-integer/p bin-integer/p))

(module+ test
  (check-match (parsed-string-result 6510-integer/p "%10")
               2)

  (check-match (parsed-string-result 6510-integer/p "$10")
               16)

  (check-match (parsed-string-result 6510-integer/p "10")
               10)

  (check-match (parsed-string-result 6510-integer/p "10ab") ;; ab is not parsed/yet
               10)

  (check-exn exn:fail?
             (lambda () (parsed-string-result 6510-integer/p "-10")))

  (check-exn exn:fail?
             (lambda () (parsed-string-result 6510-integer/p "A0")))

  (check-exn exn:fail?
             (lambda () (parsed-string-result 6510-integer/p "_17"))))

(define 6510-eol/p
  (do (many/p space-or-tab/p)
      (or/p (do  (char/p #\;)
                (many/p (char-not/p #\newline)))
            void/p)
    (or/p (char/p #\newline)
          eof/p)))

(define 6510-label-letter/p
  (satisfy/p (lambda (c) (or (and (char>=? c #\A)
                          (char<=? c #\Z))
                       (and (char>=? c #\a)
                          (char<=? c #\z))
                       (char=? c #\_)))))

(define 6510-label/p
  (do
      [colon <- (syntax/p (char/p #\:))]
      [first-letter <- (syntax/p 6510-label-letter/p)]
    [new-label <- (syntax/p (many+/p (or/p 6510-label-letter/p digit/p)))]
    (pure (datum->syntax new-label
                        (list (string->symbol "LABEL")
                              '("<label>")
                              (string-append ":"
                                             (string (syntax->datum first-letter))
                                             (list->string (syntax->datum new-label))))
                        colon))))

(define 6510-label-byte/p
  (do
      [colon <- (syntax/p (char/p #\:))]
      [first-letter <- (syntax/p 6510-label-letter/p)]
    [new-label <- (syntax/p (many+/p (or/p 6510-label-letter/p digit/p)))]
    (char/p #\-)
    [hl-indicator <- (syntax/p (or/p (char-ci/p #\H) (char-ci/p #\L)))]
    (pure (datum->syntax new-label
                        (list (string->symbol "LABEL")
                              '("<label>")
                              (string-append ":"
                                             (string (syntax->datum first-letter))
                                             (list->string (syntax->datum new-label))
                                             "-"
                                             (string-upcase (string (syntax->datum hl-indicator)))))
                        colon))))

(module+ test
  (check-match (parsed-string-result 6510-label/p ":abc")
               '(LABEL ("<label>") ":abc"))

  (check-match (parsed-string-result 6510-label-byte/p ":abc-h")
               '(LABEL ("<label>") ":abc-H"))

  (check-match (parsed-string-result 6510-label-byte/p ":abc-L")
               '(LABEL ("<label>") ":abc-L"))

  (check-match (parsed-string-result 6510-label/p ":abc-x")
               '(LABEL ("<label>") ":abc"))

  (check-match (parsed-string-result 6510-label/p ":abc12")
               '(LABEL ("<label>") ":abc12"))

  (check-match (parsed-string-result 6510-label/p ":ab1c2")
               '(LABEL ("<label>")  ":ab1c2"))

  (check-match (parsed-string-result 6510-label/p ":_aB1c2")
               '(LABEL ("<label>")  ":_aB1c2"))

  (check-exn exn:fail?
             (lambda () (parsed-string-result 6510-label-byte/p ":abc-x")))

  (check-exn exn:fail?
             (lambda () (parsed-string-result 6510-label/p "abc"))))


(define word/p
  (guard/p 6510-integer/p (λ (x) (and (>= x 0) (<= x 65535)))
           "integer in range [$0000,$FFFF]"))

(define byte/p
  (guard/p 6510-integer/p (λ (x) (and (>= x 0) (<= x 255)))
           "integer in range [$00,$FF]"))

;; return a parser that will parse the given opcode with absolute addressing
(define/c (abs-opcode/p opcode)
  (-> string? parser?)
  (do
      (try/p (string-cia/p opcode))
      (many/p space-or-tab/p)
    [x <- (or/p word/p
               6510-label/p)]  ;; could be a string too
    6510-eol/p
    (pure (append (list (string->symbol (string-upcase opcode)))
                  (if (number? x)
                      (list (number->string x))
                      (list (last (syntax->datum x))))))))

(module+ test
  (check-match (parsed-string-result (abs-opcode/p "JMP") "JMP $1000")
               '(JMP "4096"))

  (check-match (parsed-string-result (abs-opcode/p "JSR") "JSR $1000")
               '(JSR "4096"))

  (check-match (parsed-string-result (abs-opcode/p "JSR") "JSR %0001000000000000")
               '(JSR "4096"))

  (check-match (parsed-string-result (abs-opcode/p "ADC") "ADC 4096")
               '(ADC "4096"))

  (check-match (parsed-string-result (abs-opcode/p "LDA") "LDA :some")
               '(LDA ":some"))

  (check-exn exn:fail?
             (lambda () (parsed-string-result (abs-opcode/p "JSR") "JSR $10000"))))

;; return a parser that will parse the given opcode with relative addressing (e.g. branches)
(define/c (rel-opcode/p opcode)
  (-> string? parser?)
  (do
      (try/p (string-cia/p opcode))
      (many/p space-or-tab/p)
    [x <- (or/p byte/p
               6510-label/p)]
    6510-eol/p
    (pure (append (list (string->symbol (string-upcase opcode)))
                  (if (number? x)
                      (list (number->string x))
                      (list (last (syntax->datum x))))))))

;; return parser to match the given string or fail
(define/c (chars-ci/p str)
  (-> (or/c string? char?) parser?)
  (if (zero? (string-length str))
      (pure "")
      (label/p str (do (char-ci/p (string-ref str 0))
                       (string-cia/p (substring str 1))
                     (pure str)))))

(module+ test
  (check-match (parsed-string-result (chars-ci/p "abc") "abc")
               "abc")
  (check-match (parsed-string-result (chars-ci/p "abc") "abcd")
               "abc")
  (check-match (parsed-string-result (chars-ci/p "") "")
               "")
  (check-match (parsed-string-result (chars-ci/p "") "a")
               "")  
  (check-exn exn:fail?
             (lambda () (parsed-string-result (chars-ci/p "abc") "  abc  d")))
  (check-exn exn:fail?
             (lambda () (parsed-string-result (chars-ci/p "abc") "dabc"))))

;; return parser to match the given string or fail
(define/c (string-cia/p string)
  (-> string? parser?)
  (chars-ci/p string))

(define accumulator/p (do (char-ci/p #\A) (pure '(A))))
(define immediate/p
  (do (char/p #\#)
      [x <- byte/p]
    (pure (list (string-append "#" (number->string x))))))
(define zero-page-or-relative/p
  (do
      [b <- (or/p byte/p 6510-label-byte/p)]
      (pure (list (if (number? b) (number->string b) (last (syntax->datum b)))))))
(define absolute/p (do [mem <- (or/p 6510-label/p word/p)]
                       (pure (list (if (number? mem) (number->string mem) (last (syntax->datum mem)))))))
(define indirect-x/p (do (char/p #\() [mem <- (or/p 6510-label/p byte/p)] (string-cia/p ",x") (char/p #\))
                         (pure `(< ,(if (number? mem) (number->string mem) (last mem)) x >))))
(define indirect-y/p (do (char/p #\() [mem <- (or/p 6510-label/p byte/p)] (char/p #\)) (string-cia/p ",y")  (pure `(< ,(number->string mem) > y))))
(define indirect/p (do (char/p #\() [mem <- (or/p 6510-label/p word/p)] (char/p #\)) (pure `(< ,(if (number? mem) (number->string mem) (last (syntax->datum mem))) >))))
(define absolute-x/p (do [x <- word/p] (string-cia/p ",x") (pure (list (number->string x) 'x))))
(define absolute-y/p (do [x <- word/p] (string-cia/p ",y") (pure (list (number->string x) 'y))))
(define zero-page-x/p (do [x <- byte/p] (string-cia/p ",x") (pure (list (number->string x) 'x))))
(define zero-page-y/p (do [x <- byte/p] (string-cia/p ",y") (pure (list (number->string x) 'y))))

(module+ test #| indirect/p indirect-x/p absolute/p |#
  (check-match (parsed-string-result immediate/p "#$10")
               '("#16"))
  (check-match (parsed-string-result zero-page-or-relative/p ":some-l")
               '(":some-L"))
  (check-match (parsed-string-result zero-page-or-relative/p "$11")
               '("17"))
  (check-match (parsed-string-result indirect/p "($1000)")
               '(< "4096" >))
  (check-match (parsed-string-result indirect-x/p "($10,x)")
               '(< "16" x >))
  (check-match (parsed-string-result indirect-y/p "($10),y")
               '(< "16" > y))
  (check-match (parsed-string-result absolute/p "$1000")
               '("4096"))
  (check-match (parsed-string-result absolute/p ":out")
               '(":out")))

;; transform opcode string to a list w/ capitalized opcode as symbol
(define/c (opcode->list4pure opcode)
  (-> string? (listof symbol?))
  (list (string->symbol (string-upcase opcode))))

(module+ test #| opcode->list4pure |#
  (check-equal? (opcode->list4pure "jmp")
                '(JMP)))

;; return a parser that will parse the given opcode combined with the available addressing modes
(define/c (adr-modes-opcode/p opcode adr-mode-list)
  (-> string? (listof symbol?) parser?)
  (do
      [actual-opcode <- (syntax/p (try/p (string-cia/p opcode)))]

      ;; order is relevant (for parser to get the max matching string per line
      [res <- (or/p (try/p (guard/p (do (many/p space-or-tab/p) [a-res <- (syntax/p accumulator/p)]
                                     (pure (datum->syntax a-res (append (opcode->list4pure opcode)
                                                                  (list (list '#:line (syntax-line a-res)
                                                                              '#:org-cmd (string-append opcode " " (string-downcase (symbol->string (first (syntax->datum a-res)))))))
                                                                  `,(syntax->datum a-res)) actual-opcode)))
                                   (lambda (x) (member 'accumulator adr-mode-list)) "no accumlator"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [i-res <- (syntax/p immediate/p)]
                                     (pure (datum->syntax i-res (append (opcode->list4pure opcode)
                                                                 (list (list '#:line (syntax-line i-res)
                                                                             '#:org-cmd (string-append opcode " " (first (syntax->datum i-res)))))
                                                                 (syntax->datum i-res)) actual-opcode)))
                                   (lambda (x) (member 'immediate adr-mode-list)) "no immediate"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [indx-res <- (syntax/p indirect-x/p)]
                                     (pure (datum->syntax indx-res (append (opcode->list4pure opcode)
                                                                 (list (list '#:line (syntax-line indx-res)
                                                                             '#:org-cmd (string-append opcode " (" (second (syntax->datum indx-res)) ",x)")))
                                                                 (syntax->datum indx-res)) actual-opcode)))
                                   (lambda (x) (member 'indirect-x adr-mode-list)) "no indirect, x"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [indy-res <- (syntax/p indirect-y/p)]
                                     (pure (datum->syntax indy-res (append (opcode->list4pure opcode)
                                                                 (list (list '#:line (syntax-line indy-res)
                                                                             '#:org-cmd (string-append opcode " (" (second (syntax->datum indy-res)) "),y")))
                                                                 (syntax->datum indy-res)) actual-opcode)))
                                   (lambda (x) (member 'indirect-y adr-mode-list)) "no indirect, y"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [ind-res <- (syntax/p indirect/p)]
                                     (pure (datum->syntax ind-res (append (opcode->list4pure opcode)
                                                                 (list (list '#:line (syntax-line ind-res)
                                                                             '#:org-cmd (string-append opcode " (" (second (syntax->datum ind-res)) ")")))
                                                                 (syntax->datum ind-res)) actual-opcode)))
                                   (lambda (x) (member 'indirect adr-mode-list)) "no indirect"))  
                   (try/p (guard/p (do (many/p space-or-tab/p) [absx-res <- (syntax/p absolute-x/p)]
                                     (pure (datum->syntax absx-res (append (opcode->list4pure opcode)
                                                                 (list (list '#:line (syntax-line absx-res)
                                                                             '#:org-cmd (string-append opcode " " (first (syntax->datum absx-res)) ",x")))
                                                                 (syntax->datum absx-res)) actual-opcode)))
                                   (lambda (x) (member 'absolute-x adr-mode-list)) "no absolute, x"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [absy-res <- (syntax/p absolute-y/p)]
                                     (pure (datum->syntax absy-res (append (opcode->list4pure opcode)
                                                                 (list (list '#:line (syntax-line absy-res)
                                                                             '#:org-cmd (string-append opcode " " (first (syntax->datum absy-res)) ",y")))
                                                                 (syntax->datum absy-res)) actual-opcode)))
                                   (lambda (x) (member 'absolute-y adr-mode-list)) "no absolute, y"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [zpx-res <- (syntax/p zero-page-x/p)]
                                     (pure (datum->syntax zpx-res (append (opcode->list4pure opcode)
                                                                 (list (list '#:line (syntax-line zpx-res)
                                                                             '#:org-cmd (string-append opcode " " (first (syntax->datum zpx-res)) ",x")))
                                                                 (syntax->datum zpx-res)) actual-opcode)))
                                   (lambda (x) (member 'zero-page-x adr-mode-list)) "no zero page, x"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [zpy-res <- (syntax/p zero-page-y/p)]
                                     (pure (datum->syntax zpy-res (append (opcode->list4pure opcode)
                                                                 (list (list '#:line (syntax-line zpy-res)
                                                                             '#:org-cmd (string-append opcode " " (first (syntax->datum zpy-res)) ",y")))
                                                                 (syntax->datum zpy-res)) actual-opcode)))
                                   (lambda (x) (member 'zero-page-y adr-mode-list)) "no zero page, y"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [zp-res <- (syntax/p zero-page-or-relative/p)]
                                     (pure (datum->syntax zp-res (append (opcode->list4pure opcode)
                                                                 (list (list '#:line (syntax-line zp-res)
                                                                             '#:org-cmd (string-append opcode " " (first (syntax->datum zp-res)))))
                                                                 (syntax->datum zp-res)) actual-opcode)))
                                   (lambda (x) (or (member 'zero-page adr-mode-list) (member 'relative adr-mode-list))) "no zero page nor relative"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [abs-res <- (syntax/p absolute/p)]
                                     (pure (datum->syntax abs-res (append (opcode->list4pure opcode)
                                                                 (list (list '#:line (syntax-line abs-res)
                                                                             '#:org-cmd (string-append opcode " " (first (syntax->datum abs-res)))))
                                                                 (syntax->datum abs-res)) actual-opcode)))
                                   (lambda (x) (member 'absolute adr-mode-list)) "no absolute"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [label-res <- (syntax/p 6510-label/p)]
                                     (pure (datum->syntax label-res (append (opcode->list4pure opcode)
                                                                           (list (last (syntax->datum label-res)))) label-res)))
                                   (lambda (x) (or (member 'relative adr-mode-list) (member 'absolute adr-mode-list))) "no relative label"))
                   (try/p (guard/p (do void/p
                                       (pure (datum->syntax actual-opcode (opcode->list4pure opcode) actual-opcode)))
                                   (lambda (x) (member 'implicit adr-mode-list) ) "no implicit"))
                   )]
    (pure res)))

;; parser for ".data (byte/p (","|ml_whitespace/p) ...)
(define data-bytes/p
  (do
      (try/p (string-cia/p ".data"))
      (many/p space-or-tab/p)
    [result <- (many/p byte/p #:sep (do (char/p #\,) ml-whitespace/p))]
    (pure (list 'BYTES '("bytes") result))))

(module+ test #| data-bytes/p |#
  (check-match (parsed-string-result data-bytes/p ".data    2,3,4")
               '(BYTES ("bytes") (2 3 4)))
  (check-match (parsed-string-result data-bytes/p ".data $20,	%10,  4")
               '(BYTES ("bytes") (32 2 4))))

(module+ test
  (check-match (parsed-string-result (adr-modes-opcode/p "jmp" '(indirect)) "jmp ($1000)")
               '(JMP (#:line 1 #:org-cmd "jmp (4096)") < "4096" >))
  (check-match (parsed-string-result (adr-modes-opcode/p "jmp" '(absolute)) "jmp $1000")
               '(JMP (#:line 1 #:org-cmd "jmp 4096")  "4096" )))

;; parser for valid assembler lines (including bytes and labels)
(define 6510-opcode/p
  (do (or/p
       (adr-modes-opcode/p "adc" '(immediate zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y))
       (adr-modes-opcode/p "and" '(immediate zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y))
       (adr-modes-opcode/p "asl" '(accumulator zero-page zero-page-x absolute-x absolute))
       (adr-modes-opcode/p "bcc" '(relative))
       (adr-modes-opcode/p "bcs" '(relative))
       (adr-modes-opcode/p "beq" '(relative))
       (adr-modes-opcode/p "bit" '(zero-page absolute))
       (adr-modes-opcode/p "bmi" '(relative))
       (adr-modes-opcode/p "bne" '(relative))
       (adr-modes-opcode/p "bpl" '(relative))
       (adr-modes-opcode/p "brk" '(implicit))
       (adr-modes-opcode/p "bvc" '(relative))
       (adr-modes-opcode/p "bvs" '(relative))
       (adr-modes-opcode/p "clc" '(implicit))
       (adr-modes-opcode/p "cld" '(implicit))
       (adr-modes-opcode/p "cli" '(implicit))
       (adr-modes-opcode/p "clv" '(implicit))
       (adr-modes-opcode/p "cmp" '(immediate zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y))
       (adr-modes-opcode/p "cpx" '(immediate zero-page absolute))
       (adr-modes-opcode/p "cpy" '(immediate zero-page absolute))
       (adr-modes-opcode/p "dec" '(zero-page zero-page-x absolute absolute-x))
       (adr-modes-opcode/p "dex" '(implicit))
       (adr-modes-opcode/p "dey" '(implicit))
       (adr-modes-opcode/p "eor" '(immediate zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y))
       (adr-modes-opcode/p "inc" '(zero-page zero-page-x absolute absolute-x))
       (adr-modes-opcode/p "inx" '(implicit))
       (adr-modes-opcode/p "iny" '(implicit))
       (adr-modes-opcode/p "jmp" '(absolute indirect))
       (adr-modes-opcode/p "jsr" '(absolute))
       (adr-modes-opcode/p "lda" '(immediate zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y))
       (adr-modes-opcode/p "ldx" '(immediate zero-page zero-page-y absolute absolute-y))
       (adr-modes-opcode/p "ldy" '(immediate zero-page zero-page-x absolute absolute-x))
       (adr-modes-opcode/p "lsr" '(zero-page implicit absolute zero-page-x absolute-x))
       (adr-modes-opcode/p "nop" '(implicit))
       (adr-modes-opcode/p "ora" '(immediate zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y))
       (adr-modes-opcode/p "pha" '(implicit))
       (adr-modes-opcode/p "php" '(implicit))
       (adr-modes-opcode/p "pla" '(implicit))
       (adr-modes-opcode/p "plp" '(implicit))
       (adr-modes-opcode/p "rol" '(zero-page implicit absolute zero-page-x absolute-x))
       (adr-modes-opcode/p "ror" '(zero-page implicit absolute zero-page-x absolute-x))
       (adr-modes-opcode/p "rti" '(implicit))
       (adr-modes-opcode/p "rts" '(implicit))
       (adr-modes-opcode/p "sbc" '(immediate zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y))
       (adr-modes-opcode/p "sec" '(implicit))
       (adr-modes-opcode/p "sed" '(implicit))
       (adr-modes-opcode/p "sei" '(implicit))
       (adr-modes-opcode/p "sta" '(zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y))
       (adr-modes-opcode/p "stx" '(zero-page absolute zero-page-y))
       (adr-modes-opcode/p "sty" '(zero-page absolute zero-page-x))
       (adr-modes-opcode/p "tax" '(implicit))
       (adr-modes-opcode/p "tay" '(implicit))
       (adr-modes-opcode/p "tsx" '(implicit))
       (adr-modes-opcode/p "txa" '(implicit))
       (adr-modes-opcode/p "txs" '(implicit))
       (adr-modes-opcode/p "tya" '(implicit))
       data-bytes/p
       6510-label/p)))

;; parser for origin definition
(define 6510-program-origin/p
  (do (char/p #\*) (many/p space-or-tab/p)
    (char/p #\=) (many/p space-or-tab/p)
    [origin <- 6510-integer/p]
    6510-eol/p
    (pure origin)))

;; parser for a complete assembler program
(define 6510-program/p
  (do ml-whitespace/p
      [origin <- 6510-program-origin/p]
    ml-whitespace/p
    [opcodes <- (many/p (do [op <- 6510-opcode/p] ml-whitespace/p (pure op)))]
    eof/p
    (pure (list origin opcodes))))

(define 6510-opcodes/p
  (do [opcodes <- (many/p (do [op <- 6510-opcode/p] ml-whitespace/p (pure op)))]
      (pure opcodes)))


(define-namespace-anchor eval-ns-anchor)
(define eval-ns (namespace-anchor->namespace eval-ns-anchor))

;; compile one opcode to a list of bytes
(define (compile-opcode str)
  (define parsed (syntax->datum (parse-result! (parse-string (syntax/p 6510-opcode/p) str))))
  (define stripped-src-location-data (filter (lambda (el) (not (and (list? el) (equal? (car el) '#:line)))) parsed))
  (commands->bytes 0 (list (eval stripped-src-location-data eval-ns))))

(module+ test #| compile-opcode |#
  (check-equal? (JSR "$2000")
                '('opcode 32 0 32))
  (check-equal? (compile-opcode "JSR $FFD2")
                '(32 210 255))
  (check-equal? (compile-opcode ".data $FF, $10")
                '(255 16)))

;; apply the method 'values' to all list elements
(define (list->values list)
  (apply values list))

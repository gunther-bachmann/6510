#lang racket
(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)
(require (rename-in racket/contract [define/contract define/c]))

(require (only-in "6510-utils.rkt" parse-number-string ->string))
(require "6510-alt.rkt") ;; necessary to resolve all syntax macros of 6510 dsl

(provide 6510-program/p parse-opcodes)

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

;; considered whitespace (space, newline, tab, all chars behind ';')
(define ml-whitespace/p
  (many/p (or/p (char/p #\newline)
                space-or-tab/p
                (do (char/p #\;)
                    (many/p (char-not/p #\newline))))))

(define hex-integer/p
  (do (char/p #\$)
      [x <-  hex-string/p]
    (pure (parse-number-string (string-append "$" (->string x))))))

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
    (pure (parse-number-string (string-append "%" (->string x))))))

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

(define 6510-label-byte-ind/p
  (or/p (char/p #\>) (char/p #\<)))

(define 6510-label/p
  (do      
      [first-letter <- (syntax/p 6510-label-letter/p)]
    [new-label <- (syntax/p (many+/p (or/p 6510-label-letter/p digit/p (char/p #\-))))]
    (pure (datum->syntax new-label
                        (list (string->symbol "label")
                              (string-append (->string first-letter)
                                             (list->string (syntax->datum new-label))))))))


(define 6510-label-def/p
  (do
      [result <- (syntax/p 6510-label/p)]
      (char/p #\:)
    (pure (datum->syntax result result))))

(module+ test #| label-def |#
  (check-match (parsed-string-result 6510-label-def/p "abc-x:")
               '(label "abc-x")))

(define 6510-label-byte/p
  (do
      [lowhigh <- (syntax/p 6510-label-byte-ind/p)]
      [first-letter <- (syntax/p 6510-label-letter/p)]
    [new-label <- (syntax/p (many+/p (or/p 6510-label-letter/p digit/p (char/p #\-))))]
    (pure (datum->syntax new-label
                        (list (string->symbol "label")
                              (string-append (string (syntax->datum lowhigh))
                                             (string (syntax->datum first-letter))
                                             (list->string (syntax->datum new-label))
                                             ))))))

(define 6510-any-label/p
  (do
      [label <- (or/p (syntax/p 6510-label/p)
                    (syntax/p 6510-label-byte/p))]
      (pure label)))

(module+ test #| label-def |#
  (check-match (parsed-string-result 6510-label/p "abc-x")
               '(label "abc-x"))
  (check-match (parsed-string-result 6510-any-label/p "<abc-x")
               '(label "<abc-x")))

(module+ test
  (check-match (parsed-string-result 6510-label/p "abc")
               '(label "abc"))

  ;; (check-match (parsed-string-result 6510-label-byte/p ">abc")
  ;;              '(label ">abc"))

  ;; (check-match (parsed-string-result 6510-label-byte/p "<abc")
  ;;              '(label "<abc"))

  (check-match (parsed-string-result 6510-label/p "abc-x")
               '(label "abc-x"))

  (check-match (parsed-string-result 6510-label/p "abc12")
               '(label "abc12"))

  (check-match (parsed-string-result 6510-label/p "ab1c2")
               '(label "ab1c2"))

  (check-match (parsed-string-result 6510-label/p "_aB1c2")
               '(label "_aB1c2"))

  (check-exn exn:fail?
             (lambda () (parsed-string-result 6510-label-byte/p ":abc-x")))

  (check-exn exn:fail?
             (lambda () (parsed-string-result 6510-label/p "!bc"))))

(define word/p
  (guard/p 6510-integer/p (λ (x) (and (>= x 0) (<= x 65535)))
           "integer in range [$0000,$FFFF]"))

(define byte/p
  (guard/p 6510-integer/p (λ (x) (and (>= x 0) (<= x 255)))
           "integer in range [$00,$FF]"))

;; return parser to match the given string (case insensitive) or fail
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

(define accumulator/p
  (do (char-ci/p #\A) (pure '(A))))

(define immediate/p
  (do (char/p #\#)
      [x <- (or/p 6510-any-label/p byte/p)]
    (pure (list (string-append "!" (if (number? x) (number->string x) (->string (last (syntax->datum x)))))))))

(define zero-page-or-relative/p
  (do [b <- (or/p byte/p 6510-any-label/p)]
      (pure (list (if (number? b) (number->string b) (last (syntax->datum b)))))))

(define absolute/p
  (do [mem <- (or/p 6510-any-label/p word/p)]
      (pure (list (if (number? mem) (number->string mem) (last (syntax->datum mem)))))))

(define indirect-x/p
  (do (char/p #\()
      [mem <- (or/p 6510-any-label/p byte/p)]
    (string-cia/p ",x")
    (char/p #\))                       
    (pure (list (list (if (number? mem) (number->string mem) (last (syntax->datum mem))) ',x )))))

(module+ test #| indirect-x |#
    (check-match (parsed-string-result indirect-x/p "($11,x)")
                 '(("17",x))))

(define indirect-y/p
  (do 
      (char/p #\()
      [mem <- (or/p 6510-any-label/p byte/p)]
    (char/p #\))
    (string-cia/p ",y")
    (pure (list (list (if (number? mem) (number->string mem) (last (syntax->datum mem)))) ',y))))

(define indirect/p
  (do (char/p #\()
      [mem <- (or/p 6510-any-label/p word/p)]
    (char/p #\))
    (pure `(( ,(if (number? mem) (number->string mem) (last (syntax->datum mem))) )))))

(define absolute-x/p
  (do [x <- (or/p word/p 6510-any-label/p)]
      (string-cia/p ",x")
    (pure (list (if (number? x) (number->string x) (last (syntax->datum x))) ',x))))

(define absolute-y/p
  (do [x <- (or/p word/p 6510-any-label/p)]
      (string-cia/p ",y")
    (pure (list (if (number? x) (number->string x) (last (syntax->datum x))) ',y))))

(define zero-page-x/p
  (do [x <- (or/p byte/p 6510-any-label/p)]
      (string-cia/p ",x")
    (pure (list (if (number? x) (number->string x) (last (syntax->datum x))) ',x))))

(define zero-page-y/p
  (do [x <- (or/p byte/p 6510-any-label/p)]
      (string-cia/p ",y")
    (pure (list (if (number? x) (number->string x) (last (syntax->datum x))) ',y))))

(module+ test #| indirect/p indirect-x/p absolute/p |#
  (check-match (parsed-string-result immediate/p "#$10")
               '("!16"))
  (check-match (parsed-string-result zero-page-or-relative/p "<some")
               '("<some"))
  (check-match (parsed-string-result zero-page-or-relative/p "$11")
               '("17"))
  (check-match (parsed-string-result indirect/p "($1000)")
               '( ("4096") ))
  (check-match (parsed-string-result indirect-x/p "($10,x)")
               '( ("16",x) ))
  (check-match (parsed-string-result indirect-x/p "(some,x)")
               '( ("some",x) ))
  (check-match (parsed-string-result indirect-y/p "($10),y")
               '( ("16"),y))
  (check-match (parsed-string-result absolute/p "$1000")
               '("4096"))
  (check-match (parsed-string-result absolute/p "hello")
               '("hello")))

;; transform opcode string to a list w/ capitalized opcode as symbol
(define/c (opcode->list4pure opcode)
  (-> string? (listof symbol?))
  (list (string->symbol (string-upcase opcode))))

(module+ test #| opcode->list4pure |#
  (check-equal? (opcode->list4pure "jmp")
                '(JMP)))

(define/c (args->string opcode-args-stx)
  (-> syntax? string?)
  (let ((args (syntax->datum opcode-args-stx)))
    (cond [(list? args)
           (string-join (map ->string args) "")]
          [#t (->string args)])))

(module+ test #| args->string |#
  (check-equal? (args->string #'( ("16"),y))
                "(16),y")
  (check-equal? (args->string #'(label $2000 ,x))
                "label$2000,x"))

(define/c (construct-ref-meta-info opcode-stx operands-stx)
  (-> syntax? syntax? list?)
   (list '#:line (syntax-line operands-stx)
         '#:org-cmd (format "~a ~a" (syntax->datum opcode-stx) (args->string operands-stx))))

(define (result->syntax operands-stx actual-opcode-stx)
  (datum->syntax actual-opcode-stx
                (append (opcode->list4pure (syntax->datum actual-opcode-stx))
                        (list (construct-ref-meta-info actual-opcode-stx operands-stx))
                        (syntax->datum operands-stx))
                actual-opcode-stx))

(define (opcode-addressing/p actual-opcode addressing adr-mode-list rule/p)
  (let ((addressings (flatten (list addressing))))
        (try/p (guard/p (do (many/p space-or-tab/p) [a-res <- (syntax/p rule/p)]
                          (pure (result->syntax a-res actual-opcode)))
                        (lambda (_) (ormap (λ (addr) (member addr adr-mode-list)) addressings))
                        (format "addressing ~a not applicable" addressing)))))

;; return a parser that will parse the given opcode combined with the available addressing modes
(define/c (adr-modes-opcode/p opcode adr-mode-list)
  (-> string? (listof symbol?) parser?)
  (do
      [actual-opcode <- (syntax/p (try/p (string-cia/p opcode)))]

      ;; order is relevant (for parser to get the max matching string per line
      [res <- (or/p (opcode-addressing/p actual-opcode 'accumulator adr-mode-list accumulator/p)
                   (opcode-addressing/p actual-opcode 'immediate adr-mode-list immediate/p)
                   (opcode-addressing/p actual-opcode 'indirect-x adr-mode-list indirect-x/p)
                   (opcode-addressing/p actual-opcode 'indirect-y adr-mode-list indirect-y/p)
                   (opcode-addressing/p actual-opcode 'indirect adr-mode-list indirect/p)
                   (opcode-addressing/p actual-opcode 'absolute-x adr-mode-list absolute-x/p)
                   (opcode-addressing/p actual-opcode 'absolute-y adr-mode-list absolute-y/p)
                   (opcode-addressing/p actual-opcode 'zero-page-x adr-mode-list zero-page-x/p)
                   (opcode-addressing/p actual-opcode 'zero-page-y adr-mode-list zero-page-y/p)
                   (opcode-addressing/p actual-opcode '(zero-page relative) adr-mode-list zero-page-or-relative/p)
                   (opcode-addressing/p actual-opcode 'absolute adr-mode-list absolute/p)
                   (try/p (guard/p (do (many/p space-or-tab/p) [label-res <- (syntax/p 6510-label/p)]
                                     (pure (datum->syntax label-res (append (opcode->list4pure opcode)
                                                                           (list (last (syntax->datum label-res)))) label-res)))
                                   (lambda (x) (or (member 'relative adr-mode-list) (member 'absolute adr-mode-list)))
                                   "no relative label"))
                   (try/p (guard/p (do void/p
                                       (pure (datum->syntax actual-opcode (opcode->list4pure opcode) actual-opcode)))
                                   (lambda (x) (member 'implicit adr-mode-list) )
                                   "no implicit"))
                   )]
    (pure res)))

;; parser for '.asc "some string"'
(define asc-string/p
  (do
      (try/p (string-cia/p ".asc"))
      (many/p space-or-tab/p)
    (char/p #\")
    [result <- (many/p (char-not/p #\"))]
    (char/p #\")
    (pure (list 'asc (list->string result)))
    ))

(module+ test #| asc-string/p |#
  (check-match (parsed-string-result asc-string/p ".asc  \"some\"")
               '(asc "some"))
  (check-match (parsed-string-result asc-string/p ".asc  \"some 'other'\"")
               '(asc "some 'other'")))

;; parser for ".data (byte/p (","|ml_whitespace/p) ...)
(define data-bytes/p
  (do
      (try/p (string-cia/p ".data"))
      (many/p space-or-tab/p)
    [result <- (many/p byte/p #:sep (do (char/p #\,) ml-whitespace/p))]
    (pure (append (list 'byte) result))))

(module+ test #| data-bytes/p |#
  (check-match (parsed-string-result data-bytes/p ".data    2,3,4")
               '(byte 2 3 4))
  (check-match (parsed-string-result data-bytes/p ".data $20,	%10,  4")
               '(byte 32 2 4)))

(module+ test #| adr-modes-opcode/p |#
  (check-match (parsed-string-result (adr-modes-opcode/p "lda" '(immediate)) "lda #$10")
               '(LDA (#:line 1 #:org-cmd "lda !16")
                     "!16"))
  (check-match (parsed-string-result (adr-modes-opcode/p "lda" '(zero-page)) "lda $10")
               '(LDA (#:line 1 #:org-cmd "lda 16")
                     "16"))
  (check-match (parsed-string-result (adr-modes-opcode/p "lda" '(indirect-x)) "lda ($10,x)")
               '(LDA (#:line 1 #:org-cmd "lda (16,x)")
                     ("16" ,x)))
  (check-match (parsed-string-result (adr-modes-opcode/p "lda" '(indirect-x)) "lda (<end,x)")
               '(LDA (#:line 1 #:org-cmd "lda (<end,x)")
                     ("<end" ,x)))
  (check-match (parsed-string-result (adr-modes-opcode/p "stx" '(indirect-y)) "stx ($10),y")
               '(STX (#:line 1 #:org-cmd "stx (16),y")
                     ("16") ,y))
  (check-match (parsed-string-result (adr-modes-opcode/p "jmp" '(indirect)) "jmp ($1000)")
               '(JMP (#:line 1 #:org-cmd "jmp (4096)")
                 ("4096")))
  (check-match (parsed-string-result (adr-modes-opcode/p "jmp" '(absolute)) "jmp $1000")
               '(JMP (#:line 1 #:org-cmd "jmp 4096")
                     "4096" )))

;; parser for valid assembler lines (including bytes and labels)
(define 6510-opcode/p
  (do (or/p
       (try/p 6510-label-def/p)
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
       asc-string/p
       )))

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
    [opcodes <- 6510-opcodes/p]
    eof/p
    (pure (list origin opcodes))))

(define 6510-opcodes/p
  (do [opcodes <- (many/p (do [op <- 6510-opcode/p] ml-whitespace/p (pure op)))]
      (pure opcodes)))

(define-namespace-anchor eval-ns-anchor)
(define eval-ns (namespace-anchor->namespace eval-ns-anchor))

(define (filter-meta-data command)
  (filter (lambda (el) (not (and (list? el) (equal? (car el) '#:line)))) command))

(define (parse-opcodes str)
  (define parsed (syntax->datum (parse-result! (parse-string (syntax/p 6510-opcodes/p) str))))
  (define stripped-src-location-data (map (lambda (command) (filter-meta-data command)) parsed))
  (map (lambda (command) (eval command eval-ns)) stripped-src-location-data))

(define (parse-program str)
  (define parsed (syntax->datum (parse-result! (parse-string (syntax/p 6510-program/p) str))))
  (define stripped-src-location-data (map (lambda (command) (filter-meta-data command)) (cadr parsed)))

  (map (lambda (command) (eval command eval-ns)) stripped-src-location-data)
  )

(module+ test #| compile-opcodes |#
  (check-equal? (parse-opcodes "JSR $FFD2")
                '((opcode 32 210 255)))
  (check-equal? (parse-opcodes ".data $FF, $10")
                (list (ast-bytes-cmd '(255 16))))
  (check-equal? (parse-opcodes "LDX $ff00\nsome: JSR some")
                `((opcode 174 0 255) ,(ast-label-def-cmd "some") (opcode 32 (resolve-word "some"))))
  (check-equal? (parse-program #<<EOF
       *=$0810        ; origin (basic start, to make loading and executing easier)

;        ;; lda #23
;        ;; sta 53272
;
         ldx hello
 sout:   lda hello,x
         jsr $ffd2
         dex
         bne sout

         clc
         ldx #$05       ; repeat .. times
 some:
         lda #$41       ; load character A (dec 65)
         jsr cout      ; print this character to screen
         adc #1         ; load character B (dec 66)
         jsr cout      ; print this character to screen
         adc #1
         jsr cout      ; print this character to screen
         lda #%00001101 ; $0d
         jsr cout
 end:    dex
         bne some
         rts            ; end of execution

 cout:   jsr $ffd2
         rts

 hello:  .data 18 ; number of bytes to print (string length)
         .data $0d ; line feed
         .asc "!DLROw WEN OLLEh"
         .data $0e ; switch to lower letter mode

         adc (<end),y
EOF
)
  `((decide (((resolve-byte "hello") opcode 166)
             ((resolve-word "hello") opcode 174)))
   ,(ast-label-def-cmd "sout")
    (decide (((resolve-byte "hello") opcode 181)
             ((resolve-word "hello") opcode 189)))
    (opcode 32 210 255)
    (opcode 202)
    (rel-opcode 208 (resolve-relative "sout"))
    (opcode 24)
    (opcode 162 5)
   ,(ast-label-def-cmd "some")
    (opcode 169 65)
    (opcode 32 (resolve-word "cout"))
    (opcode 105 1)
    (opcode 32 (resolve-word "cout"))
    (opcode 105 1)
    (opcode 32 (resolve-word "cout"))
    (opcode 169 13)
    (opcode 32 (resolve-word "cout"))
   ,(ast-label-def-cmd "end")
    (opcode 202)
    (rel-opcode 208 (resolve-relative "some"))
    (opcode 96)
   ,(ast-label-def-cmd "cout")
    (opcode 32 210 255)
    (opcode 96)
   ,(ast-label-def-cmd "hello")
   ,(ast-bytes-cmd '(18))
   ,(ast-bytes-cmd '(13))
   ,(ast-bytes-cmd '(33 68 76 82 79 119 32 87 69 78 32 79 76 76 69 104))
   ,(ast-bytes-cmd '(14))
    (opcode 113 (resolve-byte "<end"))))

  (check-equal? (parse-program #<<EOF
        *=$0810        ; origin (basic start, to make loading and executing easier)
        LDX $c000
hello:  LDA #$10
        LDA hello
EOF
)
                `((opcode 174 0 192)
                 ,(ast-label-def-cmd "hello")
                  (opcode 169 16)
                  (decide (((resolve-byte "hello") opcode 165)
                           ((resolve-word "hello") opcode 173))))))

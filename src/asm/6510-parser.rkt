#lang racket
#|

 parser for 6510 assembler code

 usage:
 - create file with content
     #lang reader "6510-reader.rkt"
     *=$C000
     lda #$40
 - C-c to open repl for this source file
 - follow the hints printed

 naming convention:
   .../p is or returns a parser

 |#

(require (only-in megaparsack parser?
                  eof/p
                  guard/p
                  label/p
                  many+/p
                  many/p
                  or/p
                  parse-result!
                  satisfy/p
                  syntax/p
                  try/p
                  void/p)

         (only-in megaparsack/text
                  char-ci/p
                  char-not/p
                  char/p
                  digit/p
                  integer/p
                  parse-string))

(require (only-in threading ~>>))
(require (only-in data/monad do <-))
(require (only-in data/applicative pure))
(require (rename-in racket/contract [define/contract define/c]))

(require (only-in "../6510-utils.rkt" parse-number-string ->string))
(require "../6510.rkt") ;; necessary to resolve all syntax macros of 6510 dsl


(provide 6510-program/p asm->ast scheme-asm->ast)

(module+ test
  (require rackunit)
  (require (only-in "../6510-test-utils.rkt" drop-meta-infos))
  (begin-for-syntax
    (require rackunit))

  (define (parsed-string-result syntax string)
    (syntax->datum (parse-result! (parse-string (syntax/p syntax) string)))))

;; space or tab
(define/c space-or-tab/p
  (-> parser?)
  (or/p (char/p #\space) (char/p #\tab)))

;; is the given char a hexadecimal number digit?
(define/c (char-hex? char)
  (-> char? boolean?)
  (let ([char-int (char->integer char)])
    (or (and (>= char-int 48) (<= char-int 57))
       (and (>= char-int 65) (<= char-int 70))
       (and (>= char-int 97) (<= char-int 102)))))

(define/c hex-digit/p
  (-> parser?)
  (label/p "hex-number" (satisfy/p char-hex?)))

(define/c hex-string/p
  (-> parser?)
  (do [digits <- (many+/p hex-digit/p)]
      (pure (list->string digits))))

;; considered white space (space, newline, tab, all chars behind ';')
(define/c ml-whitespace/p
  (-> parser?)
  (many/p (or/p (char/p #\newline)
                space-or-tab/p
                (do (char/p #\;)
                    (many/p (char-not/p #\newline))))))

;; '$' hex-digit+
(define/c hex-integer/p
  (-> parser?)
  (do (char/p #\$)
      [x <-  hex-string/p]
    (pure (parse-number-string (string-append "$" (->string x))))))

;; is the given char a binary number digit?
(define/c (char-bin? char)
  (-> char? boolean?)
  (or (eq? char #\0)
     (eq? char #\1)))

;; 0 | 1
(define/c bin-digit/p
  (-> parser?)
  (label/p "bin-number" (satisfy/p char-bin?)))

;; string of binary digits (0 | 1)
(define/c bin-string/p
  (-> parser?)
  (do [digits <- (many+/p bin-digit/p)]
      (pure (list->string digits))))

;; '%' (0 | 1)+
(define/c bin-integer/p
  (-> parser?)
  (do (char/p #\%)
      [x <- bin-string/p]
    (pure (parse-number-string (string-append "%" (->string x))))))

(module+ test #| char-bin?,  bin-string/p,  bin-integer/p |#
  (check-true (char-bin? #\1))

  (check-match (parsed-string-result bin-string/p "10")
               "10")

  (check-match (parsed-string-result bin-integer/p "%10")
               2))

;; any integer (dec, hex, binary)
(define/c 6510-integer/p
  (-> parser?)
  (or/p integer/p hex-integer/p bin-integer/p))

(module+ test #| 6510-integer/p |#
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

;; end of line
(define/c 6510-eol/p
  (-> parser?)
  (do (many/p space-or-tab/p)
      (or/p (do  (char/p #\;)
                (many/p (char-not/p #\newline)))
            void/p)
    (or/p (char/p #\newline)
          eof/p)))

;; allowed label characters
(define/c 6510-label-letter/p
  (-> parser?)
  (satisfy/p (lambda (c) (or (and (char>=? c #\A)
                          (char<=? c #\Z))
                       (and (char>=? c #\a)
                          (char<=? c #\z))
                       (char=? c #\_)))))

;; label byte indicator
(define/c 6510-label-byte-ind/p
  (-> parser?)
  (or/p (char/p #\>) (char/p #\<)))

;; label parser (without > or <, but including -)
(define/c 6510-label/p
  (-> parser?)
  (do      
      [first-letter <- (syntax/p 6510-label-letter/p)]
      [new-label <- (syntax/p (many+/p (or/p 6510-label-letter/p digit/p (char/p #\-))))]
    (pure (datum->syntax new-label
                        (list (string->symbol "label")
                              (string-append (->string first-letter)
                                             (list->string (syntax->datum new-label))))))))


;; label '=' word
(define/c 6510-constant-def/p
  (-> parser?)
    (do
      [label <- (syntax/p 6510-label/p)]
        ml-whitespace/p
        (char/p #\=)
        ml-whitespace/p
      [value <- (syntax/p word/p)]
      6510-eol/p
      (pure (datum->syntax label
                          (let ([num (syntax->datum value)])
                            (list (string->symbol (if (byte? num) "byte-const" "word-const"))
                                  (cadr (syntax->datum label))
                                  (number->string num)))))))

(module+ test #| 6510-constant-def |#
  (check-match (parsed-string-result 6510-constant-def/p "abc-x = $10")
               '(byte-const "abc-x" "16"))
    (check-match (parsed-string-result 6510-constant-def/p "x-abc = $100")
               '(word-const "x-abc" "256")))

;; label ':'
(define/c 6510-label-def/p
  (-> parser?)
  (do
      [result <- (syntax/p 6510-label/p)]
      (char/p #\:)
    (pure (datum->syntax result result))))

(module+ test #| 6510-label-def |#
  (check-match (parsed-string-result 6510-label-def/p "abc-x:")
               '(label "abc-x")))

;; ( '>' | '<' ) label
(define/c 6510-label-byte/p
  (-> parser?)
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
;; ( '>' | '<' )? label
(define/c 6510-any-label/p
  (-> parser?)
  (do
      [label <- (or/p (syntax/p 6510-label/p)
                     (syntax/p 6510-label-byte/p))]
      (pure label)))

(module+ test #| 6510-label/p,  6510-any-label/p |#
  (check-match (parsed-string-result 6510-label/p "abc-x")
               '(label "abc-x"))
  (check-match (parsed-string-result 6510-any-label/p "<abc-x")
               '(label "<abc-x")))

(module+ test #| 6510-label/p,  6510-label-byte/p |#
  (check-match (parsed-string-result 6510-label/p "abc")
               '(label "abc"))

  (check-match (parsed-string-result 6510-label-byte/p ">abc")
               '(label ">abc"))

  (check-match (parsed-string-result 6510-label-byte/p "<abc")
               '(label "<abc"))

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


(define/c 6510-offset/p
  (-> parser?)
  (do
      [op <- (or/p (char/p #\-) (char/p #\+))]
      (many/p space-or-tab/p)
    [offset <- 6510-integer/p]
    (pure (list op offset))))

(module+ test #| 6510-offset/p |#
  (check-match (parsed-string-result 6510-offset/p "+ 5")
               (list #\+ 5)))

(define/c 6510-label-expression/p
  (-> parser?)
  (do
      (le <- (or/p (try/p
                   (do [l <- 6510-any-label/p]
                       (many/p space-or-tab/p)
                     [o <- 6510-offset/p]
                     (pure (list 'label (string-join (list (last (syntax->datum l)) (string (first o)) (number->string (last o))) ""))) ) ) ;; o
                  (do [l <- 6510-any-label/p]
                      (pure (syntax->datum l)))))
      (pure le)))

(module+ test #| 6510-label-expression |#
  (check-match (parsed-string-result 6510-label-expression/p "abc")
               '(label "abc"))
  (check-match (parsed-string-result 6510-label-expression/p "abc+5")
               '(label "abc+5"))
  (check-match (parsed-string-result 6510-label-expression/p "abc   + 5")
               '(label "abc+5")))

;; parse integer in word range
(define/c word/p
  (-> parser?)
  (guard/p 6510-integer/p (λ (x) (and (>= x 0) (<= x 65535)))
           "integer in range [$0000,$FFFF]"))

;; parse integer in byte range
(define/c byte/p
  (-> parser?)
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

(module+ test #| chars-ci/p |#
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

;; 'A'
(define/c accumulator/p
  (-> parser?)
  (do (char-ci/p #\A) (pure '(A))))

;; '#' (byte-label | byte)
(define/c immediate/p
  (-> parser?)
  (do (char/p #\#)
      [x <- (or/p 6510-label-expression/p byte/p)]
    (pure (list (string-append "!" (if (number? x) (number->string x) (->string (last x))))))))

;; (label | byte)
(define/c zero-page-or-relative/p
  (-> parser?)
  (do [b <- (or/p byte/p 6510-label-expression/p)]
      (pure (list (if (number? b) (number->string b) (last b))))))

;; (label | word)
(define/c absolute/p
  (-> parser?)
  (do [mem <- (or/p 6510-label-expression/p word/p)]
      (pure (list (if (number? mem) (number->string mem) (last mem))))))

;; '(' label | byte ',x)'
(define/c indirect-x/p
  (-> parser?)
  (do (char/p #\()
      [mem <- (or/p 6510-label-expression/p byte/p)]
    (string-cia/p ",x")
    (char/p #\))                       
    (pure (list (list (if (number? mem) (number->string mem) (last mem)) ',x )))))

(module+ test #| indirect-x |#
    (check-match (parsed-string-result indirect-x/p "($11,x)")
                 '(("17",x))))

;; '(' label | byte '),y'
(define/c indirect-y/p
  (-> parser?)
  (do 
      (char/p #\()
      [mem <- (or/p 6510-label-expression/p byte/p)]
    (char/p #\))
    (string-cia/p ",y")
    (pure (list (list (if (number? mem) (number->string mem) (last mem))) ',y))))

;; '(' label | word ')'
(define/c indirect/p
  (-> parser?)
  (do (char/p #\()
      [mem <- (or/p 6510-label-expression/p word/p)]
    (char/p #\))
    (pure `(( ,(if (number? mem) (number->string mem) (last mem)) )))))

;; ( label | word ) ',x'
(define/c absolute-x/p
  (-> parser?)
  (do [x <- (or/p word/p 6510-label-expression/p)]
      (string-cia/p ",x")
    (pure (list (if (number? x) (number->string x) (last x)) ',x))))

;; (label | word) ',y'
(define/c absolute-y/p
  (-> parser?)
  (do [x <- (or/p word/p 6510-label-expression/p)]
      (string-cia/p ",y")
    (pure (list (if (number? x) (number->string x) (last x)) ',y))))

;; (label | byte) ',x'
(define/c zero-page-x/p
  (-> parser?)
  (do [x <- (or/p byte/p 6510-label-expression/p)]
      (string-cia/p ",x")
    (pure (list (if (number? x) (number->string x) (last x)) ',x))))

;; (label | byte) ',y'
(define/c zero-page-y/p
  (-> parser?)
  (do [x <- (or/p byte/p 6510-label-expression/p)]
      (string-cia/p ",y")
    (pure (list (if (number? x) (number->string x) (last x)) ',y))))

(module+ test #| immediate/p,  zero-page-or-relative/p,  indirect/p,  indirect-x/p,  indirect-y/p,  absolute/p |#
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
  (check-match (parsed-string-result indirect-y/p "(some-3),y")
               '( ("some-3"),y))
  (check-match (parsed-string-result absolute/p "$1000")
               '("4096"))
  (check-match (parsed-string-result absolute/p "hello")
               '("hello"))
  (check-match (parsed-string-result absolute/p "hello + 4")
               '("hello+4"))
  )

;; transform opcode string to a list w/ capitalized opcode as symbol
(define/c (opcode->list4pure opcode)
  (-> string? (listof symbol?))
  (list (string->symbol (string-upcase opcode))))

(module+ test #| opcode->list4pure |#
  (check-equal? (opcode->list4pure "jmp")
                '(JMP)))

(define/c (args->string opcode-args-stx)
  (-> syntax? string?)
  (let ([args (syntax->datum opcode-args-stx)])
    (cond [(list? args)
           (string-join (map ->string args) "")]
          [else (->string args)])))

(module+ test #| args->string |#
  (check-equal? (args->string #'( ("16"),y))
                "(16),y")
  (check-equal? (args->string #'(label $2000 ,x))
                "label$2000,x"))

(define/c (construct-ref-meta-info opcode-stx [operands-stx (datum->syntax #f (void))])
  (->* (syntax?) (syntax?) list?)
   (list '#:line (syntax-line opcode-stx)
         '#:org-cmd (if (void? (syntax->datum operands-stx))
                        (format "~a" (syntax->datum opcode-stx))
                        (format "~a ~a" (syntax->datum opcode-stx) (args->string operands-stx)))))

(define/c (result->syntax actual-opcode-stx [operands-stx (datum->syntax #f (void))])
  (->* (syntax?) (syntax?) syntax?)
  (datum->syntax actual-opcode-stx
                (append (opcode->list4pure (syntax->datum actual-opcode-stx))
                        (list (list 'quote (construct-ref-meta-info actual-opcode-stx operands-stx)))
                        (if (void? (syntax->datum operands-stx)) (list) (syntax->datum operands-stx)))
                actual-opcode-stx))

(define (addressing-symbol? sym)
  (memq sym '(accumulator
              implicit
              immediate
              indirect-x
              indirect-y
              indirect
              absolute-x
              absolute-y
              absolute
              zero-page-x
              zero-page-y
              zero-page
              relative)))

(define/c (opcode-addressing/p actual-opcode addressing adr-mode-list rule/p)
  (-> syntax? (or/c addressing-symbol? (listof addressing-symbol?)) (listof addressing-symbol?) parser? parser?)
  (let ([addressings (flatten (list addressing))])
    (try/p (guard/p (do (many/p space-or-tab/p) [a-res <- (syntax/p rule/p)]
                      (pure (result->syntax actual-opcode a-res)))
                    (lambda (_) (ormap (λ (addr) (member addr adr-mode-list)) addressings))
                    (format "addressing ~a not applicable" addressing)))))

;; return a parser that will parse the given opcode combined with the available addressing modes
(define/c (adr-modes-opcode/p opcode adr-mode-list)
  (-> string? (listof addressing-symbol?) parser?)
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
                   (opcode-addressing/p actual-opcode '(absolute relative)  adr-mode-list 6510-label/p)
                   (opcode-addressing/p actual-opcode 'implicit adr-mode-list void/p))]
    (pure res)))

;; parser for '.asc "some string"'
(define/c asc-string/p
  (-> parser?)
  (do
      (try/p (string-cia/p ".asc"))
      (many/p space-or-tab/p)
    (char/p #\")
    [result <- (many/p (char-not/p #\"))]
    (char/p #\")
    (pure (list 'asc (list->string result)))))

(module+ test #| asc-string/p |#
  (check-match (parsed-string-result asc-string/p ".asc  \"some\"")
               '(asc "some"))
  (check-match (parsed-string-result asc-string/p ".asc  \"some 'other'\"")
               '(asc "some 'other'")))

;; parser for ".data (byte/p (","|ml_whitespace/p) ...)
(define/c data-bytes/p
  (-> parser?)
  (do
      [data-cmd <- (syntax/p (try/p (string-cia/p ".data")))]
      (many/p space-or-tab/p)
    [result <- (many/p byte/p #:sep (do (char/p #\,) ml-whitespace/p))]
    (pure (append (list 'byte) (list (list 'quote (list '#:line (syntax-line data-cmd)))) result))))

(module+ test #| data-bytes/p |#
  (check-match (parsed-string-result data-bytes/p ".data    2,3,4")
               '(byte '(#:line 1) 2 3 4))
  (check-match (parsed-string-result data-bytes/p ".data $20,	%10,  4")
               '(byte '(#:line 1) 32 2 4)))

(module+ test #| adr-modes-opcode/p |#
  (check-match (parsed-string-result (adr-modes-opcode/p "php" '(implicit)) "php")
               '(PHP '(#:line 1 #:org-cmd "php")))
  (check-match (parsed-string-result (adr-modes-opcode/p "lda" '(immediate)) "lda #$10")
               '(LDA '(#:line 1 #:org-cmd "lda !16")
                     "!16"))
  (check-match (parsed-string-result (adr-modes-opcode/p "lda" '(zero-page)) "lda $10")
               '(LDA '(#:line 1 #:org-cmd "lda 16")
                     "16"))
  (check-match (parsed-string-result (adr-modes-opcode/p "lda" '(indirect-x)) "lda ($10,x)")
               '(LDA '(#:line 1 #:org-cmd "lda (16,x)")
                     ("16" ,x)))
  (check-match (parsed-string-result (adr-modes-opcode/p "lda" '(indirect-x)) "lda (<end,x)")
               '(LDA '(#:line 1 #:org-cmd "lda (<end,x)")
                     ("<end" ,x)))
  (check-match (parsed-string-result (adr-modes-opcode/p "stx" '(indirect-y)) "stx ($10),y")
               '(STX '(#:line 1 #:org-cmd "stx (16),y")
                     ("16") ,y))
  (check-match (parsed-string-result (adr-modes-opcode/p "jmp" '(indirect)) "jmp ($1000)")
               '(JMP '(#:line 1 #:org-cmd "jmp (4096)")
                 ("4096")))
  (check-match (parsed-string-result (adr-modes-opcode/p "jmp" '(absolute)) "jmp $1000")
               '(JMP '(#:line 1 #:org-cmd "jmp 4096")
                     "4096" )))

;; parser for valid assembler lines (including bytes and labels)
(define/c 6510-opcode/p
  (-> parser?)
  (do (or/p
       (try/p 6510-constant-def/p)
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
(define/c 6510-program-origin/p
  (-> parser?)
  (do (char/p #\*) (many/p space-or-tab/p)
    (char/p #\=) (many/p space-or-tab/p)
    [origin <- 6510-integer/p]
    6510-eol/p
    (pure origin)))

;; parser for a complete assembler program
(define/c 6510-program/p
  (-> parser?)
  (do ml-whitespace/p
      [origin <- 6510-program-origin/p]
    ml-whitespace/p
    [opcodes <- 6510-opcodes/p]
    eof/p
    (pure (list origin opcodes))))

(define/c 6510-opcodes/p
  (-> parser?)
  (do [opcodes <- (many/p (do [op <- 6510-opcode/p] ml-whitespace/p (pure op)))]
      (pure opcodes)))

(define-namespace-anchor eval-ns-anchor)
(define eval-ns (namespace-anchor->namespace eval-ns-anchor))

(define/c (filter-meta-data command)
  (-> (listof any/c) (listof any/c))
  (filter (lambda (el) (not (and (list? el) (equal? (car el) '#:line)))) command))

(module+ test #| filter-meta-data |#
  (check-equal? (filter-meta-data '(PHP (#:line 1 #:org-cmd "php")))
                '(PHP)))

;; convert native assembler -> assember with scheme syntax
(define/c (asm->scheme-asm str [parse-rule 6510-opcodes/p])
  (->* (string?) (parser?) list?)
  (define parsed (syntax->datum (parse-result! (parse-string (syntax/p parse-rule) str))))
  (map (lambda (command) (filter-meta-data command)) parsed))

(module+ test #| asm->scheme-asm |#
  (check-equal? (asm->scheme-asm "LDA #$20") '((LDA '(#:line 1 #:org-cmd "lda !32") "!32")))
  (check-equal? (asm->scheme-asm ".data $20") '((byte '(#:line 1) 32)))
  (check-equal? (asm->scheme-asm "LDA #$20\nJSR $FFD2")
                '((LDA '(#:line 1 #:org-cmd "lda !32") "!32")
                  (JSR '(#:line 2 #:org-cmd "jsr 65490") "65490"))))

;; convert scheme assembler -> ast commands
(define/c (scheme-asm->ast scheme-asm-command-list)
  (-> list? (listof ast-command?))
  ;; this eval will result in macro expansion
  (map (lambda (command) (eval command eval-ns)) scheme-asm-command-list))

(module+ test #| scheme-asm->ast |#
  (check-equal? (scheme-asm->ast '((LDA '(#:line 1) "!32")))
                (list (ast-opcode-cmd '(#:line 1) '(169 32))))
  (check-equal? (scheme-asm->ast '((byte '(#:line 1) 32)))
                (list (ast-bytes-cmd '(#:line 1) '(32))))
  (check-equal? (drop-meta-infos (scheme-asm->ast '((LDA "!32")(JSR "65490"))))
                (drop-meta-infos (list (ast-opcode-cmd '() '(169 32))
                                      (ast-opcode-cmd '() '(32 210 255))))))

;; convert native assembler into ast 
(define/c (asm->ast str [parse-rule 6510-opcodes/p])
  (->* (string?) (parser?) (listof ast-command?))
  (~>> (asm->scheme-asm str parse-rule)
     (scheme-asm->ast _)))

;; convert native whole assembler program into ast commands (ignoring origin aka *=)
(define/c (parse-program str)
  (-> string? (listof ast-command?))
  (define parsed (syntax->datum (parse-result! (parse-string (syntax/p 6510-program/p) str))))
  ;; TODO: get metadata for each command (line and original string)
  ;;       in a second step generate program-counter -> source line map into a file
  (define stripped-src-location-data (map (lambda (command) (filter-meta-data command)) (cadr parsed)))
  (scheme-asm->ast stripped-src-location-data))

(module+ test #| asm->ast, parse-program |#
  (check-equal? (asm->ast "PHP")
                (list (ast-opcode-cmd '(#:line 1 #:org-cmd "php") '(8))))
  (check-equal? (asm->ast "JSR $FFD2")
                (list (ast-opcode-cmd '(#:line 1 #:org-cmd "jsr 65490") '(32 210 255))))
  (check-equal? (asm->ast ".data $FF, $10")
                (list (ast-bytes-cmd '(#:line 1) '(255 16))))
  (check-equal? (asm->ast "LDX $ff00\nsome: JSR some")
                (list (ast-opcode-cmd '(#:line 1 #:org-cmd "ldx 65280") '(174 0 255))
                      (ast-label-def-cmd '() "some")
                      (ast-unresolved-opcode-cmd '(#:line 2 #:org-cmd "jsr some") '(32) (ast-resolve-word-scmd "some"))))
  (check-equal? (parse-program #<<EOF
       *=$0810        ; origin (basic start, to make loading and executing easier)

        ;; lda #23
        ;; sta 53272

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
  (list
   (ast-decide-cmd
    '(#:line 6 #:org-cmd "ldx hello")
    (list (ast-unresolved-opcode-cmd '(#:line 6 #:org-cmd "ldx hello") '(166) (ast-resolve-byte-scmd "hello" 'low-byte))
          (ast-unresolved-opcode-cmd '(#:line 6 #:org-cmd "ldx hello") '(174) (ast-resolve-word-scmd "hello"))))
   (ast-label-def-cmd '() "sout")
   (ast-decide-cmd
    '(#:line 7 #:org-cmd "lda hello,x")
    (list (ast-unresolved-opcode-cmd '(#:line 7 #:org-cmd "lda hello,x") '(181) (ast-resolve-byte-scmd "hello" 'low-byte))
          (ast-unresolved-opcode-cmd '(#:line 7 #:org-cmd "lda hello,x") '(189) (ast-resolve-word-scmd "hello"))))
   (ast-opcode-cmd '(#:line 8 #:org-cmd "jsr 65490") '(32 210 255))
   (ast-opcode-cmd '(#:line 9 #:org-cmd "dex") '(202))
   (ast-unresolved-rel-opcode-cmd '(#:line 10 #:org-cmd "bne sout") '(208) (ast-resolve-byte-scmd "sout" 'relative))
   (ast-opcode-cmd '(#:line 12 #:org-cmd "clc") '(24))
   (ast-opcode-cmd '(#:line 13 #:org-cmd "ldx !5") '(162 5))
   (ast-label-def-cmd '() "some")
   (ast-opcode-cmd '(#:line 15 #:org-cmd "lda !65") '(169 65))
   (ast-unresolved-opcode-cmd '(#:line 16 #:org-cmd "jsr cout") '(32) (ast-resolve-word-scmd "cout"))
   (ast-opcode-cmd '(#:line 17 #:org-cmd "adc !1") '(105 1))
   (ast-unresolved-opcode-cmd '(#:line 18 #:org-cmd "jsr cout") '(32) (ast-resolve-word-scmd "cout"))
   (ast-opcode-cmd '(#:line 19 #:org-cmd "adc !1") '(105 1))
   (ast-unresolved-opcode-cmd '(#:line 20 #:org-cmd "jsr cout") '(32) (ast-resolve-word-scmd "cout"))
   (ast-opcode-cmd '(#:line 21 #:org-cmd "lda !13") '(169 13))
   (ast-unresolved-opcode-cmd '(#:line 22 #:org-cmd "jsr cout") '(32) (ast-resolve-word-scmd "cout"))
   (ast-label-def-cmd '() "end")
   (ast-opcode-cmd '(#:line 23 #:org-cmd "dex") '(202))
   (ast-unresolved-rel-opcode-cmd '(#:line 24 #:org-cmd "bne some") '(208) (ast-resolve-byte-scmd "some" 'relative))
   (ast-opcode-cmd '(#:line 25 #:org-cmd "rts") '(96))
   (ast-label-def-cmd '() "cout")
   (ast-opcode-cmd '(#:line 27 #:org-cmd "jsr 65490") '(32 210 255))
   (ast-opcode-cmd '(#:line 28 #:org-cmd "rts") '(96))
   (ast-label-def-cmd '() "hello")
   (ast-bytes-cmd '(#:line 30) '(18))
   (ast-bytes-cmd '(#:line 31) '(13))
   (ast-bytes-cmd '() '(33 68 76 82 79 119 32 87 69 78 32 79 76 76 69 104))
   (ast-bytes-cmd '(#:line 33) '(14))
   (ast-unresolved-opcode-cmd '(#:line 35 #:org-cmd "adc (<end),y") '(113) (ast-resolve-byte-scmd "end" 'low-byte))))

  (check-equal? (parse-program #<<EOF
        *=$0810        ; origin (basic start, to make loading and executing easier)
        LDX $c000
hello:  LDA #$10
        LDA hello
EOF
)
                (list
                 (ast-opcode-cmd '(#:line 2 #:org-cmd "ldx 49152") '(174 0 192))
                 (ast-label-def-cmd '() "hello")
                 (ast-opcode-cmd '(#:line 3 #:org-cmd "lda !16") '(169 16))
                 (ast-decide-cmd
                  '(#:line 4 #:org-cmd "lda hello")
                  (list (ast-unresolved-opcode-cmd '(#:line 4 #:org-cmd "lda hello") '(165) (ast-resolve-byte-scmd "hello" 'low-byte))
                        (ast-unresolved-opcode-cmd '(#:line 4 #:org-cmd "lda hello") '(173) (ast-resolve-word-scmd "hello")))))))

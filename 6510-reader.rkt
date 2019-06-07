#lang racket
(require syntax/strip-context)
(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)
(require "6510.rkt")
(require "6510-interpreter.rkt")

;; TODO add parser for ASL
;; TODO writer parameterized parser for all addressing modes

; usage:
; - create file with content
;     #lang reader "6510-reader.rkt"
;     10
; - C-c to open repl for this source file
; - enter 'data' to see if 10 is successfully parsed

(module+ test
  (require rackunit)

  (define (parsed-string-result syntax string)
    (syntax->datum (parse-result! (parse-string (syntax/p syntax) string)))))

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))

(define (literal-read in)
  (syntax->datum
   (literal-read-syntax #f in)))

(define space-or-tab/p
  (or/p (char/p #\space) (char/p #\tab)))

(define (char-hex? char)
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

(define (char-bin? char)
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
             (lambda () (parsed-string-result abs-opcode/p "-10")))

  (check-exn exn:fail?
             (lambda () (parsed-string-result abs-opcode/p "A0")))

  (check-exn exn:fail?
             (lambda () (parsed-string-result abs-opcode/p "_17"))))

(define 6510-eol/p
  (do (many/p space-or-tab/p)
      (or/p (do  (char/p #\;)
                (many/p (char-not/p #\newline)))
            void/p)
    (or/p (char/p #\newline)
          eof/p)))

(define 6510-label/p
  (do
      (char/p #\:)
      [new-label <- (many+/p (or/p letter/p digit/p (char/p #\_)))]
    (pure (list (string->symbol "LABEL") (string-append ":" (list->string new-label))))))

(module+ test
  (check-match (parsed-string-result 6510-label/p ":abc")
               '(LABEL ":abc"))

  (check-match (parsed-string-result 6510-label/p ":abc12")
               '(LABEL ":abc12"))

  (check-match (parsed-string-result 6510-label/p ":ab1c2")
               '(LABEL ":ab1c2"))

  (check-match (parsed-string-result 6510-label/p ":1ab1c2")
               '(LABEL ":1ab1c2"))

  (check-match (parsed-string-result 6510-label/p ":1ab1_c2")
               '(LABEL ":1ab1_c2"))

  (check-match (parsed-string-result 6510-label/p ":1ab1_c2:8")
               '(LABEL ":1ab1_c2"))

  (check-exn exn:fail?
             (lambda () (parsed-string-result 6510-label/p "abc"))))


(define word/p
    (guard/p 6510-integer/p (λ (x) (<= x 65535))
             "integer in range [$0000,$FFFF]"))

(define byte/p
    (guard/p 6510-integer/p (λ (x) (<= x 255))
             "integer in range [$00,$FF]"))

(define (abs-opcode/p opcode)
  (do
      (try/p (string-ci/p opcode))
      (many/p space-or-tab/p)
    [x <- (or/p word/p
               6510-label/p)]  ;; could be a string too
    6510-eol/p
    (pure (append (list (string->symbol (string-upcase opcode)))
                  (if (number? x)
                      (list (number->string x))
                      (list (last x)))))))

(module+ test
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

(define (rel-opcode/p opcode)
  (do
      (try/p (string-ci/p opcode))
      (many/p space-or-tab/p)
    [x <- (or/p byte/p
               6510-label/p)]
    6510-eol/p
    (pure (append (list (string->symbol (string-upcase opcode)))
                  (if (number? x)
                      (list (number->string x))
                      (list (last x)))))))

(define (opcode/p opcode)
  (do
      (try/p (string-ci/p opcode))
      6510-eol/p
    (pure (list (string->symbol (string-upcase opcode))))))

(define (iia-opcode/p opcode immediate?)
  (do
      (try/p (string-ci/p opcode))
      (many/p space-or-tab/p)
    (or/p (if immediate? (iia-opcode-immediate opcode) void/p)
          (or/p (iia-opcode-indirect opcode)
                (iia-opcode-absolute opcode #t)))))

(define (iia-opcode-immediate opcode)
  (do
      (char/p #\#)
      [x <- byte/p]
    (pure `(,(string->symbol (string-upcase opcode)) ,(string-append "#" (number->string x))))))

(define (iia-opcode-absolute opcode index-y?)
  (do
      [operand <- word/p]
      [appendix <- (or/p (do (char/p #\,)
                            (or/p  (string-ci/p "x")
                                   (if index-y? (string-ci/p "y") void/p)))
                        void/p)]
    (let ([base-result-lst `(,(string->symbol (string-upcase opcode)) ,(number->string operand))])
      (if (void? appendix)
          (pure base-result-lst)
          (pure (append base-result-lst `(,(string->symbol (string-downcase appendix)))))))
      ))

(define (iia-opcode-indirect opcode)
  (do
      (char/p #\()
      [x <- 6510-integer/p]
    [ind <- (or/p (string-ci/p "),y")
                 (string-ci/p ",x)"))]
    (if (equal? ind "),y")
        (pure `(,(string->symbol (string-upcase opcode)) < ,(number->string x) > y))
        (pure `(,(string->symbol (string-upcase opcode)) < ,(number->string x) x >)))))

(define (zax-opcode/p opcode)
  (do
      (try/p (string-ci/p opcode))
      (many/p space-or-tab/p)
    (iia-opcode-absolute opcode #f)))

(define (data-bytes/p)
  (do
      (try/p (string-ci/p ".data"))
      (many/p space-or-tab/p)
    [result <- (many/p byte/p #:sep (do (char/p #\,) ml-whitespace/p))]
    (pure (list 'BYTES result))))

;; immediate, indirect and absolute addressing
(define 6510-opcode/p
  (do (or/p (iia-opcode/p "adc" #t)
            (rel-opcode/p "beq")
            (rel-opcode/p "bcc")
            (rel-opcode/p "bcs")
            (rel-opcode/p "bmi")
            (rel-opcode/p "bne")
            (rel-opcode/p "bpl")
            (opcode/p "brk")
            (rel-opcode/p "bvc")
            (rel-opcode/p "bvs")
            (zax-opcode/p "dec")
            (zax-opcode/p "inc")
            (abs-opcode/p "jsr")
            (iia-opcode/p "lda" #t)
            (iia-opcode/p "sta" #f)
            (opcode/p "rts")
            (data-bytes/p)
            6510-label/p)))

(define 6510-program-origin/p
  (do (char/p #\*) (many/p space-or-tab/p)
    (char/p #\=) (many/p space-or-tab/p)
    [origin <- 6510-integer/p]
    6510-eol/p
    (pure origin)))

(define 6510-program/p
  (do ml-whitespace/p
      [origin <- 6510-program-origin/p]
    ml-whitespace/p
    [opcodes <- (many/p (do [op <- 6510-opcode/p] ml-whitespace/p (pure op)))]
    eof/p
    (pure (list origin opcodes))))

(define (list->values list)
  (apply values list))

(define (literal-read-syntax src in)
  (let*-values ([(parsed-string) (parse-string (syntax/p 6510-program/p) (port->string in))]
                [(parse-result) (parse-result! parsed-string)]
                [(origin parsed-opcodes) (list->values (syntax->datum parse-result))])
    (with-syntax ([(str ...) parsed-opcodes]
                  [org origin])
      (strip-context
       #'(module compiled6510 racket
           (require "6510.rkt")
           (require "6510-interpreter.rkt")
           (provide program raw-program data resolved-program pretty-program raw-bytes)
           (define raw-program '(str ...))
           (define program `(,str ...))
           (define resolved-program (replace-labels program org))
           (define raw-bytes (commands->bytes org `(,str ...)))
           (define data (6510-load (initialize-cpu) org raw-bytes))
           (displayln "program execution:")
           (let ([_ (run (set-pc-in-state data org))])
             (void))
           (displayln "(have a look at raw-program, resolved-program, raw-bytes and pretty-program)")
           (define pretty-program (pretty-print-program resolved-program raw-program))
           ; (create-prg (commands->bytes org program) org "test.prg")
           ; (run-emulator "test.prg")
           )))))

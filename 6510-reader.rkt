#lang racket
(require syntax/strip-context)
(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)
(require "6510.rkt")
(require "6510-interpreter.rkt")

; usage:
; - create file with content
;     #lang reader "6510-reader.rkt"
;     *=$C000
;     lda #$40
; - C-c to open repl for this source file
; - follow the hints printed

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
      (try/p (string-cia/p opcode))
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
      (try/p (string-cia/p opcode))
      (many/p space-or-tab/p)
    [x <- (or/p byte/p
               6510-label/p)]
    6510-eol/p
    (pure (append (list (string->symbol (string-upcase opcode)))
                  (if (number? x)
                      (list (number->string x))
                      (list (last x)))))))

(define (chars-ci/p str)
  (if (zero? (string-length str))
      (pure "")
      (label/p str (do (char-ci/p (string-ref str 0))
                       (string-cia/p (substring str 1))
                     (pure str)))))

(define (string-cia/p string)
  (chars-ci/p string))

(define accumulator/p (do (char-ci/p #\A) (pure '(A))))
(define immediate/p (do (char/p #\#) [x <- byte/p] (pure (list (string-append "#" (number->string x))))))
(define zero-page-or-relative/p (do [x <- byte/p] (pure (list (number->string x)))))
(define absolute/p (do [mem <- (or/p 6510-label/p word/p)] (pure (list (if (number? mem) (number->string mem) (last mem))))))
(define indirect-x/p (do (char/p #\() [mem <- (or/p 6510-label/p byte/p)] (string-cia/p ",x") (char/p #\))
                         (pure `(< ,(if (number? mem) (number->string mem) (last mem)) x >))))
(define indirect-y/p (do (char/p #\() [mem <- byte/p] (char/p #\)) (string-cia/p ",y") (pure `(< ,(number->string mem) > y))))
(define absolute-x/p (do [x <- word/p] (string-cia/p ",x") (pure (list (number->string x) 'x))))
(define absolute-y/p (do [x <- word/p] (string-cia/p ",y") (pure (list (number->string x) 'y))))
(define zero-page-x/p (do [x <- byte/p] (string-cia/p ",x") (pure (list (number->string x) 'x))))

(module+ test
  (check-match (parsed-string-result indirect-x/p "($10,x)")
               '(< "16" x >))
  (check-match (parsed-string-result absolute/p "$1000")
               '("4096"))
  (check-match (parsed-string-result absolute/p ":out")
               '(":out")))

(define (opcode->list4pure opcode)
  (list (string->symbol (string-upcase opcode))))

(define (adr-modes-opcode/p opcode adr-mode-list)
  (do
      (try/p (string-cia/p opcode))

      ;; order is relevant (for parser to get the max matching string per line
      [res <- (or/p (try/p (guard/p (do (many/p space-or-tab/p) [a-res <- accumulator/p] (pure (append (opcode->list4pure opcode) a-res)))
                                   (lambda (x) (member 'accumulator adr-mode-list)) "no accumlator"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [i-res <- immediate/p] (pure (append (opcode->list4pure opcode) i-res)))
                                   (lambda (x) (member 'immediate adr-mode-list)) "no immediate"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [indx-res <- indirect-x/p] (pure (append (opcode->list4pure opcode) indx-res)))
                                   (lambda (x) (member 'indirect-x adr-mode-list)) "no indirect, x"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [indy-res <- indirect-y/p] (pure (append (opcode->list4pure opcode) indy-res)))
                                   (lambda (x) (member 'indirect-y adr-mode-list)) "no indirect, y"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [absx-res <- absolute-x/p] (pure (append (opcode->list4pure opcode) absx-res)))
                                   (lambda (x) (member 'absolute-x adr-mode-list)) "no absolute, x"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [absy-res <- absolute-y/p] (pure (append (opcode->list4pure opcode) absy-res)))
                                   (lambda (x) (member 'absolute-y adr-mode-list)) "no absolute, y"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [zpx-res <- zero-page-x/p] (pure (append (opcode->list4pure opcode) zpx-res)))
                                   (lambda (x) (member 'zero-page-x adr-mode-list)) "no zero page, x"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [abs-res <- absolute/p] (pure (append (opcode->list4pure opcode) abs-res)))
                                   (lambda (x) (member 'absolute adr-mode-list)) "no absolute"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [zp-res <- zero-page-or-relative/p] (pure (append (opcode->list4pure opcode) zp-res)))
                                   (lambda (x) (or (member 'zero-page adr-mode-list) (member 'relative adr-mode-list))) "no zero page nor relative"))
                   (try/p (guard/p (do (many/p space-or-tab/p) [label-res <- 6510-label/p] (pure (append (opcode->list4pure opcode) (list (last label-res)))))
                                   (lambda (x) (or (member 'relative adr-mode-list) (member 'absolute adr-mode-list))) "no relative label"))
                   (try/p (guard/p (do void/p (pure (opcode->list4pure opcode)))
                                   (lambda (x) (member 'implicit adr-mode-list) ) "no implicit"))
                   )]
    (pure res)))

(define data-bytes/p
  (do
      (try/p (string-cia/p ".data"))
      (many/p space-or-tab/p)
    [result <- (many/p byte/p #:sep (do (char/p #\,) ml-whitespace/p))]
    (pure (list 'BYTES result))))

;; immediate, indirect and absolute addressing
(define 6510-opcode/p
  (do (or/p
       (adr-modes-opcode/p "adc" '(immediate zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y))
       (adr-modes-opcode/p "asl" '(accumulator zero-page zero-page-x absolute-x absolute))
       (adr-modes-opcode/p "beq" '(relative))
       (adr-modes-opcode/p "bcc" '(relative))
       (adr-modes-opcode/p "bcs" '(relative))
       (adr-modes-opcode/p "bmi" '(relative))
       (adr-modes-opcode/p "bne" '(relative))
       (adr-modes-opcode/p "bpl" '(relative))
       (adr-modes-opcode/p "brk" '(implicit))
       (adr-modes-opcode/p "bvc" '(relative))
       (adr-modes-opcode/p "bvs" '(relative))
       (adr-modes-opcode/p "dec" '(zero-page zero-page-x absolute absolute-x))
       (adr-modes-opcode/p "inc" '(zero-page zero-page-x absolute absolute-x))
       (adr-modes-opcode/p "jsr" '(absolute))
       (adr-modes-opcode/p "lda" '(immediate zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y))
       (adr-modes-opcode/p "sta" '(zero-page zero-page-x absolute absolute-x absolute-y indirect-x indirect-y))
       (adr-modes-opcode/p "rts" '(implicit))
       data-bytes/p
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
           ;; (create-prg (commands->bytes org program) org "test.prg")
           (create-image-with-program (commands->bytes org program) org "test.prg" "test.d64" "test")
           (displayln "execute the program in vice via (run-emulator \"test.d64\")")
           ;; (run-emulator "test.d64")
           )))))

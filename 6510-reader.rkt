#lang racket
(require syntax/strip-context)
(require megaparsack megaparsack/text)
(require data/monad)
(require data/applicative)
(require "6510.rkt")

; usage:
; - create file with content
;     #lang reader "6510-reader.rkt"
;     10
; - C-c to open repl for this source file
; - enter 'data' to see if 10 is successfully parsed

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))

(define (literal-read in)
  (syntax->datum
   (literal-read-syntax #f in)))

(define space-or-tab/p
  (or/p (char/p #\space) (char/p #\tab)))

(define hex-digit/p
  (or/p (char/p #\0)
        (char/p #\1)
        (char/p #\2)
        (char/p #\3)
        (char/p #\4)
        (char/p #\5)
        (char/p #\6)
        (char/p #\7)
        (char/p #\8)
        (char/p #\9)
        (char-ci/p #\A)
        (char-ci/p #\B)
        (char-ci/p #\C)
        (char-ci/p #\D)
        (char-ci/p #\E)
        (char-ci/p #\F)))

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

(define 6510-integer/p
  (or/p integer/p hex-integer/p))

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
      [new-label <- (many+/p (or/p letter/p digit/p))]
    (pure (list (string->symbol "LABEL") (list->string new-label)))))

(define (abs-opcode/p opcode)
  (do
      (string-ci/p opcode)
      (many/p space-or-tab/p)
    [x <- (or/p 6510-integer/p
                6510-label/p)]  ;; could be a string too
    6510-eol/p
    (pure (append (list (string->symbol (string-upcase opcode))) (if (number? x) (list (number->string x)) (list ''label-ref-absolute (last x)))))))

(define (opcode/p opcode)
  (do
      (string-ci/p opcode)
      6510-eol/p
    (pure (list (string->symbol (string-upcase opcode))))))

;; immediate, indirect and absolute addressing
(define (imm-ind-abs-opcode/p opcode)
  (do
      (string-ci/p opcode)
      (many/p space-or-tab/p)
    [immediate <- (or/p void/p (char/p #\#) (char/p #\())]
    [x <- 6510-integer/p]
    [closing <- (or/p void/p (char/p #\)))]
    [appendix <- (or/p (do (char/p #\,)
                           (or/p (string-ci/p "x")
                                 (string-ci/p "y")))
                       void/p)]
    [closing2 <- (or/p void/p (char/p #\)))]
    6510-eol/p
    (let* [(immediate-str (if (void? immediate) "" "#"))
           (base-result-lst `(,(string->symbol (string-upcase opcode)) ,(string-append immediate-str (number->string x))))]
      (pure (cond [(void? appendix) base-result-lst]
                  [(and (void? closing2) (void? closing)) (append base-result-lst `(',(string->symbol (string-downcase appendix))))]
                  [else base-result-lst])))))

(define 6510-opcode/p
  (do (or/p (imm-ind-abs-opcode/p "adc")
            (imm-ind-abs-opcode/p "lda")
            (opcode/p "brk")
            (opcode/p "rts")
            (abs-opcode/p "jsr")
            6510-label/p)))

(define 6510-program-origin/p
  (do (char/p #\*) (many/p space-or-tab/p)
    (char/p #\=) (many/p space-or-tab/p)
    [origin <- hex-integer/p]
    6510-eol/p
    (pure origin)))

(define 6510-program/p
  (do ml-whitespace/p
      [origin <- 6510-program-origin/p]
    ml-whitespace/p
    [opcodes <- (many/p (do [op <- 6510-opcode/p] ml-whitespace/p (pure op)))]
    (pure (list origin opcodes))))

(define (literal-read-syntax src in)
  (let* ([parsed-string (parse-result! (parse-string (syntax/p 6510-program/p) (port->string in)))]
         [origin (first (syntax->datum parsed-string))]
         [parsed-opcodes (last (syntax->datum parsed-string))])
    (with-syntax ([(str ...) parsed-opcodes]
                  [org origin])
      (strip-context
       #'(module anything racket
           (require "6510.rkt")
           (provide program raw-program data)
           ; str ...
           (define program `(,str ...))
           (define raw-program '(str ...))
           (define data (assembler-program (initialize-cpu) org `(,str ...)))
           (displayln "program parsed:")
           (displayln program)
           (displayln raw-program)
           (displayln (replace-labels program org))
           (displayln "program execution:")
           (run (set-pc-in-state data org))
           )))))

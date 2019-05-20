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

(define hex-digit/p (or/p (char/p #\0)
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

(define hex-string/p (do
                         [digits <- (many+/p hex-digit/p)]
                         (pure (list->string digits))))

(define ml-whitespace/p (many/p (or/p (char/p #\newline) space/p)))

(define hex-integer/p (do (char/p #\$)
                          [x <-  hex-string/p]
                        (pure (parse-number-string (string-append "$" x)))))

(define 6510-integer/p (or/p integer/p hex-integer/p))

(define 6510-eol/p (do (many/p space/p)
                       (or/p (do  (char/p #\;)
                                 (many/p (or/p (char-not/p #\newline)
                                               eof/p)))
                             void/p)
                     (or/p (char/p #\newline)
                           eof/p)))

;; immediate, indirect and absolute addressing
(define (imm-ind-abs-opcode/p opcode)
  (do
      (string-ci/p opcode)
      (many/p space/p)
    [immediate <- (or/p void/p (char/p #\#))]
    [x <- 6510-integer/p]
    [appendix <- (or/p (do (char/p #\,)
                           (or/p (string-ci/p "x")
                                 (string-ci/p "y")))
                       void/p)]
    6510-eol/p
    (let* [(immediate-str (if (void? immediate) "" (string immediate)))
           (base-result-lst `(,(string->symbol (string-upcase opcode)) ,(string-append immediate-str (number->string x))))]
      (pure (cond [(void? appendix) base-result-lst]
                  [else (append base-result-lst `(',(string->symbol (string-downcase appendix))))])))
    ))

(define 6510-opcode/p (do (many/p space/p) (or/p (imm-ind-abs-opcode/p "adc"))))

(define 6510-program/p (do ml-whitespace/p
                           (many/p 6510-opcode/p)
                         ))

(define (literal-read-syntax src in)
  (with-syntax ([(str ...) (parse-result! (parse-string (syntax/p 6510-program/p) (port->string in)))])
    (strip-context
     #'(module anything racket
         (require "6510.rkt")
         (provide program raw-program)
         ; str ...
         (define program `(,str ...))
         (define raw-program '(str ...))
         (define data (assembler-program (initialize-cpu) 0 `(,str ...)))
         (displayln "program parsed:")
         (displayln program)
         (displayln raw-program)
         ))))

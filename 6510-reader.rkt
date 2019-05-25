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
    (pure (append (list (string->symbol (string-upcase opcode)))
                  (if (number? x)
                      (list (number->string x))
                      (list ''label-ref-absolute (last x)))))))

(define (opcode/p opcode)
  (do
      (string-ci/p opcode)
      6510-eol/p
    (pure (list (string->symbol (string-upcase opcode))))))

(define (iia-opcode/p opcode)
  (do
      (string-ci/p opcode)
      (many/p space-or-tab/p)
    (or/p (iia-opcode-immediate opcode)
          (or/p (iia-opcode-indirect opcode)
                (iia-opcode-absolute opcode)))))

(define (iia-opcode-immediate opcode)
  (do
      (char/p #\#)
      [x <- 6510-integer/p]
    (pure `(,(string->symbol (string-upcase opcode)) ,(string-append "#" (number->string x))))))

(define (iia-opcode-absolute opcode)
  (do
      [x <- 6510-integer/p]
      [appendix <- (or/p (string-ci/p ",x")
                        (string-ci/p ",y")
                        void/p)]
    (let ([base-result-lst `(,(string->symbol (string-upcase opcode)) ,(number->string x))])
      (if (void? appendix)
          (pure base-result-lst)
          (pure (append base-result-lst `(',(string->symbol (string-downcase appendix))))))
      )))

(define (iia-opcode-indirect opcode)
  (do
      (char/p #\()
      [x <- 6510-integer/p]
    [ind <-  (or/p (string-ci/p "),y")
                  (string-ci/p ",x)"))]
    (if (equal? ind "),y")
        (pure `(,(string->symbol (string-upcase opcode)) < ,(number->string x) > y))
        (pure `(,(string->symbol (string-upcase opcode)) < ,(number->string x) x >)))))

;; immediate, indirect and absolute addressing
(define 6510-opcode/p
  (do (or/p (iia-opcode/p "adc")
            (opcode/p "brk")
            (iia-opcode/p "lda")
            (abs-opcode/p "jsr")
            (opcode/p "rts")
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
           ;; (displayln "program parsed:")
           ;; (displayln program)
           ;; (displayln raw-program)
           ;; (displayln (replace-labels program org))
           (displayln "program execution:")
           (run (set-pc-in-state data org))
           )))))

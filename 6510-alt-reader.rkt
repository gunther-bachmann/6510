#lang racket
(require syntax/strip-context)
(require megaparsack megaparsack/text)
(require (only-in "6510-alt-parser.rkt" 6510-program/p))
(require (rename-in  racket/contract [define/contract define/c]))

(provide (rename-out [literal-read read]
                     [literal-read-syntax read-syntax]))

; usage:
; - create file with content
;     #lang reader "6510-reader.rkt"
;     *=$C000
;     lda #$40
; - C-c to open repl for this source file
; - follow the hints printed

; naming convention:
;   .../p is or returns a parser

(define/c (literal-read in)
  (-> any/c (or/c list? #f))
  (syntax->datum
   (literal-read-syntax #f in)))

;; apply the method 'values' to all list elements
(define (list->values list)
  (apply values list))

;; read and compile assembler program
(define (literal-read-syntax src in)
  (let*-values ([(syntaxed-program) (syntax/p 6510-program/p)]
                [(parsed-string) (parse-string syntaxed-program (port->string in))]
                [(parse-result) (parse-result! parsed-string)]
                [(origin parsed-opcodes) (list->values (syntax->datum parse-result))]
                [(unenc-prg) (syntax-e (last (syntax-e parse-result)))])
    (with-syntax ([(str ...) parsed-opcodes]
                  [(sy-str ...) unenc-prg]
                  [org origin])
      (strip-context
       #`(module compiled6510 racket
           (require "6510-alt.rkt")
           (require "6510-interpreter.rkt")
           (require "6510-prg-generator.rkt")
           (require "6510-dsl-utils.rkt")
           (require "6510-debugger.rkt")
           (provide program
                    raw-program
                    resolved-program 
                    raw-bytes
                    stx-program
                    sy-program)
           (define stx-program (syntax #,unenc-prg)) ;; examine how to pass source location into the generated racket program
           ;; execute this construction of sy-program in the 'with-syntax clause outside this s-expr!! and splice it in here
           (define sy-program `(,(let* ([datum (syntax->datum (syntax sy-str))]
                                        [sy-datum (syntax sy-str)])
                                   (append (list (first datum))
                                           (list `(#:line ,(syntax-line sy-datum)
                                                   #:org-cmd ,(if (and (> (length datum) 1)
                                                                     (list? (second datum)))
                                                                  (last (second datum))
                                                                  (symbol->string (first datum)))))
                                           (drop datum (min (length datum) 2)))) ...))

           (define raw-program '(str ...))
           (define program `(,str ...))
           ;; (define resolved-program (replace-labels program org))
           ;; (define raw-bytes (commands->bytes org `(,str ...)))
           (displayln "(have a look at sy-program, raw-program, resolved-program or raw-bytes)")
           ;; (create-prg (commands->bytes org program) org "test.prg")
           ;; (create-image-with-program (commands->bytes org program) org "test.prg" "test.d64" "test")
           (displayln "execute the program in vice via (run-emulator \"test.d64\")")
           (displayln (format "execute interpreter via (run-interpreter ~a raw-bytes)" org))
           (displayln (format "execute debugger on the program via (run-debugger ~a raw-bytes)" org))
           ;; (run-emulator "test.d64")
           ;; (run-interpreter org raw-bytes)
           ;; (run-debugger org raw-bytes)
           )))))



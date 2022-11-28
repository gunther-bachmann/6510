#lang racket
(require syntax/strip-context)
(require megaparsack megaparsack/text)
(require (only-in "6510-parser.rkt" 6510-program/p))
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
                [(unenc-prg) (syntax-e (last (syntax-e parse-result)))]
                [(f-name) (file-name-from-path src)]
                [(prg-name) (path->string (path-replace-extension f-name ".prg"))]
                [(d64-name) (path->string (path-replace-extension f-name ".d64"))])
    (with-syntax ([(str ...) parsed-opcodes]
                  [(sy-str ...) unenc-prg]
                  [org origin]
                  [d64-name d64-name]
                  [f-name f-name]
                  [prg-name prg-name])
      (strip-context
       #`(module compiled6510 racket
           (require "6510.rkt")
           (require "6510-resolver.rkt")
           (require "6510-relocator.rkt")
           (require "6510-interpreter.rkt")
           (require "6510-prg-generator.rkt")
           (require "6510-debugger.rkt")
           (provide program
                    program-p1
                    program-p2
                    raw-program
                    ;; resolved-program
                    raw-bytes
                    stx-program
                    ;; sy-program
                    )
           (define stx-program (syntax #,unenc-prg)) ;; examine how to pass source location into the generated racket program
           ;; execute this construction of sy-program in the 'with-syntax clause outside this s-expr!! and splice it in here
           ;; (define sy-program `(,(let* ([datum (syntax->datum (syntax sy-str))]
           ;;                              [sy-datum (syntax sy-str)])
           ;;                         (append (list (first datum))
           ;;                                 (list `(#:line ,(syntax-line sy-datum)
           ;;                                         #:org-cmd ,(if (and (> (length datum) 1)
           ;;                                                           (list? (second datum)))
           ;;                                                        (last (second datum))
           ;;                                                        (symbol->string (first datum)))))
           ;;                                 (drop datum (min (length datum) 2)))) ...))

           (define raw-program '(str ...))
           (define program `(,str ...))
           (define program-p1 (->resolved-decisions (label-instructions program) program))
           (define program-p2 (->resolve-labels org (label-string-offsets org program-p1) program-p1 '()))
           (define raw-bytes (resolved-program->bytes program-p2))
           (displayln "(have a look at raw-program, program, program-p1, program-p2 or raw-bytes)")
           (create-prg raw-bytes org prg-name)
           (create-image-with-program raw-bytes org prg-name d64-name (path->string (path-replace-extension f-name "")))
           (displayln (format "execute the program in vice via (run-emulator \"~a\")" d64-name))
           (displayln (format "execute interpreter via (run-interpreter ~a raw-bytes)" org))
           (displayln (format "execute debugger on the program via (run-debugger ~a raw-bytes)" org))
           ;; (run-emulator "test.d64")
           ;; (run-interpreter org raw-bytes)
           ;; (run-debugger org raw-bytes)
           )))))



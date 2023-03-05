#lang racket
#|

 provide a reader for 6510 syntax, making heavy use of the parser

 |#

(require syntax/strip-context)
(require megaparsack megaparsack/text)
(require (only-in "6510-parser.rkt" 6510-program/p))
(require (only-in "root.rkt" project-root))
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

(define (steps-to-project-root a-path steps)
  (define project-root-str (path->string project-root))
  (define a-path-str       (path->string a-path))
  (when (< (string-length a-path-str) (string-length project-root-str))
    (raise-user-error "folder is not a subfolder of the project root!"))
  (if (string=? project-root-str a-path-str)
      steps
      (steps-to-project-root (simplify-path (path->complete-path ".." a-path)) (add1 steps))))

;; read and compile assembler program
(define (literal-read-syntax src in)
  (let*-values ([(syntaxed-program) (syntax/p 6510-program/p)]
                [(parsed-string) (parse-string syntaxed-program (port->string in))]
                [(parse-result) (parse-result! parsed-string)]
                [(origin parsed-opcodes) (list->values (syntax->datum parse-result))]
                [(unenc-prg) (syntax-e (last (syntax-e parse-result)))]
                [(f-name) (file-name-from-path src)]
                [(prg-name) (path->string (path-replace-extension f-name ".prg"))]
                [(d64-name) (path->string (path-replace-extension f-name ".d64"))]
                [(steps-to-root) (steps-to-project-root src -1)]
                [(folder-prefix) (if (< 0 steps-to-root)
                                     (string-join (make-list steps-to-root "..") "/")
                                     ".")]
                [(6510-folder) (string-append folder-prefix "/src/6510.rkt")]
                [(6510-resolver-folder) (string-append folder-prefix "/src/ast/6510-resolver.rkt")]
                [(6510-relocator-folder) (string-append folder-prefix "/src/ast/6510-relocator.rkt")]
                [(6510-interpreter-folder) (string-append folder-prefix "/src/tools/6510-interpreter.rkt")]
                [(6510-prg-generator-folder) (string-append folder-prefix "/src/6510-prg-generator.rkt")]
                [(6510-debugger-folder) (string-append folder-prefix "/src/tools/6510-debugger.rkt")]
                [(6510-constants-folder) (string-append folder-prefix "/src/ast/6510-constants.rkt")])
    (with-syntax ([(str ...) parsed-opcodes]
                  [(sy-str ...) unenc-prg]
                  [org origin]
                  [d64-name d64-name]
                  [f-name f-name]
                  [prg-name prg-name]
                  [6510-folder 6510-folder]
                  [6510-resolver-folder 6510-resolver-folder]
                  [6510-relocator-folder 6510-relocator-folder]
                  [6510-interpreter-folder 6510-interpreter-folder]
                  [6510-prg-generator-folder 6510-prg-generator-folder]
                  [6510-debugger-folder 6510-debugger-folder]
                  [6510-constants-folder 6510-constants-folder])
      (strip-context
       #`(module compiled6510 racket
           (require 6510-folder)
           (require 6510-resolver-folder)
           (require 6510-relocator-folder)
           (require 6510-interpreter-folder)
           (require 6510-prg-generator-folder)
           (require 6510-debugger-folder)
           (require 6510-constants-folder)
           (provide program
                    program-p1
                    program-p2
                    raw-program
                    org-program
                    raw-bytes
                    stx-program)
           (define stx-program (syntax #,unenc-prg))
           (define org-program '(sy-str ...))
           (define raw-program '(str ...))
           (define program `(,str ...))
           (define program-p1 (->resolved-decisions (label-instructions program) program))
           (define program-p2 (->resolve-labels org (label-string-offsets org program-p1) program-p1 '()))
           (define program-p3 (resolve-constants (constant-definitions-hash program-p1) program-p2))
           (define raw-bytes (resolved-program->bytes program-p3))
           (displayln 6510-folder)
           (displayln "(have a look at raw-program, program, program-p1, program-p2, program-p3 or raw-bytes)")
           (create-prg raw-bytes org prg-name)
           (create-image-with-program raw-bytes org prg-name d64-name (path->string (path-replace-extension f-name "")))
           (displayln (format "execute the program in vice via (run-emulator \"~a\")" d64-name))
           (displayln (format "execute interpreter via (run-interpreter ~a raw-bytes)" org))
           (displayln (format "execute debugger on the program via (run-debugger ~a raw-bytes)" org))

           ;; (run-emulator "test.d64")
           ;; (run-interpreter org raw-bytes)
           ;; (run-debugger org raw-bytes)
           )))))



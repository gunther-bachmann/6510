#lang racket

(provide create-source-map
         create-source-map-for-debug)

#|

 generate sourcemaps usable by debugger and/or other tools

 |#

(require (rename-in  racket/contract [define/contract define/c]))
(require (only-in "../6510-utils.rkt" word/c))
(require (only-in "../ast/6510-command.rkt" ast-command? ast-command-meta-information ast-opcode-cmd))
(require (only-in "../ast/6510-resolver.rkt" resolved-instruction->bytes))
(require (only-in "../ast/6510-assembler.rkt"
                  assembly-code-list?
                  assembly-code-list-org-code-sequences))

(module+ test
  (require rackunit))

(define/c (-create-source-map org program (file-name "") (result (list)) )
  (->* [word/c (listof ast-command?)] [string? list?] list?)
  (if (empty? program)
      (reverse result)
      (let* [(cmd (car program))
             (meta-information (ast-command-meta-information cmd))
             (bytes (resolved-instruction->bytes cmd))
             (len-bytes (length bytes))]
        (if (> len-bytes 0)
            (-create-source-map
             (+ org (length bytes))
             (cdr program)
             file-name
             (cons (append meta-information `(#:pc ,org) (if (< 0 (string-length file-name)) `(#:filename ,file-name) (list)))
                   result))
            (-create-source-map org (cdr program) file-name result)))))

(module+ test
  (check-equal? (-create-source-map
                 1000
                 (list (ast-opcode-cmd '(#:line 10 #:orig-command "lda #$20") '(10 20))
                       (ast-opcode-cmd '() '(20))
                       (ast-opcode-cmd '() '())
                       (ast-opcode-cmd '(#:line 12 #:orig-command "jmp $ffd2") '(40 #xd2 #xff)))
                 "file.rkt"
                 (list))
                (list '(#:line 10 #:orig-command "lda #$20" #:pc 1000 #:filename "file.rkt")
                      '(#:pc 1002 #:filename "file.rkt")
                      '(#:line 12 #:orig-command "jmp $ffd2" #:pc 1003 #:filename "file.rkt"))))

(define/c (create-source-map raw-bytes org f-name program )
  (-> (listof byte?) word/c string? (listof ast-command?) void?)
  (define source-map-lines (-create-source-map org program f-name))
  (define map-name (string-join (list f-name "map") "."))
  (display-to-file ";; mapping pc to source code line\n" map-name #:mode 'binary #:exists 'replace)
  (for ([id source-map-lines])
    (write-to-file id map-name #:mode 'text #:exists 'append)
    (display-to-file "\n" map-name #:mode 'binary #:exists 'append)))

(define/c (create-source-map-for-debug code-list)
  (-> assembly-code-list? any/c)
  (define map-name "debug-session.map")
  (display-to-file ";; mapping pc to source code line\n" map-name #:mode 'binary #:exists 'replace)
  (map (lambda (assembly-code)
         (define source-map-lines (-create-source-map (car assembly-code) (cdr assembly-code)))
         (for ([id source-map-lines])
           (write-to-file id map-name #:mode 'text #:exists 'append)
           (display-to-file "\n" map-name #:mode 'binary #:exists 'append)))
       (assembly-code-list-org-code-sequences code-list)))

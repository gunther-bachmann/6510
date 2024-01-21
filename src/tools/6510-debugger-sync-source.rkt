#lang racket

#|

 allow sync of source buffer (in emacs) with debuging steps

|#

(require (rename-in  racket/contract [define/contract define/c]))
(require (only-in "../6510-utils.rkt" word/c byte/c word->hex-string))

(provide
 load-source-map
 overlay-source
 remove-overlay-source
 -instrument-source-with-address ;; TODO remove
 instrument-source-with-address
 remove-source-addresses
 (struct-out pc-source-map-entry))

(struct pc-source-map-entry (line source-code file)
  #:guard (struct-guard/c nonnegative-integer? string? string?))

(module+ test
  (require rackunit))

(define/c (load-source-map file-name)
  (-> string? (hash/c nonnegative-integer? pc-source-map-entry?))
  ;; TODO: read map file from .rkt.map, use something like: (read (open-input-string "(+ (- 5 2))"))
  (with-input-from-file (format "./~a.map" file-name)
    (thunk
     (let ([result (make-hash)])
       (for ([line (in-lines)])
         (let ([parsed-line (read (open-input-string line))])
           (when (list? parsed-line)
             (let ([pc-mark (memq '#:pc parsed-line)]
                   [line-mark (memq '#:line parsed-line)]
                   [org-cmd-mark (memq '#:org-cmd parsed-line)])
               (when (and pc-mark line-mark)
                 (hash-set! result
                           (cadr pc-mark)
                           (pc-source-map-entry
                            (cadr line-mark)
                            (if org-cmd-mark (cadr org-cmd-mark) "")
                            file-name)))))))
       result))))
    ;; (hash 2067 (pc-source-map-entry 20 "sout:   lda hello,x" file-name))

(define/c (-instrument-source-with-address file-name pc line)
  (-> string? word/c nonnegative-integer? any/c)
  (define pc-str (format "~a " (word->hex-string pc)))
  (parameterize ([current-output-port (open-output-nowhere)])
    (system* (find-executable-path "emacsclient") "-e" (format "(6510-debugger--overlay-source-line \"~a\" ~a \"~a\")" file-name (add1 line) pc-str))))

(define/c (remove-source-addresses file-name)
  (-> string? any/c)
  (parameterize ([current-output-port (open-output-nowhere)])
    (system* (find-executable-path "emacsclient") "-e" (format "(6510-debugger--remove-overlay-source-lines \"~a\")" file-name))))

(define/c (instrument-source-with-address file-name source-map)
  (-> string? (hash/c nonnegative-integer? pc-source-map-entry?) any/c)
  (for ([(pc entry) source-map])
    (-instrument-source-with-address file-name pc (pc-source-map-entry-line entry))))

;; display an overlay on the given source file at position of the program counter
(define/c (overlay-source file-name line disassembled)
  (-> string? nonnegative-integer? string? any/c)
  (parameterize ([current-output-port (open-output-nowhere)])
    (system* (find-executable-path "emacsclient") "-e" (format "(6510-debugger--overlay-source \"~a\" ~a \"~a\")" file-name (add1 line) disassembled))))

(define/c (remove-overlay-source file-name)
  (-> string? any/c)
  (parameterize ([current-output-port (open-output-nowhere)])
    (system* (find-executable-path "emacsclient") "-e" (format "(6510-debugger--remove-overlay-source \"~a\")" file-name))))

#lang racket

#|

 allow sync of source buffer (in emacs) with debuging steps

|#

(require (rename-in  racket/contract [define/contract define/c]))
(require (only-in "../6510-utils.rkt" word/c byte/c word->hex-string))
(require (only-in "./6510-emacs-integration.rkt" 6510-debugger--execute-elisp-expression))

(provide
 load-source-map
 6510-debugger--remove-all-addresses-on-source
 6510-debugger--show-disassembly-on-source-lines
 6510-debugger--show-address-on-source-lines
 6510-debugger--move-cursor-to-source-line
 6510-debugger--remove-disassembly-on-source-lines

 (struct-out pc-source-map-entry))

(struct pc-source-map-entry (line source-code file)
  #:guard (struct-guard/c nonnegative-integer? string? string?))

(module+ test
  (require rackunit))

(define elisp-function-remove-disassembly-on-source-lines "6510-debugger--remove-disassembly-on-source-lines")
(define elisp-function-show-disassembly-on-source-line "6510-debugger--show-disassembly-on-source-lines")
(define elisp-function-move-cursor-to-source-line "6510-debugger--move-cursor-to-source-line")
(define elisp-function-remove-all-addresses-on-source "6510-debugger--remove-all-overlay")
(define elisp-function-show-address-on-source-line "6510-debugger--show-address-on-source-line")
;; (define elisp-function-remove-highlighted-execution-line "6510-debugger--remove-highlighted-execution-line")

;; load a source map for the given file, returning a hashmap mapping addresses to map entries
(define/c (load-source-map file-name)
  (-> string? (hash/c nonnegative-integer? pc-source-map-entry?))
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

;; instrument a single source line with a prefix of the address
(define/c (-show-address-on-source-line file-name pc line)
  (-> string? word/c nonnegative-integer? any/c)
  (define pc-str (format "~a " (word->hex-string pc)))
  (6510-debugger--execute-elisp-expression
   (format "(~a \"~a\" ~a \"~a\")"
           elisp-function-show-address-on-source-line
           file-name
           (add1 line)
           pc-str)))

;; remove all address overlays from the source file
(define/c (6510-debugger--remove-disassembly-on-source-lines file-name)
  (-> string? any/c)
  (6510-debugger--execute-elisp-expression
   (format "(~a \"~a\")"
           elisp-function-remove-disassembly-on-source-lines
           file-name)))

;; instrument all source lines with a prefix of the corresponding address
(define/c (6510-debugger--show-address-on-source-lines file-name source-map)
  (-> string? (hash/c nonnegative-integer? pc-source-map-entry?) any/c)
  (for ([(pc entry) source-map])
    (-show-address-on-source-line file-name pc (pc-source-map-entry-line entry))))

(define/c (6510-debugger--move-cursor-to-source-line file-name line)
  (-> string? nonnegative-integer? any/c)
  (6510-debugger--execute-elisp-expression
   (format "(~a \"~a\" ~a)"
           elisp-function-move-cursor-to-source-line
           file-name
           (add1 line))))

;; display an overlay on the given source file at position of the program counter
(define/c (6510-debugger--show-disassembly-on-source-lines file-name line disassembled)
  (-> string? nonnegative-integer? string? any/c)
  (6510-debugger--execute-elisp-expression
   (format "(~a \"~a\" ~a \"~a\")"
           elisp-function-show-disassembly-on-source-line
           file-name
           (add1 line)
           disassembled)))

;; remove all source overlays (disassemblies)
(define/c (6510-debugger--remove-all-addresses-on-source file-name)
  (-> string? any/c)
  (6510-debugger--execute-elisp-expression
   (format "(~a \"~a\")"
           elisp-function-remove-all-addresses-on-source
           file-name)))

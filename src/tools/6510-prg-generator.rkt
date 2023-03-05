#lang racket
#|

 generate .prg, .d64 file for c64 vice execution

 |#


(require (only-in "../6510-utils.rkt" high-byte low-byte))
(require (rename-in  racket/contract [define/contract define/c]))

(provide
         create-prg
         run-emulator
         create-image-with-program)

(module+ test
  (require rackunit))

; (run (assembler-program (initialize-cpu) 0 (list (LDA_i #x41) (JSR_abs #xFFFF) (BRK))))


;; 0801 0c 08 ; next line number at 080c
;; 0803 0a 00 ; line number 10
;; 0805 158 ; sys
;; 0806 32 ; space
;; 0807 50 48 54 52 00 ; string "2064"
;; 080c 00 00 ; next line 0000 = end of program
(define c64-basic-loader-at-0801 '(#x0c #x08 #x0a #x00 #x9e #x20 #x32 #x30 #x36 #x34 #x00 #x00 #x00 #x00 #x00))

(define (create-prg program org file-name)
  (define org-with-basic-loader (- org (length c64-basic-loader-at-0801)))
  (display-to-file (list->bytes (append `(,(low-byte org-with-basic-loader) ,(high-byte org-with-basic-loader)) c64-basic-loader-at-0801 program))
                   file-name
                   #:mode 'binary
                   #:exists 'replace))

(define (run-emulator file-name)
  (system (string-append "LD_LIBRARY_PATH=  x64sc -8 " file-name)))

(define (create-d64 name)
  (system (string-append "c1541 -format \"" name ",01\" d64 " name)))

(define (add-prg-to-d64 prg-file-name d64 target-name)
  (system (string-append "c1541 -attach " d64 " -write " prg-file-name " " target-name)))

(define (create-image-with-program program org file-name d64 target-name)
  (create-prg program org file-name)
  (create-d64 d64)
  (add-prg-to-d64 file-name d64 target-name))

#lang racket
#|

 generate .prg, .d64 file for c64 vice execution

 requires the following external programs to exist:
   c1541 : tool to create d64 images
   x64sc : vice c64 emulator

|#


(require (only-in "../6510-utils.rkt" high-byte low-byte byte/c word/c))
(require (rename-in  racket/contract [define/contract define/c]))

(provide
         create-prg                ;; create prg file with basic loader (currently org must be #x0810 to work with basic loader)
         run-emulator              ;; run vice on some generated d64 image
         create-image-with-program ;; create a d64 image with prg file
         )

(module+ test
  (require rackunit))

; (run (assembler-program (initialize-cpu) 0 (list (LDA_i #x41) (JSR_abs #xFFFF) (BRK))))


;; 0801 0c 08 ; next line number at 080c
;; 0803 0a 00 ; line number 10
;; 0805 158 ; sys
;; 0806 32 ; space
;; 0807 50 48 54 52 00 ; string "2064"
;; 080c 00 00 ; next line 0000 = end of program
;; 080e 00 00 ; just padding 
(define c64-basic-loader-at-0801 '(#x0c #x08 #x0a #x00 #x9e #x20 #x32 #x30 #x36 #x34 #x00 #x00 #x00 #x00 #x00))
(define org-of-basic-loader #x0801)

;; create prg file with basic loader (currently org must be #x0810 to work with basic loader)
(define/c (create-prg program org file-name)
  (-> (listof byte/c) word/c string? void?)
  (display-to-file (list->bytes
                    (append (list (low-byte org-of-basic-loader) (high-byte org-of-basic-loader))
                            c64-basic-loader-at-0801
                            (make-list (- org (+ org-of-basic-loader (length c64-basic-loader-at-0801))) 0)
                            program))
                   file-name
                   #:mode 'binary
                   #:exists 'replace))

;; run vice on some generated d64 image
(define/c (run-emulator file-name)
  (-> string? boolean?)
  (system (string-append "LD_LIBRARY_PATH=  x64sc -8 " file-name)))

(define/c (create-d64 name)
  (-> string? boolean?)
  (system (string-append "c1541 -format \"" name ",01\" d64 " name " >/dev/null")))

(define/c (add-prg-to-d64 prg-file-name d64 target-name)
  (-> string? string? string? boolean?)
  (system (string-append "c1541 -attach " d64 " -write " prg-file-name " " target-name " >/dev/null")))

;; create a d64 image with prg file
(define/c (create-image-with-program program org file-name d64 target-name)
  (-> (listof byte/c) word/c string? string? string? boolean?)
  (create-prg program org file-name)
  (create-d64 d64)
  (add-prg-to-d64 file-name d64 target-name))

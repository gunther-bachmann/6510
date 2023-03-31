#lang racket

(require "../../6510.rkt")
(require "../mil-6510-commands.rkt")

(provide MILRT_INIT_CONS_CELL_PAGE)

(module+ test
  (require "../../6510-test-utils.rkt"))

(define MILRT_ZP_CONSTANTS
  (list
   (byte-const page-status     #x01) ;; 0-byte on page (to identify page)
   (byte-const temp-ptr-1      #x07)
   (byte-const temp-ptr-1-high #x08)))

;; initialize a page (high-byte passed in A) 
;; it is filled with the given memory layout
;;
;; [page-high-byte][00]       : page status byte
;; [page-high-byte][01]       : reference count cons cell 0
;; [page-high-byte][02]       : reference count cons cell 1
;; [page-high-byte][03]       : reference count cons cell 2
;;                 [04]..[07]  : cons cell 0
;;                 [08]..[0b]  : cons cell 1
;;                 [0c]..[0f]  : cons cell 2
;;                 [10]       : reference count cons cell 3
;;                 [11]..[3f]  : reference count cons cell 4 .. 50
;;                 [40]..[43]  : cons cell 3
;;                 [44]..[47]  : cons cell 4
;;                 ...
;;                 [fc]..[ff]  : cons cell 50
;;
;; * temp-ptr-1 = pointer for initializing the page
;; A = high byte of next cons cell page (page-high-byte)
;; X *
;; Y *
(define MILRT_INIT_CONS_CELL_PAGE
  (list
                        (STA temp-ptr-1-high)
                        (LDA !$0)
                        (STA temp-ptr-1)
                        (LDY !$FF)
                        (LDA !0)
    (label _FILL0)
                        (STA (temp-ptr-1),y)
                        (DEY)
                        (BNE _FILL0)

                        ;; set page status on first byte of the page
                        (LDA !page-status)
                        (STA (temp-ptr-1),y)

                        ;; fill first 3 cells (special)

                        ;; now 0000 0100, 0000 0101 <- 0000 1000, high-byte
                        (LDY !$04)
                        (LDA !$08) ;; point to next cell at 0000 1000
                        (STA (temp-ptr-1),y)
                        (INY)
                        (LDA temp-ptr-1-high)
                        (STA (temp-ptr-1),y)
                        ;; next 0000 1000, 0000 1001 <- 0000 1100, high-byte
                        (LDY !$08)
                        (LDA !$0c) ;; point to next cell at 0000 1100
                        (STA (temp-ptr-1),y)
                        (INY)
                        (LDA temp-ptr-1-high)
                        (STA (temp-ptr-1),y)
                        ;; next 0000 1100, 0000 1101 <- 0100 000, high byte
                        (LDY !$0c)
                        (LDA !$40) ;; point to next cell at 0100 0000
                        (STA (temp-ptr-1),y)
                        (TAX)
                        (INY)
                        (LDA temp-ptr-1-high)
                        (STA (temp-ptr-1),y)
                        ;; next 0100 0000, 0100 0001 <- 0100 0100, high byte

                        ;; fill the 48 other cells starting at 0100 0000
    (label _loop_init_cons)
                        (TXA)
                        (TAY)
                        (CLC)
                        (ADC !$04) ;; next cell at this+4
                        (STA (temp-ptr-1),y) ;; point to next cell
                        (TAX) ;; keep ptr to next cell in x
                        (INY)
                        (LDA temp-ptr-1-high)
                        (STA (temp-ptr-1),y) ;; point to next cell high byte
                        (CPX !$FC) ;; was last cell to point to next
                        
                        (BNE _loop_init_cons)

                        ;; last cell marked with two zeros
                        (RTS)))

(module+ test
  (require (only-in "../../tools/6510-interpreter.rkt" memory-list run-interpreter))

  (define org 2064)
  (define program (append (list (LDA !$c0)) MILRT_ZP_CONSTANTS MILRT_INIT_CONS_CELL_PAGE))

  ;; ensure that all cons-cells LSR two times point to the ref count cell
  (check-equal? (arithmetic-shift #x04 -2) #x01)
  (check-equal? (arithmetic-shift #x08 -2) #x02)
  (check-equal? (arithmetic-shift #x0c -2) #x03)

  (check-equal? (arithmetic-shift #x40 -2) #x10)
  (check-equal? (arithmetic-shift #x44 -2) #x11)
  (check-equal? (arithmetic-shift #x48 -2) #x12)
  (check-equal? (arithmetic-shift #x4c -2) #x13)

  (check-equal? (arithmetic-shift #xf0 -2) #x3c)
  (check-equal? (arithmetic-shift #xf4 -2) #x3d)
  (check-equal? (arithmetic-shift #xf8 -2) #x3e)
  (check-equal? (arithmetic-shift #xfc -2) #x3f)
  
  (check-equal? (memory-list (run-interpreter org program #f)
                             #xc000 #xc00f) 
                (list 1                   ;; page status
                      0 0 0               ;; rc 0..2
                      #x08 #xc0 0 0       ;; car (next free cell), cdr 0
                      #x0c #xc0 0 0 
                      #x40 #xc0 0 0))

  (check-equal? (memory-list (run-interpreter org program #f)
                             #xc010 #xc03f) 
                (list 0 0 0 0              ;; rc 3..6
                      0 0 0 0              ;;    7..10
                      0 0 0 0              ;;   11..14
                      0 0 0 0              ;;   15..18
                      0 0 0 0              ;;   19..22
                      0 0 0 0              ;;   23..26
                      0 0 0 0              ;;   27..30
                      0 0 0 0              ;;   31..34
                      0 0 0 0              ;;   35..38
                      0 0 0 0              ;;   39..42
                      0 0 0 0              ;;   43..46
                      0 0 0 0              ;;   47..50
                      ))

  (check-equal? (memory-list (run-interpreter org program #f)
                             #xc040 #xc04f) 
                (list #x44 #xc0 0 0       ;; car (next free cell), cdr 3
                      #x48 #xc0 0 0       ;; car (next free cell), cdr 4
                      #x4c #xc0 0 0       ;; car (next free cell), cdr 5
                      #x50 #xc0 0 0       ;; car (next free cell), cdr 6
                      ))
  (check-equal? (memory-list (run-interpreter org program #f)
                             #xc0f0 #xc0ff) 
                (list #xf4 #xc0 0 0       ;; car (next free cell), cdr 47
                      #xf8 #xc0 0 0       ;; car (next free cell), cdr 48
                      #xfc #xc0 0 0       ;; car (next free cell), cdr 49
                      #x00 #x00 0 0       ;; car (NO next free cell), cdr 50
                      )))


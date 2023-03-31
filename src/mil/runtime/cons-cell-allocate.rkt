#lang racket

(require "../../6510.rkt")
(require "../../ast/6510-calc-opcode-facades.rkt")

(provide MILRT_ALLOCATE_CONS_CELL MILRT_ALLOCATE_CONS_CELL_C)

(module+ test
  (require "../../6510-test-utils.rkt"))

(define MILRT_ALLOCATE_CONS_CELL_C
  (list
   (byte-const page-status                   #x01) ;; 0-byte on page (to identify page)
   (byte-const free-cons-cell-ptr            #x3b) ;; next free cell
   (byte-const free-cons-cell-ptr-high       #x3c)
   (byte-const high-byte-next-free-cons-page #x3f)
   (byte-const cons-cell-ptr                 #x40) ;; next free cell
   (byte-const cons-cell-ptr-high            #x41)
   (byte-const high-byte-expression-stack    #x0e)))


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
(define MILRT_ALLOCATE_CONS_CELL
  (list
   (label MILRT_ALLOCATE_CONS_CELL)
                           (LDA free-cons-cell-ptr)
                           (STA cons-cell-ptr)
                           (LDA free-cons-cell-ptr-high)
                           (STA cons-cell-ptr-high)
                        
                           (LDY !0)
                           (LDA (cons-cell-ptr),y) ;; cdr of allocated cons cell
                           (BEQ MILRT_ALLOC_CONS_CELL_c1)
                           (STA free-cons-cell-ptr)
                           (INY)
                           (LDA (cons-cell-ptr),y)
                           (STA free-cons-cell-ptr-high)
                           (RTS)

   (label MILRT_ALLOC_CONS_CELL_c1)
                           (DEC high-byte-next-free-cons-page) ;; grow downwards
                           (LDA high-byte-next-free-cons-page)
                           (CMP high-byte-expression-stack)
                           (BEQ MILRT_ALLOC_CONS_CELL_OUT_OF_MEM)

                           (JSR MILRT_INIT_CONS_CELL_PAGE)
                           (STA free-cons-cell-ptr-high)
                           (LDA !4) ;; first free cell is located at byte 4
                           (STA free-cons-cell-ptr)
                           (RTS)

   (label MILRT_ALLOC_CONS_CELL_OUT_OF_MEM) ;;out of memory
                            (LDA-immediate (char->integer #\O))
                            (JMP $FFD2)))

(module+ test
  (require (only-in "../../tools/6510-interpreter.rkt" memory-list run-interpreter))
  (require "./cons-cell-page-init.rkt")

  (define org 2064)
  (define init-page-allocate-first
    (append MILRT_ALLOCATE_CONS_CELL_C
            (list (LDA !$c0) ;; create page at c0
                  (JSR MILRT_INIT_CONS_CELL_PAGE)
                  (STA free-cons-cell-ptr-high)
                  (LDA !4) ;; first free cell is located at byte 4
                  (STA free-cons-cell-ptr)
                  (JMP MILRT_ALLOCATE_CONS_CELL))
            MILRT_INIT_CONS_CELL_PAGE_C
            MILRT_INIT_CONS_CELL_PAGE
            MILRT_ALLOCATE_CONS_CELL))

  ;; (require "../../tools/6510-debugger.rkt")
  ;;(run-debugger org init-page-allocate-first)  
  
  (check-equal? (memory-list (run-interpreter org init-page-allocate-first #f)
                             #x0040 #x0041)
                (list #x04 #xc0)
                "cons-cell register holds pointer to first free cons cell allocated")
  (check-equal? (memory-list (run-interpreter org init-page-allocate-first #f)
                             #x003b #x003c)
                (list #x08 #xc0)
                "after allocation, next free cons cell is referenced by free list") 
  (skip "check that allocation after three will use 'higher' cons cells" #f)
  (skip "check that allocation of last cell will init new page" #f)
  (skip "check that after alloc last and init new page, out of memory is written, if expression stack is on that page" #f)
  ) 


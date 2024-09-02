#lang racket

(require "../../6510.rkt")
(require "../../ast/6510-calc-opcode-facades.rkt")

(provide MILRT_ALLOCATE_CONS_CELL MILRT_ALLOCATE_CONS_CELL_C)

(module+ test
  (require "../../6510-test-utils.rkt"))

(define MILRT_ALLOCATE_CONS_CELL_C
  (list
   (byte-const page_status                   #x01) ;; 0-byte on page (to identify page)
   (byte-const free_cons_cell_ptr            #x3b) ;; next free cell
   (byte-const free_cons_cell_ptr_high       #x3c)
   (byte-const high_byte_next_free_cons_page #x3f)
   (byte-const cons_cell_ptr                 #x40) ;; next free cell
   (byte-const cons_cell_ptr_high            #x41)
   (byte-const high_byte_expression_stack    #x0e)))


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
                           (LDA free_cons_cell_ptr)
                           (STA cons_cell_ptr)
                           (LDA free_cons_cell_ptr_high)
                           (STA cons_cell_ptr_high)
                        
                           (LDY !0)
                           (LDA (cons_cell_ptr),y) ;; cdr of allocated cons cell
                           (BEQ MILRT_ALLOC_CONS_CELL_c1)
                           (STA free_cons_cell_ptr)
                           (INY)
                           (LDA (cons_cell_ptr),y)
                           (STA free_cons_cell_ptr_high)
                           (RTS)

   (label MILRT_ALLOC_CONS_CELL_c1)
                           (DEC high_byte_next_free_cons_page) ;; grow downwards
                           (LDA high_byte_next_free_cons_page)
                           (CMP high_byte_expression_stack)
                           (BEQ MILRT_ALLOC_CONS_CELL_OUT_OF_MEM)

                           (JSR MILRT_INIT_CONS_CELL_PAGE)
                           (STA free_cons_cell_ptr_high)
                           (LDA !4) ;; first free cell is located at byte 4
                           (STA free_cons_cell_ptr)
                           (RTS)

   (label MILRT_ALLOC_CONS_CELL_OUT_OF_MEM) ;;out of memory
                            (LDA-immediate (char->integer #\O))
                            (JMP $FFD2)))

(module+ test
  (require (only-in "../../tools/6510-interpreter.rkt" memory-list run-interpreter cpu-state-accumulator))
  (require "./cons-cell-page-init.rkt")

  (define org 2064)
  (define init-page-allocate-first
    (append MILRT_ALLOCATE_CONS_CELL_C
            (list (LDA !$c1) ;; create page at c0
                  (JSR MILRT_INIT_CONS_CELL_PAGE)
                  (STA free_cons_cell_ptr_high)
                  (LDA !4) ;; first free cell is located at byte 4
                  (STA free_cons_cell_ptr)
                  (JMP MILRT_ALLOCATE_CONS_CELL))
            MILRT_INIT_CONS_CELL_PAGE_C
            MILRT_INIT_CONS_CELL_PAGE
            MILRT_ALLOCATE_CONS_CELL))

  (define init-page-allocate-3rd
    (append MILRT_ALLOCATE_CONS_CELL_C
            (list (LDA !$c1) ;; create page at c0
                  (JSR MILRT_INIT_CONS_CELL_PAGE)
                  (STA free_cons_cell_ptr_high)
                  (LDA !$0c) ;; third free cell is located at byte 0c
                  (STA free_cons_cell_ptr)
                  (JMP MILRT_ALLOCATE_CONS_CELL))
            MILRT_INIT_CONS_CELL_PAGE_C
            MILRT_INIT_CONS_CELL_PAGE
            MILRT_ALLOCATE_CONS_CELL))

  (define init-page-allocate-last
    (append MILRT_ALLOCATE_CONS_CELL_C
            (list (LDA !$c1) ;; create page at c0
                  (STA high_byte_next_free_cons_page)
                  (JSR MILRT_INIT_CONS_CELL_PAGE)
                  (STA free_cons_cell_ptr_high)
                  (LDA !$fc) ;; last free cell is located at fc
                  (STA free_cons_cell_ptr)
                  (JMP MILRT_ALLOCATE_CONS_CELL))
            MILRT_INIT_CONS_CELL_PAGE_C
            MILRT_INIT_CONS_CELL_PAGE
            MILRT_ALLOCATE_CONS_CELL))

  (define init-page-allocate-last-failing
    (append MILRT_ALLOCATE_CONS_CELL_C
            (list (LDA !$c1) ;; create page at c0
                  (STA high_byte_next_free_cons_page)
                  (STA high_byte_expression_stack)
                  (DEC high_byte_expression_stack)
                  (JSR MILRT_INIT_CONS_CELL_PAGE)
                  (STA free_cons_cell_ptr_high)
                  (LDA !$fc) ;; last free cell is located at fc
                  (STA free_cons_cell_ptr)
                  (JMP MILRT_ALLOCATE_CONS_CELL))
            MILRT_INIT_CONS_CELL_PAGE_C
            MILRT_INIT_CONS_CELL_PAGE
            MILRT_ALLOCATE_CONS_CELL))

  ;; (require "../../tools/6510-debugger.rkt")
  ;;(run-debugger org init-page-allocate-first)  
  
  (check-equal? (memory-list (run-interpreter org init-page-allocate-first #f)
                             #x0040 #x0041)
                (list #x04 #xc1)
                "cons-cell register holds pointer to first free cons cell allocated")
  (check-equal? (memory-list (run-interpreter org init-page-allocate-first #f)
                             #x003b #x003c)
                (list #x08 #xc1)
                "after allocation, next free cons cell is referenced by free list")
  (check-equal? (memory-list (run-interpreter org init-page-allocate-3rd #f)
                             #x003b #x003c)
                (list #x40 #xc1)
                "check that allocation after three will use 'higher' cons cells")
  (check-equal? (memory-list (run-interpreter org init-page-allocate-last #f)
                             #x003b #x003c)
                (list #x04 #xc0)
                "check that allocation of last cell will init new page")
  (check-equal? (memory-list (run-interpreter org init-page-allocate-last #f)
                             #x003f #x003f)
                (list #xc0)
                "check that allocation of last cell will init new page")
  (check-equal? (cpu-state-accumulator (run-interpreter org init-page-allocate-last-failing #f))
                (char->integer #\O)
                "check that after alloc last and init new page, out of memory is written, if expression stack is on that page"))

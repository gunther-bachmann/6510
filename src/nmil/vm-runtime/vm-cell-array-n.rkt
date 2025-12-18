#lang racket/base

(provide
 WRITE_ARR_ATa_RA_TO_RT          ;; write from cell array RA: cell at index A into RT (replacing what has been in RT)
 WRITE_ARR_ATa_RT_TO_RT          ;; write from cell array RT: cell at index A into RT (overwriting it)
 WRITE_RT_TO_ARR_ATa_RA
 WRITE_RP_TO_ARR_AT0_RT          ;; overwrite rt, but rt is put into cell0 of freshly allocated cell-pair => no refcnt mod needed here
 POP_CELL_EVLSTK_TO_ARR_AT1_RT
 ALLOC_CELL_ARRAY_TO_RT
 ;; FREE_CELL_ARRAY_RZ
 ;; INIT_CELL_ARRAY_RA_WITH_RT      ;; fill array with cell that currently is in RT
)

#|
 provide functions to access cell-arrays.


 - for lists (cell-arrays of size 2) it is common to use RT as array ptr to access elements (0,1) during list function execution e.g. car

 - for regular array access of cell arrays, it is more common to use the array register, since there might be multiple accesses to that array
   and it is unwanted to have the pointer to that array "overwritten" on every get/put


 layout of cell-arrays:
   | offset | content
   |--------|-------------------
   | 0      | reference count
   | 1      | 00xx xxxx size of array (during gc slightly different semantics)
   | 2..3    | cell @0
   | 4..5    | cell @1
   | ...     |

 |#

(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
         (only-in "./vm-cell-stack.rkt"
                  POP_CELL_EVLSTK_TO_RP)
         (only-in "./vm-m1-slots-n.rkt"
                  ALLOC_M1_SLOT_TO_RT_N)
         (only-in "./vm-memory-map.rkt"
                  TAGGED_NIL
                  TAG_BYTE_CELL_ARRAY
                  ZP_RP
                  ZP_RA
                  ZP_TEMP
                  ZP_RT
                  VM_MEMORY_MANAGEMENT_CONSTANTS))


(module+ test
  (require (only-in racket/list
                    range
                    flatten
                    make-list)
           "../../6510-test-utils.rkt"
           (only-in "../../ast/6510-relocator.rkt" code-len)
           (only-in "../../tools/6510-interpreter.rkt"
                    peek
                    memory-list
                    cpu-state-clock-cycles)
           (only-in "../vm-inspector-utils.rkt"
                    vm-page-n->strings)
           (only-in "./vm-m1-slots-n.rkt"
                    ALLOC_M1_SLOT_TO_RT_N
                    ALLOC_M1_P0_SLOT_TO_RT_N
                    vm-m1-slot-code)
           (only-in "./vm-pages-n.rkt"
                    vm-pages-code)
           "./vm-memory-manager-test-utils.rkt"
           (only-in "./vm-memory-map.rkt"
                    ZP_TEMP
                    VM_MEMORY_MANAGEMENT_CONSTANTS)
           (only-in "./vm-pages-n.rkt"
                    VM_ALLOCATE_NEW_PAGE_N
                    VM_DEALLOCATE_PAGE_N
                    VM_INITIALIZE_PAGE_MEMORY_MANAGER_N)
           (only-in "./vm-register-functions.rkt"
                    CP_RT_TO_RA
                    CP_RA_TO_RT
                    CP_RA_TO_RB
                    SWAP_RA_RB
                    SWAP_ZP_WORD))

  (define PAGE_AVAIL_0 #xcf)      ;; high byte of first page available for allocation
  (define PAGE_AVAIL_0_W #xcf00)  ;; word (absolute address) of first page available
  (define PAGE_AVAIL_1 #xce)      ;; high byte of second page available for allocation
  (define PAGE_AVAIL_1_W #xce00) ;; word (absolute address) of second page available

  (define test-runtime
    (append
     ALLOC_CELL_ARRAY_TO_RT
     vm-m1-slot-code
     vm-pages-code
     VM_MEMORY_MANAGEMENT_CONSTANTS

     CP_RA_TO_RT
     CP_RT_TO_RA
     CP_RA_TO_RB
     SWAP_RA_RB
     SWAP_ZP_WORD
     (list (label INIT_CELLSTACK_PAGE_X) (RTS)))))

;; allocate a cell array of A number of cells
;;
;; input:  A = number of cells that should be available in the array
;; output: RT = pointer to allocated cell-array
;;         cell array ref count = 1
;;
;; variant: ALLOC_CELL_ARRAY_P0_TO_RT
;; allocate a cell array of profile 0 with 2 cells
;; input:  -
;; output: RT = pointer to allocated cell-array with two cells
;;         cell array ref count = 1
(define ALLOC_CELL_ARRAY_P0_TO_RT #t)
(define-vm-function ALLOC_CELL_ARRAY_TO_RT
  (list
          (PHA)                 ;; number of cells
          (ASL A)               ;; *2 since each cell is 2 bytes
          (JSR ALLOC_M1_SLOT_TO_RT_N)
          (PLA)                 ;; get back number of cells
          (LDY !$01)            ;;
          (STA (ZP_RT),y)       ;; set slot type to cell array with x elements (00xx xxxx)
          (RTS)

   (label ALLOC_CELL_ARRAY_P0_TO_RT)
          (JSR ALLOC_M1_P0_SLOT_TO_RT_N) ;; returns with y = 0!
          (LDA !$02)
          (LDY !$01)
          (STA (ZP_RT),y) ;; set slot type to cell array with 2 elements
          (RTS)))


(module+ test #| test-alloc-cell-array-to-rt |#
  (define test-alloc-cell-array-to-rt
    (compact-run-code-in-test-
     ;; #:debug #t
     #:runtime-code test-runtime
     #:init-label "VM_INITIALIZE_PAGE_MEMORY_MANAGER_N20"
     (fill-page-with PAGE_AVAIL_0 #xff)

     ;; now allocate the page
     (LDA !$05) ;; size 5 cells = 10 + 2 payload = profile 2
     (JSR ALLOC_CELL_ARRAY_TO_RT)
     ))

  (check-equal?
   (memory-list test-alloc-cell-array-to-rt (+ #x02 PAGE_AVAIL_0_W) (+ #x0d PAGE_AVAIL_0_W))
   (list #x01 #x05 #xff #xff #xff #xff #xff #xff #xff #xff #xff #xff)
   "refcount #x01, length of array #x05, lots of uninitialized cells")

  (check-equal?
   (memory-list test-alloc-cell-array-to-rt (+ #x00 PAGE_AVAIL_0_W) (+ #x01 PAGE_AVAIL_0_W))
   (list #x22 #x01)
   "page type 0010 0010 (m1 page, profile 2)")

  (check-equal?
   (memory-list test-alloc-cell-array-to-rt (+ #xfe PAGE_AVAIL_0_W) (+ #xff PAGE_AVAIL_0_W))
   (list #x0e #x00)
   "next free slot at #x0e, next page #x00"))

;; no refcnt adjustments!
(define-vm-function WRITE_ARR_ATa_RA_TO_RT
  (list
          (ASL A)
          ;; (CLC)                       ;; should be 0, since asl a should push 0 into carry
          (ADC !$02)                    ;; get y to point to low byte of cell at index
   (label WRITE_ARR_ATal_RA_TO_RT)
          (TAY)
   (label WRITE_ARR_ATyl_RA_TO_RT)
          (LDA (ZP_RA),y)               ;; copy low byte
          (STA ZP_RT)
          (INY)
          (LDA (ZP_RA),y)               ;; copy high byte
          (STA ZP_RT+1)
          (RTS)))

;; no refcnt adjustments!
(define-vm-function WRITE_RT_TO_ARR_ATa_RA
  (list
         (ASL A)
         ;; (CLC)                       ;; should be 0, since asl a should push 0 into carry
         (ADC !$02)                    ;; get y to point to low byte of cell at index
         (TAY)
         (LDA ZP_RT)                   ;; copy low byte
         (STA (ZP_RA),y)
         (INY)
         (LDA ZP_RT+1)                 ;; copy high byte
         (STA (ZP_RA),y)
         (RTS)))

;; no refcnt adjustments!
(define-vm-function WRITE_ARR_ATa_RT_TO_RT
  (list
          (ASL A)
          ;; (CLC)                       ;; should be 0, since asl a should push 0 into carry
          (ADC !$02)                    ;; get y to point to low byte of cell at index
   (label WRITE_ARR_ATal_RT_TO_RT)
          (TAY)
   (label WRITE_ARR_ATyl_RT_TO_RT)
          (LDA (ZP_RT),y)               ;; copy low byte
          (TAX)
          (INY)
          (LDA (ZP_RT),y)               ;; copy high byte
          (STA ZP_RT+1)
          (STX ZP_RT)                   ;; store low byte
          (RTS)))

;; no refcnt adjustments!
(define POP_CELL_EVLSTK_TO_ARR_AT1_RT #t)
(define-vm-function-wol WRITE_RP_TO_ARR_AT0_RT
  (list
   (label POP_CELL_EVLSTK_TO_ARR_AT1_RT)
          (JSR POP_CELL_EVLSTK_TO_RP)
          (LDY !$04)
          (BNE WRITE_RP_TO_ARR_ATyl_RT)
   (label WRITE_RP_TO_ARR_AT0_RT)
          (LDY !$02)
   (label WRITE_RP_TO_ARR_ATyl_RT)
          (LDA ZP_RP)
          (STA (ZP_RT),y)               ;; copy low byte
          (INY)
          (LDA ZP_RP+1)
          (STA (ZP_RT),y)               ;; copy high byte
          (RTS)))

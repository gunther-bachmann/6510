#lang racket/base

#|

  define all functions, data and constants for generic page level management

|#

(require (only-in racket/list
                  flatten)
         "../6510.rkt"
         (only-in "./vm-inspector-utils.rkt"
                  vm-regt->string)
         (only-in "./vm-memory-map.rkt"
                  ZP_RP
                  VM_MEMORY_MANAGEMENT_CONSTANTS))

(provide
         VM_INITIALIZE_MEMORY_MANAGER     ;; initialize memory management (must be called before first allocation)

         FREE_PAGE_A                      ;; free a page (the type specific stuff, of any, must have finished)
         ALLOC_PAGE_TO_X                  ;; allocate new page (not initialized)

         VM_INITIAL_MM_REGS               ;; memory management registers
         VM_PAGE_SLOT_DATA                ;; page that holds allocation status and/or first free slot per page

         ;; GLOBAL_CELL_FREE_LIST            ;; head of free cell list
         ;; GLOBAL_CELLPAIR_FREE_LIST        ;; head of free cell-pair list
         ;; GLOBAL_CELLPAIR_PAGE_FOR_ALLOC  ;; first page for cell-pair allocation
         )

(module+ test
  (require  "../6510-test-utils.rkt"
            (only-in "../tools/6510-interpreter.rkt"
                     peek)
            (only-in "../util.rkt"
                     format-hex-byte)
            "./vm-memory-manager-test-utils.rkt"
            (only-in "./vm-mm-register-functions.rkt"
                     WRITE_INT_AY_TO_RT))


  (define PAGE_AVAIL_0 #x8d)      ;; high byte of first page available for allocation
  (define PAGE_AVAIL_0_W #x8d00)  ;; word (absolute address) of first page available
  (define PAGE_AVAIL_1 #x8c)      ;; high byte of second page available for allocation
  (define PAGE_AVAIL_1_W #x8c00)  ;; word (absolute address) of second page available

  (define test-runtime
    (append
     ALLOC_PAGE_TO_X
     FREE_PAGE_A
     VM_INITIALIZE_MEMORY_MANAGER
     VM_MEMORY_MANAGEMENT_CONSTANTS

     (list (label INIT_CELLSTACK_PAGE_X) (RTS)) ;; ignore calls to this function during memory initialization

     WRITE_INT_AY_TO_RT
     (list (org #xcec0))
     VM_INITIAL_MM_REGS
     (list (org #xcf00))
     VM_PAGE_SLOT_DATA)))

;; initial data for the memory management registers
;; put into memory @ #xcec0 - len (currently 3)
(define VM_INITIAL_MM_REGS
  (list
   ;; (org #xcec0)
   (label VM_INITIAL_MM_REGS)

   ;; $cec0
   ;; @DC-M: GLOBAL_CELL_PAGE_FOR_ALLOC, group: page
   (label GLOBAL_CELL_PAGE_FOR_ALLOC) ;; page with free cells
          (byte $00)
   ;; $cec1
   (label VM_FREE_CALL_STACK_PAGE) ;; call stack page with free space
          (byte $00) ;; initial -> first allocation will allocate a new page
   ;; $cec2
   (label VM_FREE_CODE_PAGE) ;; code page with free space
          (byte $00)

   ;; $cec3
   (label GLOBAL_CELLPAIR_PAGE_FOR_ALLOC) ;; page with free cell-pairs
          (byte $00) ;; none

   ;; $cec4
   (label VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; what is the highest page to start searching for a free page
          (byte $cd) ;; safe to start with $cd as index

   ;; $cec5
   ;; @DC-M: GLOBAL_CELLPAIR_FREE_LIST, group: cell_pair
   ;; this queue holds only cell-pairs, cell0 is always the pointer to the next in queue of this free cells
   ;; cell1 is left untouched => may still hold live references => to reuse a cell-pair of this queue,
   ;; cell1 must be checked (if ptr, decr ref count and possibly free, else ignore)
   (label GLOBAL_CELLPAIR_FREE_LIST) ;; list of cell-pairs that are unused but only potentially partially freed (second cell may still hold references to heap objects)
          (word $0000) ;; if high byte is 0, the tree is empty!

   ;; $cec7
   (label GLOBAL_M1_PX_PAGE_FOR_ALLOC)
          (byte $00) ;; cell page with free slots for m1 page p0 pages
          (byte $00) ;; cell page with free slots for m1 page p1 pages
          (byte $00) ;; cell page with free slots for m1 page p2 pages
          (byte $00) ;; cell page with free slots for m1 page p3 pages
          (byte $00) ;; cell page with free slots for m1 page p5 pages

   ;; $cecc..cecd
   (label GLOBAL_CELL_FREE_LIST) ;; list of cells that are unused but still allocated (reusable)
          (word $0000)

   ;; cece,,cecf
   (label VM_P0_QUEUE_ROOT_OF_ARRAYS_TO_FREE)
          (word $0000)
   ;; ced0,,ced1
   (label VM_P1_QUEUE_ROOT_OF_ARRAYS_TO_FREE)
          (word $0000)
   ;; ced2,,ced3
   (label VM_P2_QUEUE_ROOT_OF_ARRAYS_TO_FREE)
          (word $0000)
   ;; ced4,,ced5
   (label VM_P3_QUEUE_ROOT_OF_ARRAYS_TO_FREE)
          (word $0000)
   ;; ced6,,ced7
   (label VM_P4_QUEUE_ROOT_OF_ARRAYS_TO_FREE)
          (word $0000)

   ;; $ced8..$ceff (unused) (40 bytes)
   ))

;; keep a list of cells (16-bit), this is the head (at cecc)
;; a cell in the free list is allocated on a page and initialized to be used as cell
;; it contains the ptr to the next free list or 0000 (only high byte is actually checked)
;; (define GLOBAL_CELL_FREE_LIST #xcecc)

;; page that has free cells (must be a cell page)
;; it may be full in which case allocation will allocate a new page and link the new page to this full page, and put it here
;; otherwise a cell on this page is allocated
;; (define GLOBAL_CELLPAIR_PAGE_FOR_ALLOC #xcec3)

;; keep list of cell-pairs that are (partially) free, cell0 is used to build the list,
;; cell1 is untouched and may have to be dec refcnt'd if it is a pointer, before reusing it
;; (define GLOBAL_CELLPAIR_FREE_LIST #xcec5)

;; each page is described by one byte, page 00 at 0, page 01 at 1 ...
;; 01 = reserved
;; ff = unused (free)
;; nn = first free slot of this page is located at nn
;; 00 = allocated for use, but not initialized/full
(define VM_PAGE_SLOT_DATA
  (list
   ;; (org #xcf00)
   (label VM_PAGE_SLOT_DATA)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem 0000-07ff is unavailable (zero page, stack ... screen)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem 0800-0fff is unavailable (start of basic ram)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem 1000-17ff is unavailable
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem 1800-1fff is unavailable
          (byte $01 $01 $ff $ff  $ff $ff $ff $ff)     ;; mem 2000-27ff 2000-21ff is unavailable, 2200-27ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 2800-2fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 3000-37ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 3800-3fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 4000-47ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 4800-4fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 5000-57ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 5800-5fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 6000-67ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 6800-6fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 7000-77ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 7800-7fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 8000-87ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 8800-8fff is free
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem 9000-97ff is unavailable (bc interpreter)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem 9800-9cff is unavailable (bc interpreter)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem A000-A7ff is unavailable (C64 BASIC)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem A800-Afff is unavailable (C64 BASIC)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem B000-B7ff is unavailable (C64 BASIC)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem B800-Bfff is unavailable (C64 BASIC)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem c000-c7ff is unavailable (vm code)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem c800-cdff is unavailable (vm code, ce00-ceff = other memory management registers + bitmap, cf00-cfff =  used by next free page mapping
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem D000-D7ff is unavailable (C64 I/O)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem D800-Dfff is unavailable (C64 I/O)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem E000-E7ff is unavailable (C64 KERNAL)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem E800-Efff is unavailable (C64 KERNAL)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem F000-F7ff is unavailable (C64 KERNAL)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)))   ;; mem F800-Ffff is unavailable (C64 KERNAL)


;; @DC-FUN: VM_INITIALIZE_MEMORY_MANAGER, group: pages
;; initialize memory management (paging)
;; - setup 'next free page' information, basically initializing the whole page with zeros
;; - setup cell stack (to empty)
;;
;; destroys: A Y
(define VM_INITIALIZE_MEMORY_MANAGER
  (flatten
   (list
    (label VM_INITIALIZE_MEMORY_MANAGER)

           ;; initialize NEXT_FREE_PAGE_PAGE (256 byte)
           (LDA !$ff)
           (TAY)
    (label VM_INITIALIZE_MEMORY_MANAGER__LOOP)
           ;; highbyte of this address should be using the constant NEXT_FREE_PAGE_PAGE
           ;; (STA $cf00,y) ;; encoded directly in the next couple of bytes
           ;; (take (ast-opcode-cmd-bytes (STA $cf00,y)) 2)
           (byte 153 0) (byte-ref NEXT_FREE_PAGE_PAGE) ;; fill CF00..CFFF with $FF!
           (INY)
           (BNE VM_INITIALIZE_MEMORY_MANAGER__LOOP)

           ;; alloc cell stack
           (JSR ALLOC_PAGE_TO_X)
           (LDA !$00)
           (STA ZP_CELL_STACK_LB_PTR)
           (JSR INIT_CELLSTACK_PAGE_X)
           (STX ZP_CELL_STACK_LB_PTR+1)

           (JSR ALLOC_PAGE_TO_X)
           (LDA !$00)
           (STA ZP_CELL_STACK_HB_PTR)
           (JSR INIT_CELLSTACK_PAGE_X)
           (STX ZP_CELL_STACK_HB_PTR+1)

           (LDA !$00)
           (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC)
           (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC+1)
           (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC+2)
           (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC+3)
           (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC+4)

           ;; make sure the list of partially gc'd cell arrays is empty!
           (STA ZP_PART_GCD_CELL_ARRAYS)
           (STA ZP_PART_GCD_CELL_ARRAYS+1)

           (LDA !$01)
           (STA ZP_CELL_STACK_TOS)

           (LDX !$00)
           (STX ZP_RT) ;; set registers (RT, RA, RZ) to hold no value!
           (STX ZP_RT+1)
           (STX ZP_RA)
           (STX ZP_RA+1)
           (STX ZP_RZ)
           (STX ZP_RZ+1)
           (RTS))))

;; @DC-FUN: ALLOC_PAGE_TO_X, group: pages
;; does a linear search for the next free page
;; allocate a page (completely uninitialized), just the page, update the memory page status in VM_PAGE_SLOT_DATA
;; parameter: (none)
;; result: X = allocated free page (uninitialized)
;; uses: A, X
(define ALLOC_PAGE_TO_X
  (list
   (label ALLOC_PAGE_TO_X)
          (LDX VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH)

   (label LOOP__ALLOC_PAGE_TO_X)
          (LDA VM_PAGE_SLOT_DATA,x)
          (DEX)
          (BEQ OUT_OF_MEMORY__ALLOC_PAGE_TO_X) ;; cannot be lower then 1!!
          (CMP !$ff)
          (BNE LOOP__ALLOC_PAGE_TO_X)

          ;; found page marked unallocated ($ff)
          (INX) ;; restore original index
          (LDA !$00) ;; mark as initially full but allocated
          (STA VM_PAGE_SLOT_DATA,x)
          (STX VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; set index for next search

          (RTS)

   (label OUT_OF_MEMORY__ALLOC_PAGE_TO_X)
          (BRK)
          ))

;; @DC-FUN: FREE_PAGE_A, group: pages
;; whether a page is free or used is kept in the 256 bytes starting at VM_PAGE_SLOT_DATA
;; each byte represents one page
;;   00 = allocated (used) but no free slots
;;   01 = system page, not available for memory management
;;   ff = free page (not allocated yet)
;; VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH  (255..0) keeps the max idx to start looking for a page that is free
;; parameter: a = page
;; result: (none)
(define FREE_PAGE_A
  (list
   (label FREE_PAGE_A)
          (CMP VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH)
          (BMI NO_CHANGE__FREE_PAGE_A)
          (STA VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH)

   (label NO_CHANGE__FREE_PAGE_A)
          (TAY)
          (LDA !$ff) ;; free/unallocated page
          (STA VM_PAGE_SLOT_DATA,y)
          (RTS)))

(module+ test #| vm-free-page and vm-alloc-page--page-uninit |#
  (define vm-free-page-state
    (compact-run-code-in-test-
     ;; #:debug #t
     #:runtime-code test-runtime
     (JSR ALLOC_PAGE_TO_X) ;; page is in A ($cc)
     (TXA)
     (PHA)
     (JSR ALLOC_PAGE_TO_X) ;; page is in A ($cb)
     (TXA)
     (JSR WRITE_INT_A_TO_RT)
     (PLA)
     (JSR FREE_PAGE_A )
     (JSR ALLOC_PAGE_TO_X) ;; allocated page should be $cc again
     (TXA)
     (STA ZP_RP)))

  (check-equal? (peek vm-free-page-state ZP_RP)
                 PAGE_AVAIL_0)
  (check-equal? (vm-regt->string vm-free-page-state)
                (format "int $00~a" (format-hex-byte PAGE_AVAIL_1))))

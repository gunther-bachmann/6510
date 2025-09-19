#lang racket/base

#|

  all functions around cell-pairs

|#

(require "../6510.rkt"
         (only-in "../ast/6510-resolver.rkt"
                  add-label-suffix
                  replace-labels)
         (only-in "./vm-memory-map.rkt"
                  TAGGED_NIL
                  ZP_RP
                  ZP_RT
                  VM_MEMORY_MANAGEMENT_CONSTANTS))

(provide INIT_CELLPAIR_PAGE_X_TO_AX        ;; initialize page A for ref counted cell-pairs
         ALLOC_CELLPAIR_AX_TO_RT           ;; allocate a cell-pair a from page x (if page has no free cell-pairs, a new page is allocated and is used to get a free cell-pair!)
         ALLOC_CELLPAIR_TO_RT              ;; allocate a cell-pair from the current page (or from a new page if full)
         GET_FRESH_CELLPAIR_TO_AX          ;; get the page and unused cellpair for allocation
         WRITE_CELLPAIR_RT_CELLy_TO_RT     ;; write cellY from cell-pair RT into RT (overwriting it)
         WRITE_CELLPAIR_RP_CELLy_TO_RP     ;; write cellY from cell-pair RP into RP (overwriting it)
         WRITE_CELLPAIR_RT_CELLy_TO_RP     ;; write cellY from cell-pair RT into RP
         WRITE_RP_TO_CELLy_CELLPAIR_RT     ;; write RP into cellY of cell-pair RT
         WRITE_RT_TO_CELLy_CELLPAIR_RP     ;; write RT into cellY of cell-pair RP
         INC_REFCNT_CELLPAIR_RT            ;; increment ref count to cell-pair in RT
         DEC_REFCNT_CELLPAIR_RZ            ;; decrement ref count to cell-pair in RZ
         FREE_CELLPAIR_RZ                  ;; free the given cell-pair in RZ (cells must not reference anything that needs garbage collection)
         GC_CELLPAIR_FREE_LIST             ;; garbage collect the complete cell-pair free list

         ;; jump targets with bool definitions
         WRITE_CELLPAIR_RT_CELL0_TO_RT
         WRITE_CELLPAIR_RT_CELL1_TO_RT
         WRITE_CELLPAIR_RP_CELL0_TO_RP
         WRITE_CELLPAIR_RP_CELL1_TO_RP
         WRITE_CELLPAIR_RT_CELL0_TO_RP
         WRITE_CELLPAIR_RT_CELL1_TO_RP
         WRITE_RP_TO_CELL0_CELLPAIR_RT
         WRITE_RP_TO_CELL1_CELLPAIR_RT
         WRITE_RT_TO_CELL0_CELLPAIR_RP
         WRITE_RT_TO_CELL1_CELLPAIR_RP)

(module+ test
  (require (only-in racket/list make-list)
           "../6510-test-utils.rkt"
           (only-in "../tools/6510-interpreter.rkt" memory-list)
           (only-in "../util.rkt" format-hex-byte)
           (only-in "./vm-inspector-utils.rkt"
                    vm-regt->string
                    vm-cell-pair-free-tree->string
                    vm-deref-cell-pair-w->string
                    vm-refcount-cell-pair-ptr
                    vm-regp->string
                    vm-page->strings)
           "./vm-memory-manager-test-utils.rkt"
           (only-in "./vm-pages.rkt"
                    ALLOC_PAGE_TO_X
                    VM_PAGE_SLOT_DATA
                    VM_INITIAL_MM_REGS
                    VM_INITIALIZE_MEMORY_MANAGER)
           (only-in "./vm-register-functions.rkt"
                    CP_RT_TO_RZ
                    CP_RT_TO_RP
                    CP_RZ_TO_RT
                    CP_RA_TO_RZ
                    WRITE_INT_AY_TO_RT
                    WRITE_NIL_TO_RP))

  (define PAGE_AVAIL_0 #x8d)      ;; high byte of first page available for allocation
  (define PAGE_AVAIL_0_W #x8d00)  ;; word (absolute address) of first page available
  (define PAGE_AVAIL_1 #x8c)      ;; high byte of second page available for allocation
  (define PAGE_AVAIL_1_W #x8c00) ;; word (absolute address) of second page available

  (define test-runtime
    (append
     ALLOC_PAGE_TO_X
     ALLOC_CELLPAIR_AX_TO_RT
     ALLOC_CELLPAIR_TO_RT
     GET_FRESH_CELLPAIR_TO_AX
     WRITE_CELLPAIR_RT_CELLy_TO_RT
     WRITE_CELLPAIR_RP_CELLy_TO_RP
     WRITE_CELLPAIR_RT_CELLy_TO_RP
     WRITE_RP_TO_CELLy_CELLPAIR_RT
     WRITE_RT_TO_CELLy_CELLPAIR_RP
     INIT_CELLPAIR_PAGE_X_TO_AX
     FREE_CELLPAIR_RZ
     GC_CELLPAIR_FREE_LIST

     WRITE_NIL_TO_RP
     CP_RT_TO_RZ
     CP_RT_TO_RP
     CP_RZ_TO_RT
     CP_RA_TO_RZ
     WRITE_INT_AY_TO_RT
     (INC_REFCNT_CELLPAIR_RT "LSR__INC_RFCNT_CELLPAIR__")
     (list (label DONE__) (RTS))
     (DEC_REFCNT_CELLPAIR_RZ "CELLPAIR_ALREADY_LSRED__")
     VM_INITIALIZE_MEMORY_MANAGER
     VM_MEMORY_MANAGEMENT_CONSTANTS
     (list (label INIT_CELLSTACK_PAGE_X) (RTS))
     (list (label DEC_REFCNT_RT) (JMP DEC_REFCNT_CELLPAIR_RT)) ;; assume that dec refcnt is operated on cellpair
     (list (label DEC_REFCNT_RZ) (JMP DEC_REFCNT_CELLPAIR_RZ)) ;; assume that dec refcnt is operated on cellpair
     (list (label INC_REFCNT_RT) (JMP INC_REFCNT_CELLPAIR_RT)) ;; assume that inc refcnt is operated on cellpair
     (list (org #xcec0))
     VM_INITIAL_MM_REGS
     (list (org #xcf00))
     VM_PAGE_SLOT_DATA)))

;; @DC-FUN: INIT_CELLPAIR_PAGE_X_TO_AX, group: cell_pair
;; cell-pair page layout  (new layout with cell-pair-ptr having bit0 always set and bit1 always unset!)
;; offset  content
;; ---------------
;; 00      #b01xx xxxx page type + number of used slots
;; 01      ref-count cell-pair at 05 (cell-pair 0)
;; 02      ref-count cell-pair at 09 (cell-pair 1)
;; 03..04   unused (2)
;; 05..08   cell-pair 0     (#b0000 01[01] >> 2 = #b0000 0001)
;; 09..0c   cell-pair 1     (#b0000 10[01] >> 2 = #b0000 0010)
;; 0d..0f   unused (3)
;; 10      ref-count for cell-pair at 40 (cell-pair 2)
;; 11      ref-count for cell-pair at 44 (cell-pair 3)
;; ..3e     ref-count for cell-pair at f9 (cell-pair 48)
;; 3f..40   unused (2)
;; 41..44   cell-pair 2     (#b0100 00[01] >> 2 = #b0001 0000)
;; 45..48   cell-pair 3     (#b0100 01[01] >> 2 = #b0001 0001)
;; ...
;; f9..fc   cell-pair 48    (#b1111 10[01] >> 2 = #b0011 1110)
;; fd..fe   unused (2)
;; ff      previous page of this type
;;
;; VM_PAGE_SLOT_DATA + pageidx: holds the index within the page of the first free cell-pair on that page (0 = no free cell-pair on this page)
;; the free cell-pair holds in byte 0 of the cell-pair the offset of the next free cell-pair (0 = no other free cell-pair)
;;
;; allocate a complete new page and initialize it to hold reference counted cell-pairs
;; connect all cell-pairs in a free-list
;; also set the first free slot of this allocated page (in VM_PAGE_SLOT_DATA + pageidx)
;;
;; input:  X = page allocated but uninitialized
;; output: X = page allocated and initialized for cell-pairs usage
;;         A = first free slot ($05)
;; usage:  X, Y, ZP_TEMP, ZP_TEMP2
(define INIT_CELLPAIR_PAGE_X_TO_AX
  (add-label-suffix
   "__" "__INIT_CELLPAIR_PAGE_X_TO_AX"
    (list
     (label INIT_CELLPAIR_PAGE_X_TO_AX)
            ;; page is in X

            (STX ZP_TEMP+1)

            (LDY !$00)
            (STY ZP_TEMP)
            (LDA !$40) ;; page type cell-pairs w/ 0 slots allocated
            (STA (ZP_TEMP),y)

            (LDY !$ff)
            (LDA GLOBAL_CELLPAIR_PAGE_FOR_ALLOC)
            (STA (ZP_TEMP),y) ;; previous page of this type is (@$ff = )

            (STX GLOBAL_CELLPAIR_PAGE_FOR_ALLOC)

            ;; first write all reference count fields (zero)
            ;; block0 (two ref counts)
            (LDY !$01)
            (LDA !$00) ;; reference count initialized with 0
            (STA (ZP_TEMP),y) ;; @ 01
            (INY)
            (STA (ZP_TEMP),y) ;; @ 02

            (LDY !$10)
            (STY ZP_TEMP)
            (LDY !$2F)

     (label SECOND_RZ_BLOCK__)
            (STA (ZP_TEMP),y)
            (DEY)
            (BPL SECOND_RZ_BLOCK__)

            (STA ZP_TEMP) ;; clear lowbyte of ptr

            ;; write all cell-pairs to point to next free one
            (LDY !$05)
            (LDA !$09)
            (STA (ZP_TEMP),y) ;; @05 <- 09
            (TAY)
            (LDA !$41)
            (STA (ZP_TEMP),y) ;; @09 <- 41

     (label SECOND_CELL_PAIR_BLOCK__)
            (TAY)
            (CLC)
            (ADC !$04)
            (STA (ZP_TEMP),y)
            (CMP !$F9)
            (BNE SECOND_CELL_PAIR_BLOCK__)

            (TAY)
            (LDA !$00)
            (STA (ZP_TEMP),y) ;; last cell points to 0

            (LDX ZP_TEMP+1) ;; return page initialized in X (high byte)
            (LDA !$05)      ;; return first free slot in A (low byte)
            (STA VM_PAGE_SLOT_DATA,x)

            (RTS))))

(module+ test #| vm_alloc_page_for_cell_pairs |#
  (define vm-alloc-page-for-cell-pairs-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDA !$a0)
     (STA GLOBAL_CELLPAIR_PAGE_FOR_ALLOC)
     (JSR ALLOC_PAGE_TO_X)
     (JSR INIT_CELLPAIR_PAGE_X_TO_AX)
     (STX ZP_RT+1) ;; to test read out actual page
     (STA ZP_RT)))

  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state ZP_RT (+ 1 ZP_RT))
                (list #x05 PAGE_AVAIL_0)
                "page and slot .. was allocated")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state PAGE_AVAIL_0_W (+ PAGE_AVAIL_0_W #x02))
                (list #b01000000 #x00 #x00)
                "page type is #b01000000 and refcounts cell0 and cell1 are both 0")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #x05) (+ PAGE_AVAIL_0_W #x05))
                (list #x09)
                "cell0 first byte points to next free (09)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #x09) (+ PAGE_AVAIL_0_W #x09))
                (list #x41)
                "cell1 first byte points to next free (41)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #x10) (+ PAGE_AVAIL_0_W #x3e))
                (make-list #x2f #x00)
                "refcounts are all 0 (in block 2)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #x41) (+ PAGE_AVAIL_0_W #x41))
                (list #x45)
                "cell1 first byte points to next free (45)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #x45) (+ PAGE_AVAIL_0_W #x45))
                (list #x49)
                "cell2 first byte points to next free (49)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #xf5) (+ PAGE_AVAIL_0_W #xf5))
                (list #xf9)
                "cell47 first byte points to next free (f9)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #xf9) (+ PAGE_AVAIL_0_W #xf9))
                (list #x00)
                "last cell first byte points to 0 (no next free)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #xff) (+ PAGE_AVAIL_0_W #xff))
                (list #xa0)
                "last byte on page points to previous free page of cell-pairs"))

;; @DC-FUN: ALLOC_CELLPAIR_AX_TO_RT, group: cell_pair
;; allocate a cell-pair from this page (if page has no free cell-pairs, a new page is allocated and is used to get a free cell-pair!)
;; this will not check the free cell-pair tree!
;; input:  X : page to allocate cell-pair on (a new page is allocated, if this page does not have any free cell-pairs)
;;         A : slot to allocated (must be the first free slot of this page)
;; output: ZP_RT
;; WARNING: ZP_RT IS OVERWRITTEN !! NO PUSH INTO THE CELL-STACK IS DONE!
(define ALLOC_CELLPAIR_AX_TO_RT
  (add-label-suffix
   "__" "__ALLOC_CELLPAIR_AX_TO_RT"
  (list
   (label ALLOC_CELLPAIR_PFL_X_TO_RT)
          (LDA VM_PAGE_SLOT_DATA,x)

   (label ALLOC_CELLPAIR_AX_TO_RT) ;; <-- real entry point of this function
          (STX ZP_RT+1) ;; safe as highbyte of ptr
          (STA ZP_RT)
          (LDY !$00)
          (LDA (ZP_RT),y) ;; next free cell
          (STA VM_PAGE_SLOT_DATA,x)

          ;; increase the slot number used on this page
          (STX INC_CMD__+2) ;; overwrite $c0 (page in following INC command)
   (label INC_CMD__)
          (INC $c000)
          (RTS))))

(module+ test #| vm-alloc-cell-pair-on-page-a-into-rt |#
  (define vm-alloc-cell-pair-on-page-a-into-rt-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_PAGE_TO_X)
     (JSR INIT_CELLPAIR_PAGE_X_TO_AX)
     (JSR ALLOC_CELLPAIR_AX_TO_RT)))

  (check-equal? (vm-page->strings vm-alloc-cell-pair-on-page-a-into-rt-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09"))
  (check-equal? (vm-regt->string vm-alloc-cell-pair-on-page-a-into-rt-state)
                (format "pair-ptr[0] $~a05" (format-hex-byte PAGE_AVAIL_0))))

;; @DC-FUN: ALLOC_CELLPAIR_TO_RT, group: cell_pair
;; try to reuse root of free tree: use root but make sure to deallocate cell1 of the root (since this might still point to some data)
;; if no free tree available, find page with free cells (GLOBAL_CELLPAIR_PAGE_FOR_ALLOC)
;; if no free cell page is available, allocate a new page and used the first free slot there
;; NOTE: the cell-pair is not initialized (cell0 and/or cell1 may contain old data that needs to be overwritten!)
;; input:  GLOBAL_CELLPAIR_FREE_LIST
;; usage:  A, X, Y
;; output: RT
;; funcs:
;;   GET_FRESH_CELLPAIR_TO_AX
;;   ALLOC_CELLPAIR_AX_TO_RT
;;   WRITE_CELLPAIR_RT_CELL1_TO_RT
;;   DEC_REFCNT_RT
(define ALLOC_CELLPAIR_TO_RT
  (add-label-suffix
   "__" "__ALLOC_CELLPAIR_TO_RT"
  (list
   (label ERROR__ALLOC_CELLPAIR_GFL_TO_RT)
          (BRK)

   ;; ----------------------------------------
   (label ALLOC_CELLPAIR_TO_RT)
          (LDA GLOBAL_CELLPAIR_FREE_LIST+1) ;; get highbyte (page) from ptr to cell-pair
          (BNE REUSE_CELL_PAIR__)   ;; if != 0, cell-pair can be reused
          ;; no cell-pair to reuse available => need to allocate a new one

          ;; get a cell-pair on the given page (or allocate a new page)
          (JSR GET_FRESH_CELLPAIR_TO_AX)
          (JMP ALLOC_CELLPAIR_AX_TO_RT)

   (label ALLOC_CELLPAIR_GFL_TO_RT)
          (LDA GLOBAL_CELLPAIR_FREE_LIST+1)
          (BEQ ERROR__ALLOC_CELLPAIR_GFL_TO_RT) ;; no free cell in global free list

   (label REUSE_CELL_PAIR__)
          ;; put root of free tree into zp_rt (and copy in TEMP_PTR of this function)
          (STA ZP_RT+1)
          (STA TEMP_PTR__+1)
          (LDA GLOBAL_CELLPAIR_FREE_LIST)
          (STA ZP_RT)
          (STA TEMP_PTR__)

          ;; set new tree root for free tree to original cell0
          (LDY !$00)
          (LDA (ZP_RT),y)
          (BEQ CELL0_IS_NO_PTR__) ;; is zero => completely empty
          (AND !$03)
          (CMP !$03)
          (BEQ CELL0_IS_NO_PTR__) ;; is no ptr
          (INY)
          (LDA (ZP_RT),y)
          (BEQ CELL0_IS_NO_PTR__) ;; it is nil which is handled same as no pointer
          (DEY)

          ;; ;; cell0 is a cell-ptr or cell-pair-ptr
          ;; (LSR)
          ;; (BCC CELL0_IS_CELL_PTR__) ;; <- this cannot happen! cell0 is freed before entering it into the tree

          ;; cell0 is a cell-pair-ptr => make new root of free queue
          ;; (LDA (ZP_RT),y)
          (STA GLOBAL_CELLPAIR_FREE_LIST+1)
          (LDA (ZP_RT),y)
          (STA GLOBAL_CELLPAIR_FREE_LIST)
          (BNE CHECK_CELL1__) ;; since must be !=0, it cannot be on page 0 always branch!

   ;; (label CELL0_IS_CELL_PTR__)
   ;;        ;; cell0 is a cell-ptr => decrement cell0
   ;;        (JSR WRITE_CELLPAIR_RT_CELL0_TO_RT)
   ;;        (JSR DEC_REFCNT_RT)
   ;;        ;; restore original cell-pair-ptr
   ;;        (LDA TEMP_PTR__+1)
   ;;        (STA ZP_RT+1)
   ;;        (LDA TEMP_PTR__)
   ;;        (STA ZP_RT)
   ;;        ;; continue as if cell0 was no ptr, since cell-ptr was handled already

   (label CELL0_IS_NO_PTR__)
          (LDA !$00)
          (STA GLOBAL_CELLPAIR_FREE_LIST+1)
          ;; (STA GLOBAL_CELLPAIR_FREE_LIST)

   (label CHECK_CELL1__)
          ;; check whether cell1 is non-ptr or ptr
          (LDY !$02)
          (LDA (ZP_RT),y) ;; get low byte
          (BEQ CELL1_IS_NO_PTR__) ;; = 0 means totally empty => no ptr
          (AND !$03)       ;; mask out all but low 2 bits
          (CMP !$03)
          (BEQ CELL1_IS_NO_PTR__) ;; no need to do further deallocation
          (INY)
          (LDA (ZP_RT),y) ;; get high byte (page)
          (BEQ CELL1_IS_NO_PTR__) ;; this is nil, no need for deallocation

          ;; write cell1 into zp_ptr and decrement
          (JSR WRITE_CELLPAIR_RT_CELL1_TO_RT)
          (JSR DEC_REFCNT_RT)
          ;; continue as if cell1 is atomic, since it was already handled

          ;; restore zp_ptr to the cell-pair to be reused
          (LDA TEMP_PTR__+1)
          (STA ZP_RT+1)
          (LDA TEMP_PTR__)
          (STA ZP_RT)

   (label CELL1_IS_NO_PTR__)
          ;; cleanup cell-pair to ensure it is empty
          (LDA !$00)
          (LDY !$03)
   (label LOOP_CLEAN_RT__)
          (STA (ZP_RT),y)
          (DEY)
          (BPL LOOP_CLEAN_RT__)

          (RTS)

   (label TEMP_PTR__)
          (word $0000))))

(module+ test #| vm-allocate-cell-pair-ptr-to-rt |#
  ;; test case 1: allocate new page, allocate first cell-pair on new page, no existing (reusable) pair available
  (define vm-allocate-cell-pair-ptr-to-rt-1-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELLPAIR_TO_RT)))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-1-state)
                (format "pair-ptr[0] $~a05" (format-hex-byte PAGE_AVAIL_0)))

  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-1-state #xcec3 #xcec3)
                (list PAGE_AVAIL_0))

  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-1-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09"))

  ;; test case 2: allocate another cell-pair on existing page with already allocated slots
  (define vm-allocate-cell-pair-ptr-to-rt-2-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR ALLOC_CELLPAIR_TO_RT)))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-2-state)
                (format "pair-ptr[0] $~a09" (format-hex-byte PAGE_AVAIL_0)))

  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-2-state #xcec3 #xcec3)
                (list PAGE_AVAIL_0))

  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-2-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41"))

  ;; test case 3: allocate last cell-pair on existing page
  (define vm-allocate-cell-pair-ptr-to-rt-3-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
            (LDX !49)
     (label TEST_LOOP__ALLOC_CELLPAIR_TO_RT_3_CODE)
            (TXA)
            (PHA)
            (JSR ALLOC_CELLPAIR_TO_RT)
            (PLA)
            (TAX)
            (DEX)
            (BNE TEST_LOOP__ALLOC_CELLPAIR_TO_RT_3_CODE)))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-3-state)
                (format "pair-ptr[0] $~af9" (format-hex-byte PAGE_AVAIL_0)))

  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-3-state #xcec3 #xcec3)
                (list PAGE_AVAIL_0))

  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-3-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     49"
                      "next free slot: $00"))

  ;; test case 3a: allocate one past last cell-pair on existing page
  (define vm-allocate-cell-pair-ptr-to-rt-3a-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
            (LDX !50)
     (label TEST_LOOP__ALLOC_CELLPAIR_TO_RT_3A_CODE)
            (TXA)
            (PHA)
            (JSR ALLOC_CELLPAIR_TO_RT)
            (PLA)
            (TAX)
            (DEX)
            (BNE TEST_LOOP__ALLOC_CELLPAIR_TO_RT_3A_CODE)))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-3a-state)
                (format "pair-ptr[0] $~a05" (format-hex-byte PAGE_AVAIL_1)))

  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-3a-state #xcec3 #xcec3)
                (list PAGE_AVAIL_1))

  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-3a-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     49"
                      "next free slot: $00"))
  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-3a-state PAGE_AVAIL_1)
                (list "page-type:      cell-pair page"
                      (format "previous page:  $~a" (format-hex-byte PAGE_AVAIL_0))
                      "slots used:     1"
                      "next free slot: $09"))

  ;; test case 4: allocate first of pairs stored in the free tree, which is filled with just non ptr atomic cells => no gc
  (define vm-allocate-cell-pair-ptr-to-rt-4-state
    (compact-run-code-in-test-
      #:runtime-code test-runtime
      (JSR ALLOC_CELLPAIR_TO_RT)
      ;; copy allocated cell-pair ptr to the tree
      (LDA ZP_RT)
      (STA GLOBAL_CELLPAIR_FREE_LIST)
      (LDA ZP_RT+1)
      (STA GLOBAL_CELLPAIR_FREE_LIST+1)

      ;; fill cell0 with zeros, cell1 within int1
      (LDA !$00)
      (STA ZP_RP)
      (STA ZP_RP+1)
      (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)
      (JSR CP_RT_TO_RZ)
      (JSR WRITE_INT1_TO_RT)
      (JSR CP_RT_TO_RP)
      (JSR CP_RZ_TO_RT)
      (JSR WRITE_RP_TO_CELL1_CELLPAIR_RT)

      (JSR ALLOC_CELLPAIR_TO_RT)))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-4-state)
                (format "pair-ptr[0] $~a05" (format-hex-byte PAGE_AVAIL_0))
                "cell pair at cc05 is reused (was head of tree)")
  (check-equal? (vm-cell-pair-free-tree->string vm-allocate-cell-pair-ptr-to-rt-4-state)
                "root is initial")

  ;; test case 4: allocate first of pairs stored in the free tree, which has its slot1 filled with ptr that needs to be gc'ed
  (define vm-allocate-cell-pair-ptr-to-rt-5-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR CP_RT_TO_RZ)
     (JSR WRITE_INT1_TO_RT)
     (JSR CP_RT_TO_RP)
     (JSR CP_RZ_TO_RT)
     (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)
     (JSR WRITE_RP_TO_CELL1_CELLPAIR_RT)                      ;; cc05: 03 01 03 01
     (JSR INC_REFCNT_CELLPAIR_RT)           ;; cc01 = 1
     (JSR CP_RT_TO_RP)                               ;; RA = cc05

     (JSR ALLOC_CELLPAIR_TO_RT)                 ;; RT = cc09

     ;; copy allocated cell-pair ptr to the tree
     (LDA ZP_RT)
     (STA GLOBAL_CELLPAIR_FREE_LIST)
     (LDA ZP_RT+1)
     (STA GLOBAL_CELLPAIR_FREE_LIST+1)

     ;; fill cell0 with zeros, cell1 cell-pair-ptr (with refcount = 1)
     (JSR WRITE_RP_TO_CELL1_CELLPAIR_RT)                     ;; cc09: 05 cc 00 00

     (LDA !$00)
     (STA ZP_RP)
     (STA ZP_RP+1)
     (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)

     (JSR ALLOC_CELLPAIR_TO_RT)))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-5-state)
                (format "pair-ptr[0] $~a09" (format-hex-byte PAGE_AVAIL_0))
                "cellp-pair at cc09 is reused (was at head of tree)")
  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-5-state (+ PAGE_AVAIL_0_W #x01) (+ PAGE_AVAIL_0_W #x01))
                (list #x00)
                "refcount of cc05 (is at cc01) is zero again!")
  (check-equal? (vm-cell-pair-free-tree->string vm-allocate-cell-pair-ptr-to-rt-5-state)
                (format "pair $~a05 -> [ pair-ptr NIL . int $0001 ]" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-5-state (+ PAGE_AVAIL_0_W #x06) (+ PAGE_AVAIL_0_W #x08))
                (list #x00 #x03 #x01)
                "pair at cc06 holds 00 in cell0-page (no further elements in queue) and int 1 in cell1")
  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-5-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41")
                "still both are used on the page, one was allocated (reused) from tree, and the other is now head of the tree"))

;; @DC-FUN: GET_FRESH_CELLPAIR_TO_AX, group: cell_pair
;; get the page and unused cellpair for allocation
;;
;; get the complete ptr, do not allocate this cellpair yet
;; allocate a new page if necessary
;; do not use any cellpair free list
;; input:  VM_FREE_CELL__PAIR_PAGE
;;         VM_PAGE_SLOT_DATA+PAGE
;; output: A = lowbyte
;;         X = highbyte (page)
;;         Y = ?
(define GET_FRESH_CELLPAIR_TO_AX
  (list
   (label GET_FRESH_CELLPAIR_TO_AX)
          (LDX GLOBAL_CELLPAIR_PAGE_FOR_ALLOC)
          (BEQ PAGE__GET_FRESH_CELLPAIR_TO_AX)
          (LDA VM_PAGE_SLOT_DATA,x)
          (BNE DONE__GET_FRESH_CELLPAIR_TO_AX) ;; allocate new page first

   (label PAGE__GET_FRESH_CELLPAIR_TO_AX)
          (JSR ALLOC_PAGE_TO_X)
          (JMP INIT_CELLPAIR_PAGE_X_TO_AX)

   (label DONE__GET_FRESH_CELLPAIR_TO_AX)
          (RTS)))

(module+ test #| get-page-for-alloc-cellpair-to-ax |#
  (define get-page-for-alloc-cellpair-to-ax-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
      (JSR ALLOC_PAGE_TO_X)
      (STX GLOBAL_CELLPAIR_PAGE_FOR_ALLOC)
      (LDA !$09)                           ;; make first free slot on page to be 08
      (STA VM_PAGE_SLOT_DATA,x)

      (JSR GET_FRESH_CELLPAIR_TO_AX)  ;; no new allocate should take place => stay on page_0

      (STA ZP_RT)
      (STX ZP_RT+1)))

  (check-equal? (memory-list get-page-for-alloc-cellpair-to-ax-state ZP_RT (add1 ZP_RT))
                (list #x09 PAGE_AVAIL_0)
                "cell 09 is allocated on page_0"))

(module+ test #| get-page-for-alloc-cellpair-to-ax |#
  (define get-page-for-alloc-cellpair-to-ax-2-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
      (JSR ALLOC_PAGE_TO_X)
      (STX GLOBAL_CELLPAIR_PAGE_FOR_ALLOC)
      (LDA !$00)                          ;; mark page to have no free cells
      (STA VM_PAGE_SLOT_DATA,x)

      (JSR GET_FRESH_CELLPAIR_TO_AX) ;; should allocate a new page

      (STA ZP_RT)
      (STX ZP_RT+1)))

  (check-equal? (memory-list get-page-for-alloc-cellpair-to-ax-2-state ZP_RT (add1 ZP_RT))
                (list #x05 PAGE_AVAIL_1)
                "since first page (page_0) is marked as full, first slot (05) of page_1 is allocated"))


;; @DC-FUN: WRITE_CELLPAIR_RT_CELLy_TO_RT, group: cell_pair
;; overwrite the given cell-pair ptr in RT with the value of the Y's cell pointed to by the cell-pair
;; no reference count is adjusted! this has to be taken care of by caller!
;; input:  Y - 0 (cell0), 2 (cell1)
;;         RT (must be cell-pair ptr) -> [A][B]
;; output: RT = A (Y=0) or B (Y=2)
(define WRITE_CELLPAIR_RT_CELL1_TO_RT #t)
(define WRITE_CELLPAIR_RT_CELL0_TO_RT #t)
(define WRITE_CELLPAIR_RT_CELLy_TO_RT
  (list
   (label WRITE_CELLPAIR_RT_CELL1_TO_RT)
          (LDY !$02)
          (BNE WRITE_CELLPAIR_RT_CELLy_TO_RT)

   (label WRITE_CELLPAIR_RT_CELL0_TO_RT)
          (LDY !$00)

   ;; ----------------------------------------
   (label WRITE_CELLPAIR_RT_CELLy_TO_RT)
          (LDA (ZP_RT),y)
          (TAX)
          (INY)
          (LDA (ZP_RT),y)
          (STA ZP_RT+1)
          (STX ZP_RT)
          (RTS)))

(module+ test #| vm-write-rt-celly-to-rt |#
  (define vm-write-rt-celly-to-rt-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
      (JSR ALLOC_CELLPAIR_TO_RT)                 ;; rt <- [empty][empty]
      (JSR CP_RT_TO_RZ)
      (LDA !$01)
      (LDY !$10)
      (JSR WRITE_INT_AY_TO_RT)
      (JSR CP_RT_TO_RP)                          ;; rp <- int 1001
      (JSR CP_RZ_TO_RT)
      (LDY !$00)
      (JSR WRITE_RP_TO_CELLy_CELLPAIR_RT)        ;; rt <- [int 1001][empty]
      (JSR CP_RT_TO_RZ)
      (LDA !$10)
      (LDY !$01)
      (JSR WRITE_INT_AY_TO_RT)
      (JSR CP_RT_TO_RP)                          ;; rp <- 0110
      (JSR CP_RZ_TO_RT)
      (LDY !$02)
      (JSR WRITE_RP_TO_CELLy_CELLPAIR_RT)        ;; rt <- [int 1001][int 0110]
      (JSR WRITE_CELLPAIR_RT_CELL0_TO_RT)))     ;; rt <- int 1001

  (check-equal? (vm-regt->string vm-write-rt-celly-to-rt-state)
                "int $1001")

  (check-equal? (vm-deref-cell-pair-w->string vm-write-rt-celly-to-rt-state (+ PAGE_AVAIL_0_W #x05))
                "(int $1001 . int $0110)"))

;; @DC-FUN: WRITE_CELLPAIR_RT_CELLy_TO_RP, group: cell_pair
;; write into RP the value of the Y's cell pointed to by the cell-pair ptr RT
;; no reference count is adjusted! this has to be taken care of by caller!
;; input:  Y - 0 (cell0), 2 (cell1)
;;         RT (must be cell-pair ptr) -> [A][B]
;; output: RP = A (Y=0) or B (Y=2)
(define WRITE_CELLPAIR_RT_CELL1_TO_RP #t)
(define WRITE_CELLPAIR_RT_CELL0_TO_RP #t)
(define WRITE_CELLPAIR_RT_CELLy_TO_RP
  (list
   (label WRITE_CELLPAIR_RT_CELL1_TO_RP)
          (LDY !$02)
          (BNE WRITE_CELLPAIR_RT_CELLy_TO_RP)

   (label WRITE_CELLPAIR_RT_CELL0_TO_RP)
          (LDY !$00)

   ;; ----------------------------------------
   (label WRITE_CELLPAIR_RT_CELLy_TO_RP)
          (LDA (ZP_RT),y)
          (STA ZP_RP)
          (INY)
          (LDA (ZP_RT),y)
          (STA ZP_RP+1)
          (RTS)))

(module+ test #| vm-write-rt-celly-to-ra |#
  (define vm-write-rt-celly-to-rp-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
      (JSR ALLOC_CELLPAIR_TO_RT)                ;; rt <- [empty][empty]
      (JSR CP_RT_TO_RZ)
      (LDA !$01)
      (LDY !$10)
      (JSR WRITE_INT_AY_TO_RT)
      (JSR CP_RT_TO_RP)                         ;; rp <- int 1001
      (JSR CP_RZ_TO_RT)
      (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)       ;; rt <- [int 1001][empty]
      (LDA !$10)
      (LDY !$01)
      (JSR WRITE_INT_AY_TO_RT)
      (JSR CP_RT_TO_RP)                         ;; rp <- int 0110
      (JSR CP_RZ_TO_RT)
      (JSR WRITE_RP_TO_CELL1_CELLPAIR_RT)       ;; rt <- [int 1001][int 0110]
      (JSR WRITE_INT0_TO_RT)
      (JSR CP_RT_TO_RP)                         ;; rp <- int 0000
      (JSR CP_RZ_TO_RT)
      (JSR WRITE_CELLPAIR_RT_CELL0_TO_RP)))    ;; rp <- int 1001

  (check-equal? (vm-regp->string vm-write-rt-celly-to-rp-state)
                "int $1001")

  (check-equal? (vm-deref-cell-pair-w->string vm-write-rt-celly-to-rp-state (+ PAGE_AVAIL_0_W #x05))
                "(int $1001 . int $0110)"))

;; @DC-FUN: WRITE_CELLPAIR_RP_CELLy_TO_RP, group: cell_pair
;; overwrite the given cell-pair ptr in RP with the value of the Y's cell pointed to by the cell-pair
;; no reference count is adjusted! this has to be taken care of by caller!
;; input:  Y - 0 (cell0), 2 (cell1)
;;         RP (must be cell-pair ptr) -> [A][B]
;; output: RP = A (Y=0) or B (Y=2)
(define WRITE_CELLPAIR_RP_CELL0_TO_RP #t)
(define WRITE_CELLPAIR_RP_CELL1_TO_RP #t)
(define WRITE_CELLPAIR_RP_CELLy_TO_RP
  (list
   (label WRITE_CELLPAIR_RP_CELL1_TO_RP)
          (LDY !$02)
          (BNE WRITE_CELLPAIR_RP_CELLy_TO_RP)

   (label WRITE_CELLPAIR_RP_CELL0_TO_RP)
          (LDY !$00)

   ;; ----------------------------------------
   (label WRITE_CELLPAIR_RP_CELLy_TO_RP)
          (LDA (ZP_RP),y)
          (TAX)
          (INY)
          (LDA (ZP_RP),y)
          (STA ZP_RP+1)
          (TXA)
          (STA ZP_RP)
          (RTS)))

(module+ test #| vm-write-ra-celly-to-ra |#
    (define vm-write-rp-celly-to-rp-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
      (JSR ALLOC_CELLPAIR_TO_RT)                ;; rt <- [empty][empty]
      (JSR CP_RT_TO_RZ)
      (LDA !$01)
      (LDY !$10)
      (JSR WRITE_INT_AY_TO_RT)
      (JSR CP_RT_TO_RP)                         ;; rp <- int 1001
      (JSR CP_RZ_TO_RT)
      (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)       ;; rt <- [int 1001][empty]
      (LDA !$10)
      (LDY !$01)
      (JSR WRITE_INT_AY_TO_RT)
      (JSR CP_RT_TO_RP)                         ;; rp <- int 0110
      (JSR CP_RZ_TO_RT)
      (JSR WRITE_RP_TO_CELL1_CELLPAIR_RT)       ;; rt <- [int 1001][int 0110]
      (JSR CP_RZ_TO_RT)
      (JSR CP_RT_TO_RP)                         ;; rp <- [int 1001][int 0110]
      (JSR WRITE_CELLPAIR_RP_CELL0_TO_RP)))    ;; rp <- int 1001

  (check-equal? (vm-regp->string vm-write-rp-celly-to-rp-state)
                "int $1001")

  (check-equal? (vm-deref-cell-pair-w->string vm-write-rp-celly-to-rp-state (+ PAGE_AVAIL_0_W #x05))
                "(int $1001 . int $0110)"))

;; write the cell in RP into the CELL Y (0|2) of the cell-pair referenced in RT
;; input:  Y, RT, RP
;; usage:  A, Y
;; output: RT@Y <- RP    Y=0: RT -> [RP][...],  Y=2: RT -> [...][RP]
;; funcs:  -
(define WRITE_RP_TO_CELL0_CELLPAIR_RT #t)
(define WRITE_RP_TO_CELL1_CELLPAIR_RT #t)
(define WRITE_RP_TO_CELLy_CELLPAIR_RT
  (list
   (label WRITE_RP_TO_CELL1_CELLPAIR_RT)
          (LDY !$02)
          (BNE WRITE_RP_TO_CELLy_CELLPAIR_RT)

   (label WRITE_RP_TO_CELL0_CELLPAIR_RT)
   (label WRITE_RP_TO_CELL0_CELLPTR_RT) ;; TODO naming is misleading here
          (LDY !$00)

   ;; ----------------------------------------
   (label WRITE_RP_TO_CELLy_CELLPAIR_RT)
          (LDA ZP_RP)
          (STA (ZP_RT),y)
          (INY)
          (LDA ZP_RP+1)
          (STA (ZP_RT),y)
          (RTS)))

(module+ test #| vm-write-rp-to-celly-rt |#
  (define vm_write_rp_to_celly_rt_state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
      (JSR ALLOC_CELLPAIR_TO_RT)                ;; rt <- [emtpy][empty]
      (JSR CP_RT_TO_RZ)
      (LDA !$01)
      (LDY !$10)
      (JSR WRITE_INT_AY_TO_RT)
      (JSR CP_RT_TO_RP)                         ;; rp <- int 1001
      (JSR CP_RZ_TO_RT)
      (LDY !$00)
      (JSR WRITE_RP_TO_CELLy_CELLPAIR_RT)       ;; rt <- [int 1001][empty]
      (JSR CP_RT_TO_RZ)
      (LDA !$10)
      (LDY !$01)
      (JSR WRITE_INT_AY_TO_RT)
      (JSR CP_RT_TO_RP)                         ;; rp <- int 0110
      (JSR CP_RZ_TO_RT)
      (LDY !$02)
      (JSR WRITE_RP_TO_CELLy_CELLPAIR_RT)))    ;; rt <- [int 1001][0110]

  (check-equal? (vm-regp->string vm_write_rp_to_celly_rt_state)
                "int $0110"
                "rp is filled with last written int 0110")

  (check-equal? (vm-regt->string vm_write_rp_to_celly_rt_state)
                (format "pair-ptr[0] $~a05" (format-hex-byte PAGE_AVAIL_0))
                "rt is a cell-pair ptr to the first cell (05) of the first page available")

  (check-equal? (vm-deref-cell-pair-w->string vm_write_rp_to_celly_rt_state (+ PAGE_AVAIL_0_W #x05))
                "(int $1001 . int $0110)"
                "dereferencing the cell pair in rt, yields the int pairs 1001 and 0110"))

;; pass in the entry label which uses lowbyte (tag byte) already lsr'd twice!
(define (INC_REFCNT_CELLPAIR_RT lsred-label)
  (list
   (label INC_REFCNT_CELLPAIR_RT)
          (LDA ZP_RT)
          (LSR)
          (LSR)
   (ast-label-def-cmd '() lsred-label)
   ;; (label LSR__INC_RFCNT_CELLPAIR__)
          (TAX)
          (LDA ZP_RT+1)
          (BEQ DONE__)
          ;; not nil
          (STA INC_PAGE_CNT__CELLPAIR__+2) ;; store high byte (page) into inc-command high-byte (thus +2 on the label)
   (label INC_PAGE_CNT__CELLPAIR__)
          (INC $c000,x) ;; c0 is overwritten by page (see above)
          (RTS)))


(module+ test #| vm-refcount-mmcr-rt--cell-pair-ptr |#
  (define vm-refcount-mmcr-rt--cell-pair-ptr-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR INC_REFCNT_CELLPAIR_RT)
     (JSR INC_REFCNT_CELLPAIR_RT)))

  (check-equal? (vm-regt->string vm-refcount-mmcr-rt--cell-pair-ptr-state)
                (format "pair-ptr[2] $~a05" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-refcount-cell-pair-ptr vm-refcount-mmcr-rt--cell-pair-ptr-state (+ PAGE_AVAIL_0_W #x05))
                2)

  (define vm-refcount-mmcr-rt--cell-pair-ptr-state2
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR INC_REFCNT_CELLPAIR_RT)
     (JSR INC_REFCNT_CELLPAIR_RT)
     (JSR DEC_REFCNT_CELLPAIR_RT)))

  (check-equal? (vm-regt->string vm-refcount-mmcr-rt--cell-pair-ptr-state2)
                (format "pair-ptr[1] $~a05" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-refcount-cell-pair-ptr vm-refcount-mmcr-rt--cell-pair-ptr-state2 (+ PAGE_AVAIL_0_W #x05))
                1))

;; pass in the entry label which uses lowbyte (tag byte) already lsr'd twice!
(define (DEC_REFCNT_CELLPAIR_RZ lsred-label)
  (list
   (label DEC_REFCNT_CELLPAIR_RA)
          (JSR CP_RA_TO_RZ)
          (CLC)
          (BCC DEC_REFCNT_CELLPAIR_RZ)

   (label DEC_REFCNT_CELLPAIR_RT)
          (JSR CP_RT_TO_RZ)

   ;; input: cell-pair ptr in ZP_RA
   ;; decrement ref count, if 0 deallocate
   (label DEC_REFCNT_CELLPAIR_RZ)
          (LDA ZP_RZ)
          (LSR)
          (LSR)

   (ast-label-def-cmd '() lsred-label)
   ;; (label CELLPAIR_ALREADY_LSRED__)
          (TAX)
          ;; now decrement cell count
          (LDA ZP_RZ+1)
          (BEQ DONE__) ;; nil -> done
          (STA DEC_PAGE_CELLPAIR_CNT__+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
   (label DEC_PAGE_CELLPAIR_CNT__)
          (DEC $c000,x) ;; c0 is overwritten by page (see above)
          (BNE DONE__)
          (JMP FREE_CELLPAIR_RZ) ;; free (since refcnt dropped to 0)
))

;; @DC-FUN: FREE_CELLPAIR_RZ, group: gc
;; put the given cellpair on the global free list
;; dec-refcnt cell0 if it is a ptr, defer dec-refcnt cell1 to allocation/reuse time
;; input: RZ (RT RA), GLOBAL_CELLPAIR_FREE_LIST
;; usage: A, X, Y, RZ
;; output: GLOBAL_CELLPAIR_FREE_LIST << RZ
;; funcs:
;;   DEC_REFCNT_RZ>>
(define FREE_CELLPAIR_RT #t)
(define FREE_CELLPAIR_RA #t)
(define FREE_CELLPAIR_RZ
  (add-label-suffix
   "__" "__NEW_FREE_CELLPAIR_RZ"
   (list
    (label FREE_CELLPAIR_RA)
           (JSR CP_RA_TO_RZ)
           ;; (CLC)
           ;; (BCC FREE_CELLPAIR_RZ)
           (JMP FREE_CELLPAIR_RZ)

    (label FREE_CELLPAIR_RT)
           (JSR CP_RT_TO_RZ)

    (label FREE_CELLPAIR_RZ)
           (LDY !$00)
           (STY ZP_TEMP+1) ;; indicator of a ptr to free is set to 0 => currently no additional ptr marked for free

           ;; check cell0
           (LDA (ZP_RZ),y) ;; LOWBYTE OF FIRST cell0
           (BEQ CELL_0_FREE_TO_USE__) ;; empty
           (STA ZP_TEMP)
           (AND !$03)
           (CMP !$03)
           (BEQ CELL_0_FREE_TO_USE__)
           ;; make sure to call free on cell0 (could be any type of cell)
           ;; remember ZP_PTR

           ;; store cell0 into ZP_TEMP (for later tail call of free)
           (INY)
           (LDA (ZP_RZ),y)
           (STA ZP_TEMP+1) ;; cell0 was stored in zp_temp -> cell0 is now free to be used as link for free cell pairs

   ( label CELL_0_FREE_TO_USE__)
           ;; cell0 is no ptr and can thus be discarded (directly)

           ;; enqueue rt as new root of free cellpairs

           ;; simply add this cell-pair as head to free tree
           ;; set cell0 to point to old root
           (LDY !$01)
           (LDA GLOBAL_CELLPAIR_FREE_LIST+1)
           (STA (ZP_RZ),y)
           (DEY)
           (LDA GLOBAL_CELLPAIR_FREE_LIST)
           (STA (ZP_RZ),y)

           ;; set new root to point to cell-pair
           (LDA ZP_RZ+1)
           (STA GLOBAL_CELLPAIR_FREE_LIST+1)
           (LDA ZP_RZ)
           (STA GLOBAL_CELLPAIR_FREE_LIST)

           ;; write original cell0 -> zp_rc
           (LDA ZP_TEMP+1)
           (BEQ DONE__)
           ;; otherwise zp_temp was used to store a pointer that needs to be decremented
           (STA ZP_RZ+1)
           (LDA ZP_TEMP)
           (STA ZP_RZ)

           (JMP DEC_REFCNT_RZ) ;; chain call

    (label DONE__)
           (RTS))))


(module+ test #| new-free-cell-pair-ptr-in-rt |#
  (define new-free-cell-pair-ptr-in-rt-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDA !$00)
     (STA ZP_RT)
     (STA ZP_RT+1)

     (JSR FREE_CELLPAIR_RT)))

  (check-equal? (vm-regt->string new-free-cell-pair-ptr-in-rt-state)
                "empty")

  (define new-free-cell-pair-ptr-in-rt-2-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELLPAIR_TO_RT)

     (JSR FREE_CELLPAIR_RT)))

  (check-equal? (vm-page->strings new-free-cell-pair-ptr-in-rt-2-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09")
                "page has still 1 slot in use (it was freed, but is now in free list, not completely unallocated)")
  (check-equal? (vm-cell-pair-free-tree->string new-free-cell-pair-ptr-in-rt-2-state)
                (format "pair $~a05 -> [ empty . empty ]" (format-hex-byte PAGE_AVAIL_0)))

  (define new-free-cell-pair-ptr-in-rt-3-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR CP_RT_TO_RZ)
     (JSR WRITE_INT1_TO_RT)
     (JSR CP_RT_TO_RP)
     (JSR CP_RZ_TO_RT)
     (JSR WRITE_RP_TO_CELL1_CELLPAIR_RT)
     (JSR WRITE_INT0_TO_RT)
     (JSR CP_RT_TO_RP)
     (JSR CP_RZ_TO_RT)
     (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)
     (JSR INC_REFCNT_RT)
     (JSR CP_RT_TO_RP)

     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)
     (JSR WRITE_NIL_TO_RP)
     (JSR WRITE_RP_TO_CELL1_CELLPAIR_RT)

     ;; rt = ( -+ . NIL )                cell-pair @ cc09
     ;;         |
     ;;         +--> ( int0 . int1 )     cell-pair @ cc05

     (JSR FREE_CELLPAIR_RT)))

  (check-equal? (vm-page->strings new-free-cell-pair-ptr-in-rt-3-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41")
                "page has still 2 slot in use (it was freed, but is now in free list, not completely unallocated)")
  (check-equal? (vm-cell-pair-free-tree->string new-free-cell-pair-ptr-in-rt-3-state)
                (format "pair $~a05 -> [ pair-ptr[-] $~a09 . int $0001 ]"
                        (format-hex-byte PAGE_AVAIL_0)
                        (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-deref-cell-pair-w->string new-free-cell-pair-ptr-in-rt-3-state (+ PAGE_AVAIL_0_W #x09))
                "(empty . pair-ptr NIL)")

;; TODO: move this test in some integration area
  ;; (define new-free-cell-pair-ptr-in-rt-4-state
  ;;   (compact-run-code-in-test-
  ;;    #:runtime-code test-runtime

  ;;    ;; #:debug #t
  ;;    (JSR ALLOC_CELL_TO_RT)                     ;; RT -> [ ]
  ;;    (JSR INC_REFCNT_RT)
  ;;    (JSR CP_RT_TO_RZ)                          ;; RZ -> [ ]

  ;;    (JSR WRITE_INTm1_TO_RT)                    ;; RT = -1
  ;;    (JSR CP_RT_TO_RP)                          ;; RP = -1
  ;;    (JSR CP_RZ_TO_RT)                          ;; RT -> [ ]
  ;;    (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)        ;; RT -> [-1]
  ;;    (JSR CP_RT_TO_RP)                          ;; RP -> [-1]

  ;;    (JSR ALLOC_CELLPAIR_TO_RT)                 ;; RT -> [ ][ ]
  ;;    (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)        ;; RT -> [->[-1]][ ]
  ;;    (JSR CP_RT_TO_RZ)                          ;; RZ -> [->[-1]][ ]
  ;;    (JSR WRITE_INT1_TO_RT)                     ;; RT = -1
  ;;    (JSR CP_RT_TO_RP)                          ;; RP = -1
  ;;    (JSR CP_RZ_TO_RT)                          ;; RT -> [->[-1]][ ]
  ;;    (JSR WRITE_RP_TO_CELL1_CELLPAIR_RT)        ;; RT -> [->[-1]][1]

  ;;    ;; rt -> ( -+ . int1 )               cell-pair @ cb05
  ;;    ;;          |
  ;;    ;;          +--> ( int-1 )           cell      @ cc02

  ;;    (JSR FREE_CELLPAIR_RT)))

  ;; (check-equal? (vm-page->strings new-free-cell-pair-ptr-in-rt-4-state PAGE_AVAIL_0)
  ;;               (list "page-type:      cell page"
  ;;                     "previous page:  $00"
  ;;                     "slots used:     1"
  ;;                     "next free slot: $08")
  ;;               "page has still 1 slot in use (it was freed, but is now in free list, not completely unallocated)")
  ;; (check-equal? (vm-page->strings new-free-cell-pair-ptr-in-rt-4-state PAGE_AVAIL_1)
  ;;               (list "page-type:      cell-pair page"
  ;;                     "previous page:  $00"
  ;;                     "slots used:     1"
  ;;                     "next free slot: $09")
  ;;               "page has still 1 slot in use (it was freed, but is now in free list, not completely unallocated)")
  ;; (check-equal? (vm-cell-pair-free-tree->string new-free-cell-pair-ptr-in-rt-4-state)
  ;;               (format "pair $~a05 -> [ empty . int $0001 ]" (format-hex-byte PAGE_AVAIL_1)))
  ;; ;; (check-equal? (vm-deref-cell-w->string new-free-cell-pair-ptr-in-rt-4-state GLOBAL_CELL_FREE_LIST)
  ;; ;;               (format "ptr[-] $~a02" (format-hex-byte PAGE_AVAIL_0)))
  ;; (check-equal? (vm-deref-cell-w->string new-free-cell-pair-ptr-in-rt-4-state (+ PAGE_AVAIL_0 #x02))
  ;;               "empty")
  )

;; @DC-FUN: WRITE_RT_TO_CELLy_CELLPAIR_RP, group: cell_pair
;; write the cell in RT into the CELL Y (0|2) of the cell-pair referenced in RP
;; input:  Y, RT, RP
;; usage:  A, Y
;; output: RP@Y <- RT    Y=0: RP -> [RT][...],  Y=2: RP -> [...][RT]
;; funcs:  -
(define WRITE_RT_TO_CELL0_CELLPAIR_RP #t)
(define WRITE_RT_TO_CELL1_CELLPAIR_RP #t)
(define WRITE_RT_TO_CELLy_CELLPAIR_RP
  (list
   (label WRITE_RT_TO_CELL1_CELLPAIR_RP)
          (LDY !$02) ;; offset 2 for cell1
          (BNE WRITE_RT_TO_CELLy_CELLPAIR_RP)

   (label WRITE_RT_TO_CELL0_CELLPAIR_RP)
          (LDY !$00) ;; offset 0 for cell0

   ;; ----------------------------------------
   (label WRITE_RT_TO_CELLy_CELLPAIR_RP)
          (LDA ZP_RT)
          (STA (ZP_RP),y)
          (INY)
          (LDA ZP_RT+1)
          (STA (ZP_RP),y)
          (RTS)))

(module+ test #| vm-write-rt-to-celly-ra |#
  (define vm_write_rt_to_celly_rp_state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELLPAIR_TO_RT)                ;; rt <- [empty][empty]
      (JSR CP_RT_TO_RP)                         ;; rp <- [empty][empty]
      (LDA !$01)
      (LDY !$10)
      (JSR WRITE_INT_AY_TO_RT)                  ;; rt <- [int 1001]
      (LDY !$00)
      (JSR WRITE_RT_TO_CELLy_CELLPAIR_RP)       ;; rp <- [int 1001][empty]
      (LDA !$10)
      (LDY !$01)
      (JSR WRITE_INT_AY_TO_RT)                  ;; rt <- [int 0110]
      (LDY !$02)
      (JSR WRITE_RT_TO_CELLy_CELLPAIR_RP)))    ;; rp <- [int 1001][int 0110]

  (check-equal? (vm-regt->string vm_write_rt_to_celly_rp_state)
                "int $0110"
                "rt is filled with last written int 0110")

  (check-equal? (vm-regp->string vm_write_rt_to_celly_rp_state)
                (format "pair-ptr[0] $~a05" (format-hex-byte PAGE_AVAIL_0))
                "rp is a cell-pair ptr to the first cell (05) of the first page available")

  (check-equal? (vm-deref-cell-pair-w->string vm_write_rt_to_celly_rp_state (+ PAGE_AVAIL_0_W #x05))
                "(int $1001 . int $0110)"
                "dereferencing the cell pair in rp, yields the int pairs 1001 and 0110"))

;; @DC-FUN: GC_CELLPAIR_FREE_LIST, group: gc
;; actively free all enqueued cell pairs of the free-list!
;; can be useful to find out whether a whole page is not used at all. free cells are still marked as used on a page.
;; input:  GLOBAL_CELLPAIR_FREE_LIST
;; usage:  A, Y, RZ
;; output: GLOBAL_CELLPAIR_FREE_LIST+1 = 0
;; funcs:
;;   DEC_REFCNT_RZ >>
;;   GC_CELLPAIR_FREE_LIST
(define GC_CELLPAIR_FREE_LIST
  (add-label-suffix
   "__" "__GC_CELLPAIR_FREE_LIST"
  (list
   (label GC_CELLPAIR_FREE_LIST)
          (LDA GLOBAL_CELLPAIR_FREE_LIST+1) ;; get highbyte (page) from ptr to cell-pair
          (BNE CONTINUE__)   ;; if = 0, queue is empty, i'm done
          (RTS)

   (label CONTINUE__)
          ;; put ptr to cell-pair into RZ, now RZ->cell0,cell1 with cell0 = pointer to the next in queue, cell1 could still be something that needs gc
          (STA ZP_RZ+1)
          (LDA GLOBAL_CELLPAIR_FREE_LIST)
          (STA ZP_RZ)

          ;; set new tree root for free tree to original cell0
          (LDY !$00)
          (LDA (ZP_RZ),y)
          (BEQ CELL0_IS_NO_PTR__) ;; is zero => completely empty
          (AND !$03)
          (CMP !$03)
          (BEQ CELL0_IS_NO_PTR__) ;; is no ptr
          (INY)
          (LDA (ZP_RZ),y)
          (BEQ CELL0_IS_NO_PTR__) ;; is nil
          (DEY)

          ;; cell0 is a cell-pair-ptr => make new root of free queue
          (STA GLOBAL_CELLPAIR_FREE_LIST+1)
          (LDA (ZP_RZ),y)
          (STA GLOBAL_CELLPAIR_FREE_LIST)
          (BNE CHECK_CELL1__) ;; since must be !=0, it cannot be on page 0 always branch!

   (label CELL0_IS_NO_PTR__)
          ;; queue is now empty, this was the last cell-pair
          ;; clear queue
          (LDA !$00)
          (STA GLOBAL_CELLPAIR_FREE_LIST+1) ;; just reset highbyte (checked at start of this function)
          ;; (STA GLOBAL_CELLPAIR_FREE_LIST)

   (label CHECK_CELL1__)
          ;; now check cell1 on remaining ptrs
          (LDY !$02)
          (LDA (ZP_RZ),y) ;; get low byte
          (TAX) ;; remember low byte
          (BEQ CELL1_IS_NO_PTR__) ;; = 0 means totally empty => no ptr
          (AND !$03)       ;; mask out all but low 2 bits
          (CMP !$03)
          (BEQ CELL1_IS_NO_PTR__) ;; no need to do further deallocation
          (INY)
          (LDA (ZP_RZ),y)
          (BEQ CELL1_IS_NO_PTR__) ;; is nil

          ;; write cell1 into zp_rc and decrement
          (TAY)

          (LDA ZP_RZ)
          (STA RC_COPY__)
          (LDA ZP_RZ+1)
          (STA RC_COPY__+1)

          (STX ZP_RZ)
          (STY ZP_RZ+1)

          (JSR DEC_REFCNT_RZ) ;; this may change the queue again, which is alright, since RZ was removed from queue

          (LDA RC_COPY__)
          (STA ZP_RZ)
          (LDA RC_COPY__+1)
          (STA ZP_RZ+1)

  (label CELL1_IS_NO_PTR__)
          ;; now add ra to its page as free cell-pair on that page
          (LDX ZP_RZ+1)                 ;; A = page -> x
          (LDA $cf00,x)         ;; current first free cell offset
          (LDY !$00)
          (STA (ZP_RZ),y)       ;; write into lowbyte of cell pointed to by RZ
          ;; (INY)
          ;; (TXA)
          ;; (STA (ZP_RZ),y)       ;; write page into highbyte of cell pointed to by RZ
          (LDA ZP_RZ)             ;; get offset into page of cell RZ points to
          (STA $cf00,x)           ;; new first free cell now points to RZ
          (LDA ZP_RZ+1)
          (STA DEC_COMMAND__+2)
   (label DEC_COMMAND__)
          (DEC $c000)         ;; decrement number of used slots on cell-pair page (c0 is overwritten with page in zp_ra+1
          (JMP GC_CELLPAIR_FREE_LIST) ;; do this until queue is empty

   (label RC_COPY__)
          (word 0))))

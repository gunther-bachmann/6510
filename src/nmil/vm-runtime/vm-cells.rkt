#lang racket/base


#|

  cell functions

|#

(require "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix
                  replace-labels)
         (only-in "./vm-memory-map.rkt"
                  TAGGED_NIL
                  ZP_RP
                  ZP_RT
                  VM_MEMORY_MANAGEMENT_CONSTANTS))

(provide INIT_CELL_PAGE_X_TO_AX                ;; initialize page (in a) for cell usage
         INC_REFCNT_CELL_RT                    ;; increment refcount of the cell, rt is pointing to
         DEC_REFCNT_CELL_RZ                    ;; decrement refcount of the cell pointed to by rz
         FREE_CELL_RZ                          ;; free the cell pointed to by rz (must not contain anything that needs gc)
         ALLOC_CELL_TO_RT                      ;; allocate a cell and put the pointer to that cell into rt
         GET_FRESH_CELL_TO_AX                  ;; get a completely fresh cell into A/X (no cell reuse)
         ALLOC_CELL_AX_TO_RT                   ;; allocate the cell A/X into RT
         GC_CELLS)                             ;; garbage collect all cells

(module+ test
  (require (only-in racket/list make-list)
           "../../6510-test-utils.rkt"
           (only-in "../../tools/6510-interpreter.rkt" peek memory-list)
           (only-in "../vm-inspector-utils.rkt" vm-page->strings)
           "./vm-memory-manager-test-utils.rkt"
           (only-in "./vm-pages.rkt"
                    ALLOC_PAGE_TO_X
                    VM_PAGE_SLOT_DATA
                    VM_INITIAL_MM_REGS
                    VM_INITIALIZE_MEMORY_MANAGER)
           (only-in "./vm-register-functions.rkt"
                    CP_RT_TO_RZ
                    CP_RT_TO_RA
                    CP_RA_TO_RT
                    CP_RT_TO_RP
                    CP_RA_TO_RZ))

  (define PAGE_AVAIL_0 #x8d)      ;; high byte of first page available for allocation
  (define PAGE_AVAIL_0_W #x8d00)  ;; word (absolute address) of first page available
  (define PAGE_AVAIL_1 #x8c)      ;; high byte of second page available for allocation
  (define PAGE_AVAIL_1_W #x8c00) ;; word (absolute address) of second page available

  (define WRITE_RP_TO_CELL_POINTED_TO_BY_RT
    (list
     (label WRITE_RP_TO_CELL_POINTED_TO_BY_RT)
          (LDY !$00)
          (LDA ZP_RP)
          (STA (ZP_RT),y)
          (INY)
          (LDA ZP_RP+1)
          (STA (ZP_RT),y)
          (RTS)))

  (define test-runtime
    (append
     ALLOC_PAGE_TO_X
     ALLOC_CELL_TO_RT
     ALLOC_CELL_AX_TO_RT
     GET_FRESH_CELL_TO_AX
     INIT_CELL_PAGE_X_TO_AX
     (DEC_REFCNT_CELL_RZ "CELL_ALREADY_LSRED__" "DEC_REFCNT_CELL_RZ_TO_M1_SLOT__")
     (list (label DEC_REFCNT_CELL_RZ_TO_M1_SLOT__) (BRK))
     (INC_REFCNT_CELL_RT "NOW_INCREMENT_REFCNT__CELL__")
     (list (label DONE__) (RTS))
     FREE_CELL_RZ
     (list (label INC_REFCNT_M1_PAGE) (RTS))
     (list (label FREE_CELLPAIR_RZ) (RTS))
     (list (label DEC_REFCNT_CELLARR_RZ) (RTS))
     (list (label DEC_REFCNT_NATIVEARR_RZ) (RTS))
     (list (label DEC_REFCNT_RZ) (JMP DEC_REFCNT_CELL_RZ))


     WRITE_RP_TO_CELL_POINTED_TO_BY_RT
     ;;WRITE_NIL_TO_RP
     CP_RT_TO_RZ
     CP_RT_TO_RP
     CP_RA_TO_RZ
     CP_RA_TO_RT
     CP_RT_TO_RA
     VM_INITIALIZE_MEMORY_MANAGER
     VM_MEMORY_MANAGEMENT_CONSTANTS
     (list (label INIT_CELLSTACK_PAGE_X) (RTS))
     (list (org #xcec0))
     VM_INITIAL_MM_REGS
     (list (org #xcf00))
     VM_PAGE_SLOT_DATA)))

  ;; page type cell page (slot size 2b) (refcount @ ptr >> 1) 84 cells (85th slot is used for previous page pointer)
  ;; offset content
  ;; 00     #b1zzz zzzz page type + number of used slots
  ;; 01     ref-count for cell at 02 (cell 0)
  ;; 02..03 cell 0  (@2 = 8 = next free cell)
  ;; 04     ref-count for cell at 08 (cell 1)
  ;; ...
  ;; 07     ref-count for cell at 08 (cell 4)
  ;; 08..09 cell 1 (@08 = 0a = next free cell)
  ;; ...
  ;; 0e..0f cell 4 (@0e = 20 = next free cell
  ;; 10    ref-count for cell at 20 (cell 5)
  ;; ...
  ;; 1f    ref-count for cell at 20 (cell 20)
  ;; 20..21 cell 5 (@20 = 22 = next free cell)
  ;; ...
  ;; 3e..3f cell 20 (@3e = 80 = next free cell)
  ;; 40..7e ref-count for cell at 80..fc (cell 21..83)
  ;; 7f    unused
  ;; 80..fd cell 21..83
  ;; fe    unused
  ;; ff    previous page of this type
  ;;
  ;; input:  X = allocated uninitialized page
  ;; output: X = initialized page (of type cell page)
  ;;         A = first free slot
  ;;         vm_free_cell_page is new head of the list
  ;;         the page is initialized with each cell pointing to the next free cell on this page (0 marks the end)
  ;; uses: ZP_TEMP, ZP_TEMP2
  (define INIT_CELL_PAGE_X_TO_AX
    (add-label-suffix
     "__" "__INIT_CELL_PAGE_X_TO_AX"
    (list
     (label INIT_CELL_PAGE_X_TO_AX)
            ;; page is in A
            (STX ZP_TEMP+1)
            (LDA !$02)
            (STA VM_PAGE_SLOT_DATA,x) ;; set slot @02 as the first free slot

            (LDA !$03)
            (STA BLOCK_LOOP_COUNT__) ;; how many blocks do we have (3)

            (LDA !$00)
            (STA ZP_TEMP)

            (LDY !$01)
            (LDX !$01)
            (STX LOOP_COUNT__)

     ;; option: optimization: maybe clearing the whole page would be faster (and shorter) for setting all refcounts to 0?
     (label LOOP_REF_COUNT__)
            (STA (ZP_TEMP),y) ;; refcount set to 0
            (INY)
            (DEX)
            (BNE LOOP_REF_COUNT__)
            (LDA LOOP_COUNT__)
            (ASL A)
            (ASL A) ;; times 4
            (STA LOOP_COUNT__)
            (TAX)
            (TAY) ;;
            (LDA !$00)
            (DEC BLOCK_LOOP_COUNT__)
            (BPL LOOP_REF_COUNT__)

            ;; initialize the free list of the cells (first byte in a cell = offset to next free cell)
            (LDA !$02)
            (STA BLOCK_LOOP_COUNT__) ;; how many blocks do we have (3, but the first block is written separately)

            ;; block 1
            (LDY !$02)
            (LDA !$08)
            (STA LOOP_COUNT__)
            (STA (ZP_TEMP),y)

            ;; block 2
            (TAY)
            (LDX !$04)
            (LDA !$0a)
            (DEX) ;; one loop less

     ;; blocks and their numbers
     ;; iterations offset next free (
     ;; #01        02     <- 08
     ;; #04        08..0f <- 0a.. last= 20   ->  # = prev offset*2, offset = prev offset*4
     ;; #10        20..3f <- 22.. last= 80
     ;; #40        80..7d <- 82.. last= 00

     (label LOOP_NEXT_FREE__)
            (STA (ZP_TEMP),y)
            (TAY)
            (CLC)
            (ADC !$02)
            (DEX)
            (BNE LOOP_NEXT_FREE__)

            ;; block n+1
            ;; write last entry
            (LDA LOOP_COUNT__)
            (ASL A)
            (TAX)
            (ASL A)
            (STA LOOP_COUNT__)
            (STA (ZP_TEMP),y)
            (TAY)
            (CLC)
            (ADC !$02)
            (DEX)
            (DEC BLOCK_LOOP_COUNT__)
            (BPL LOOP_NEXT_FREE__)

            ;; write last entry
            (LDA !$00)
            (LDY !$fc) ;; fc..fd is the last cell, fe..ff is unusable (since ff holds the previous page)
            (STA (ZP_TEMP),y)

            (LDY !$ff)
            (LDA GLOBAL_CELL_PAGE_FOR_ALLOC) ;; store last free cell page in $ff
            (STA (ZP_TEMP),y)

            ;; store page type in byte 0
            (LDY !$00)
            (LDA !$80)
            (STA (ZP_TEMP),y)

            (LDX ZP_TEMP+1) ;; page
            (STX GLOBAL_CELL_PAGE_FOR_ALLOC) ;; store allocated page as new free cell page
            (LDA !$02)

            (RTS)

     (label LOOP_COUNT__)
            (byte $00)
     (label BLOCK_LOOP_COUNT__)
            (byte $00))))

(module+ test #| vm_alloc_page__cell |#
  (define test-alloc-page--cell-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
            ;; fill page with cc
            (LDX !$00)
            (LDA !$77)
     (label FILL_PAGE__TEST_ALLOC_PAGE__CELL)
            ;; (STA $9700,x)
            (ast-opcode-cmd '() (list 157 0 PAGE_AVAIL_0))
            (DEX)
            (BNE FILL_PAGE__TEST_ALLOC_PAGE__CELL)

            ;; now do allocation and write structure data into the page
            (JSR ALLOC_PAGE_TO_X)
            (JSR INIT_CELL_PAGE_X_TO_AX) ))

  (check-equal? (memory-list test-alloc-page--cell-state-after PAGE_AVAIL_0_W (+ PAGE_AVAIL_0_W #x0f))
                (list #x80
                      #x00       ;; ref count cell 0 (@2)
                      #x08 #x77  ;; cell0: next free @8
                      #x00       ;; refcount cell1 (@8)
                      #x00       ;; refcount cell2 (@a)
                      #x00       ;; refcount cell3 (@c)
                      #x00       ;; refcount cell4 (@e)
                      #x0a #x77  ;; cell1: next free @10
                      #x0c #x77  ;; cell2: next free @12
                      #x0e #x77  ;; cell3: next free @14
                      #x20 #x77  ;; cell4: next free @32
                      ))
  (check-equal? (memory-list test-alloc-page--cell-state-after (+ PAGE_AVAIL_0_W #x10) (+ PAGE_AVAIL_0_W #x1f))
                (make-list #x10 #x0))
  (check-equal? (memory-list test-alloc-page--cell-state-after (+ PAGE_AVAIL_0_W #x20) (+ PAGE_AVAIL_0_W #x27))
                (list #x22 #x77  ;; cell5: next free @34
                      #x24 #x77  ;; cell6: next free @36
                      #x26 #x77  ;; cell7: next free @38
                      #x28 #x77  ;; cell8: next free @40
                      ))
  (check-equal? (memory-list test-alloc-page--cell-state-after (+ PAGE_AVAIL_0_W #x38) (+ PAGE_AVAIL_0_W #x3f))
                (list #x3a #x77  ;; cell17: next free @58
                      #x3c #x77  ;; cell18: next free @60
                      #x3e #x77  ;; cell19: next free @62
                      #x80 #x77  ;; cell20: next free @128
                      ))
  (check-equal? (memory-list test-alloc-page--cell-state-after (+ PAGE_AVAIL_0_W #x40) (+ PAGE_AVAIL_0_W #x7e))
                (make-list #x3f #x0)
                "refcounts are all zero")
  (check-equal? (memory-list test-alloc-page--cell-state-after (+ PAGE_AVAIL_0_W #x80) (+ PAGE_AVAIL_0_W #x87))
                (list #x82 #x77  ;; cell21: next free @130
                      #x84 #x77  ;; cell22: next free @132
                      #x86 #x77  ;; cell23: next free @134
                      #x88 #x77  ;; cell24: next free @136
                      ))
  (check-equal? (memory-list test-alloc-page--cell-state-after (+ PAGE_AVAIL_0_W #xf8) (+ PAGE_AVAIL_0_W #xff))
                (list #xfa #x77  ;; cell: next free @250
                      #xfc #x77  ;; cell: next free @252
                      #x00 #x77  ;; cell: next free 0
                      #x00       ;; unused
                      #x00       ;; pointer to previous page
                      )))



(define (INC_REFCNT_CELL_RT now-increment-label)
  (list
   (label INC_REFCNT_CELL_RT)
          ;; find out which page type is used (cell-ptr-page, m1-page, slot-page)
          (LDA ZP_RT+1) ;; highbyte (page)
          (BEQ DONE__) ;; page=0 => empty, nothing to be done
          (STA LOAD_PAGE_TYPE__CELL__+2)
   (label LOAD_PAGE_TYPE__CELL__)
          (LDA $c000) ;; c0 is overwritten by page
          (BMI IS_CELL_PAGE__)
          (AND !$ec)
          (BEQ INC_REFCNT_M1_PAGE)

          (BRK) ;; unknown page type

   (label IS_CELL_PAGE__)
          (LDA ZP_RT) ;; lowbyte (offset)
          (LSR)
          (TAX)

   (ast-label-def-cmd '() now-increment-label)
   ;; (label NOW_INCREMENT_REFCNT__CELL__)
          (LDA ZP_RT+1)
          (STA INC_PAGE_REFCNT_CELL__+2) ;; store high byte (page) into inc-command high-byte (thus +2 on the label)
   (label INC_PAGE_REFCNT_CELL__)
          (INC $c000,x) ;; c0 is overwritten by page (see above)
          (RTS)))


(define DEC_REFCNT_CELL_RT #t)
(define DEC_REFCNT_CELL_RA #t)
;; pass in the entry label which uses lowbyte (tag byte) already lsr'd twice!
(define (DEC_REFCNT_CELL_RZ lsred-label dec-refcnt-m1-label)
  (list
   (label DEC_REFCNT_CELL_RA)
          (JSR CP_RA_TO_RZ)
          (CLC)
          (BCC DEC_REFCNT_CELL_RZ)

   (label DEC_REFCNT_CELL_RT)
          (JSR CP_RT_TO_RZ)

   ;; input: cell ptr in ZP_RA
   ;; decrement ref count, if 0 deallocate
   (label DEC_REFCNT_CELL_RZ)  ;; RZ -> [cell] | [cell-array] | [native-array]
          (LDA ZP_RZ) ;; lowbyte (offset)
          (LSR)

   (ast-label-def-cmd '() lsred-label)
   ;; (label CELL_ALREADY_LSRED__)
          ;; check what cell kind the target is: cell, cell-ptr, cell-pair-ptr, native-array, cell-array
          (LDY ZP_RZ+1)
          (BEQ DONE__) ;; nil -> done
          (STY LDA_PAGE_TYPE__+2)
          (TAX)
   (label LDA_PAGE_TYPE__)
          (LDA $c000)
          (ASL A)

          ;; (BCC DEC_REFCNT_CELL_RZ_TO_M1_SLOT__)
          (ast-unresolved-rel-opcode-cmd
           '()
           '(144)
           (ast-resolve-byte-scmd dec-refcnt-m1-label  'relative))

   (label DEC_REFCNT_CELL_RZ_TO_CELL__)
          (STY DEC_PAGE_CELL_CNT__+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
   (label DEC_PAGE_CELL_CNT__)
          (DEC $c000,x)               ;; c0 is overwritten by page (see above), x = position of refcount (a >> 1)
          (BNE DONE__)
          (JMP FREE_CELL_RZ)      ;; free (since refcnt dropped to 0), and this is definitely a cell-ptr => free-cell can be called
   ))

(module+ test
    (define dec-refcnt-rc--dec-ref--cell
    (compact-run-code-in-test-
     ;; #:debug #t
     #:runtime-code test-runtime

     (JSR ALLOC_CELL_TO_RT)    ;; new cell in RT (with refcount = 0)
     (JSR INC_REFCNT_CELL_RT) ;; now should be 1
     (JSR INC_REFCNT_CELL_RT) ;; now should be 2
     (JSR CP_RT_TO_RZ)

     ;; unit under test
     (JSR DEC_REFCNT_RZ)))

  (check-equal? (calls-to-mock dec-refcnt-rc--dec-ref--cell)
                #x00
                "no free cell has taken place!")
  (check-equal? (peek dec-refcnt-rc--dec-ref--cell (+ PAGE_AVAIL_0_W 01))
                #x01
                "remaining refcount on cell0 is 1"))

;; @DC-FUN: FREE_CELL_RZ, group: gc
;; free the given cell in RZ (RA, RT), and dec-refcnt its content (if it is a pointer)
;; it must not be a header cell of an array or something
;; input: RZ
;; usage: A, X, Y, RZ
;; output: -
;; funcs:
;;   DEC_REFCNT_RZ
;;   (CP_RA_TO_RZ)
;;   (CP_RT_TO_RZ)
(define FREE_CELL_RT #t)
(define FREE_CELL_RA #t)
(define FREE_CELL_RZ
  (add-label-suffix
   "__" "__NEW_FREE_CELL_RZ"
   (list
    (label FREE_CELL_RA)
           (JSR CP_RA_TO_RZ)
           ;; (CLC)
           ;; (BCC FREE_CELL_RZ)
           (JMP FREE_CELL_RZ)

    (label FREE_CELL_RT)
           (JSR CP_RT_TO_RZ)

    (label FREE_CELL_RZ)
           ;; clear
           (LDY !$00)
           (STY CELL_TO_FREE_NEXT__+1)

          (LDA (ZP_RZ),y)
          (TAX)
          (LSR)
          (BCC CONTAINS_A_PTR__)
          (LSR)
          (BCS CONTAINS_NEITHER_CELLPTR_NOR_CELLPAIR_PTR__)
          (JMP FREE_CELLPAIR_RZ)

   (label CONTAINS_A_PTR__)
          ;; cell contains a pointer => save pointed to for tail call in temp
          ;; enqueue this rt in list to decrement refcount
          (LDA (ZP_RZ),y)
          (STA CELL_TO_FREE_NEXT__)
          (INY)
          (LDA (ZP_RZ),y)
          (STA CELL_TO_FREE_NEXT__+1)

   (label JUST_FREE_THIS_CELL__)
          ;; COPY previous head of free cells into this cell
          (LDA GLOBAL_CELL_FREE_LIST+1)
          (STA (ZP_RZ),y)
          (LDA GLOBAL_CELL_FREE_LIST)
          (DEY)
          (STA (ZP_RZ),y)                    ;; RZ -> [cell] -> (old) FREE_CELL_LIST

          ;; write this cell as new head into the list
          (LDA ZP_RZ)
          (STA GLOBAL_CELL_FREE_LIST)
          (LDA ZP_RZ+1)
          (STA GLOBAL_CELL_FREE_LIST+1)      ;; (new) FREE_CELL_LIST -> [cell] -> ...

          (LDA CELL_TO_FREE_NEXT__+1)
          (BNE PREP_TAILCALL__)
   (label DONE__)
          (RTS)                              ;; there wasn't any further pointer => done with free

   (label PREP_TAILCALL__)
          ;; fill rc for tail calling
          (STA ZP_RZ+1)
          (LDA CELL_TO_FREE_NEXT__)
          (STA ZP_RZ)                        ;; RZ -> [cellA][cellB]  || [cell] || [cell-arr-header][cell0][cell1]...[celln] || [cell-natarr-header][byte0][byte1] ...[byten]
          (JMP DEC_REFCNT_RZ)                ;; tail call since cell did hold a reference ;; the type of the cell was alread checked so optimization could directly call the right decr function

   (label CONTAINS_NEITHER_CELLPTR_NOR_CELLPAIR_PTR__)
          ;; could still be a pointer to cellarr or nativearr
          (LDA (ZP_RZ),y)
          (CMP !TAG_BYTE_CELL_ARRAY)
          (BNE MIGHT_BE_A_NAT_ARRAY__)       ;; RZ -> [cell-arr-header][cell0][cell1]...[celln]
          (JMP DEC_REFCNT_CELLARR_RZ)
   (label MIGHT_BE_A_NAT_ARRAY__)
          (CMP !TAG_BYTE_NATIVE_ARRAY)
          (BNE JUST_FREE_THIS_CELL__)        ;; contains neither cell-ptr nor cell-pair-ptr nor nat array nor cell-array => just free the cell and ignore its content
          (JMP DEC_REFCNT_NATIVEARR_RZ)      ;; RZ -> [cell-natarr-header][byte0][byte1] ...[byten]

   (label CELL_TO_FREE_NEXT__)
          (word 0) ;; holds a cell for tail call (if necessary = is a ptr), use highbyte != 0 to detect whether pointer is set
)))

(module+ test #| new_free_cell_rc |#
  (define new-free-cell-ptr-in-rc-tailcall-state
    (compact-run-code-in-test-                  ;; global_CELL_FREE_LIST = 0000
     #:runtime-code test-runtime
     (JSR ALLOC_CELL_TO_RT)                     ;; RT -> [cell 0 @ ..02]
     (JSR INC_REFCNT_CELL_RT)
     (JSR CP_RT_TO_RP)                          ;; RA -> [cell 0]
     (JSR ALLOC_CELL_TO_RT)                     ;; RT -> [cell 1 @ ..08]
     (JSR WRITE_RP_TO_CELL_POINTED_TO_BY_RT)        ;; RT -> [cell 1] -> [cell 0]

     (JSR FREE_CELL_RT)                     ;; GLOBAL_CELL_FREE_LIST -> [cell 0] -> [cell 1] -> 0000
     ;; #:debug #t
     ))                                         ;; PFL -> [cell 2 @ ..0a]

  (check-equal? (memory-list new-free-cell-ptr-in-rc-tailcall-state #xcecc (add1 #xcecc))
                (list #x02 PAGE_AVAIL_0)
                (format "~a02 is new head of the free list" (number->string PAGE_AVAIL_0 16)))
  (check-equal? (memory-list new-free-cell-ptr-in-rc-tailcall-state (+ PAGE_AVAIL_0_W #x02) (+ PAGE_AVAIL_0_W #x03))
                (list #x08 PAGE_AVAIL_0)
                (format "~a02, which was freed, is referencing ~a08 as next in the free list"
                        (number->string PAGE_AVAIL_0 16)
                        (number->string PAGE_AVAIL_0 16)))
  (check-equal? (memory-list new-free-cell-ptr-in-rc-tailcall-state (+ PAGE_AVAIL_0_W #x08) (+ PAGE_AVAIL_0_W #x09))
                (list #x00 #x00)
                (format "~a08, which was freed, is the tail of the free list"
                        (number->string PAGE_AVAIL_0 16)))
  (check-equal? (vm-page->strings new-free-cell-ptr-in-rc-tailcall-state PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "two slots still allocated on page, they are however on the free list to be reused")

  (define new-free-cell-ptr-in-rt-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELL_TO_RT)
     (JSR FREE_CELL_RT)))

  (check-equal? (memory-list new-free-cell-ptr-in-rt-state #xcecc (add1 #xcecc))
                (list #x02 PAGE_AVAIL_0)
                "allocated cell is freed by adding it as head to the list of free cells")

  (check-equal? (memory-list new-free-cell-ptr-in-rt-state (+ PAGE_AVAIL_0_W #x02) (+ PAGE_AVAIL_0_W #x03))
                (list #x00 #x00)
                "the cell is set to 00 00, marking the end of the list of free cells")

  (check-equal? (vm-page->strings new-free-cell-ptr-in-rt-state PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page has still 1 slot in use (it was freed, but is no in free list, not completely unallocated)")

  (define new-free-cell-ptr-in-rt-realloc-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELL_TO_RT)
     (JSR FREE_CELL_RT)
     (JSR ALLOC_CELL_TO_RT)))

  (check-equal? (memory-list new-free-cell-ptr-in-rt-realloc-state #xcecc #xcecc)
                (list #x00)
                "list of free cells is empty again")

  (check-equal? (memory-list new-free-cell-ptr-in-rt-realloc-state ZP_RT (add1 ZP_RT))
                (list #x02 PAGE_AVAIL_0))

  (check-equal? (vm-page->strings new-free-cell-ptr-in-rt-realloc-state PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page has 1 slot in use")

  (define new-free-cell-ptr-in-rt-2xfree-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELL_TO_RT)
     (JSR CP_RT_TO_RA)
     (JSR ALLOC_CELL_TO_RT)
     (JSR FREE_CELL_RT)        ;; free cc08
     (JSR CP_RA_TO_RT)
     (JSR FREE_CELL_RT)))      ;; then free cc02

  (check-equal? (memory-list new-free-cell-ptr-in-rt-2xfree-state #xcecc (add1 #xcecc))
                (list #x02 PAGE_AVAIL_0)
                "last allocated cell is freed by adding it as head to the list of free cells")

  (check-equal? (memory-list new-free-cell-ptr-in-rt-2xfree-state (+ PAGE_AVAIL_0_W #x02) (+ PAGE_AVAIL_0_W #x03))
                (list #x08 PAGE_AVAIL_0)
                "the cell is set to $cc08, the next element in the free list")

  (check-equal? (memory-list new-free-cell-ptr-in-rt-2xfree-state (+ PAGE_AVAIL_0_W #x08) (+ PAGE_AVAIL_0_W #x08))
                (list #x00)
                "the cell is set to 00, marking the end of the list of free cells")

  (check-equal? (vm-page->strings new-free-cell-ptr-in-rt-2xfree-state PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "page has still 2 slot in use (it was freed, but is no in free list, not completely unallocated)"))

;; @DC-FUN: ALLOC_CELL_TO_RT, group: cell
;; allocate (or reuse from free-list) cell into rt
;;
;; input:  GLOBAL_CELL_FREE_LIST
;;         GLOBAL_CELL_PAGE_FOR_ALLOC
;;         VM_PAGE_SLOT_DATA
;;         # cells allocated on PAGE
;; output: ZP_RT: ptr to heap allocated cell (cell itself is not initialized!)
;;         GLOBAL_CELL_FREE_LIST
;;         A, X, Y: ?
(define ALLOC_CELL_TO_RT
  (add-label-suffix
   "__" "__ALLOC_CELL_TO_RT"
  (list
   (label ALLOC_CELL_TO_RT)
          (LDA GLOBAL_CELL_FREE_LIST+1)
          (BNE ALLOC_CELL_GFL_PAGE_A_TO_RT)
          (JSR GET_FRESH_CELL_TO_AX)
          (JMP ALLOC_CELL_AX_TO_RT)

   (label ALLOC_CELL_GFL_TO_RT)
          (LDA GLOBAL_CELL_FREE_LIST+1)
          ;; (BEQ ERROR_ALLOC_CELL_GFL_TO_RT)
   (label ALLOC_CELL_GFL_PAGE_A_TO_RT)
          (STA ZP_RT+1)
          (LDA GLOBAL_CELL_FREE_LIST)
          (STA ZP_RT)

          ;; read output this old cell and store its content as new head of the free list
          (LDY !$00)
          (LDA (ZP_RT),y)
          (STA GLOBAL_CELL_FREE_LIST)
          (INY)
          (LDA (ZP_RT),y)
          (STA GLOBAL_CELL_FREE_LIST+1)
          (RTS))))

(module+ test #| vm_alloc_cell_ptr_to_rt (once on a new page) |#
  (define test-alloc-cell-to-rt-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELL_TO_RT)))

  (check-equal? (memory-list test-alloc-cell-to-rt-state-after #xcecc #xcecc)
                (list #x00)
                "list of free cells is empty")

  (check-equal? (memory-list test-alloc-cell-to-rt-state-after ZP_RT (add1 ZP_RT))
                (list #x02 PAGE_AVAIL_0))

  (check-equal? (vm-page->strings test-alloc-cell-to-rt-state-after PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page has 1 slot in use"))

(module+ test #| vm_alloc_cell_ptr_to_rt (twice on a new page) |#
  (define test-alloc-cell-to-rt-twice-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELL_TO_RT)
     (JSR ALLOC_CELL_TO_RT)))

  (check-equal? (memory-list test-alloc-cell-to-rt-twice-state-after #xcecc #xcecc)
                (list #x00)
                "list of free cells is empty")

  (check-equal? (memory-list test-alloc-cell-to-rt-twice-state-after ZP_RT (add1 ZP_RT))
                (list #x08 PAGE_AVAIL_0))

  (check-equal? (vm-page->strings test-alloc-cell-to-rt-twice-state-after PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "page has 2 slots in use"))

(module+ test #| vm_alloc_cell_to_zp_ptr (twice, then free first on a new page) |#
  (define test-alloc-cell-to-rt-twicenfree-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELL_TO_RT)
     (JSR CP_RT_TO_RA)

     (JSR ALLOC_CELL_TO_RT)
     (JSR FREE_CELL_RA)))

  (check-equal? (memory-list test-alloc-cell-to-rt-twicenfree-state-after #xcecc (add1 #xcecc))
                (list #x02 PAGE_AVAIL_0)
                "free cell list has xx02 now as head of the list")

  (check-equal? (vm-page->strings test-alloc-cell-to-rt-twicenfree-state-after PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "page has still 2 slots in use (even though $cc02 was freed)")

  (check-equal? (memory-list test-alloc-cell-to-rt-twicenfree-state-after (+ PAGE_AVAIL_0_W #x02) (+ PAGE_AVAIL_0_W #x03))
                (list #x00 #x00)
                "since xx02 is now part of the free cell list, it points to the next free cell which is $0000 (none)"))

(module+ test #| vm_alloc_cell_to_zp_ptr (twice, then free first on a new page, then allocate again) |#
  (define test-alloc-cell-to-rt-twicenfreenalloc-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR ALLOC_CELL_TO_RT)
     (JSR CP_RT_TO_RA)

     (JSR ALLOC_CELL_TO_RT)
     (JSR FREE_CELL_RA)

     (JSR ALLOC_CELL_TO_RT)))

  (check-equal? (vm-page->strings test-alloc-cell-to-rt-twicenfreenalloc-state-after PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "still (only) two slots are used on the page, one from the free list was reused")

  (check-equal? (memory-list test-alloc-cell-to-rt-twicenfreenalloc-state-after #xcecc #xcecc)
                (list #x00) ;; lowbyte is zero => it is initial (high byte is not heeded in that case)
                "free cell list is initial again"))

;; @DC-FUN: GET_FRESH_CELL_TO_AX, group: cell
;; get the page and unused cell for allocation
;;
;; get the complete ptr, do not allocate this cell yet
;; allocate a new page if necessary
;; do not use any cell free list
;; input:  GLOBAL_CELL_PAGE_FOR_ALLOC
;;         VM_PAGE_SLOT_DATA+PAGE
;; output: A = lowbyte
;;         X = highbyte (page)
;;         Y = ?
(define GET_FRESH_CELL_TO_AX
  (add-label-suffix
   "__" "__GET_FRESH_CELL_TO_AX"
  (list
   (label GET_FRESH_CELL_TO_AX)
          (LDX GLOBAL_CELL_PAGE_FOR_ALLOC)
          (BEQ PAGE__)
          (LDA VM_PAGE_SLOT_DATA,x)
          (BNE DONE__) ;; allocate new page first

   (label PAGE__)
          (JSR ALLOC_PAGE_TO_X)
          (JMP INIT_CELL_PAGE_X_TO_AX)

   (label DONE__)
          (RTS))))

(module+ test #| get-page-for-alloc-cell-to-ax |#
  (define get-page-for-alloc-cell-to-ax-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
      (JSR ALLOC_PAGE_TO_X)
      (STX GLOBAL_CELL_PAGE_FOR_ALLOC)
      (LDA !$08)                           ;; make first free slot on page to be 08
      (STA VM_PAGE_SLOT_DATA,x)

      (JSR GET_FRESH_CELL_TO_AX)  ;; no new allocate should take place => stay on page_0

      (STA ZP_RT)
      (STX ZP_RT+1)))

  (check-equal? (memory-list get-page-for-alloc-cell-to-ax-state ZP_RT (add1 ZP_RT))
                (list #x08 PAGE_AVAIL_0)
                "cell 08 is allocated on page_0"))

(module+ test #| get-page-for-alloc-cell-to-ax |#
  (define get-page-for-alloc-cell-to-ax-2-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
      (JSR ALLOC_PAGE_TO_X)
      (STX GLOBAL_CELL_PAGE_FOR_ALLOC)
      (LDA !$00)                          ;; mark page to have no free cells
      (STA VM_PAGE_SLOT_DATA,x)

      (JSR GET_FRESH_CELL_TO_AX) ;; should allocate a new page

      (STA ZP_RT)
      (STX ZP_RT+1)))

  (check-equal? (memory-list get-page-for-alloc-cell-to-ax-2-state ZP_RT (add1 ZP_RT))
                (list #x02 PAGE_AVAIL_1)
                "since first page (page_0) is marked as full, first slot (02) of page_1 is allocated"))

;; @DC-FUN: ALLOC_CELL_AX_TO_RT, group: cell
;; allocate the cell at A on page X
;;
;; update next free cell in vm_page_slot_data
;; update number of allocated cells on page X
;; input:  A = lowbyte
;;         X = highbyte (page)
;;         # cells allocated on PAGE
;; output: A = next free cell
;;         X = PAGE
;;         Y = 0
;;         VM_PAGE_SLOT_DATA + PAGE = next free cell
;;         # cells allocated on PAGE ++
(define ALLOC_CELL_AX_TO_RT
  (add-label-suffix
   "__" "__ALLOC_CELL_AX_TO_RT"
  (list
   (label ALLOC_CELL_PFL_X_TO_RT)
          (LDA VM_PAGE_SLOT_DATA,x)               ;; get first free slot on page
          ;; (BEQ ERROR)
   ;;     ----------------------------
   (label ALLOC_CELL_AX_TO_RT)
          (STX ZP_RT+1)                           ;; safe as highbyte of ptr
          (STA ZP_RT)                             ;; safe as lowbyte of ptr

          (LDY !$00)
          (LDA (ZP_RT),y)                         ;; next free cell
          (STA VM_PAGE_SLOT_DATA,x)

          ;; increase the slot number on this page
          (STX INC_CMD__+2) ;; overwrite $c0
   (label INC_CMD__)
          (INC $c000)
          (RTS))))

(module+ test #| alloc-cell-a-on-page-x-to-rt |#
  (define test-alloc-cell-a-on-page-x-to-rt-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
      (JSR ALLOC_PAGE_TO_X)
      (JSR INIT_CELL_PAGE_X_TO_AX)
      (JSR ALLOC_CELL_AX_TO_RT)))

  (check-equal? (memory-list test-alloc-cell-a-on-page-x-to-rt-state ZP_RT (add1 ZP_RT))
                (list #x02 PAGE_AVAIL_0))

  (check-equal? (vm-page->strings test-alloc-cell-a-on-page-x-to-rt-state PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page has 1 slot in use"))

(module+ test
  (define test-alloc-cell-a-on-page-x-to-rt-twice-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
      (JSR ALLOC_PAGE_TO_X)
      (JSR INIT_CELL_PAGE_X_TO_AX)
      (JSR ALLOC_CELL_AX_TO_RT)
      (LDA !$08)
      (LDX ZP_RT+1)
      (JSR ALLOC_CELL_AX_TO_RT)))

  (check-equal? (memory-list test-alloc-cell-a-on-page-x-to-rt-twice-state ZP_RT (add1 ZP_RT))
                (list #x08 PAGE_AVAIL_0))

  (check-equal? (vm-page->strings test-alloc-cell-a-on-page-x-to-rt-twice-state PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "page has 2 slot in use"))

;; @DC-FUN: GC_CELLS, group: gc
;; garbage collect all cells marked as reusable in GLOBAL_CELL_FFREE_LIST
;; input:  GLOBAL_CELL_FFREE_LIST
;; usage:  A, X, Y, RZ
;; output: GLOBAL_CELL_FFREE_LIST+1 = 0   (no more cells left for reuse)
;; funcs:  -
(define GC_CELLS
  (add-label-suffix
   "__" "NEW_GC_CELLS"
   (list
    (label GC_CELLS)
           (LDX GLOBAL_CELL_FREE_LIST+1)
           (BNE CONTINUE__)
           (RTS)
    (label CONTINUE__)
           (STX ZP_RZ+1)
           (LDA GLOBAL_CELL_FREE_LIST)
           (STA ZP_RZ)

           ;; remove this cell from the list of free cells
           ;; put next of rc into vm_list_of_free
           (LDY !$00)
           (LDA (ZP_RZ),y)
           (STA GLOBAL_CELL_FREE_LIST)
           (INY)
           (LDA (ZP_RZ),y)
           (STA GLOBAL_CELL_FREE_LIST+1)


           ;; return RZ to its page free list
           ;; store in cell, pointed to by rc the previous head, now next
           (LDA VM_PAGE_SLOT_DATA,x) ;; old head of page free list
           (LDY !$00)                ;; optimization: remove this command if previous sequence makes sure y = 0
           (STA (ZP_RZ),y)
           ;; (INY)                  ;; only the lowbyte needs to be set, since hb is known
           ;; (TXA)
           ;; (STA (ZP_RZ),y)

           ;; mark RZ as new first free slot on page (head)
           (LDA ZP_RZ)
           (STA VM_PAGE_SLOT_DATA,x)


           ;; decrement number of used slots on this cell-page
           (STX DEC_CMD__+2)
           (STX DEC_CMD__+5)
    (label DEC_CMD__)
           (DEC $c000) ;; c0 is overwritten by page, cell-page holds # of slots in first byte
           (LDA $c000) ;; c0 is overwritten
           (AND !$3f)
           (BNE GC_CELLS) ;; loop

           ;; no more cells allocated on that page
           ;; return page to (completely) free pages? <- not implemented yet
           (BEQ GC_CELLS) ;; loop
           )))

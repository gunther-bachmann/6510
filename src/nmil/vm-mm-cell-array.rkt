#lang racket/base

#|

memory management for cell arrays

|#

(require "../6510.rkt")
(require (only-in "./vm-memory-map.rkt"
                  TAGGED_NIL
                  TAG_BYTE_CELL_ARRAY
                  ZP_RP
                  ZP_RA
                  ZP_TEMP
                  ZP_RT
                  VM_MEMORY_MANAGEMENT_CONSTANTS))
(require (only-in "../ast/6510-resolver.rkt"
                  add-label-suffix
                  replace-labels))
(require (only-in "./vm-mm-cell-stack.rkt"
                  POP_CELL_EVLSTK_TO_RT))
(require (only-in "./vm-mm-m1-slots.rkt"
                  ALLOC_M1_SLOT_TO_RA
                  INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX
                  VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS
                  ADD_M1_SLOT_RZ_TO_PFL
                  DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A
                  PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ))

(provide
          ALLOC_CELLARR_TO_RA          ;; allocate an array of cells (also useful for structures)

          GC_INCR_ARRAY_SLOT_RZ        ;; incremental gc of a cell-array
          GC_CELL_ARRAYS               ;; completely collect all cell arrays
          FREE_CELLARR_RZ              ;; free the given cell-array in rz (must not contain slots that need garbage collection)

          PUSH_ARR_ATa_RA_TO_EVLSTK    ;; push from cell-array RA element A onto the EVLSTK
          WRITE_RT_TO_ARR_ATa_RA)      ;; write cell in RT into cell-array RA at A

(module+ test
  (require (only-in racket/list make-list))
  (require  "../6510-test-utils.rkt")
  (require "./vm-memory-manager-test-utils.rkt")
  (require (only-in "../tools/6510-interpreter.rkt"
                    memory-list
                    cpu-state-clock-cycles))
  (require (only-in "../util.rkt" format-hex-byte))
  (require (only-in "./vm-inspector-utils.rkt"
                    vm-regt->string
                    vm-cell-pair-free-tree->string
                    vm-deref-cell-pair-w->string
                    vm-stack->strings
                    vm-page->strings))
  (require (only-in "./vm-mm-register-functions.rkt"
                    CP_RT_TO_RZ
                    CP_RT_TO_RP
                    CP_RZ_TO_RT
                    CP_RA_TO_RZ
                    CP_RA_TO_RT
                    WRITE_INT_AY_TO_RT
                    WRITE_NIL_TO_RP))
  (require (only-in "./vm-mm-pages.rkt"
                    ALLOC_PAGE_TO_X
                    VM_PAGE_SLOT_DATA
                    VM_INITIAL_MM_REGS
                    VM_INITIALIZE_MEMORY_MANAGER))
  (require (only-in "./vm-mm-cells.rkt"
                    ALLOC_CELL_AX_TO_RT
                    INIT_CELL_PAGE_X_TO_AX
                    DEC_REFCNT_CELL_RZ
                    FREE_CELL_RZ))
  (require (only-in "./vm-mm-cell-stack.rkt"
                    PUSH_XA_TO_EVLSTK
                    PUSH_RT_TO_EVLSTK))
  (require (only-in "./vm-mm-cell-pairs.rkt"
                    WRITE_RP_TO_CELLy_CELLPAIR_RT
                    WRITE_CELLPAIR_RT_CELLy_TO_RT
                    FREE_CELLPAIR_RZ
                    ALLOC_CELLPAIR_TO_RT
                    GET_FRESH_CELLPAIR_TO_AX
                    INIT_CELLPAIR_PAGE_X_TO_AX
                    ALLOC_CELLPAIR_AX_TO_RT
                    DEC_REFCNT_CELLPAIR_RZ
                    INC_REFCNT_CELLPAIR_RT
                    ))

  (define PAGE_AVAIL_0 #x9a)      ;; high byte of first page available for allocation
  (define PAGE_AVAIL_0_W #x9a00)  ;; word (absolute address) of first page available
  (define PAGE_AVAIL_1 #x99)      ;; high byte of second page available for allocation
  (define PAGE_AVAIL_1_W #x9900) ;; word (absolute address) of second page available

  (define test-runtime
    (append
     ALLOC_PAGE_TO_X
     ALLOC_CELLARR_TO_RA
     GC_INCR_ARRAY_SLOT_RZ
     GC_CELL_ARRAY
     GC_CELL_ARRAYS
     FREE_CELLARR_RZ
     WRITE_RT_TO_ARR_ATa_RA
     PUSH_ARR_ATa_RA_TO_EVLSTK

     (list (label DEC_REFCNT_CELLARR_RZ) (RTS)) ;; TODO move here (from vm-memory-manager)
     (list (label INC_REFCNT_CELLARR_RT) (RTS)) ;; TODO move here (from vm-memory-manager)

     (list (label DEC_REFCNT_RT)
           (JSR CP_RT_TO_RZ)
           (JMP DEC_REFCNT_RZ))    ;; ?needed where?
     (list (label DEC_REFCNT_NATIVEARR_RZ) (RTS)) ;; ?needed where?

     POP_CELL_EVLSTK_TO_RT
     GET_FRESH_CELLPAIR_TO_AX
     WRITE_CELLPAIR_RT_CELLy_TO_RT
     INIT_CELLPAIR_PAGE_X_TO_AX
     ALLOC_CELLPAIR_AX_TO_RT
     ALLOC_CELLPAIR_TO_RT
     FREE_CELLPAIR_RZ
     FREE_CELL_RZ
     (DEC_REFCNT_CELL_RZ "CELL_ALREADY_LSRED__" "DEC_REFCNT_CELL_RZ_TO_M1_SLOT__")
     (list (label DEC_REFCNT_CELL_RZ_TO_M1_SLOT__) (RTS))
     WRITE_RP_TO_CELLy_CELLPAIR_RT
     (DEC_REFCNT_CELLPAIR_RZ "CELLPAIR_ALREADY_LSRED__")
     (INC_REFCNT_CELLPAIR_RT "LSR__INC_RFCNT_CELLPAIR__")
     (list (label DONE__) (RTS))
     (list (label INC_REFCNT_M1_PAGE) (RTS)) ;; TODO currently in vm-memory-manager (move to m1-slot)
     ALLOC_CELL_AX_TO_RT
     INIT_CELL_PAGE_X_TO_AX
     WRITE_NIL_TO_RP
     WRITE_INT_AY_TO_RT
     DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A
     PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ
     ADD_M1_SLOT_RZ_TO_PFL
     ALLOC_M1_SLOT_TO_RA
     INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX
     VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS
     CP_RA_TO_RT
     CP_RA_TO_RZ
     CP_RT_TO_RP
     CP_RT_TO_RZ
     CP_RZ_TO_RT
     PUSH_XA_TO_EVLSTK
     PUSH_RT_TO_EVLSTK
     VM_INITIALIZE_MEMORY_MANAGER
     VM_MEMORY_MANAGEMENT_CONSTANTS
     (list (label INIT_CELLSTACK_PAGE_X) (RTS))
     (list (label DEC_REFCNT_RZ)
           (LDA ZP_RZ)
           (LSR)
           (BCS DEC_REFCNT_RZ__NO_CELL)
           (JMP DEC_REFCNT_CELL_RZ)
           (label DEC_REFCNT_RZ__NO_CELL)
           (JMP DEC_REFCNT_CELLPAIR_RZ)) ;; assume that dec refcnt is operated on cellpair
     VM_INITIAL_MM_REGS
     VM_PAGE_SLOT_DATA)))

;; allocate an array of cells (also useful for structures)
;; this does overwrite RA without check RAs content!
;; input:  A = number of cells (1..40)
;; usage:  A, X, Y, RA
;; output: RA -> points to an allocated array
;; funcs:
;;   ALLOC_M1_SLOT_TO_RA
(define ALLOC_CELLARR_TO_RA
  (add-label-suffix
   "__" "__ALLOC_CELLARR_TO_RA"
  (list
   (label ALLOC_CELLARR_TO_RA)
          ;; optional: optimization for arrays with 3 cells => s8 page!
          (PHA)
          (ASL A)       ;; *2
          (CLC)
          (ADC !$02)    ;; add (tag byte, length) to total slot size

          (JSR ALLOC_M1_SLOT_TO_RA)

          (PLA)
          (TAX) ;; save array len in x
          (ASL A)
          (TAY) ;; use number of array elements * 2 as loop counter

          ;; initialize slots/array with zeros (actually writes one byte more than needed)
          (LDA 0)
          (INY)
   (label LOOP_INIT__)
          (STA (ZP_RA),y)
          (DEY)
          (BNE LOOP_INIT__)

          ;; y = 0 now
          ;; write header cell
          (LDA !TAG_BYTE_CELL_ARRAY)
          (STA (ZP_RA),y) ;; store tag byte
          (INY)
          (TXA)
          (STA (ZP_RA),y) ;; store number of array elements

          (RTS))))

(module+ test #| vm_allocate_cell_array |#
  (define test-alloc-cell-array-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDA !$04)
     (JSR ALLOC_CELLARR_TO_RA)))

  (check-equal? (vm-page->strings test-alloc-cell-array-state-after PAGE_AVAIL_0)
                (list
                 "page-type:      m1 page p1"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $16"))
  (check-equal? (memory-list test-alloc-cell-array-state-after ZP_RA (add1 ZP_RA))
                (list #x04 PAGE_AVAIL_0))
  (check-equal? (memory-list test-alloc-cell-array-state-after (+ PAGE_AVAIL_0_W #x04)(+ PAGE_AVAIL_0_W #x0d))
                (list TAG_BYTE_CELL_ARRAY #x04
                      #x00 #x00
                      #x00 #x00
                      #x00 #x00
                      #x00 #x00)
                "array is filled with zeros")
)

;; impl missing, test missing
(define FREE_CELLARR_RZ
  (add-label-suffix
   "__" "__NEW_FREE_CELLARR_RZ"
   (list
    (label FREE_CELLARR_RZ)
           (RTS))))

;; do incremental collections until all cell arrays (and their slots) were garbage collected
;; input:  ZP_PART_GCD_CELL_ARRAYS
;; usage:  A, X, Y, RZ
;; output: ZP_PART_GCD_CELL_ARRAYS+1 = 0  (no more arrays left that are partially gc'd)
;; funcs:
;;   GC_CELL_ARRAY
;;   GC_INCR_ARRAY_SLOT_RZ
(define GC_CELL_ARRAYS
  (add-label-suffix
   "__" "__NEW_GC_CELL_ARRAYS"
   (list
   (label GC_CONT__)
          (STA ZP_RZ+1)
          (LDA ZP_PART_GCD_CELL_ARRAYS)
          (STA ZP_RZ)
          (JSR GC_INCR_ARRAY_SLOT_RZ)

   (label GC_CELL_ARRAYS)           ;; -------------------- function entry
          (LDA ZP_PART_GCD_CELL_ARRAYS+1)
          (BNE GC_CONT__) ;; only if high byte (page) != 0, there seems to be a cell array to be worked on

   (label DONE__)
          (RTS))))

;; keep collecting until the whole (single) array was collected but stop then!
;; input:  ZP_PART_GCD_CELL_ARRAYS
;; usage:  A, X, Y, RZ
;; output: <<ZP_PART_GCD_CELL_ARRAYS<<
;; funcs:
;;   GC_INCR_ARRAY_SLOT_RZ
(define GC_CELL_ARRAY
  (add-label-suffix
   "__" "__NEW_GC_ARRAY"
   (list
    (label GC_CELL_ARRAY)
           (LDA ZP_PART_GCD_CELL_ARRAYS+1)
           (BEQ DONE__)

    (label GC_CONT__)
           (STA PREV_ARRAY__+1)
           (LDX ZP_PART_GCD_CELL_ARRAYS)
           (STX PREV_ARRAY__)

    (label LOOP__)
           (STA ZP_RZ+1)
           (STX ZP_RZ)
           (JSR GC_INCR_ARRAY_SLOT_RZ)
           (LDA ZP_PART_GCD_CELL_ARRAYS+1)
           (BEQ DONE__)
           (CMP PREV_ARRAY__+1)
           (BNE DONE__)
           (LDX ZP_PART_GCD_CELL_ARRAYS)
           (CPX PREV_ARRAY__)
           (BEQ LOOP__)

    (label DONE__)
           (RTS)

    (label PREV_ARRAY__)
           (word 0))))

;; incrementally garbage collect an array by slots
;; may destroy RZ (on dec refcnt of a cell in the array)
;; will free this cell-array, if no refcnts need to be dec (anymore)
;; will add this cell array to ZP_PART_GCD_CELL_ARRAYS if not completely gc'd
;; input: RZ (RA, RT)
;; usage: A, X, Y, RZ
;; output: -
;; funcs:
;;   DEC_REFCNT_RZ
;;   ADD_M1_SLOT_RZ_TO_PLF
(define GC_INCR_ARRAY_SLOT_RZ
  (add-label-suffix
   "__" "__GC_INCR_ARRAY_SLOT_RZ"
  (list

   (label GC_INCR_ARRAY_SLOT_RA)
          (JSR CP_RA_TO_RZ)
          (JMP GC_INCR_ARRAY_SLOT_RZ)

   (label GC_INCR_ARRAY_SLOT_RT)
          (JSR CP_RT_TO_RZ)
          (JMP GC_INCR_ARRAY_SLOT_RZ)

   (label GC_INCR_CELLARR_GFL)
          (LDA ZP_PART_GCD_CELL_ARRAYS+1)
          (BNE GC_CONT__) ;; only if high byte (page) != 0, there seems to be a cell array to be worked on

   (label RETURN__)
          (RTS)

   (label GC_CONT__)
          (STA ZP_RZ+1)
          (LDA ZP_PART_GCD_CELL_ARRAYS)
          (STA ZP_RZ)

   (label GC_INCR_ARRAY_SLOT_RZ) ;; cellarr layout: 00 = type, 01 = #of cells, 02/03 = cell0, 04/05 = cell1 ...
          ;; loop over slots and decrement their slots
          (LDY !$01)
          (LDA (ZP_RZ),y)  ;; a = number of array elements
          (STA PREV_LAST_ENTRY__)
          (BEQ RETURN__) ;; number = 0 => nothing to do (should never happen, since it should have been collected then)
          (ASL A) ;; e.g. 1 => 2 (low byte of last position)
          (TAY) ;;

   (label LOOP__)
          (LDA (ZP_RZ),y) ;; load tagged low byte
          (BEQ NEXT__)

          (TAX)
          (AND !$03)
          (CMP !$03)
          (BEQ NEXT__) ;; cannot be other pointer than cell-ptr or cell-pair-ptr => this not a pointer => move on to next

   (label IS_PTR__)
          (INY)             ;;  RZ -> [cell-array-header] [len n+1] [cell0]... [celln]
          (LDA (ZP_RZ),y)   ;;  A = hb celln, X = lb celln
          (PHA)

          ;; check if working on the head of partially garbage collected cell arrays (and skip if so)
          (LDA ZP_PART_GCD_CELL_ARRAYS+1)
          (CMP ZP_RZ+1)
          (BNE PUT_OLD_HEAD__) ;; != => not the head => enqueue this cell array as head

          (LDA ZP_PART_GCD_CELL_ARRAYS)
          (CMP ZP_RZ)
          (BEQ CONT_WITH_DEC_PREP__) ;; == => is head => do not enqueue, but continue

          ;; put old head of ZP_PART_GCD_CELL_ARRAYS into last cell (which will be dec_refcnt ed)
          (LDA ZP_PART_GCD_CELL_ARRAYS+1)
   (label PUT_OLD_HEAD__)
          (STA (ZP_RZ),y)   ;;  RZ -> [cell-array-header] [len n+1] [cell0]... [lbcelln hb RZ]
          (DEY)             ;;
          (LDA ZP_PART_GCD_CELL_ARRAYS)
          (STA (ZP_RZ),y)   ;; RZ -> [cell-array-header] [len n+1] [cell0]... [old head ZP_PART_GCD_CELL_ARRAYS]

          ;; put this array as new head of ZP_PART_GCD_CELL_ARRAYS (only if it is not already part of the list, right?)
          (LDA ZP_RZ+1)
          (STA ZP_PART_GCD_CELL_ARRAYS+1)
          (LDA ZP_RZ)
          (STA ZP_PART_GCD_CELL_ARRAYS)

   (label CONT_WITH_DEC_PREP__)

          ;; calc new # of cells left in this cell array
          (TYA)
          (LDY !$01)
          (LSR)
          (SEC)
          (SBC !$01)
          (STA (ZP_RZ),y) ;; set new number of (relevant) cells in the array
          (BNE CONT_WITHOUT_FREE_ARRAY__)

          (STX TEMP__)
          (JSR DONE__) ;; dequeue from global free cell-array list, put into page local free list
          (LDX TEMP__)
   (label CONT_WITHOUT_FREE_ARRAY__)
          (PLA) ;; get hb
          ;; (re)store cell to be dec_refcnt ed into rc
          (STA ZP_RZ+1)
          (STX ZP_RZ)

          (JMP DEC_REFCNT_RZ) ;; do tailcall (decrement refcount of this cell-ptr)

   (label NEXT__)
          (DEY)
          (DEY)
          (BNE LOOP__)

          ;; now completely done, all cells were inspected

          (TYA) ;; a := 0
          (INY) ;; y := 1
          (STA (ZP_RZ),y) ;; set new number of (relevant) cells in the array to 0 (completely gc'd)

   (label DONE__)
          ;; if this cell array was enqueued as head, make sure to dequeue!!
          ;; compare with head, if equal => dequeue (need last entry before running)
          (LDA ZP_RZ)
          (CMP ZP_PART_GCD_CELL_ARRAYS)
          (BNE NO_MORE_DEQUEUE__)
          (LDA ZP_RZ+1)
          (CMP ZP_PART_GCD_CELL_ARRAYS+1)
          (BNE NO_MORE_DEQUEUE__)

          (LDA PREV_LAST_ENTRY__)
          (ASL A) ;; e.g. 1 => 2 (low byte of last position)
          (TAY) ;;
          (INY)
          (INY) ;; now low byte right behind last position [actually data for next head]
          (LDA (ZP_RZ),y)
          (STA ZP_PART_GCD_CELL_ARRAYS)
          (INY)
          (LDA (ZP_RZ),y)
          (STA ZP_PART_GCD_CELL_ARRAYS+1)

   (label NO_MORE_DEQUEUE__)
          (JMP ADD_M1_SLOT_RZ_TO_PFL)

   (label PREV_LAST_ENTRY__)
          (byte 0)
   (label TEMP__)
          (byte 0))))


(module+ test #| use case: allocate, free, reallocate small list of cell-pairs |#
  (define use-case-2-a-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)            ;; rt = freshly allocated cell (cc05)
     (JSR INC_REFCNT_CELLPAIR_RT)          ;; ref(rt) ++ (=1)
     ;; set cdr to nil
     (JSR WRITE_NIL_TO_RP)
     (JSR WRITE_RP_TO_CELL1_CELLPAIR_RT)   ;; (cdr rt) := nil
     ;; set car to int 0
     (JSR CP_RT_TO_RZ)
     (JSR WRITE_INT1_TO_RT)
     (JSR CP_RT_TO_RP)
     (JSR CP_RZ_TO_RT)
     (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)   ;; (car rt) := int0

     (JSR CP_RT_TO_RP)                     ;; ra := rt

     (JSR ALLOC_CELLPAIR_TO_RT)            ;; rt = freshly allocated cell (cc09)
     (JSR INC_REFCNT_CELLPAIR_RT)          ;; ref(rt) ++ (=1)

     ;; set cdr
     (JSR WRITE_RP_TO_CELL1_CELLPAIR_RT)   ;; (cdr rt) := ra
     (JSR CP_RT_TO_RZ)
     ;; set car to int0
     (JSR WRITE_INT0_TO_RT)
     (JSR CP_RT_TO_RP)
     (JSR CP_RZ_TO_RT)
     (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)   ;; (car rt) := int0

     ;; now:
     ;;   rt[cc09|1] (int0 . ->[cc05|1](int0 . nil))
     ;; notation:
     ;;   [<mem-location>|<ref-count>]
     ;;   (<car-cell> . <cdr-cell>)
     ;;   intX, nil :: atomic value cells
     ;;   -> :: cell-ptr
     ))

  (define use-case-2-a-state-after
    (apply compact-run-code-in-test-
           use-case-2-a-code
     #:runtime-code test-runtime))

  (check-equal? (vm-deref-cell-pair-w->string use-case-2-a-state-after (+ PAGE_AVAIL_0_W #x09))
                (format "(int $0000 . pair-ptr[1] $~a05)" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-deref-cell-pair-w->string use-case-2-a-state-after (+ PAGE_AVAIL_0_W #x05))
                "(int $0001 . pair-ptr NIL)")
  (check-equal? (vm-regt->string use-case-2-a-state-after)
                (format "pair-ptr[1] $~a09" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-page->strings use-case-2-a-state-after PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41"))

  (define use-case-2-b-code
    (append use-case-2-a-code ;; zp_ptr[cc08|1] (int0 . ->[cc04|1](int0 . nil))
            (list
             (JSR DEC_REFCNT_CELLPAIR_RT)
             ;; now:
             ;;   free_tree -> [cc08|0] (int0 . ->[cc04|1] (int0 . nil))
             )))

  (define use-case-2-b-state-after
    (apply compact-run-code-in-test-
           use-case-2-b-code
     #:runtime-code test-runtime
           ))

  (check-equal? (vm-cell-pair-free-tree->string use-case-2-b-state-after)
                (format "pair $~a09 -> [ empty . pair-ptr[-] $~a05 ]"
                        (format-hex-byte PAGE_AVAIL_0)
                        (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-page->strings use-case-2-b-state-after PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41"))

  (define use-case-2-c-code
    (append use-case-2-b-code ;; free_tree -> [cd08|0] (int0 . ->[cd04|1] (int0 . nil))
            (list (LDA !$FF) ;; marker for debug, remove when done
                  (JSR ALLOC_CELLPAIR_TO_RT)
                  (JSR INC_REFCNT_CELLPAIR_RT)
                  ;; now:
                  ;;   zp_rt = [cd08|1] not initialized
                  ;;   free_tree -> [cd04|0] (int0 . nil)
                  )))

  (define use-case-2-c-state-after
    (apply compact-run-code-in-test-
           use-case-2-c-code
     #:runtime-code test-runtime
           ))

  (check-equal? (vm-regt->string use-case-2-c-state-after)
                (format "pair-ptr[1] $~a09"
                        (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-cell-pair-free-tree->string use-case-2-c-state-after)
                (format "pair $~a05 -> [ pair-ptr NIL . pair-ptr NIL ]" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-page->strings use-case-2-c-state-after PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41")))


(module+ test #| vm_gc_array_slot_ptr |#
  (define test-gc-array-slot-ptr-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDA !$04)
     (JSR ALLOC_CELLARR_TO_RA)         ;; ZP_RA = pointer to the allocated array (with 4 cells)

     (JSR ALLOC_CELLPAIR_TO_RT)        ;; ZP_RT = allocated cell-pair
     (JSR INC_REFCNT_CELLPAIR_RT)

     ;; wrote a new cell-pair @2
     (LDA !$02)
     (JSR WRITE_RT_TO_ARR_ATa_RA)      ;; tos (cell-pair) -> @2

     (JSR PUSH_INT_m1_TO_EVLSTK)       ;; int -1 -> stack
     (LDA !$01)
     (JSR WRITE_RT_TO_ARR_ATa_RA)      ;; tos (int -1) -> @1

     (JSR CP_RA_TO_RT)                 ;; overwrite tos (-1) with ptr to array
     (JSR GC_INCR_ARRAY_SLOT_RT)
     (JSR GC_CELL_ARRAYS)          ;; run gc on slot elements -> cell-pair should be gc'd
     ))

  (check-equal? (vm-stack->strings test-gc-array-slot-ptr-state-after)
                (list "stack holds 2 items"
                      (format "ptr[0] $~a04  (rt)" (format-hex-byte PAGE_AVAIL_0))
                      (format "pair-ptr[0] $~a05" (format-hex-byte PAGE_AVAIL_1))))
  (check-equal? (vm-page->strings test-gc-array-slot-ptr-state-after PAGE_AVAIL_1)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09"))
  (check-equal? (memory-list test-gc-array-slot-ptr-state-after (+ PAGE_AVAIL_1_W #x01) (+ PAGE_AVAIL_1_W #x01))
                (list #x00)
                "refcount for cell-pair at cb04..cb07 is at cb01 = 0 (was freed)")
  (check-equal? (vm-cell-pair-free-tree->string test-gc-array-slot-ptr-state-after)
                (format "pair $~a05 -> [ empty . empty ]" (format-hex-byte PAGE_AVAIL_1))
                "...and added as free tree root (for reuse)"))


(define WRITE_RT_TO_ARR_ATa_RA
  (add-label-suffix
   "__" "__WRITE_RT_TO_ARR_ATa_RA"
   (list
;; pop tos into array element a (0 indexed), array pointed to by RA
;; it will dec-refcnt on previous array entry, if it is a pointer that is overwritten
;; input:  A = index (0 indexed)
;;         RA = pointer to array
;;         RT = cell to store
;;         EVLSTK
;; usage:  A, X, Y, RT, RA, RZ
;; output: (RA),A <- RT, RT<<EVLSTK
;; funcs:
;;   DEC_REFCNT_RZ
;;   WRITE_RT_ARR_ATa_RA
;;   POP_CELL_EVLSTK_TO_RT
;; NO BOUNDS CHECK!
    (label POP_EVLSTK_TO_ARR_ATa_RA)
           (JSR WRITE_RT_TO_ARR_ATa_RA)
           (JMP POP_CELL_EVLSTK_TO_RT)

;; pop tos into array element a (0 indexed), array pointed to by RA
;; same as POP_EVLSTK_TO_ARR_ATa_RA
;; but with BOUNDS CHECK
    (label POP_EVLSTK_TO_ARR_ATa_RA__CHECK_BOUNDS)
           (JSR WRITE_RT_TO_ARR_ATa_RA__CHECK_BOUNDS)
           (JMP POP_CELL_EVLSTK_TO_RT)

;; write the tos into array element a (0 indexed), array pointed to by RA
;; same as WRITE_RT_TO_ARR_ATa_RA
;; but with BOUNDS CHECK
    (label WRITE_RT_TO_ARR_ATa_RA__CHECK_BOUNDS)
           (LDY !$01)
           (CMP (ZP_RA),y)
           (BPL BOUNDS_ERR__)
           (CMP !$00)
           (BPL WRITE_RT_TO_ARR_ATa_RA)
    (label BOUNDS_ERR__)
           (BRK)

;; write the tos into array element a (0 indexed), array pointed to by RA
;; it will dec-refcnt on previous array entry, if it is a pointer that is overwritten
;; it will NOT inc-refcnt on RT even though it is now in RT and the array, this has to be taken care of by the caller!
;; input:  A = index (0 indexed)
;;         RA = pointer to array
;;         RT = cell to store
;; usage:  A, X, Y, RT, RA, RZ
;; output: (RA),A <- RT
;; funcs:
;;   DEC_REFCNT_RZ
;; NO CHECKING (NO BOUNDS, NO TYPE ...)
    (label WRITE_RT_TO_ARR_ATa_RA)
           (ASL A)
           (CLC)
           (ADC !$02) ;; point to first cell (index 0)
           ;; get previous content into rt and decr ref count (if applicable)
           (TAY)
           (LDA (ZP_RA),y) ;; if low byte (tagged)
           (BEQ NO_DEC_REFCNT__)
           (STA ZP_RZ)     ;; store for later dec-refcnt!
           (AND !$03)
           (CMP !$03)
           (BEQ NO_DEC_REFCNT__)
           (INY)
           (LDA (ZP_RA),y) ;; if high byte is 0, it is nil, no gc there
           (BEQ NO_DEC_REFCNT_AND_DEC_Y__)
           (STA ZP_RZ+1)   ;; store for later dec-refcnt!
           (DEY)
           (BNE MAYBE_DEC_REFCNT__) ;; lowbyte is always != 0 => branch is always taken

    (label NO_DEC_REFCNT_AND_DEC_Y__)
           (DEY)
    (label NO_DEC_REFCNT__)
           (LDA !$00)
           (STA ZP_RZ) ;; mark RZ as empty
    (label MAYBE_DEC_REFCNT__)
           (LDA ZP_RT)
           (STA (ZP_RA),y) ;;
           (INY)
           (LDA ZP_RT+1)
           (STA (ZP_RA),y)
           (LDA ZP_RZ)
           (BEQ DONE__) ;; if RZ is marked as empty, return with DEC_REFCNT
           (JMP DEC_REFCNT_RZ) ;; decrement

    (label DONE__)
           (RTS))))

(module+ test #| vm_cell_stack_write_tos_to_array_ata_ptr |#
  (define vm_cell_stack_write_tos_to_array_ata_ptr-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDA !$04)
     (JSR ALLOC_CELLARR_TO_RA)

     (LDA !$ff)
     (LDX !$01)
     (JSR PUSH_INT_TO_EVLSTK)

     (LDA !$02)
     (JSR WRITE_RT_TO_ARR_ATa_RA)))

  (check-equal? (vm-page->strings vm_cell_stack_write_tos_to_array_ata_ptr-state-after PAGE_AVAIL_0)
                (list
                 "page-type:      m1 page p1"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $16"))
  (check-equal? (memory-list vm_cell_stack_write_tos_to_array_ata_ptr-state-after ZP_RA (add1 ZP_RA))
                (list #x04 PAGE_AVAIL_0)
                "points to the cell-array in this m1 page")
  (check-equal? (memory-list vm_cell_stack_write_tos_to_array_ata_ptr-state-after (+ PAGE_AVAIL_0_W #x04) (+ PAGE_AVAIL_0_W #x0d))
                (list TAG_BYTE_CELL_ARRAY #x04
                      #x00 #x00
                      #x00 #x00
                      #x07 #xff
                      #x00 #x00)
                (string-append "slot is a cell-array, with 4 elements"
                               "slot 0 = empty"
                               "slot 1 = empty"
                               "slot 2 = int $1fff"
                               "slot 3 = empty")))

(module+ test #| write to array bounds checks |#
  (define to-array-ata-ra-4-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
      (LDA !$04)
      (JSR ALLOC_CELLARR_TO_RA)

      (LDA !$ff)
      (LDX !$01)
      (JSR PUSH_INT_TO_EVLSTK)

      (LDA !$04) ;; out of bounds
      (JSR WRITE_RT_TO_ARR_ATa_RA__CHECK_BOUNDS)

      (JSR PUSH_INT_0_TO_EVLSTK)
    ))

  (check-equal? (vm-stack->strings to-array-ata-ra-4-state)
               (list "stack holds 1 item"
                     "int $01ff  (rt)")
               "never got to pushing 0 since access index 4 is out of bounds")

  (define to-array-ata-ra-ff-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
      (LDA !$04)
      (JSR ALLOC_CELLARR_TO_RA)

      (LDA !$ff)
      (LDX !$01)
      (JSR PUSH_INT_TO_EVLSTK)

      (LDA !$ff) ;; out of bounds
      (JSR WRITE_RT_TO_ARR_ATa_RA__CHECK_BOUNDS)

      (JSR PUSH_INT_0_TO_EVLSTK)
    ))

  (check-equal? (vm-stack->strings to-array-ata-ra-ff-state)
               (list "stack holds 1 item"
                     "int $01ff  (rt)")
               "never got to pushing 0 since access index ff is out of bounds")

  (define to-array-ata-ra-0-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
      (LDA !$04)
      (JSR ALLOC_CELLARR_TO_RA)

      (LDA !$ff)
      (LDX !$01)
      (JSR PUSH_INT_TO_EVLSTK)

      (LDA !$00) ;; in bounds
      (JSR WRITE_RT_TO_ARR_ATa_RA__CHECK_BOUNDS)

      (JSR PUSH_INT_0_TO_EVLSTK)
    ))

  (check-equal? (vm-stack->strings to-array-ata-ra-0-state)
               (list "stack holds 2 items"
                     "int $0000  (rt)"
                     "int $01ff")
               "got to pushing 0 since access index 0 is in bounds")

  (define to-array-ata-ra-3-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDA !$04)
      (JSR ALLOC_CELLARR_TO_RA)

      (LDA !$ff)
      (LDX !$01)
      (JSR PUSH_INT_TO_EVLSTK)

      (LDA !$03) ;; in bounds
      (JSR WRITE_RT_TO_ARR_ATa_RA__CHECK_BOUNDS)

      (JSR PUSH_INT_0_TO_EVLSTK)
    ))

  (check-equal? (vm-stack->strings to-array-ata-ra-3-state)
               (list "stack holds 2 items"
                     "int $0000  (rt)"
                     "int $01ff")
               "got to pushing 0 since access index 0 is in bounds"))

;; --------------------
;; PUSH_ARR_ATa_RA_TO_EVLSTK
;; push the cell at A of the array in RA onto the Stack (RT+EVLSTK)
;; (RA),A -> RT+EVLSTK
;; input:  A, RA, RT+EVLSTK
;; usage:  A, X, Y
;; output: RT+EVLSTK
;; funcs:
;;   PUSH_RT_TO_EVLSTK_IF_NONEMPTY
(define PUSH_ARR_ATa_RA_TO_EVLSTK
  (add-label-suffix
   "__" "PUSH_ARR_ATa_RA_TO_EVLSTK"
   (list
    (label PUSH_ARR_ATa_RA_TO_EVLSTK)
           (PHA)
           (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
           (PLA)
           (JMP WRITE_ARR_ATa_RA_TO_RT)

    (label PUSH_ARR_ATa_RA_TO_EVLSTK__CHECK_BOUNDS)
           (PHA)
           (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
           (PLA)

    (label CHECK_BOUNDS__)
           (LDY !$01)
           (CMP (ZP_RA),y)
           (BPL BOUNDS_ERR__)
           (CMP !$00)
           (BPL WRITE_ARR_ATa_RA_TO_RT)
    (label BOUNDS_ERR__)
           (BRK)  ;; out of bounds error

;; --------------------
;; WRITE_ARR_ATa_RA_TO_EVLSTK
;; write the cell at A of the array in RA into RT
;; (RA),A -> RT
;; input:  A, RA, RT
;; usage:  A, Y
;; output: RT
;; funcs:  -
    (label WRITE_ARR_ATa_RA_TO_RT)
           (ASL A)
           (CLC)
           (ADC !$03)                    ;; get y to point to high byte of cell at index
           (TAY)
           (LDA (ZP_RA),y)               ;; copy high byte
           (STA ZP_RT+1)
           (DEY)
           (LDA (ZP_RA),y)               ;; copy low byte
           (STA ZP_RT)
           (RTS))))

(module+ test #| vm_cell_stack_push_array_ata_ptr |#
  (define test-cell-stack-push-array-ata-ptr-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDA !$04)
     (JSR ALLOC_CELLARR_TO_RA)

     (LDA !$02)
     (JSR PUSH_ARR_ATa_RA_TO_EVLSTK) ;; @2 = empty -> stack => stack is still empty

     (LDA !$ff)
     (LDX !$01)
     (JSR PUSH_INT_TO_EVLSTK)            ;; int $1ff -> stack

     (LDA !$02)
     (JSR WRITE_RT_TO_ARR_ATa_RA) ;; tos (int $1ff) -> @2 (overwriting 0 (empty) in array)

     (LDA !$02)
     (JSR PUSH_ARR_ATa_RA_TO_EVLSTK)  ;; @2 (now int $1ff) -> stack
     ))

  (inform-check-equal? (cpu-state-clock-cycles test-cell-stack-push-array-ata-ptr-state-after)
                1274)
  (check-equal? (vm-stack->strings test-cell-stack-push-array-ata-ptr-state-after)
                (list "stack holds 2 items"
                      "int $01ff  (rt)"
                      "int $01ff")))

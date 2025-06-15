#lang racket/base
;; [[pdfview:~/Downloads/Small memory software patterns for limited memory systems.2001.pdf::261++0.00][Small memory software patterns for limited memory systems.2001.pdf: Page 261]]

#|

combines all the implementation of basic memory primitives for the native 6510 assembler implementation of mil.

implements the integrative functions, making use of vm-mm-... files and their implementations

primitives are e.g. allocation of cell-pair(s)
evalation stack operations
call frame primitives etc.

|#

;; see also vm-memory-manager.org

;;(require (only-in racket/format ~a))
(require (only-in racket/list flatten take empty? drop make-list))

(require "../6510.rkt")
(require (only-in "../util.rkt" format-hex-byte))
(require (only-in "../ast/6510-resolver.rkt"
                  add-label-suffix
                  replace-labels))
(require (only-in "../tools/6510-interpreter.rkt" peek))
(require (only-in "./vm-memory-map.rkt"
                  VM_MEMORY_MANAGEMENT_CONSTANTS
                  ZP_RT
                  ZP_RP
                  ZP_RA
                  ZP_RZ
                  ZP_CELL_STACK_TOS
                  ZP_CELL_STACK_LB_PTR
                  ZP_CELL_STACK_HB_PTR
                  TAG_BYTE_BYTE_CELL
                  TAG_BYTE_CELL_ARRAY
                  TAG_BYTE_NATIVE_ARRAY
                  TAGGED_NIL
                  ZP_TEMP))
(require (only-in "./vm-mm-register-functions.rkt"
                  WRITE_NIL_TO_RT
                  WRITE_NIL_TO_RP
                  WRITE_INT_AY_TO_RT
                  CP_RA_TO_RT
                  CP_RT_TO_RB
                  CP_RA_TO_RZ
                  CP_RB_TO_RZ
                  CP_RC_TO_RZ
                  CP_RT_TO_RA
                  CP_RT_TO_RP
                  CP_RT_TO_RZ
                  CP_RZ_TO_RT
                  SWAP_ZP_WORD
                  CP_RA_TO_RB
                  SWAP_RA_RB))
(require (only-in "./vm-mm-pages.rkt"
                  VM_INITIALIZE_MEMORY_MANAGER
                  FREE_PAGE_A
                  ALLOC_PAGE_TO_X
                  VM_INITIAL_MM_REGS
                  VM_PAGE_SLOT_DATA
                  GLOBAL_CELL_FREE_LIST
                  GLOBAL_CELLPAIR_FREE_LIST
                  GLOBAL_CELLPAIR_PAGE_FOR_ALLOC))
(require (only-in "./vm-mm-cell-pairs.rkt"
                  INIT_CELLPAIR_PAGE_X_TO_AX
                  GET_FRESH_CELLPAIR_TO_AX
                  ALLOC_CELLPAIR_AX_TO_RT
                  ALLOC_CELLPAIR_TO_RT
                  WRITE_CELLPAIR_RT_CELLy_TO_RT
                  WRITE_CELLPAIR_RP_CELLy_TO_RP
                  WRITE_CELLPAIR_RT_CELLy_TO_RP
                  WRITE_RP_TO_CELLy_CELLPAIR_RT
                  WRITE_RT_TO_CELLy_CELLPAIR_RP
                  INC_REFCNT_CELLPAIR_RT
                  DEC_REFCNT_CELLPAIR_RZ
                  FREE_CELLPAIR_RZ
                  GC_CELLPAIR_FREE_LIST))
(require (only-in "./vm-mm-cells.rkt"
                  INIT_CELL_PAGE_X_TO_AX
                  INC_REFCNT_CELL_RT
                  DEC_REFCNT_CELL_RZ
                  FREE_CELL_RZ
                  ALLOC_CELL_TO_RT
                  GET_FRESH_CELL_TO_AX
                  ALLOC_CELL_AX_TO_RT
                  GC_CELLS))
(require (only-in "./vm-mm-cell-stack.rkt"
                  INIT_CELLSTACK_PAGE_X
                  PUSH_XA_TO_EVLSTK
                  POP_CELL_EVLSTK_TO_RT
                  PUSH_RT_TO_EVLSTK
                  POP_CELL_EVLSTK_TO_CELLy_RT
                  POP_CELL_EVLSTK_TO_RA))
(require (only-in "./vm-mm-m1-slots.rkt"
                  INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX
                  DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A
                  PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ
                  ADD_M1_SLOT_RZ_TO_PFL
                  FREE_M1_SLOT_RA
                  VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS
                  VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS
                  INC_REFCNT_M1_SLOT_RA
                  FREE_M1_SLOT_RZ
                  ALLOC_M1_SLOT_TO_RA))
(require (only-in "./vm-mm-cell-array.rkt"
                  ALLOC_CELLARR_TO_RA
                  GC_INCR_ARRAY_SLOT_RZ
                  GC_CELL_ARRAYS
                  FREE_CELLARR_RZ
                  PUSH_ARR_ATa_RA_TO_EVLSTK
                  WRITE_RT_TO_ARR_ATa_RA))
(require (only-in "./vm-mm-native-array.rkt"
                  ALLOC_NATARR_TO_RA))

(provide vm-memory-manager
          ;; ---------------------------------------- refcount
          INC_REFCNT_RT         ;; generic increment of refcount (dispatches depending on type)

          DEC_REFCNT_RZ         ;; generic decrement of refcount (dispatches depending on type)

          FREE_RT               ;; (includes FREE_RZ and _RA) free pointer (is cell-ptr, cell-pair-ptr, m1-slot-ptr, native-array, cell-array)
)

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in "../ast/6510-relocator.rkt" command-len))
  (require (only-in "./vm-inspector-utils.rkt"
                     vm-regt->string
                     vm-refcount-cell-pair-ptr
                     vm-refcount-cell-ptr))
  (require (only-in "./vm-memory-manager-test-utils.rkt"
                    calls-to-mock
                    compact-run-code-in-test-))

  ;; ;; run the given code in test, wrapping it with mocks and counters, entering interactive debugger, if requested
  ;; (define (run-code-in-test bc (debug #f) #:mock (mocked-code-list (list)))
  ;;   (run-code-in-test-on-code (wrap-code-for-test bc vm-memory-manager mocked-code-list) debug))

  ;; run the given code using mocks, calls being counted, and label suffixes for the test-code
  (define (compact-run-code-in-test #:debug (debug #f) #:mock (mocked-labels (list)) . cmds)
    (apply compact-run-code-in-test- cmds #:runtime-code vm-memory-manager #:debug debug #:mock mocked-labels )
))

(module+ test
  (define PAGE_AVAIL_0 #x9a)      ;; high byte of first page available for allocation
  (define PAGE_AVAIL_0_W #x9a00)  ;; word (absolute address) of first page available
  (define PAGE_AVAIL_1 #x99)      ;; high byte of second page available for allocation
  (define PAGE_AVAIL_1_W #x9900)) ;; word (absolute address) of second page available

;; macro that does pointer detection and jumping to certain labels for the respective pointer type
(define (PTR_DETECTION_MACRO_Rx
         register
         label-unknown
         label-cell
         label-cellpair
         label-cellarr
         label-nativearr
         label-m1)
  (replace-labels
   (hash "REGISTER"   register
         "UNKNOWN"    label-unknown
         "CELL"       label-cell
         "CELLPAIR"   label-cellpair
         "CELLARR"    label-cellarr
         "NATIVEARR"  label-nativearr
         "M1"         label-m1)
   (list
    (LDA REGISTER)                      ;; load zero page register tagged low byte
    (BEQ UNKNOWN)                       ;; tagged low byte 0 => illegal or nil => jump to unknown
    (LSR)                               ;;
    (BCC CELL)                          ;; lowbyte ends on '0' => is a cell pointer
    (LSR)
    (BCC CELLPAIR)                      ;; lowbyte ends on '01' => is a cell-pair pointer

    (CMP !TAG_BYTE_CELL_ARRAY_LSR2)
    (BEQ CELLARR)                       ;; tagged low byte = cell-array tag => is cell-array
    (CMP !TAG_BYTE_NATIVE_ARRAY_LSR2)
    (BEQ NATIVEARR)                     ;; tagged low byte = native-array tag => is native-array

    ;; fall-back
    (LDY  !$00)
    (LDA (REGISTER),y)                  ;; get first byte of page
    (AND !$f8)                          ;; mask out low 3 bits
    (CMP !$10)                          ;;
    (BEQ M1))))                         ;; jump if page tag b00010xxx => m1-slots page

(define (PTR_DETECTION_MACRO_RZ
         label-unknown
         label-cell
         label-cellpair
         label-cellarr
         label-nativearr
         label-m1)
  (PTR_DETECTION_MACRO_Rx
   "ZP_RZ"
   label-unknown
   label-cell
   label-cellpair
   label-cellarr
   label-nativearr
   label-m1))

(define (PTR_DETECTION_MACRO_RT
         label-unknown
         label-cell
         label-cellpair
         label-cellarr
         label-nativearr
         label-m1)
  (PTR_DETECTION_MACRO_Rx
   "ZP_RT"
   label-unknown
   label-cell
   label-cellpair
   label-cellarr
   label-nativearr
   label-m1))

;; @DC-FUN: INC_REFCNT_RT, group: gc
;; find out what kind of cell zp_rt points to,
;; then call the right decrement refcounts function
;; input:  ZP_RT
;; output: the right refcount is decremented
;;         (in case of m1 pages, @ZP_RT-1)
;;         (in case of cell pages @ZP_RT>>1)
;;         (in case of cell-pair pages @ZP_RT>>2)
(define INC_REFCNT_CELLARR_RT #t)
(define INC_REFCNT_NATIVEARR_RT #t)
(define INC_REFCNT_RT
  (add-label-suffix
   "__" "__INC_REFCNT_RT"
   (flatten
  (list
   (label INC_REFCNT_RT)
          (PTR_DETECTION_MACRO_RT
           "UNKNOWN__"
           "INC_REFCNT_CELL_RT"
           "LSR__INC_RFCNT_CELLPAIR__"
           "INC_REFCNT_M1_PAGE"
           "INC_REFCNT_M1_PAGE"
           "INC_REFCNT_M1_PAGE")

   (label UNKNOWN__)
          ;; unknown object type (or atomic value that cannot be ref counted and MUST NOT END UP in ZP_RT)
   (label DONE__)
          (RTS)

   (INC_REFCNT_CELLPAIR_RT "LSR__INC_RFCNT_CELLPAIR__")

   (INC_REFCNT_CELL_RT "NOW_INCREMENT_REFCNT__CELL__")

   (label INC_REFCNT_CELLARR_RT)
   (label INC_REFCNT_NATIVEARR_RT)
   (label INC_REFCNT_M1_PAGE)
          (LDX ZP_RT)
          (DEX)
          (BNE NOW_INCREMENT_REFCNT__CELL__) ;; is never 0! for m1 pages
          ))))

(module+ test #| vm-refcount-decr-rt |#
  (define vm-refcount-decr-rt-state
    (compact-run-code-in-test
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR INC_REFCNT_RT)
     (JSR INC_REFCNT_RT)))

  (check-equal? (vm-refcount-cell-pair-ptr vm-refcount-decr-rt-state (+ PAGE_AVAIL_0_W #x05))
                2)

  (define vm-refcount-decr-rt-state2
    (compact-run-code-in-test
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR INC_REFCNT_RT)
     (JSR INC_REFCNT_RT)
     (JSR DEC_REFCNT_RT)))

  (check-equal? (vm-refcount-cell-pair-ptr vm-refcount-decr-rt-state2 (+ PAGE_AVAIL_0_W #x05))
                1))



(module+ test #| vm-refcount-mmcr-rt--cell-pair-ptr |#
  (define vm-refcount-mmcr-rt--cell-ptr-state
    (compact-run-code-in-test
     (JSR ALLOC_CELL_TO_RT)
     (JSR INC_REFCNT_CELL_RT)
     (JSR INC_REFCNT_CELL_RT)))

  (check-equal? (vm-regt->string vm-refcount-mmcr-rt--cell-ptr-state)
                (format "ptr[2] $~a02" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-refcount-cell-ptr vm-refcount-mmcr-rt--cell-ptr-state (+ PAGE_AVAIL_0_W #x02))
                2)

  (define vm-refcount-mmcr-rt--cell-ptr-state2
    (compact-run-code-in-test
     (JSR ALLOC_CELL_TO_RT)
     (JSR INC_REFCNT_CELL_RT)
     (JSR INC_REFCNT_CELL_RT)
     (JSR DEC_REFCNT_CELL_RT)))

  (check-equal? (vm-regt->string vm-refcount-mmcr-rt--cell-ptr-state2)
                (format "ptr[1] $~a02" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-refcount-cell-ptr vm-refcount-mmcr-rt--cell-ptr-state2 (+ PAGE_AVAIL_0_W #x02))
                1))

;; @DC-FUN: FREE_RT, group: gc
;; free nonatomic (is cell-ptr, cell-pair-ptr, cell-array-ptr, native-array-ptr)
;; parameter: zp_rt
(define FREE_RT    ;; TODO: FREE_CELL_RT should work only cell pointers only, anyone in need of generic free uses this function
  (add-label-suffix
   "__" "FREE_RT"
   (flatten
  (list
   (label FREE_RA)
          (JSR CP_RA_TO_RZ)
          (JMP FREE_RZ)

   (label FREE_RT)
          (JSR CP_RT_TO_RZ)

   (label FREE_RZ)
          (PTR_DETECTION_MACRO_RZ
           "UNKNOWN__"
           "FREE_CELL__"
           "FREE_CELLPAIR__"
           "FREE_CELL_ARRAY__"
           "FREE_NATIVE_ARRAY__"
           "FREE_M1__")

          ;; unknown pointer type in zp_rt
   (label UNKNOWN__)
          (BRK)

   (label FREE_CELL__)
          (JMP FREE_CELL_RZ)

   (label FREE_CELLPAIR__)
          (JMP FREE_CELLPAIR_RZ)

   (label FREE_CELL_ARRAY__)
          (JMP GC_INCR_ARRAY_SLOT_RZ)

   (label FREE_NATIVE_ARRAY__)
          (JMP ADD_M1_SLOT_RZ_TO_PFL)

   (label FREE_M1__)
          (JMP FREE_M1_SLOT_RZ)))))

(module+ test #| vm-free-ptr-in-rt |#
  (define free-ptr-in-rt-state
    (compact-run-code-in-test
     #:mock (list (label FREE_CELL_RZ))
     (JSR ALLOC_CELL_TO_RT)
     (JSR FREE_RT)))

  (check-equal? (calls-to-mock free-ptr-in-rt-state)
                #x01
                "dispatches call to free-cell-ptr routine")

  (define free-ptr-in-rt-2-state
    (compact-run-code-in-test
     #:mock (list (label FREE_CELLPAIR_RZ))
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR FREE_RT)))

  (check-equal? (calls-to-mock free-ptr-in-rt-2-state)
                #x01
                "dispatches call to free-cell-pair-ptr routine"))

;; impl complete, test missing

;; @DC-FUN: DEC_REFCNT_RZ, group: gc
;; decrement the refcount (if a pointer) in RZ, call respective free if refcount drops to 0
;; input:  RZ (RA, RT)
;; usage:  A, X, Y, RZ
;; output:
;; funcs:
;;   FREE_M1_SLOT_RZ >>
;;   FREE_CELL_RZ >>
;;   FREE_CELLPAIR_RZ >>
(define FREE_M1_SLOT_RZm1 #t)
(define DEC_REFCNT_M1_SLOT_RZ #t)
(define DEC_REFCNT_CELLARR_RZ #t)
(define DEC_REFCNT_NATIVEARR_RZ #t)
(define DEC_REFCNT_M1_SLOT_RT #t)
(define DEC_REFCNT_CELLARR_RT #t)
(define DEC_REFCNT_NATIVEARR_RT #t)
(define DEC_REFCNT_M1_SLOT_RA #t)
(define DEC_REFCNT_CELLARR_RA #t)
(define DEC_REFCNT_NATIVEARR_RA #t)
(define DEC_REFCNT_RT #t)
(define DEC_REFCNT_RA #t)
(define DEC_REFCNT_RB #t)
(define DEC_REFCNT_RC #t)
(define DEC_REFCNT_RZ
  (add-label-suffix
   "__" "__DEC_REFCNT_RZ"
   (flatten
   (list
    (label DEC_REFCNT_RC)
           (JSR CP_RC_TO_RZ)
           (JMP DEC_REFCNT_RZ)

    (label DEC_REFCNT_RB)
           (JSR CP_RB_TO_RZ)
           (JMP DEC_REFCNT_RZ)

    (label DEC_REFCNT_RA)
           (JSR CP_RA_TO_RZ)
           (JMP DEC_REFCNT_RZ)

    (label DEC_REFCNT_RT)
           (JSR CP_RT_TO_RZ)

   (label DEC_REFCNT_RZ)    ;; RZ -> [cell] || [cellA][cellB] || [cell-natarr-header][byte0][byte1] ...[byten] || [cell-arr-header][cell0][cell1]...[celln]
          (PTR_DETECTION_MACRO_RZ
           "UNKNOWN__"
           "CELL_ALREADY_LSRED__"
           "CELLPAIR_ALREADY_LSRED__"
           "DEC_REFCNT_CELLARR_RZ"
           "DEC_REFCNT_NATIVEARR_RZ"
           "DEC_REFCNT_M1_SLOT_RZ")

   (label UNKNOWN__)
          ;; unknown object type (or atomic value that cannot be ref counted and MUST NOT END UP in ZP_RZ)
   (label DONE__)
          (RTS)


   (label DEC_REFCNT_M1_SLOT_RA)
   (label DEC_REFCNT_CELLARR_RA)
   (label DEC_REFCNT_NATIVEARR_RA)
          (JSR CP_RT_TO_RA)
          (JMP DEC_REFCNT_M1_SLOT_RZ)

   (label DEC_REFCNT_M1_SLOT_RT)
   (label DEC_REFCNT_CELLARR_RT)
   (label DEC_REFCNT_NATIVEARR_RT)
          (JSR CP_RT_TO_RZ)

   (label DEC_REFCNT_M1_SLOT_RZ)
   (label DEC_REFCNT_CELLARR_RZ)
   (label DEC_REFCNT_NATIVEARR_RZ)
          ;; TODO decrement count (both are on m1 pages, but native arrays can be freed without looking at its contents!)
          (DEC ZP_RZ) ;; no page boundary will be crossed, since lowbyte always > 0
          (LDY !$00)
          (LDA (ZP_RZ),y)
          (SEC)
          (SBC !$01)
          (STA (ZP_RZ),y)
          (BEQ FREE_M1_SLOT_RZm1) ;; m1 since ZP_RZ was decremented and currently points to the refcnt
          (INC ZP_RZ)               ;; restore original rc
          (RTS)

   (label FREE_M1_SLOT_RZm1)
          ;; if native array, simply add this m1slot to the free list of this m1 page profile
          ;; if cell array, follow the algorithm
          (INC ZP_RZ)
          (JMP FREE_M1_SLOT_RZ)


   ;; include cellpair code here
   (DEC_REFCNT_CELLPAIR_RZ "CELLPAIR_ALREADY_LSRED__")

   ;; include cell code here
   (DEC_REFCNT_CELL_RZ "CELL_ALREADY_LSRED__" "DEC_REFCNT_CELL_RZ_TO_M1_SLOT__")

   (label DEC_REFCNT_CELL_RZ_TO_M1_SLOT__)
          (LDX ZP_RZ)
          (DEX)
          (STY DEC_PAGE_M1_SLOT_CNT__+2)
   (label DEC_PAGE_M1_SLOT_CNT__)
          (DEC $C000,x)
          (BNE DONE__)
          (JMP FREE_M1_SLOT_RZ)

   ;; (label DEC_REFCNT_CELL_RZ_TO_CELL__)
   ;;        (STY DEC_PAGE_CELL_CNT__+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
   ;; (label DEC_PAGE_CELL_CNT__)
   ;;        (DEC $c000,x)               ;; c0 is overwritten by page (see above), x = position of refcount (a >> 1)
   ;;        (BNE DONE__)
   ;;        (JMP FREE_CELL_RZ)      ;; free (since refcnt dropped to 0), and this is definitely a cell-ptr => free-cell can be called
          ))))

(module+ test #| DEC_REFCNT_RZ |#
  (define dec-refcnt-rc--dec-ref--cell
    (compact-run-code-in-test
     ;; #:debug #t
     #:mock (list (label FREE_CELL_RZ))

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
                "remaining refcount on cell0 is 1")

  (define dec-refcnt-rc--dec-ref-to-0--cell
    (compact-run-code-in-test
     #:mock (list (label FREE_CELL_RZ))

     (JSR ALLOC_CELL_TO_RT)    ;; new cell in RT (with refcount = 0)
     (JSR INC_REFCNT_CELL_RT) ;; now should be 1
     (JSR CP_RT_TO_RZ)

     ;; unit under test
     (JSR DEC_REFCNT_RZ)))

  (check-equal? (calls-to-mock dec-refcnt-rc--dec-ref-to-0--cell)
                #x01
                "free cell has taken place!")
  (check-equal? (peek dec-refcnt-rc--dec-ref-to-0--cell (+ PAGE_AVAIL_0_W 01))
                #x00
                "remaining refcount on cell0 is 0")


  (define dec-refcnt-rc--dec-ref-to-0--cellpair
    (compact-run-code-in-test
     #:mock (list (label FREE_CELLPAIR_RZ))

     (JSR ALLOC_CELLPAIR_TO_RT)     ;; new cellpair in RT (with refcount = 0)
     (JSR INC_REFCNT_CELLPAIR_RT)   ;; now should be 1
     (JSR CP_RT_TO_RZ)

     ;; unit under test
     (JSR DEC_REFCNT_RZ)))

  (check-equal? (calls-to-mock dec-refcnt-rc--dec-ref-to-0--cellpair)
                #x01
                "free cellpair has taken place!")
  (check-equal? (peek dec-refcnt-rc--dec-ref-to-0--cellpair (+ PAGE_AVAIL_0_W 01))
                #x00
                "remaining refcount on cellpair0 is 0")

  (define dec-refcnt-rc--dec-ref--cellpair
    (compact-run-code-in-test
     #:mock (list (label FREE_CELLPAIR_RZ))

     (JSR ALLOC_CELLPAIR_TO_RT)     ;; new cellpair in RT (with refcount = 0)
     (JSR INC_REFCNT_CELLPAIR_RT)   ;; now should be 1
     (JSR INC_REFCNT_CELLPAIR_RT)   ;; now should be 2
     (JSR CP_RT_TO_RZ)

     ;; unit under test
     (JSR DEC_REFCNT_RZ)))

  (check-equal? (calls-to-mock dec-refcnt-rc--dec-ref--cellpair)
                #x00
                "no free cellpair has taken place!")
  (check-equal? (peek dec-refcnt-rc--dec-ref--cellpair (+ PAGE_AVAIL_0_W 01))
                #x01
                "remaining refcount on cellpair0 is 1")

  (define dec-refcnt-rc--dec-ref-to-0--m1_slot
    (compact-run-code-in-test
     #:mock (list (label FREE_M1_SLOT_RZ))

     (LDA !$10)
     (JSR ALLOC_M1_SLOT_TO_RA)     ;; new m1_slot in RA (with refcount = 0)
     (JSR INC_REFCNT_M1_SLOT_RA)   ;; now should be 1
     (JSR CP_RA_TO_RZ)

     ;; unit under test
     (JSR DEC_REFCNT_RZ)))

  (check-equal? (calls-to-mock dec-refcnt-rc--dec-ref-to-0--m1_slot)
                #x01
                "free m1_slot has taken place!")
  (check-equal? (peek dec-refcnt-rc--dec-ref-to-0--m1_slot (+ PAGE_AVAIL_0_W 03))
                #x00
                "remaining refcount on m1_slot0 is 0")

  (define dec-refcnt-rc--dec-ref--m1_slot
    (compact-run-code-in-test
     #:mock (list (label FREE_M1_SLOT_RZ))

     (LDA !$10)
     (JSR ALLOC_M1_SLOT_TO_RA)     ;; new m1_slot in RT (with refcount = 0)
     (JSR INC_REFCNT_M1_SLOT_RA)   ;; now should be 1
     (JSR INC_REFCNT_M1_SLOT_RA)   ;; now should be 2
     (JSR CP_RA_TO_RZ)

     ;; unit under test
     (JSR DEC_REFCNT_RZ)))

  (check-equal? (calls-to-mock dec-refcnt-rc--dec-ref--m1_slot)
                #x00
                "no free m1_slot has taken place!")
  (check-equal? (peek dec-refcnt-rc--dec-ref--m1_slot (+ PAGE_AVAIL_0_W 03))
                #x01
                "remaining refcount on m1_slot 0 is 1"))

;; @DC-FUN: GC_ALL, group: gc
;; garbage collect all cells, all cell-pairs and all cell arrays marked for reuse or partially collected
;; input:  GLOBAL_CELL_FREE_LIST, GLOBAL_CELLPAIR_FREE_LIST, ZP_PART_GCD_CELL_ARRAYS
;; usage:  A, X, Y, RZ
;; output: GLOBAL_CELL_FREE_LIST+1      = 0
;;         GLOBAL_CELLPAIR_FREE_LIST+1  = 0
;;         ZP_PART_GCD_CELL_ARRAYS+1    = 0
;; funcs:
;;   GC_CELL_ARRAYS
;;   GC_CELL_ARRAY
;;   GC_INCR_ARRAY_SLOT_RZ
;;   GC_CELLPAIR_FREE_LIST
;;   DEC_REFCNT_RZ >>
;;   GC_CELLS
(define GC_ALL
  (add-label-suffix
   "__" "__NEW_GC_ALL"
   (list
    (label GC_ALL)
          (JSR GC_CELL_ARRAYS)    ;; until no more cell arrays are available
          (JSR GC_CELLPAIR_FREE_LIST) ;; until no more cell pairs are available
          (JMP GC_CELLS)          ;; until no more cells are available
          )))




;; idea: have a list of code pages (adding new page as head if allocated)
;;       TODO: how does relocation work here?
;; idea: function id = ptr to bytecode
;; idea: relocatable function ids = function id = ptr into reloc table (identified by lowest bit = 1)
;;       reloc table is a page with entries:
;;             function id -> [lowbyte] [highbyte] <- actual location of function
;;             moving a function can be done by rewriting function id entry in that table
;;             calling that function needs to do one more indirection
;;             functions currently in the call stack cannot (easily) be relocated!
;; module descriptor
;;   module name string
;;   child modules (list of references to child modules)
;;   modules code pages head (ptr to the head of the list of code pages of this module)

;; code page
;;   00           : page type
;;   01..02       : pointer to module descriptor
;;   03           : len of bytecode 0..127 (blen), highest bit set means exported function <-- start of first function descriptor
;;   04           : #locals
;;   05           : #params
;;   06..06+blen  : bytecode    <- function id points on first byte of byte code? [must start word aligned!]
;;   07+blen      : len of function name str (slen) [only if exported function]
;;   08+blen..    : function name                   [only if exported function]
;;   09+blen+slen :                        <-- start of next function descriptor
;;

;; could a constants pool be put into these pages, too? (relocation would be difficult, though)

;; load from disk: https://c64os.com/post/c64kernalrom#file_load


;; input:  x,y  id
;; output: x,y  ptr to function bytecode
(define VM_LOCATE_FUNCTION_BY_ID
  (list
   (label VM_LOCATE_FUNCTION_BY_ID)
   (RTS)))

;; input:  zp_ptr ptr to function name string
;; output: x,y    ptr to function bytecode
(define VM_LOCATE_FUNCTION_BY_NAME
  (list
   (label VM_LOCATE_FUNCTION_BY_NAME)
          (BRK))) ;; not implemented yet

;; input:  x/y  ptr to the bytecode descriptor
;;         descriptor:
;;           00..01  ptr to corresponding module descriptor
;;           02     byte code len
;;           03     function name string len
;;           04     #locals (max)
;;           05     #params
;;           06..   function byte code
;;           ...    function name string
;; output: x/y  function id
;; make sure to have a page allocated that can hold this function
;;
(define VM_REGISTER_FUNCTION
  (list
   (label VM_REGISTER_FUNCTION)
          ;; check for space on a code page of the given module (else allocate)
          ;; copy data into the code page
          ;; optional: create function id mapping on function id mapping page
          (RTS))) ;; not implemented yet

(define vm-memory-manager
  (append VM_MEMORY_MANAGEMENT_CONSTANTS
          VM_INITIALIZE_MEMORY_MANAGER

          ;; ---------------------------------------- alloc/free pages
          FREE_PAGE_A                                       ;; free a page (the type specific stuff, of any, must have finished)
          ALLOC_PAGE_TO_X                                   ;; allocate new page (not initialized)

          INIT_CELL_PAGE_X_TO_AX                                  ;; initialize page (in a) for cell usage
          INIT_CELLPAIR_PAGE_X_TO_AX                             ;; initialize page (in x, free slot in a) for ref counted cell-pairs
          INIT_CELLSTACK_PAGE_X                              ;; initialize page with previous references to previous cell stack pages

          INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX                         ;; allocate page and initialize for ref counted m1 slots of a specific profile (and thus size)
          ;; VM_ALLOC_PAGE_FOR_S8_SLOTS                         ;; allocate page and initialize to hold ref counted 8 byte slots <- really, maybe s8 slots can be removed alltogether

          ;; VM_ALLOC_PAGE_FOR_MODULE_CODE                      ;; allocate page and initialize to hold immutable byte code (not ref counted)

          ;; ---------------------------------------- alloc/free cells, pairs, slots
          GET_FRESH_CELL_TO_AX
          ALLOC_CELL_AX_TO_RT
          ;; ALLOC_CELL_PFL_X_TO_RT
          ALLOC_CELL_TO_RT

          GET_FRESH_CELLPAIR_TO_AX
          ALLOC_CELLPAIR_AX_TO_RT               ;; allocate a cell-pair from this page (if page has no free cell-pairs, a new page is allocated and is used to get a free cell-pair!)
          ;; ALLOC_CELLPAIR_ON_PAGE_X_TO_RT

          ALLOC_CELLPAIR_TO_RT                       ;; allocate a cell-pair from the current page (or from a new page if full)
          FREE_CELLPAIR_RZ

          GC_CELLPAIR_FREE_LIST                     ;; reclaim all cell-pairs in the queue of free cells

          ALLOC_NATARR_TO_RA                        ;; allocate an array of bytes (native) (also useful for strings)
          ALLOC_CELLARR_TO_RA                          ;; allocate an array of cells (also useful for structures)

          ALLOC_M1_SLOT_TO_RA                             ;; allocate a slot of min A size, allocating a new page if necessary
          FREE_M1_SLOT_RA                              ;; free a slot (adding it to the free list)
          ;; VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS
          VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS                  ;; remove full pages in the free list of pages of the same type as are currently in ZP_RA
          VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS               ;; put this page as head of the page free list for slots of type as in ZP_RA

          ;; VM_ALLOC_MODULE_CODE_SLOT_TO_ZP_PTR                ;; allocate a slot for module code
          ;; VM_FREE_MODULE
          ;; VM_RELOCATE_MODULE_X_TO_                           ;; relocate module identified by page x to ??

          ;; ---------------------------------------- refcount
          ;; DEC_REFCNT_RT                                ;; generic decrement of refcount (dispatches depending on type)
          INC_REFCNT_RT                                ;; generic increment of refcount (dispatches depending on type)

          ;; DEC_REFCNT_CELLPAIR_RT                 ;; decrement refcount, calling vm_free_cell_pair_in_zp_ptr if dropping to 0
          ;; DEC_REFCNT_CELL_RT                      ;; decrement refcount, calling vm_free_cell_in_zp_ptr if dropping to 0

          ;; INC_REFCNT_CELLPAIR_RT                 ;; increment refcount of cell-pair
          INC_REFCNT_M1_SLOT_RA                         ;; increment refcount of m1-slot
          ;; INC_REFCNT_CELL_RT                      ;; increment refcount of the cell, rt is pointing to

          ;; DEC_REFCNT_RA                                ;; generic decrement of refcount (dispatches depending on type)
          ;; DEC_REFCNT_CELLPAIR_RA                 ;; decrement refcount, calling vm_free_cell_pair_in_zp_ptr if dropping to 0
          ;; DEC_REFCNT_M1_SLOT_RA                       ;; decrement refcount, calling vm_free_m1_slot_in_zp_ptr if dropping to 0
          ;; DEC_REFCNT_CELL_RA                      ;; decrement refcount, calling vm_free_cell_in_zp_ptr if dropping to 0
          ;; ---------------------------------------- call frame

          ;; ---------------------------------------- misc

          ;; VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS                ;; remove full pages in the free list of pages of the same type as are currently in ZP_PTR2
          ;; VM_ENQUEUE_PAGE_AS_HEAD_FOR_PTR2_SLOTS             ;; put this page as head of the page free list for slots of type as in ZP_PTR2

          ;; GC_ARRAY_SLOT_RT                               ;; execute garbage collection on a cell array (decr-ref all array elements and collect if 0)
          
          FREE_RT                                 ;; free pointer (is cell-ptr, cell-pair-ptr, m1-slot-ptr, slot8-ptr)

          ;; ---------------------------------------- CELL_STACK / RT / RA
          POP_CELL_EVLSTK_TO_RT                                ;; pop cell-stack into RT (discarding RT)
          POP_CELL_EVLSTK_TO_RA

          PUSH_XA_TO_EVLSTK                               ;; push value into RT, pushing RT onto the call frame cell stack if not empty
          ;; vm_cell_stack_push_rt_if_nonempty
          PUSH_RT_TO_EVLSTK                         ;; push RT onto call frame cell stack

          ;; WRITE_ARR_ATa_RA_TO_RT
          PUSH_ARR_ATa_RA_TO_EVLSTK

          ;; POP_EVLSTK_TO_ARR_ATa_RA
          ;; POP_EVLSTK_TO_ARR_ATa_RA__CHECK_BOUNDS
          ;; WRITE_RT_TO_ARR_ATa_RA__CHECK_BOUNDS
          WRITE_RT_TO_ARR_ATa_RA             ;; write RT into array in RA at index A (GC previous slot entry, if applicable)

          ;; WRITE_INTm1_TO_RT
          ;; WRITE_INT1_TO_RT
          ;; WRITE_INT0_TO_RT
          ;; WRITE_INT_A_TO_RT
          WRITE_INT_AY_TO_RT                             ;; int in A(lowbyte)/Y(highbyte), x=0 -> RT, x=2 -> RA

          WRITE_NIL_TO_RP
          WRITE_NIL_TO_RT

          ;; WRITE_CELLPAIR_RT_CELL1_TO_RT
          ;; WRITE_CELLPAIR_RT_CELL0_TO_RT
          WRITE_CELLPAIR_RT_CELLy_TO_RT                            ;; write CELLy (y=0 cell0, y=2 cell1) pointed to by RT into RT
          WRITE_CELLPAIR_RP_CELLy_TO_RP

          WRITE_RP_TO_CELLy_CELLPAIR_RT

          WRITE_CELLPAIR_RT_CELLy_TO_RP
          WRITE_RT_TO_CELLy_CELLPAIR_RP

          CP_RT_TO_RA
          CP_RT_TO_RB
          CP_RA_TO_RT
          CP_RA_TO_RZ
          CP_RB_TO_RZ
          CP_RC_TO_RZ
          CP_RT_TO_RZ
          CP_RT_TO_RP
          CP_RZ_TO_RT

          SWAP_ZP_WORD
          CP_RA_TO_RB
          SWAP_RA_RB

          POP_CELL_EVLSTK_TO_CELLy_RT                           ;; POP the cell-stack top into CELLy (y=0 cell0, y=2 cell1) pointed to by RT, reducing the stack size by 1, keeping rt as tos

          DEC_REFCNT_RZ
          FREE_CELL_RZ ;; includes FREE_CELL_RT and _RA
          GC_INCR_ARRAY_SLOT_RZ
          GC_CELL_ARRAYS
          GC_CELLS
          GC_ALL
          DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A
          PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ
          ADD_M1_SLOT_RZ_TO_PFL
          FREE_CELLARR_RZ
          FREE_M1_SLOT_RZ


    (list (label END__MEMORY_MANAGER))
          ;; ---------------------------------------- registers and maps
          (list (org #xcec0))
          VM_INITIAL_MM_REGS
          (list (org #xcf00))
          VM_PAGE_SLOT_DATA))

(module+ test #| vm-memory-manager |#
  (inform-check-equal? (foldl + 0 (map command-len (flatten vm-memory-manager)))
                       1815))

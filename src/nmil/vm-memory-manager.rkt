#lang racket/base
;; [[pdfview:~/Downloads/Small memory software patterns for limited memory systems.2001.pdf::261++0.00][Small memory software patterns for limited memory systems.2001.pdf: Page 261]]

#|

implementation of basic memory primitives for the native 6510 assembler implementation of mil.

primitives are e.g. allocation of cell-pair(s)
evalation stack operations
call frame primitives etc.

|#

;; see also vm-memory-manager.org

(require (only-in racket/format ~a))
(require (only-in racket/list flatten take empty? drop make-list))

(require "../6510.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader))
(require (only-in "../ast/6510-calc-opcode-facades.rkt" LDA-immediate))
(require (only-in "../util.rkt" bytes->int low-byte high-byte format-hex-byte format-hex-word))
(require (only-in "../ast/6510-relocator.rkt" command-len))
(require (only-in "../ast/6510-resolver.rkt"
                  add-label-suffix
                  replace-labels))
(require (only-in "../tools/6510-interpreter.rkt"
                  peek-word-at-address
                  cpu-state-clock-cycles
                  6510-load
                  6510-load-multiple
                  initialize-cpu
                  run-interpreter
                  run-interpreter-on
                  memory-list
                  cpu-state-accumulator
                  peek))
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
                  CP_RA_TO_RZ
                  CP_RT_TO_RA
                  CP_RT_TO_RP
                  CP_RT_TO_RZ
                  CP_RZ_TO_RT))
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
                  WRITE_RP_TO_CELLy_CELLPAIR_RT
                  WRITE_CELLPAIR_RT_CELLy_TO_RP
                  INC_REFCNT_CELLPAIR_RT
                  DEC_REFCNT_CELLPAIR_RZ
                  FREE_CELLPAIR_RZ))

(provide vm-memory-manager
         POP_CELL_EVLSTK_TO_RT
         POP_CELL_EVLSTK_TO_RA

         ALLOC_CELLARR_TO_RA
         GC_CELLPAIR_FREE_LIST


          INIT_CELL_PAGE_X_TO_AX                                  ;; initialize page A for ref counted cells
          ;; INIT_CELLSTACK_PAGE_A                             ;; initialize page A to previous cell stack page (X)
          INIT_CELLSTACK_PAGE_X
          ;; ---------------------------------------- alloc/free cells, pairs, slots
          ALLOC_CELL_TO_RT

          ;; GC_CELLPAIR_FREE_LIST                     ;; reclaim all cell-pairs in the queue of free cells

          ALLOC_CELLARR_TO_RA                          ;; allocate an array of cells (also useful for structures)

          ;; ---------------------------------------- refcount
          ;; DEC_REFCNT_RT                                ;; generic decrement of refcount (dispatches depending on type)
          INC_REFCNT_RT                                ;; generic increment of refcount (dispatches depending on type)

          ;; DEC_REFCNT_CELLPAIR_RT                 ;; decrement refcount, calling vm_free_cell_pair_in_zp_ptr if dropping to 0
          ;; DEC_REFCNT_CELL_RT                      ;; decrement refcount, calling vm_free_cell_in_zp_ptr if dropping to 0

          ;; INC_REFCNT_CELLPAIR_RT                 ;; increment refcount of cell-pair
          INC_REFCNT_CELL_RT                      ;; increment refcount of the cell, rt is pointing to

          ;; DEC_REFCNT_RA                                ;; generic decrement of refcount (dispatches depending on type)
          ;; DEC_REFCNT_CELLPAIR_RA                 ;; decrement refcount, calling vm_free_cell_pair_in_zp_ptr if dropping to 0
          ;; DEC_REFCNT_CELL_RA                      ;; decrement refcount, calling vm_free_cell_in_zp_ptr if dropping to 0
          ;; ---------------------------------------- call frame

          ;; ---------------------------------------- misc

          ;; VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS                ;; remove full pages in the free list of pages of the same type as are currently in ZP_PTR2
          ;; VM_ENQUEUE_PAGE_AS_HEAD_FOR_PTR2_SLOTS             ;; put this page as head of the page free list for slots of type as in ZP_PTR2

          ;; VM_GC_ARRAY_SLOT_PTR                               ;; execute garbage collection on a cell array (decr-ref all array elements and collect if 0)

          FREE_RT                                 ;; (includes FREE_RZ and _RA) free pointer (is cell-ptr, cell-pair-ptr, m1-slot-ptr, native-array, cell-array)

          ;; ---------------------------------------- CELL_STACK / RT / RA
          POP_CELL_EVLSTK_TO_RT                                ;; pop cell-stack into RT (discarding RT)

          PUSH_TO_EVLSTK                               ;; push value into RT, pushing RT onto the call frame cell stack if not empty
          ;; vm_cell_stack_push_rt_if_nonempty
          PUSH_RT_TO_EVLSTK                         ;; push RT onto call frame cell stack

          ;; WRITE_NIL_TO_RP
          ;; WRITE_NIL_TO_RT

          ;; WRITE_RT_TO_CELLy_CELLPAIR_RA                            ;; write RT cell into CELLy (y=0 cell0, y=2 cell1) pointer to by RA
          WRITE_RT_TO_CELLy_CELLPAIR_RP

          POP_CELL_EVLSTK_TO_CELLy_RT                           ;; POP the cell-stack top into CELLy (y=0 cell0, y=2 cell1) pointed to by RT, reducing the stack size by 1, keeping rt as tos
)

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in "./vm-inspector-utils.rkt"
                    vm-cell-at-nil?
                    vm-rega->string
                    vm-regt->string
                    vm-regp->string
                    vm-deref-cell-pair-w->string
                    vm-stack->strings
                    vm-page->strings
                    vm-refcount-cell-pair-ptr
                    vm-refcount-cell-ptr
                    vm-cell-pair-free-tree->string
                    vm-deref-cell-w->string))
  (require (only-in "./vm-memory-manager-test-utils.rkt"
                    run-code-in-test-on-code
                    remove-labels-for
                    wrap-code-for-test
                    list-with-label-suffix
                    calls-to-mock
                    compact-run-code-in-test-
                    ))

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

;; write the cell in RT into the CELL Y (0|2) of the cell-pair referenced in RP
;; input:  Y, RT, RP
;; usage:  A, Y
;; output: RP@Y <- RT    Y=0: RP -> [RT][...],  Y=2: RP -> [...][RT]
;; funcs:  -
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
    (compact-run-code-in-test
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


;; write the TOS of the EVLSTK (not RT) into CELL Y of cell-pair referenced by RT
;; keep RT and pop TOS of EVLSTK ( RT+EVLSTK  -> RT+<<EVLSTK<<, and (Y=0) RT -> [<<EVLSTK][...], or (Y=1) RT -> [...][<<EVLSTK]
;; no inc/dec refcnt needs to take place, since # references to RT nor the popped TOS of EVLSTK do change
;; input:  cell-stack (TOS)
;;         RT (must be a cell-pair ptr
;;         y = (0 = cell0, 2 = cell1)
;; usage:  A, Y
;; output: cell-stack (one value less)
;;         cell0 of RA is set
;; funcs:  -
(define POP_CELL_EVLSTK_TO_CELLy_RT
  (add-label-suffix
   "__" "__POP_CELL_EVLSTK_TO_CELLy_RT"
  (list
   (label POP_CELL_EVLSTK_TO_CELL1_RT)
          (LDY !$03)
          (BNE Y_ON_HIGHBYTE__)

   (label POP_CELL_EVLSTK_TO_CELL0_RT)
          (LDY !$00)

   ;; ----------------------------------------
   (label POP_CELL_EVLSTK_TO_CELLy_RT)
          (INY)
   (label Y_ON_HIGHBYTE__)
          (STY ZP_TEMP)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (TAX)
          (LDA (ZP_CELL_STACK_HB_PTR),y)
          (LDY ZP_TEMP)
          (STA (ZP_RT),y)
          (DEY)
          (TXA)
          (STA (ZP_RT),y)
          (DEC ZP_CELL_STACK_TOS)
          (RTS))))

(module+ test #| vm-pop-fstos-to-celly-rt |#
  (define vm-pop-fstos-to-celly-rt-state
    (compact-run-code-in-test
     (JSR PUSH_INT_1_TO_EVLSTK)
     (JSR PUSH_INT_m1_TO_EVLSTK)
     (JSR PUSH_INT_1_TO_EVLSTK)
     (JSR ALLOC_CELLPAIR_TO_RT)
     (LDY !$00)
     (JSR POP_CELL_EVLSTK_TO_CELLy_RT)
     (LDY !$02)
     (JSR POP_CELL_EVLSTK_TO_CELLy_RT)))

  (check-equal? (vm-stack->strings vm-pop-fstos-to-celly-rt-state)
                (list "stack holds 1 item"
                      (format  "pair-ptr[0] $~a05  (rt)" (format-hex-byte PAGE_AVAIL_0))))
  (check-equal? (vm-deref-cell-pair-w->string vm-pop-fstos-to-celly-rt-state (+ PAGE_AVAIL_0_W #x05))
                "(int $1fff . int $0001)"))


;; push rt onto the evlstack, no dec/inc refcnt is done!
;; allocate new evlstk page if necessary
;; input:  RT+EVLSTK
;; usage:  A, X, Y
;; output: RT +(EVLSTK << RT)
;; funcs:
;;   ALLOC_PAGE_TO_X
;;   INIT_CELLSTACK_PAGE_X
;; CHECK STACK PAGE OVERFLOW
(define PUSH_RT_TO_EVLSTK
  (add-label-suffix
   "__" "__PUSH_RT_TO_EVLSTK"
  (list
   (label PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
          (LDY ZP_RT)
          ;; if RT empty?  = $00 
          (BEQ DONE__)        ;; then no push

   ;; ----------------------------------------
   (label PUSH_RT_TO_EVLSTK)
          (LDY ZP_CELL_STACK_TOS)
          (INY)
          [BNE NO_ERROR__]

   (label ALLOCATE_NEW_STACK_PAGE__)
          (JSR ALLOC_PAGE_TO_X)
          (LDA ZP_CELL_STACK_LB_PTR+1)
          (JSR INIT_CELLSTACK_PAGE_X)
          (STX ZP_CELL_STACK_LB_PTR+1)

          (JSR ALLOC_PAGE_TO_X)
          (LDA ZP_CELL_STACK_HB_PTR+1)
          (JSR INIT_CELLSTACK_PAGE_X)
          (STX ZP_CELL_STACK_HB_PTR+1)

          (LDY !$02)                          ;; new tos starts 

   (label NO_ERROR__)
          (LDA ZP_RT+1)
          (STA (ZP_CELL_STACK_HB_PTR),y)      ;; write high byte! 
          (LDA ZP_RT)
          (STA (ZP_CELL_STACK_LB_PTR),y)      ;; write low byte 
          (STY ZP_CELL_STACK_TOS)             ;; set new tos

   (label DONE__)
          (RTS))))

(module+ test #| vm-cell-stack-just-push-rt |#
  (define vm-cell-stack-just-push-rt-state
    (compact-run-code-in-test
     (JSR WRITE_INTm1_TO_RT)
     (JSR PUSH_RT_TO_EVLSTK)))

  (check-equal? (vm-stack->strings vm-cell-stack-just-push-rt-state)
                (list "stack holds 2 items"
                      "int $1fff  (rt)"
                      "int $1fff")))

;; push an atomic cell onto the stack (that is push the RegT, if filled, and write the value into RegT)
;; input: call-frame stack, RT
;;        A = high byte,
;;        X = tagged low
;; output: call-frame stack, RT
(define PUSH_TO_EVLSTK
  (list

   ;; ints are saved high byte first, then low byte !!!!
   ;; X = high byte of int (max 31 = $1f) (stored in low byte (tagged) position)
   ;; A = low byte of int (0..255) (stored in high byte (untagged) position)
   (label PUSH_INT_TO_EVLSTK)         ;; idea: can be optimized since it is known that this is an atomic value
          (TAY)
          (TXA)
          (ASL A)
          (ASL A)
          (ORA !$03)           ;; mask in lower two bits
          (AND !$7f)           ;; mask out top bit
          (TAX)
          (TYA)
          (JMP PUSH_TO_EVLSTK)

   (label PUSH_INT_m1_TO_EVLSTK)
          (LDA !$ff) ;; 1f << 2
          (LDX !$7f)
          (BNE PUSH_TO_EVLSTK)

   (label PUSH_INT_2_TO_EVLSTK)
          (LDA !$02)
          (LDX !$03)
          (BNE PUSH_TO_EVLSTK)

   (label PUSH_INT_1_TO_EVLSTK)
          (LDA !$01)
          (LDX !$03)
          (BNE PUSH_TO_EVLSTK)

   (label PUSH_INT_0_TO_EVLSTK)
          (LDA !$00)
          (LDX !$03)
          (BNE PUSH_TO_EVLSTK)

   ;; push NIL (cell-pair-ptr)           ;; idea: can be optimized since it is known that this is cell-pair-ptr
   (label PUSH_NIL_TO_EVLSTK)
          (LDX !<TAGGED_NIL)
          (LDA !>TAGGED_NIL)

   ;; push a cell
   ;; A = high byte
   ;; X = tagged low byte
   (label PUSH_TO_EVLSTK)
          (PHA)
          (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY) ;; uses A and Y
          (PLA)

   (label VM_WRITE_AX_TO_RT)
          (STX ZP_RT)          
          (STA ZP_RT+1)
          (RTS)))

(module+ test #| vm_cell_stack_push_r (basically on write into rt, since stack is completely empty) |#
  (define vm_cell_stack_push_r_int0_state
    (compact-run-code-in-test (JSR PUSH_INT_0_TO_EVLSTK)))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_int0_state)
                "int $0000")
  (check-equal? (memory-list vm_cell_stack_push_r_int0_state ZP_RT (add1 ZP_RT))
                (list #x03 #x00))

  (define vm_cell_stack_push_r_int1_state
    (compact-run-code-in-test (JSR PUSH_INT_1_TO_EVLSTK)))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_int1_state)
                "int $0001")
  (check-equal? (memory-list vm_cell_stack_push_r_int1_state ZP_RT (add1 ZP_RT))
                (list #x03 #x01))

  (define vm_cell_stack_push_r_intm1_state
    (compact-run-code-in-test (JSR PUSH_INT_m1_TO_EVLSTK)))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_intm1_state)
                "int $1fff")
  (check-equal? (memory-list vm_cell_stack_push_r_intm1_state ZP_RT (add1 ZP_RT))
                (list #x7f #xff))

  (define vm_cell_stack_push_r_nil_state
    (compact-run-code-in-test (JSR PUSH_NIL_TO_EVLSTK)))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_nil_state)
                "pair-ptr NIL")
  (check-equal? (memory-list vm_cell_stack_push_r_nil_state ZP_RT (add1 ZP_RT))
                (list #x01 #x00))

  (define vm_cell_stack_push_r_cell_ptr_state
    (compact-run-code-in-test
     (LDX !$03)
     (LDA !$ce)
     (JSR PUSH_TO_EVLSTK)))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_cell_ptr_state)
                "int $00ce")
  (check-equal? (memory-list vm_cell_stack_push_r_cell_ptr_state ZP_RT (add1 ZP_RT))
                (list #x03 #xce))

  (define vm_cell_stack_push_r_cell_pair_ptr_state
    (compact-run-code-in-test
     (LDX !$05)
     (LDA !$ce)
     (JSR PUSH_TO_EVLSTK)))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_cell_pair_ptr_state)
                "pair-ptr[0] $ce05")
  (check-equal? (memory-list vm_cell_stack_push_r_cell_pair_ptr_state ZP_RT (add1 ZP_RT))
                (list #x05 #xce)))

(module+ test #| vm_cell_stack_push_r (push rt, and write rt) |#
  (define vm_cell_stack_push_r_push1_state
    (compact-run-code-in-test
     (JSR PUSH_INT_m1_TO_EVLSTK)
     (JSR PUSH_INT_1_TO_EVLSTK)))

  (check-equal? (vm-stack->strings vm_cell_stack_push_r_push1_state)
                (list "stack holds 2 items"
                      "int $0001  (rt)"
                      "int $1fff"))

  (check-equal? (memory-list vm_cell_stack_push_r_push1_state ZP_RT (add1 ZP_RT))
                (list #x03 #x01))

  (define vm_cell_stack_push_r_push2_state
    (compact-run-code-in-test
     (JSR PUSH_INT_m1_TO_EVLSTK)
     (JSR PUSH_INT_1_TO_EVLSTK)
     (JSR PUSH_NIL_TO_EVLSTK)))

  (check-equal? (vm-stack->strings vm_cell_stack_push_r_push2_state)
                (list "stack holds 3 items"
                      "pair-ptr NIL  (rt)"
                      "int $0001"
                      "int $1fff"))

  (check-equal? (memory-list vm_cell_stack_push_r_push2_state ZP_RT (add1 ZP_RT))
                (list #x01 #x00)))

;; pop cell from stack (that is, discard RegT, move tos of call-frame stack into RegT (if available))
;; input: call-frame stack, RT
;; output: call-frame stack reduced by`1, RT <- popped value
;; NO GC CHECKS!
;; pop cell from stack (that is, discard RegT, move tos of call-frame stack into RegT (if available))
;; input: call-frame stack, RT
;; output: call-frame stack reduced by`1, RT <- popped value
;; NO GC CHECKS!
(define POP_CELL_EVLSTK_TO_RA
  (list
   (label POP_CELL_EVLSTK_TO_RA)
          ;; optional: stack marked empty? => error: cannot pop from empty stack!
          ;; (LDY !$00)
          ;; (BEQ ERROR_NO_VALUE_ON_STACK)

          ;; is call-frame stack empty? => mark stack as empty and return | alternatively simply write NIL into RT
          (LDY ZP_CELL_STACK_TOS)
          (CPY !$01) ;; stack empty?
          (BEQ WRITE_00_TO_RA) ;; which effectively clears the RT
          ;; pop value from call-frame stack into RT!
          (LDA (ZP_CELL_STACK_LB_PTR),y) ;; tagged low byte
          (STA ZP_RA)


          ;; (optional) quick check for atomic cells [speeds up popping atomic cells, slows popping cell-ptr, slight slows popping cell-pair-ptr
          ;; (AND !$03)
          ;; (BEQ WRITE_TOS_TO_RA__POP_CELL_EVLSTK_TO_RA)
          ;; (TXA)

          (LDA (ZP_CELL_STACK_HB_PTR),y) ;; high byte
          (STA ZP_RA+1)
          (DEC ZP_CELL_STACK_TOS)
          (RTS)

   (label WRITE_00_TO_RA)
          ;; mark RA as empty
          (LDA !$00)
          (STA ZP_RA)
          (STA ZP_RA+1)
          (RTS)))

(define POP_CELL_EVLSTK_TO_RT
  (list
   (label POP_CELL_EVLSTK_TO_RT)
          ;; optional: stack marked empty? => error: cannot pop from empty stack!
          ;; (LDY !$00)
          ;; (BEQ ERROR_NO_VALUE_ON_STACK)

          ;; is call-frame stack empty? => mark stack as empty and return | alternatively simply write NIL into RT
          (LDY ZP_CELL_STACK_TOS)
          (CPY !$01) ;; stack empty?
          (BEQ WRITE_00_TO_RT) ;; which effectively clears the RT
          ;; pop value from call-frame stack into RT!
          (LDA (ZP_CELL_STACK_LB_PTR),y) ;; tagged low byte
          (STA ZP_RT)


          ;; (optional) quick check for atomic cells [speeds up popping atomic cells, slows popping cell-ptr, slight slows popping cell-pair-ptr
          ;; (AND !$03)
          ;; (BEQ WRITE_TOS_TO_RT__POP_CELL_EVLSTK_TO_RT)
          ;; (TXA)

          (LDA (ZP_CELL_STACK_HB_PTR),y) ;; high byte
          (STA ZP_RT+1) 
          (DEC ZP_CELL_STACK_TOS)
          (RTS)

   (label WRITE_00_TO_RT)
          ;; mark RT as empty
          (LDA !$00)
          (STA ZP_RT)
          (STA ZP_RT+1)
          (RTS)))

(module+ test #| vm_cell_stack_pop_r (just one value) |#
  (define vm_cell_stack_pop3_r_state
    (compact-run-code-in-test
     (JSR PUSH_INT_1_TO_EVLSTK)
     (JSR PUSH_INT_m1_TO_EVLSTK)
     (JSR PUSH_INT_0_TO_EVLSTK)
     (JSR POP_CELL_EVLSTK_TO_RT)))

  (check-equal? (vm-stack->strings vm_cell_stack_pop3_r_state)
                (list "stack holds 2 items"
                      "int $1fff  (rt)"
                      "int $0001"))

  (check-equal? (memory-list vm_cell_stack_pop3_r_state ZP_RT (add1 ZP_RT))
                (list #x7f #xff))

  (define vm_cell_stack_pop2_r_state
    (compact-run-code-in-test
     (JSR PUSH_INT_1_TO_EVLSTK)
     (JSR PUSH_INT_m1_TO_EVLSTK)
     (JSR PUSH_INT_0_TO_EVLSTK)
     (JSR POP_CELL_EVLSTK_TO_RT)
     (JSR POP_CELL_EVLSTK_TO_RT)))

  (check-equal? (vm-stack->strings vm_cell_stack_pop2_r_state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define vm_cell_stack_pop1_r_state
    (compact-run-code-in-test
     (JSR PUSH_INT_1_TO_EVLSTK)
     (JSR PUSH_INT_m1_TO_EVLSTK)
     (JSR PUSH_INT_0_TO_EVLSTK)
     (JSR POP_CELL_EVLSTK_TO_RT)
     (JSR POP_CELL_EVLSTK_TO_RT)
     (JSR POP_CELL_EVLSTK_TO_RT)))

  (check-equal? (vm-stack->strings vm_cell_stack_pop1_r_state)
                (list "stack is empty"))

  (check-equal? (memory-list vm_cell_stack_pop1_r_state ZP_RT (add1 ZP_RT))
                (list #x00 #x00)))

(module+ test #| vm_cell_stack_push_nil_r |#
  (define test-vm_cell_stack_push_nil-a-state-after
    (compact-run-code-in-test
      (JSR PUSH_NIL_TO_EVLSTK)))

  (check-equal? (vm-regt->string test-vm_cell_stack_push_nil-a-state-after)
                "pair-ptr NIL")

  (define test-vm_cell_stack_push_nil-b-state-after
    (compact-run-code-in-test
      (JSR PUSH_NIL_TO_EVLSTK) ;; 1
      (JSR PUSH_NIL_TO_EVLSTK) ;;
      (JSR PUSH_NIL_TO_EVLSTK) ;; 3
      (JSR PUSH_NIL_TO_EVLSTK) ;;
      (JSR PUSH_NIL_TO_EVLSTK) ;; 5
      (JSR PUSH_NIL_TO_EVLSTK) ;;
      (JSR PUSH_NIL_TO_EVLSTK) ;; 7
      (JSR PUSH_NIL_TO_EVLSTK))) ;; 8

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_nil-b-state-after)
                '("stack holds 8 items"
                  "pair-ptr NIL  (rt)"
                  "pair-ptr NIL"
                  "pair-ptr NIL"
                  "pair-ptr NIL"
                  "pair-ptr NIL"
                  "pair-ptr NIL"
                  "pair-ptr NIL"
                  "pair-ptr NIL")))

(module+ test #| vm_cell_push_int_r |#
  (define test-vm_cell_stack_push_int-a-state-after
    (compact-run-code-in-test
      (JSR PUSH_INT_m1_TO_EVLSTK)
      (LDA !$00) ;; -4096
      (LDX !$10)
      (JSR PUSH_INT_TO_EVLSTK)
      (JSR PUSH_INT_1_TO_EVLSTK)
      (JSR PUSH_INT_0_TO_EVLSTK)
      (LDA !$ff) ;; 4095
      (LDX !$0f)
      (JSR PUSH_INT_TO_EVLSTK)))

  (check-equal? (vm-regt->string test-vm_cell_stack_push_int-a-state-after)
                "int $0fff")
  (check-equal? (vm-stack->strings test-vm_cell_stack_push_int-a-state-after)
                '("stack holds 5 items"
                  "int $0fff  (rt)"
                  "int $0000"
                  "int $0001"
                  "int $1000"
                  "int $1fff")))


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
    (compact-run-code-in-test
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

;; cell stack page(s)
;; offset  content
;; ---------------
;; 00      #b0001 1011
;; 01      previous page (of the stack)
;; 02..ff  payload (either lowbyte or highbyte of the cell)
;;
;; input:  A old stack page
;;         X new stack page
;; output: X new stack page
;; uses:   A, X, Y
;;         ZP_TEMP, ZP_TEMP+1
(define INIT_CELLSTACK_PAGE_X
  (list
   (label INIT_CELLSTACK_PAGE_X)
          (STX ZP_TEMP+1)         ;; write page into hightbyte of ZP_TEMP ptr
          (TAX)                   ;; old page in a -> x
          (LDA !$00)
          (STA ZP_TEMP)           ;; write 0 into lowbyte of ZP_TEMP ptr
          (TAY)                   ;; init y with 0
          (LDA !$1b)              ;; id for page type: cellstack
          (STA (ZP_TEMP),y)       ;; first byte on page: page type: cellstack
          (INY)
          (TXA)                   ;; x (old stack page) -> a
          (STA (ZP_TEMP),y)       ;; second byte on page: previous stack page
          (LDX ZP_TEMP+1)         ;; restore A with new page
          (RTS)))

(module+ test #| alloc cell stack pages |#
  (define alloc-cell-stack-pages-state
    (compact-run-code-in-test
     (JSR ALLOC_PAGE_TO_X)
     (LDA !$05)
     (JSR INIT_CELLSTACK_PAGE_X)
     (STX ZP_RT+1)

     (JSR ALLOC_PAGE_TO_X)
     (LDA !$03)
     (JSR INIT_CELLSTACK_PAGE_X)
     (STX ZP_RT)))

  (check-equal? (memory-list alloc-cell-stack-pages-state ZP_RT (add1 ZP_RT))
                (list PAGE_AVAIL_1 PAGE_AVAIL_0)
                ".. is new low byte page, .. is new high byte page")
  (check-equal? (memory-list alloc-cell-stack-pages-state PAGE_AVAIL_1_W (add1 PAGE_AVAIL_1_W))
                (list #x1b #x03)
                "new low byte page is initialized with cell-stack page type and 03")
  (check-equal? (memory-list alloc-cell-stack-pages-state PAGE_AVAIL_0_W (add1 PAGE_AVAIL_0_W))
                (list #x1b #x05)
                "new low byte page is initialized with cell-stack page type and 05"))


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

;; find out what kind of cell zp_rt points to,
;; then call the right decrement refcounts function
;; input:  ZP_RT
;; output: the right refcount is decremented
;;         (in case of m1 pages, @ZP_RT-1)
;;         (in case of cell pages @ZP_RT>>1)
;;         (in case of cell-pair pages @ZP_RT>>2)
(define INC_REFCNT_CELLARR_RT #t)
(define INC_REFCNT_NATIVEARR_RT #t)
(define INC_REFCNT_CELL_RT #t)
;; (define INC_REFCNT_CELLPAIR_RT #t)
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
           "IS_M1_PAGE__"
           "IS_M1_PAGE__"
           "IS_M1_PAGE__")

   (label UNKNOWN__)
          ;; unknown object type (or atomic value that cannot be ref counted and MUST NOT END UP in ZP_RT)
   (label DONE__)
          (RTS)

   INC_REFCNT_CELLPAIR_RT

   (label INC_REFCNT_CELL_RT)
          ;; find out which page type is used (cell-ptr-page, m1-page, slot-page)
          (LDA ZP_RT+1) ;; highbyte (page)
          (BEQ DONE__) ;; page=0 => empty, nothing to be done
          (STA LOAD_PAGE_TYPE__CELL__+2)
   (label LOAD_PAGE_TYPE__CELL__)
          (LDA $c000) ;; c0 is overwritten by page
          (BMI IS_CELL_PAGE__)
          (AND !$ec)
          (BEQ IS_M1_PAGE__)

          (BRK) ;; unhandled page type

   (label INC_REFCNT_CELLARR_RT)
   (label INC_REFCNT_NATIVEARR_RT)
   (label IS_M1_PAGE__)
          (LDX ZP_RT)
          (DEX)
          (BNE NOW_INCREMENT_REFCNT__CELL__) ;; is never 0! for m1 pages

   (label IS_CELL_PAGE__)
          (LDA ZP_RT) ;; lowbyte (offset)
          (LSR)
          (TAX)

   (label NOW_INCREMENT_REFCNT__CELL__)
          (LDA ZP_RT+1)
          (STA INC_PAGE_REFCNT_CELL__+2) ;; store high byte (page) into inc-command high-byte (thus +2 on the label)
   (label INC_PAGE_REFCNT_CELL__)
          (INC $c000,x) ;; c0 is overwritten by page (see above)
          (RTS)))))

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
  (define vm-refcount-mmcr-rt--cell-pair-ptr-state
    (compact-run-code-in-test
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR INC_REFCNT_CELLPAIR_RT)
     (JSR INC_REFCNT_CELLPAIR_RT)))

  (check-equal? (vm-regt->string vm-refcount-mmcr-rt--cell-pair-ptr-state)
                (format "pair-ptr[2] $~a05" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-refcount-cell-pair-ptr vm-refcount-mmcr-rt--cell-pair-ptr-state (+ PAGE_AVAIL_0_W #x05))
                2)

  (define vm-refcount-mmcr-rt--cell-pair-ptr-state2
    (compact-run-code-in-test
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR INC_REFCNT_CELLPAIR_RT)
     (JSR INC_REFCNT_CELLPAIR_RT)
     (JSR DEC_REFCNT_CELLPAIR_RT)))

  (check-equal? (vm-regt->string vm-refcount-mmcr-rt--cell-pair-ptr-state2)
                (format "pair-ptr[1] $~a05" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-refcount-cell-pair-ptr vm-refcount-mmcr-rt--cell-pair-ptr-state2 (+ PAGE_AVAIL_0_W #x05))
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
    (compact-run-code-in-test
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
    (compact-run-code-in-test
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
    (compact-run-code-in-test
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
    (compact-run-code-in-test
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
    (compact-run-code-in-test
     (JSR ALLOC_CELL_TO_RT)))

  (check-equal? (memory-list test-alloc-cell-to-rt-state-after GLOBAL_CELL_FREE_LIST GLOBAL_CELL_FREE_LIST)
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
    (compact-run-code-in-test
     (JSR ALLOC_CELL_TO_RT)
     (JSR ALLOC_CELL_TO_RT)))

  (check-equal? (memory-list test-alloc-cell-to-rt-twice-state-after GLOBAL_CELL_FREE_LIST GLOBAL_CELL_FREE_LIST)
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
    (compact-run-code-in-test
     (JSR ALLOC_CELL_TO_RT)
     (JSR CP_RT_TO_RA)

     (JSR ALLOC_CELL_TO_RT)
     (JSR FREE_CELL_RA)))

  (check-equal? (memory-list test-alloc-cell-to-rt-twicenfree-state-after GLOBAL_CELL_FREE_LIST (add1 GLOBAL_CELL_FREE_LIST))
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
    (compact-run-code-in-test
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

  (check-equal? (memory-list test-alloc-cell-to-rt-twicenfreenalloc-state-after GLOBAL_CELL_FREE_LIST GLOBAL_CELL_FREE_LIST)
                (list #x00) ;; lowbyte is zero => it is initial (high byte is not heeded in that case)
                "free cell list is initial again"))

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

;; impl complete, test missing

;; decrement the refcount (if a pointer) in RZ, call respective free if refcount drops to 0
;; input:  RZ (RA, RT)
;; usage:  A, X, Y, RZ
;; output:
;; funcs:
;;   FREE_M1_SLOT_RZ >>
;;   FREE_CELL_RZ >>
;;   FREE_CELLPAIR_RZ >>
(define DEC_REFCNT_CELL_RZ #t)
(define DEC_REFCNT_CELL_RT #t)
(define DEC_REFCNT_CELL_RA #t)
;; (define DEC_REFCNT_CELLPAIR_RT #t)
;; (define DEC_REFCNT_CELLPAIR_RA #t)
;; (define DEC_REFCNT_CELLPAIR_RZ #t)
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
(define DEC_REFCNT_RZ
  (add-label-suffix
   "__" "__DEC_REFCNT_RZ"
   (flatten
   (list
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


   DEC_REFCNT_CELLPAIR_RZ
   ;; (label DEC_REFCNT_CELLPAIR_RA)
   ;;        (JSR CP_RA_TO_RZ)
   ;;        (CLC)
   ;;        (BCC DEC_REFCNT_CELLPAIR_RZ)

   ;; (label DEC_REFCNT_CELLPAIR_RT)
   ;;        (JSR CP_RT_TO_RZ)

   ;; ;; input: cell-pair ptr in ZP_RA
   ;; ;; decrement ref count, if 0 deallocate
   ;; (label DEC_REFCNT_CELLPAIR_RZ)
   ;;        (LDA ZP_RZ)
   ;;        (LSR)
   ;;        (LSR)
   ;; (label CELLPAIR_ALREADY_LSRED__)
   ;;        (TAX)
   ;;        ;; now decrement cell count
   ;;        (LDA ZP_RZ+1)
   ;;        (BEQ DONE__) ;; nil -> done
   ;;        (STA DEC_PAGE_CELLPAIR_CNT__+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
   ;; (label DEC_PAGE_CELLPAIR_CNT__)
   ;;        (DEC $c000,x) ;; c0 is overwritten by page (see above)
   ;;        (BNE DONE__)
   ;;        (JMP FREE_CELLPAIR_RZ) ;; free (since refcnt dropped to 0)


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
   (label CELL_ALREADY_LSRED__)
          ;; check what cell kind the target is: cell, cell-ptr, cell-pair-ptr, native-array, cell-array
          (LDY ZP_RZ+1)
          (BEQ DONE__) ;; nil -> done
          (STY LDA_PAGE_TYPE__+2)
          (TAX)
   (label LDA_PAGE_TYPE__)
          (LDA $c000)
          (ASL A)
          (BCS DEC_REFCNT_CELL_RZ_TO_CELL__)
          ;; can't really be cellpair type page (was checked before)
          ;; (LSL)
          ;; (BCS DEC_REFCNT_CELL_RZ_TO_CELLPAIR)
          ;; else must be a m1 slot
   (label DEC_REFCNT_CELL_RZ_TO_M1_SLOT__)
          (LDX ZP_RZ)
          (DEX)
          (STY DEC_PAGE_M1_SLOT_CNT__+2)
   (label DEC_PAGE_M1_SLOT_CNT__)
          (DEC $C000,x)
          (BNE DONE__)
          (JMP FREE_M1_SLOT_RZ)

   (label DEC_REFCNT_CELL_RZ_TO_CELL__)
          (STY DEC_PAGE_CELL_CNT__+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
   (label DEC_PAGE_CELL_CNT__)
          (DEC $c000,x)               ;; c0 is overwritten by page (see above), x = position of refcount (a >> 1)
          (BNE DONE__)
          (JMP FREE_CELL_RZ)      ;; free (since refcnt dropped to 0), and this is definitely a cell-ptr => free-cell can be called
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

;; impl complete, test missing
;; free the m1 slot referenced by RZ
;; input: RZ
;; usage: A, X, Y, RZ
;; output:
;; funcs:
;;   GC_INCR_ARRAY_SLOT_RZ >>
;;   ADD_M1_SLOT_RZ_TO_PFL >>
(define FREE_M1_SLOT_RZ
  (add-label-suffix
   "__" "__NEW_FREE_M1_SLOT_RZ"
   (list
    (label FREE_M1_SLOT_RZ)
           (LDY !$00)
           (LDA (ZP_RZ),y)
           (CMP !TAG_BYTE_CELL_ARRAY)
           (BEQ FREE_CELLARR__)
           (CMP !TAG_BYTE_NATIVE_ARRAY)
           (BEQ FREE_NATIVEARR__)

    (label UNKNOWN__)
          ;; unknown object type (or atomic value that cannot be ref counted and MUST NOT END UP in ZP_RZ)
           (RTS)

    (label FREE_CELLARR__)
           (JMP GC_INCR_ARRAY_SLOT_RZ)

    (label FREE_NATIVEARR__)
           (JMP ADD_M1_SLOT_RZ_TO_PFL) ;; just add this slot to the free list of the respective page (and do some housekeeping)
)))

;; impl missing, test missing
(define FREE_CELLARR_RZ
  (add-label-suffix
   "__" "__NEW_FREE_CELLARR_RZ"
   (list
    (label FREE_CELLARR_RZ)
           (RTS))))

;; impl complete, test missing
;; add the given m1 slot in RZ back to the page free list of slots
;; input:  RZ, page-meta-data
;; usage:  A, X, Y, RZ
;; output: page-meta-data
;; funcs:
;;   DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_RZ
;;   PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ
(define ADD_M1_SLOT_RZ_TO_PFL
  (add-label-suffix
   "__" "__NEW_ADD_M1_SLOT_RZ_TO_PFL"
   (list
    (label ADD_M1_SLOT_RZ_TO_PFL)
           (JSR DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_RZ)

          ;; now free the slot
   (label REGULAR_FREE__)
          (LDX ZP_RZ+1)
          (STX DEC_CMD__+2)          ;; write page for later dec execution
          (LDA VM_PAGE_SLOT_DATA,x)  ;; first free slot offset
          (BNE CONTINUE__)           ;; regular free

          ;; this page was full (since next free slot was 0) => register with the list of pages with free slots
          (JSR PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ)
          (LDX DEC_CMD__+2)          ;; restore x
          (LDA !$00)                 ;; next free slot offset (=0)

   (label CONTINUE__)
          (LDY !$00)
          (STA (ZP_RZ),y)            ;; set (zp_ptr) = previous free
          (LDA ZP_RZ)                ;; low byte of pointer = new free slot
          (STA VM_PAGE_SLOT_DATA,x)  ;; set new first free slot offset

          (DEC ZP_RZ)                ;; now points to ref count
          (TYA)                      ;; y is still 0 => a := 0
          (STA (ZP_RZ),y)            ;; set refcount := 0

   (label DEC_CMD__)                 ;; decrement number of slots used on the page
          (DEC $c002)                ;; $c0 is overwritten, 02 = location of used slots on that page type

          (RTS))))

;; put this page to the head of free m1 pages of the same profile as RZ is
;; input:  RZ, GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; usage:  A, X, Y, RZ
;; output: GLOBAL_M1_PX_PAGE_FOR_ALLOC
;;         A = first free slot of that page
;; funcs:  -
(define PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ
  (add-label-suffix
   "__" "__NEW_PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ"
  (list
   (label PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ)
          (LDA ZP_RZ)
          (STA ZP_TEMP) ;; keep for later

          (LDA !$00)    ;; set to zero
          (STA ZP_RZ)

          (LDY !$01)
          (LDA (ZP_RZ),y) ;; get previous
          (BNE CONTINUE_WITH_RESTORE__)     ;; is != 0 => is still part of the list, don't change the list
          ;; is no longer part of the free list of pages, add this page at the head of the page

          (DEY) ;; now 0
          (LDA (ZP_RZ),y) ;; get encoded page type
          (AND !$07)

          (TAX) ;; now x = page type

          (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x)

          (INY) ;; now 1
          (STA (ZP_RZ),y) ;; set previous

          ;; x = page type, a = page
          (LDA ZP_RZ+1)
          (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x)
          (TAX)  ;; x = page

   (label CONTINUE_WITH_RESTORE__)
          (LDA ZP_TEMP)
          (STA ZP_RZ) ;; restore
          (LDA VM_PAGE_SLOT_DATA,x)           ;; first free slot offset

          (RTS))))

;; drop all full pages from the list of pages with available slots
;; input:  RZ, GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; usage:  A, X, Y
;; output: GLOBAL_M1_PX_PAGE_FOR_ALLOC of this profile holds page with free slots
;; funcs:  -
(define DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A
  (add-label-suffix
   "__" "NEW_DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A"
  (list
   (label DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_RZ)
          (LDA ZP_RZ+1)
   (label DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A)
          (STA READ_ENC_PAGE_TYPE__+2)
   (label READ_ENC_PAGE_TYPE__)
          (LDA $c000)
          (AND !$07)

          (TAX) ;; now x = page profile

   ;; input: x (unchanged)
   (label DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_PROFILE_X)
          (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x)

   (label LOOP__)
          (TAY) ;; y = page now
          (LDA VM_PAGE_SLOT_DATA,y)
          (BNE DONE__)                  ;; if there is a free slot on the given page (y), then we are done (no full page at head)

          ;; remove this page (in y) from list
          (STY LOAD_PREV_PAGE_CMD__+2)
          (STY STORE_PREV_PAGE_CMD__+2)
          (LDY !$00)
   (label LOAD_PREV_PAGE_CMD__)
          (LDA $c001) ;; $c0 is overwritten with page
          (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x) ;; optional optimization: needs only be done once! (is here done in a loop)
   (label STORE_PREV_PAGE_CMD__)
          (STY $c001) ;; $c0 is overwritten
          (BNE LOOP__) ;; if the current page (in a) is 0 (we are at the end of the list), we are done and can return, else loop

   (label DONE__)
          (RTS))))

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
           (CMP PREV_ARRAY_+1)
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
    (compact-run-code-in-test                   ;; GLOBAL_CELL_FREE_LIST = 0000
     (JSR ALLOC_CELL_TO_RT)                     ;; RT -> [cell 0 @ ..02]
     (JSR INC_REFCNT_CELL_RT)
     (JSR CP_RT_TO_RP)                          ;; RA -> [cell 0]
     (JSR ALLOC_CELL_TO_RT)                     ;; RT -> [cell 1 @ ..08]
     (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)        ;; RT -> [cell 1] -> [cell 0]

     (JSR FREE_CELL_RT)                     ;; GLOBAL_CELL_FREE_LIST -> [cell 0] -> [cell 1] -> 0000
     ;; #:debug #t
     ))                                         ;; PFL -> [cell 2 @ ..0a]

  (check-equal? (memory-list new-free-cell-ptr-in-rc-tailcall-state GLOBAL_CELL_FREE_LIST (add1 GLOBAL_CELL_FREE_LIST))
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
    (compact-run-code-in-test
     (JSR ALLOC_CELL_TO_RT)
     (JSR FREE_CELL_RT)))

  (check-equal? (memory-list new-free-cell-ptr-in-rt-state GLOBAL_CELL_FREE_LIST (add1 GLOBAL_CELL_FREE_LIST))
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
    (compact-run-code-in-test
     (JSR ALLOC_CELL_TO_RT)
     (JSR FREE_CELL_RT)
     (JSR ALLOC_CELL_TO_RT)))

  (check-equal? (memory-list new-free-cell-ptr-in-rt-realloc-state GLOBAL_CELL_FREE_LIST GLOBAL_CELL_FREE_LIST)
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
    (compact-run-code-in-test
     (JSR ALLOC_CELL_TO_RT)
     (JSR CP_RT_TO_RA)
     (JSR ALLOC_CELL_TO_RT)
     (JSR FREE_CELL_RT)        ;; free cc08
     (JSR CP_RA_TO_RT)
     (JSR FREE_CELL_RT)))      ;; then free cc02

  (check-equal? (memory-list new-free-cell-ptr-in-rt-2xfree-state GLOBAL_CELL_FREE_LIST (add1 GLOBAL_CELL_FREE_LIST))
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

(module+ test #| use case: allocate, free, reallocate small list of cell-pairs |#
  (define use-case-2-a-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)                     ;; rt = freshly allocated cell (cc05)
     (JSR INC_REFCNT_CELLPAIR_RT)               ;; ref(rt) ++ (=1)
     ;; set cdr to nil
     (JSR WRITE_NIL_TO_RP)
     (JSR WRITE_RP_TO_CELL1_CELLPAIR_RT)                          ;; (cdr rt) := nil
     ;; set car to int 0
     (JSR CP_RT_TO_RZ)
     (JSR WRITE_INT1_TO_RT)
     (JSR CP_RT_TO_RP)
     (JSR CP_RZ_TO_RT)
     (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)                          ;; (car rt) := int0

     (JSR CP_RT_TO_RP)                                   ;; ra := rt

     (JSR ALLOC_CELLPAIR_TO_RT)                     ;; rt = freshly allocated cell (cc09)
     (JSR INC_REFCNT_CELLPAIR_RT)               ;; ref(rt) ++ (=1)

     ;; set cdr
     (JSR WRITE_RP_TO_CELL1_CELLPAIR_RT)                          ;; (cdr rt) := ra
     (JSR CP_RT_TO_RZ)
     ;; set car to int0
     (JSR WRITE_INT0_TO_RT)
     (JSR CP_RT_TO_RP)
     (JSR CP_RZ_TO_RT)
     (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)                          ;; (car rt) := int0

     ;; now:
     ;;   rt[cc09|1] (int0 . ->[cc05|1](int0 . nil))
     ;; notation:
     ;;   [<mem-location>|<ref-count>]
     ;;   (<car-cell> . <cdr-cell>)
     ;;   intX, nil :: atomic value cells
     ;;   -> :: cell-ptr
     ))

  (define use-case-2-a-state-after
    (apply compact-run-code-in-test use-case-2-a-code))

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
    (apply compact-run-code-in-test use-case-2-b-code))

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
    (apply compact-run-code-in-test use-case-2-c-code))

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

;; ----------------------------------------
;; page type slot w/ different sizes (refcount @ ptr-1) x cells
;; math: first entry @FIRST_REF_COUNT_OFFSET__INIT_M1Px_PAGE_A + 1, refcount @ -1, next slot += INC_TO_NEXT_SLOT__INIT_M1Px_PAGE_A, slot-size = INC_TO_NEXT_SLOT__INIT_M1Px_PAGE_A -1
;; input : Y = profile offset (0, 2, 4 ...)
;;         X = page
;; uses  : A, X, Y, RZ
;; output: X = page, initialized as m1 page of profile y
;;         A = first free slot
;; funcs:  -
(define INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX
  (add-label-suffix
   "__" "INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX"
  (list
   (label INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX)
          (STY SEL_PROFILE__)      ;; save profile index in local var
          (TYA)                 ;; profile 0..4 -> a
          (STX ZP_RZ+1)

          (LDY !$00)
          (STY ZP_RZ)

          (TAX)
          (ORA !$10)
          (STA (ZP_RZ),y)       ;; set page type in byte 0 to b0001 <profile>

          (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x) ;; current free page
          (INY)
          (STA (ZP_RZ),y)          ;; store previous page

          (LDA ZP_RZ+1)
          (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x) ;; set page with free slots to this allocated page

          (LDA !$00)
          (INY)
          (STA (ZP_RZ),y)          ;; store number of slots used

          (LDY TABLE__FIRST_REF_COUNT_OFFSET__,x) ;; y = refcount field for first slot
          (INY)
          (TYA)
          (PHA)
          (LDX ZP_RZ+1)
          (STA VM_PAGE_SLOT_DATA,x)                    ;; set first free slot, here x = page
          (DEY)
          (LDX SEL_PROFILE__) ;; profile = 0..3
          (LDA !$00)

          ;; loop to initialize refcounts of each slot to 0-
   (label REF_COUNT_LOOP__)
          (STA (ZP_RZ),y) ;; refcount = 0
          (TYA)
          (CLC)
          (ADC TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE,x) ;; calc next refcount field offset
          (BCS END_REF_COUNT_LOOP__)
          (TAY)
          ;; (ADC !$01)
          (LDA !$00)
          (BCC REF_COUNT_LOOP__) ;; still on this page?

   (label END_REF_COUNT_LOOP__)
          ;; loop to write free slot list
          (LDY TABLE__FIRST_REF_COUNT_OFFSET__,x)
          (INY)  ;; first slot  (refcount field offset + 1)
          (TYA)
   (label WRITE_FREE_LIST__)
          (CLC)
          (ADC TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE,x)
          (BCS ALMOST_DONE__) ;; no longer on the same page => almost done
          (STA (ZP_RZ),y) ;; offset of next free cell == y for next write
          (TAY)
          (BCC WRITE_FREE_LIST__) ;; carry must be clear => always jump

   (label ALMOST_DONE__)
          (LDA !$00)
          (STA (ZP_RZ),y) ;; last offset to next free slot is 00 = no next free slot!
          (LDX ZP_RZ+1)   ;; x = page
          (PLA)           ;; A = first free slot
          (RTS)

   (label SEL_PROFILE__)
          (byte $00) ;; local var

   (label TABLE__FIRST_REF_COUNT_OFFSET__)
          (byte $03) ;; first ref count is 03, add 0a to get to next slot, slot-size $09 (09), page contains 25 slots
          (byte $03) ;; first ref count is 03, add 12 to get to next slot, slot size $11 (17), page contains 14 slots
          (byte $0f) ;; first ref count is 0f, add 1e to get to next slot, slot size $1d (29), page contains 8 slots
          (byte $05) ;; first ref count is 05, add 32 to get to next slot, slot-size $31 (49), page contains 5 slots
          (byte $03) ;; first ref count is 03, add 54 to get to next slot, slot-size $53 (83), page contains 3 slots
   (label TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE)
          (byte $0a) ;; add 0a to get to next slot, slot-size $09 (09), page contains 25 slots
          (byte $12) ;; add 12 to get to next slot, slot size $11 (17), page contains 14 slots
          (byte $1e) ;; add 1e to get to next slot, slot size $1d (29), page contains 8 slots
          (byte $32) ;; add 32 to get to next slot, slot-size $31 (49), page contains 5 slots
          (byte $54) ;; add 54 to get to next slot, slot-size $53 (83), page contains 3 slots
          )))

(module+ test #| vm_alloc_m1_page |#
  (define test-alloc-m1-01-state-after
    (compact-run-code-in-test
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_01_CODE)
            (DEX)
            (ast-opcode-cmd '() (list 157 0 PAGE_AVAIL_0)) ;; (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_M1_01_CODE)

            ;; now allocate the page
            (JSR ALLOC_PAGE_TO_X)
            (LDY !$01) ;; do it explicitly: profile 1
            (JSR INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX)))

  (check-equal? (vm-page->strings test-alloc-m1-01-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p1"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $04"))
  (check-equal? (memory-list test-alloc-m1-01-state-after (+ PAGE_AVAIL_0_W #x03) (+ PAGE_AVAIL_0_W #x04))
                (list #x00 #x16)
                "slot0: refcount 0, next free slot at offset $16")
  (check-equal? (memory-list test-alloc-m1-01-state-after (+ PAGE_AVAIL_0_W #x15) (+ PAGE_AVAIL_0_W #x16))
                (list #x00 #x28)
                "slot1: refcount 0, next free slot at offset $28")
  (check-equal? (memory-list test-alloc-m1-01-state-after (+ PAGE_AVAIL_0_W #x27) (+ PAGE_AVAIL_0_W #x28))
                (list #x00 #x3a)
                "slot2: refcount 0, next free slot at offset $28")
  (check-equal? (memory-list test-alloc-m1-01-state-after (+ PAGE_AVAIL_0_W #xed) (+ PAGE_AVAIL_0_W #xee))
                (list #x00 #x00)
                "slot13: refcount 0, next free slot at offset $00 = no next")

  (define test-alloc-m1-02-state-after
    (compact-run-code-in-test
          ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_02_CODE)
            (DEX)
            (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_M1_02_CODE)

            ;; now allocate the page
            (JSR ALLOC_PAGE_TO_X)
            (LDY !$02) ;; do it explicitly
            (JSR INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX)))

  (check-equal? (vm-page->strings test-alloc-m1-02-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p2"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $10"))
  (check-equal? (memory-list test-alloc-m1-02-state-after (+ PAGE_AVAIL_0_W #x0f) (+ PAGE_AVAIL_0_W #x10))
                (list #x00 #x2e)
                "slot0: refcount 0, next free slot at offset $2c")
  (check-equal? (memory-list test-alloc-m1-02-state-after (+ PAGE_AVAIL_0_W #x2d) (+ PAGE_AVAIL_0_W #x2e))
                (list #x00 #x4c)
                "slot1: refcount 0, next free slot at offset $4a")
  (check-equal? (memory-list test-alloc-m1-02-state-after (+ PAGE_AVAIL_0_W #x4b) (+ PAGE_AVAIL_0_W #x4c))
                (list #x00 #x6a)
                "slot2: refcount 0, next free slot at offset $68")
  (check-equal? (memory-list test-alloc-m1-02-state-after (+ PAGE_AVAIL_0_W #xe1) (+ PAGE_AVAIL_0_W #xe2))
                (list #x00 #x00)
                "slot7: refcount 0, next free slot at offset $00 = no next")

  (define test-alloc-m1-03-state-after
    (compact-run-code-in-test
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_03_CODE)
            (DEX)
            (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_M1_03_CODE)

            ;; now allocate the page
            (JSR ALLOC_PAGE_TO_X)
            (LDY !$03) ;; do it explicitly
            (JSR INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX)))

  (check-equal? (vm-page->strings test-alloc-m1-03-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p3"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $06"))
  (check-equal? (memory-list test-alloc-m1-03-state-after (+ PAGE_AVAIL_0_W #x05) (+ PAGE_AVAIL_0_W #x06))
                (list #x00 #x38)
                "slot0: refcount 0, next free slot at offset $38")
  (check-equal? (memory-list test-alloc-m1-03-state-after (+ PAGE_AVAIL_0_W #x37) (+ PAGE_AVAIL_0_W #x38))
                (list #x00 #x6a)
                "slot1: refcount 0, next free slot at offset $6a")
  (check-equal? (memory-list test-alloc-m1-03-state-after (+ PAGE_AVAIL_0_W #x69) (+ PAGE_AVAIL_0_W #x6a))
                (list #x00 #x9c)
                "slot2: refcount 0, next free slot at offset $9c")
  (check-equal? (memory-list test-alloc-m1-03-state-after (+ PAGE_AVAIL_0_W #xcd) (+ PAGE_AVAIL_0_W #xce))
                (list #x00 #x00)
                "slot4: refcount 0, next free slot at offset $00 = no next")

  (define test-alloc-m1-04-state-after
    (compact-run-code-in-test
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_04_CODE)
            (DEX)
            (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_M1_04_CODE)

            ;; now allocate the page
            (JSR ALLOC_PAGE_TO_X)
            (LDY !$04) ;; do it explicitly
            (JSR INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX)))

  (check-equal? (memory-list test-alloc-m1-04-state-after (+ PAGE_AVAIL_0_W #x00) (+ PAGE_AVAIL_0_W #x02))
                (list #x14 #x00 #x00)
                "page type $13, previous page = $00, slot number used = $00")
  (check-equal? (memory-list test-alloc-m1-04-state-after (+ PAGE_AVAIL_0_W #x03) (+ PAGE_AVAIL_0_W #x04))
                (list #x00 #x58)
                "slot0: refcount 0, next free slot at offset $56")
  (check-equal? (memory-list test-alloc-m1-04-state-after (+ PAGE_AVAIL_0_W #x57) (+ PAGE_AVAIL_0_W #x58))
                (list #x00 #xac)
                "slot1: refcount 0, next free slot at offset $aa")
  (check-equal? (memory-list test-alloc-m1-04-state-after (+ PAGE_AVAIL_0_W #xab) (+ PAGE_AVAIL_0_W #xac))
                (list #x00 #x00)
                "slot2: refcount 0, next free slot at offset $00 = no next")
  (check-equal? (vm-page->strings test-alloc-m1-04-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p4"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $04")))

;; allocate a slot of min A size, allocating a new page if necessary
;; input:  A = size
;; usage:  A, X, Y, RA, GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; output: RA = available slot of the given size (or a bit more)
;;         Y = actual size
;;         GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; funcs:
;;   VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS
;;   ALLOC_PAGE_TO_X
;;   INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX
(define ALLOC_M1_SLOT_TO_RA
  (add-label-suffix
   "__" "ALLOC_M1_SLOT_TO_RA"
  (list
   (label ALLOC_M1_SLOT_TO_RA)
          (LDX !$00)
          (CMP TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE+0)
          (BPL J9PLUS__)

   (label TYPE_X_STORE__)
          (STX PAGE_TYPE_IDX__)
          (JSR VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS) ;; x stays unchanged!

   (label ALLOC_M1_SLOT_TYPE_X_TO_RA)
          (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x) ;;
          (BEQ PAGE__)     ;; if the current free page is $00 (there is no page marked as having free slots) => allocate new page

          ;; ensure zp_ra points into the page
          (STA ZP_RA+1)
          (STA INC_CMD__+2)
          (TAX)
          (LDY VM_PAGE_SLOT_DATA,x)           ;; first free slot offset
          (BEQ PAGE__)    ;; if =0 allocate new page (no more free slots on this page)
          ;; ensure zp_ptr2 points to the slot!

   (label CONTINUE__)
          (STY ZP_RA)

          ;; now get the next free slot (from linked list in this page)
          (LDY !$00)
          (LDA (ZP_RA),y) ;; content of free slot points to the next free one (or 00)
          (STA VM_PAGE_SLOT_DATA,x)           ;; set next free slot for this page (x is still page)

          ;; ensure y holds the actual available slot size
          (LDX PAGE_TYPE_IDX__)
          (LDY TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE,x)
          (DEY)

   (label INC_CMD__)
          (INC $c002) ;; $c0 is overwritten with current page (increases the number of slots actually used)

          (RTS)

   (label FIND_NEXT_FREE_PAGE__)     ;; current page is full, search first non full (or end of list)
          ;; A = page, X = page, Y = 0
          (STA NEXT_PAGE_CMD__+2)

   (label NEXT_PAGE_CMD__)
          (LDA $C001) ;; $c0 is overwritten
          (BEQ PAGE__) ;; next page ptr = $00 => end reached, no more pages
          ;; check whether this page is full
          (TAX)
          (LDY VM_PAGE_SLOT_DATA,x)
          (BEQ FIND_NEXT_FREE_PAGE__) ;; next free slot for page is 00 => page is full, try to find next
          ;; page is not full => this is the new head
          (LDX PAGE_TYPE_IDX__)
          (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x)
          (STA ZP_RA+1)
          (CLC)
          (BCC CONTINUE__)

   (label PAGE__)               ;; allocate a complete new page for page type x or find a page in the list that has free slots
          (JSR ALLOC_PAGE_TO_X)
          (LDY PAGE_TYPE_IDX__)
          (JSR INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX)
          (LDX PAGE_TYPE_IDX__)
          (CLC)
          (BCC ALLOC_M1_SLOT_TYPE_X_TO_RA)

   (label J9PLUS__)
          (CMP TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE+1)
          (BPL J17PLUS__)
          (LDX !$01)
          (BNE TYPE_X_STORE__)

   (label J17PLUS__)
          (CMP TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE+2)
          (BPL J29PLUS__)
          (LDX !$02)
          (BNE TYPE_X_STORE__)

   (label J29PLUS__)
          (CMP TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE+3)
          (BPL J49PLUS__)
          (LDX !$03)
          (BNE TYPE_X_STORE__)

   (label J49PLUS__)
          (CMP TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE+4)
          (BPL J83PLUS__)
          (LDX !$04)
          (BNE TYPE_X_STORE__)

   (label J83PLUS__)
          ;; error, no slot this large can be allocated
          (BRK)

   (label PAGE_TYPE_IDX__)
          (byte $00) ;; local variable holding the selected page typ (0 = slots up to 17 bytes, 2 up to 29 bytes ...)
          )))

(module+ test #| vm_alloc_bucket_slot, allocate one slot of size $0b |#
  (define test-alloc-bucket-slot-state-after
    (compact-run-code-in-test
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEX)
            (ast-opcode-cmd '() (list 157 0 PAGE_AVAIL_0)) ;; (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

            ;; now allocate the page
            (LDA !$0b) ;; want slot of size 11
            (JSR ALLOC_M1_SLOT_TO_RA)

            (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC+1) ;; type 1
            (STA ZP_TEMP)))

  (check-equal? (memory-list test-alloc-bucket-slot-state-after (+ PAGE_AVAIL_0_W #x03) (+ PAGE_AVAIL_0_W #x04))
                (list #x00 #x16)
                "slot0: refcount 0, next free slot at offset $16")
  (check-equal? (memory-list test-alloc-bucket-slot-state-after ZP_RA (add1 ZP_RA))
                (list #x04 PAGE_AVAIL_0)
                "allocated slot is at xx04")
  (check-equal? (memory-list test-alloc-bucket-slot-state-after ZP_TEMP ZP_TEMP)
                (list PAGE_AVAIL_0)
                "free page for slot type 0 is $cc")
  (check-equal? (vm-page->strings test-alloc-bucket-slot-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p1"
                  "previous page:  $00"
                  "slots used:     1"
                  "next free slot: $16")))

(module+ test #| vm_alloc_bucket_slot 2 times slot size $0b and $09 |#
  (define test-alloc-bucket-slot-2x-state-after
    (compact-run-code-in-test
          ;; fill page with $ff
     (LDA !$FF)
     (LDX !$00)
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
     (DEX)
     (STA $cc00,x)
     (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

     ;; now allocate the page
     (LDA !$0b) ;; want slot of size 11
     (JSR ALLOC_M1_SLOT_TO_RA)
     (LDA !$0a) ;; want slot of size 10, should be on the same page
     (JSR ALLOC_M1_SLOT_TO_RA)

     (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC+1) ;; type 1
     (STA ZP_TEMP)))

  (check-equal? (memory-list test-alloc-bucket-slot-2x-state-after (+ PAGE_AVAIL_0_W #x15) (+ PAGE_AVAIL_0_W #x16))
                (list #x00 #x28)
                "slot1: refcount 0, next free slot at offset $28")
  (check-equal? (memory-list test-alloc-bucket-slot-2x-state-after ZP_RA (add1 ZP_RA))
                (list #x16 PAGE_AVAIL_0)
                "allocated slot is at xx16")
  (check-equal? (memory-list test-alloc-bucket-slot-2x-state-after ZP_TEMP ZP_TEMP)
                (list PAGE_AVAIL_0)
                "free page for slot type 0 is $xx")
  (check-equal? (vm-page->strings test-alloc-bucket-slot-2x-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p1"
                  "previous page:  $00"
                  "slots used:     2"
                  "next free slot: $28")))

(module+ test #| vm_alloc_bucket_slot, alloc 10 x slot size $14 (actual $20)  |#
  (define test-alloc-bucket-slot-xx-state-after
    (compact-run-code-in-test
            ;; fill page with $ff
            (LDA !$FF)
            (LDY !$02) ;; 2 pages
            (LDX !$00) ;; 256 bytes
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEX)
            (ast-opcode-cmd '() (list 157 0 PAGE_AVAIL_1)) ;; (STA $cb00,x)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEY)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

            ;; loop over ...
            (LDA !$0a)
            (STA LOOP_NUM__TEST_ALLOC_BUCKET_SLOT_XX)

     (label LOOP__TEST_ALLOC_BUCKET_SLOT_XX)
            (LDA !$14) ;; want slot of size 20
            (JSR ALLOC_M1_SLOT_TO_RA) ;; ... slot allocation
            (DEC LOOP_NUM__TEST_ALLOC_BUCKET_SLOT_XX)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_XX)

            (JMP TAIL__TEST_ALLOC_BUCKET_SLOT_XX)

     (label LOOP_NUM__TEST_ALLOC_BUCKET_SLOT_XX)
            (byte $20)


     (label TAIL__TEST_ALLOC_BUCKET_SLOT_XX)
            (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC+2) ;; type 2
            (STA ZP_TEMP)))

  (check-equal? (memory-list test-alloc-bucket-slot-xx-state-after ZP_RA (add1 ZP_RA))
                (list #x2e PAGE_AVAIL_1)
                "allocated slot is at cb2e (slot1 on page 2)")
  (check-equal? (memory-list test-alloc-bucket-slot-xx-state-after (+ PAGE_AVAIL_1_W #x4b) (+ PAGE_AVAIL_1_W #x4c))
                (list #x00 #x6a)
                "first free slot page2: refcount 0, next free slot at offset $6a")
  (check-equal? (memory-list test-alloc-bucket-slot-xx-state-after ZP_TEMP ZP_TEMP)
                (list PAGE_AVAIL_1)
                "free page for slot type 1 is $cb")
  (check-equal? (vm-page->strings test-alloc-bucket-slot-xx-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p2"
                  "previous page:  $00"
                  "slots used:     8"
                  "next free slot: $00"))
  (check-equal? (vm-page->strings test-alloc-bucket-slot-xx-state-after PAGE_AVAIL_1)
                '("page-type:      m1 page p2"
                  "previous page:  $00"
                  "slots used:     2"
                  "next free slot: $4c")))

  ;; free-page for slot type 0 = cc

(define VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS #t)

;; remove full pages in the free list of pages of the same type as are currently in ZP_RA
;; input: RA
;; usage: A, X, Y, RA, GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; output: GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; funcs: -
(define VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS
  (add-label-suffix
   "__" "VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS"
  (list
   (label VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS)
          (LDA ZP_RA+1)
          (STA READ_ENC_PAGE_TYPE__+2)
   (label READ_ENC_PAGE_TYPE__)
          (LDA $c000)
          (AND !$07)

          (TAX) ;; now x = page profile

   ;; input: x (unchanged)
   (label VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS)
          (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x)

   (label LOOP_REMOVE_FULL_PAGES__)
          (TAY) ;; y = page now
          (LDA VM_PAGE_SLOT_DATA,y)
          (BNE DONE__)                  ;; if there is a free slot on the given page (y), then we are done (no full page at head)

          ;; remove this page (in y) from list
          (STY LOAD_PREV_PAGE_CMD__+2)
          (STY STORE_PREV_PAGE_CMD__+2)
          (LDY !$00)
   (label LOAD_PREV_PAGE_CMD__)
          (LDA $c001) ;; $c0 is overwritten with page
          (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x) ;; optional optimization: needs only be done once! (is here done in a loop)
   (label STORE_PREV_PAGE_CMD__)
          (STY $c001) ;; $c0 is overwritten
          (BNE LOOP_REMOVE_FULL_PAGES__) ;; if the current page (in a) is 0 (we are at the end of the list), we are done and can return, else loop

   (label DONE__)
          (RTS))))

;; put this page as head of the page free list for slots of type as in ZP_RA
;; input:  RA
;; usage:  A, X, Y, RA, TEMP
;; output: GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; funcs: -
(define VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS
  (add-label-suffix
   "__" "__VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS"
  (list
   (label VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS)
          (LDA ZP_RA)
          (STA ZP_TEMP) ;; keep for later

          (LDA !$00)    ;; set to zero
          (STA ZP_RA)

          (LDY !$01)
          (LDA (ZP_RA),y) ;; get previous
          (BNE CONTINUE_WITH_RESTORE__)     ;; is != 0 => is still part of the list, don't change the list
          ;; is no longer part of the free list of pages, add this page at the head of the page

          (DEY) ;; now 0
          (LDA (ZP_RA),y) ;; get encoded page type
          (AND !$07)

          (TAX) ;; now x = page type

          (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x)

          (INY) ;; now 1
          (STA (ZP_RA),y) ;; set previous

          ;; x = page type, a = page
          (LDA ZP_RA+1)
          (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x)
          (TAX)  ;; x = page

   (label CONTINUE_WITH_RESTORE__)
          (LDA ZP_TEMP)
          (STA ZP_RA) ;; restore
          (LDA VM_PAGE_SLOT_DATA,x)           ;; first free slot offset

          (RTS))))

;; free the m1 slot pointed to by ra, marking that slot free on the m1-page
;; no check of the slot content is done! in case of cell-arrays: the elements of the array are not checked
;; input:  RA
;; usage: A, X, Y, RA
;; output: RA is invalid
;; funcs:
;;   VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS
;;   VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS
;; currently once allocated pages are not garbage collected. this is bad and needs to be changed
;; (e.g. keep count of used slots)? used slots = 0 => free page
;; INFO: NO GC! (this must be done, freeing specific types (e.g. an array) <- knows the number of slots etc.
;;       REF COUNT IS SET TO ZERO (of this slot)
(define FREE_M1_SLOT_RA
  (add-label-suffix
   "__" "__FREE_M1_SLOT_RA"
  (list
   (label FREE_M1_SLOT_RA)
          ;; make sure to remove fulls from free page list first !!
          (JSR VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS)

          ;; now free the slot
   (label REGULAR_FREE__)
          (LDX ZP_RA+1)
          (STX DEC_CMD__+2)    ;; write page for later dec execution
          (LDA VM_PAGE_SLOT_DATA,x)           ;; first free slot offset
          (BNE CONTINUE__)     ;; regular free

          ;; this page was full (since next free slot was 0) => register with the list of pages with free slots
          (JSR VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS)
          (LDX DEC_CMD__+2)    ;; restore x
          (LDA !$00)                              ;; next free slot offset (=0)

   (label CONTINUE__)
          (LDY !$00)
          (STA (ZP_RA),y)                       ;; set (zp_ptr) = previous free
          (LDA ZP_RA)                           ;; low byte of pointer = new free slot
          (STA VM_PAGE_SLOT_DATA,x)           ;; set new first free slot offset

          (DEC ZP_RA)                           ;; now points to ref count
          (TYA)                                   ;; y is still 0 => a := 0
          (STA (ZP_RA),y)                       ;; set refcount := 0

   (label DEC_CMD__)       ;; decrement number of slots used on the page
          (DEC $c002)                             ;; $c0 is overwritten

          (RTS))))

(module+ test #| vm_free_bucket_slot  allocate two slots, free first slot |#
  (define test-free-bucket-slot-state-after
    (compact-run-code-in-test
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEX)
            (ast-opcode-cmd '() (list 157 0 PAGE_AVAIL_0)) ;; (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

            ;; now allocate the page
            (LDA !$0b) ;; want slot of size 11
            (JSR ALLOC_M1_SLOT_TO_RA)
            (JSR CP_RA_TO_RT)

            (LDA !$0a) ;; want slot of size 10, should be on the same page
            (JSR ALLOC_M1_SLOT_TO_RA)

            (JSR CP_RT_TO_RA)
            (JSR FREE_M1_SLOT_RA)))

  (check-equal? (memory-list test-free-bucket-slot-state-after (+ PAGE_AVAIL_0_W #x03)(+ PAGE_AVAIL_0_W #x04))
                (list #x00 #x28)
                "slot0 (now free): refcount 0, next free slot at offset $28")
  (check-equal? (vm-page->strings test-free-bucket-slot-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p1"
                  "previous page:  $00"
                  "slots used:     1"
                  "next free slot: $04")))

(module+ test #| vm_free_bucket_slot  allocate 16 slots, free first slot |#
  (define test-free-bucket-a20-slot-state-after
    (compact-run-code-in-test
     ;; fill page with $ff
            (LDY !$03) ;; fill two pages
            (LDA !$FF) ;; with $ff
            (LDX !$00) ;; each 256 bytes long
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEX)
            (ast-opcode-cmd '() (list 157 0 PAGE_AVAIL_1)) ;; (STA $cb00,x) ;; starting at $cb00
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEY)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

            ;; now allocate the page
            (LDA !$17)
            (STA LOOP_VAR__TEST_FREE_BUCKET_A20_SLOT_CODE)
     (label LOOP__TEST_FREE_BUCKET_A20_SLOT_CODE)
            (LDA !$14) ;; want slot of size 14 (max size $1e)
            (JSR ALLOC_M1_SLOT_TO_RA)
            (DEC LOOP_VAR__TEST_FREE_BUCKET_A20_SLOT_CODE)
            (BPL LOOP__TEST_FREE_BUCKET_A20_SLOT_CODE)
            (JMP CONT__TEST_FREE_BUCKET_A20_SLOT_CODE )
     (label LOOP_VAR__TEST_FREE_BUCKET_A20_SLOT_CODE)
            (byte $00)

     (label CONT__TEST_FREE_BUCKET_A20_SLOT_CODE)
            ;; select first pointer
            (ast-opcode-cmd '() (list 169 PAGE_AVAIL_1)) ;; (LDA !$cb)
            (STA ZP_RA+1)
            (LDA !$10)
            (STA ZP_RA)
            (JSR FREE_M1_SLOT_RA)

            (ast-opcode-cmd '() (list 169 PAGE_AVAIL_0)) ;; (LDA !$cc)
            (STA ZP_RA+1)
            (LDA !$10)
            (STA ZP_RA)
            (JSR FREE_M1_SLOT_RA)))

  (check-equal? (memory-list test-free-bucket-a20-slot-state-after #xcec7 #xcecb)
                (list #x00 #x00 PAGE_AVAIL_0 #x00 #x00)
                "first free page of profiles 0, 1, 2, 3, 4 is $cc for page profile 1")
  (check-equal? (vm-page->strings test-free-bucket-a20-slot-state-after (sub1 PAGE_AVAIL_1))
                '("page-type:      m1 page p2"
                  "previous page:  $00" ;; is removed, since full
                  "slots used:     8"
                  "next free slot: $00"))
  (check-equal? (vm-page->strings test-free-bucket-a20-slot-state-after PAGE_AVAIL_1)
                '("page-type:      m1 page p2"
                  "previous page:  $00" ;; is the last in list
                  "slots used:     7"
                  "next free slot: $10"))
  (check-equal? (vm-page->strings test-free-bucket-a20-slot-state-after PAGE_AVAIL_0)
                (list "page-type:      m1 page p2"
                      (format "previous page:  $~a" (format-hex-byte PAGE_AVAIL_1)) ;; next free
                  "slots used:     7"
                  "next free slot: $10")))

;; increment refcount of m1 slot in RA
;; IDEA for optimization: keep m1 in RA, putting +1 offset on all accesses -> DEC/INC could be saved
;; input:  RA (pointing to some m1 slot)
;; usage:  A, Y, RA
;; output: M1_SLOT Refcount++
;; funcs: -
(define INC_REFCNT_M1_SLOT_RA
  (list
   (label INC_REFCNT_M1_SLOT_RA)
          (DEC ZP_RA)           ;; m1, now pointing to reference count field, no page mod needed, since lowbyte always > 0
          (LDY !$00)
          (LDA (ZP_RA),y)
          (CLC)
          (ADC !$01)            ;; add 1 (there is no increment command for indirect addresses)
          (STA (ZP_RA),y)
          (INC ZP_RA)           ;; restore pointer
          (RTS) ;; 14 bytes


   ;; ;; slightly longer (1 byte longer), self modifying code, 5 cycles faster
   ;; (label ALT_INC_REFCNT_M1_SLOT_RA)
   ;;        (LDA ZP_RA+1)
   ;;        (LDY ZP_RA)
   ;;        (DEY)
   ;;        (STA INC_ABS__+2)
   ;;        (STY INC_ABS__+1)
   ;; (label INC_ABS__)
   ;;        (INC $c000)
   ;;        (RTS)
          ))

(module+ test #| vm_inc_ref_bucket_slot |#
  (define test-inc-ref-bucket-slot-1-state-after
    (compact-run-code-in-test
     (LDA !$f0)
     (STA $f003)
     (STA ZP_RA+1)
     (LDA !$04)
     (STA ZP_RA)

     (JSR INC_REFCNT_M1_SLOT_RA)))

  (check-equal? (memory-list test-inc-ref-bucket-slot-1-state-after #xf003 #xf003)
                (list #xf1))
  (check-equal? (memory-list test-inc-ref-bucket-slot-1-state-after ZP_RA (add1 ZP_RA))
                (list #x04 #xf0))
  (inform-check-equal? (cpu-state-clock-cycles test-inc-ref-bucket-slot-1-state-after)
                       50))

(module+ test #| vm_gc_array_slot_ptr |#
  (define test-gc-array-slot-ptr-state-after
    (compact-run-code-in-test
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

;; allocate an array of bytes (native) (also useful for strings)
;; overwrite RA no matter whether RA was filled
;; input:  A = number of bytes (1..81)
;; usage:  A, X, Y, RA
;; output: RA -> points to an allocated array (not initialized)
;; funcs:
;;   ALLOC_M1_SLOT_TO_RA
(define ALLOC_NATARR_TO_RA
  (list
   (label ALLOC_NATARR_TO_RA)
          (PHA)
          (CLC)
          (ADC !$02) ;; add to total slot size

          (JSR ALLOC_M1_SLOT_TO_RA)

          ;; write header cell
          (LDY !$00)
          (LDA !TAG_BYTE_NATIVE_ARRAY)
          (STA (ZP_RA),y) ;; store tag byte

          (INY)
          (PLA)
          (STA (ZP_RA),y) ;; store number of array elements

   ;; no initializing with 0 (might be useful for debugging, though)
   ;;        (TAX) ;; use number of array elements as loop counter

   ;;        ;; initialize slots/array with 0
   ;;        (LDA !$00)
   ;; (label LOOP_INIT__VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)
   ;;        (INY)
   ;;        (STA (ZP_RA),y)
   ;;        (DEX)
   ;;        (BNE LOOP_INIT__VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)

          (RTS)))

(module+ test #| vm_allocate_native_array |#
  (define test-alloc-native-array-state-after
    (compact-run-code-in-test
     (LDA !$10)
     (JSR ALLOC_NATARR_TO_RA)))

  (check-equal? (vm-page->strings test-alloc-native-array-state-after PAGE_AVAIL_0)
                (list
                 "page-type:      m1 page p2"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $2e"))
  (check-equal? (memory-list test-alloc-native-array-state-after ZP_RA (add1 ZP_RA))
                (list #x10 PAGE_AVAIL_0))
  (check-equal? (memory-list test-alloc-native-array-state-after (+ PAGE_AVAIL_0_W #x10) (+ PAGE_AVAIL_0_W #x11))
                (list TAG_BYTE_NATIVE_ARRAY #x10)))

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
    (compact-run-code-in-test
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
    (compact-run-code-in-test
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
    (compact-run-code-in-test
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
    (compact-run-code-in-test
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
    (compact-run-code-in-test
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
    (compact-run-code-in-test
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
           (CLC)
           (BCC WRITE_ARR_ATa_RA_TO_RT)

    (label CHECK_BOUNDS__)
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
    (compact-run-code-in-test
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
                1278)
  (check-equal? (vm-stack->strings test-cell-stack-push-array-ata-ptr-state-after)
                (list "stack holds 2 items"
                      "int $01ff  (rt)"
                      "int $01ff")))

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

          PUSH_TO_EVLSTK                               ;; push value into RT, pushing RT onto the call frame cell stack if not empty
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

          CP_RT_TO_RA                                     ;; copy RT -> RA
          CP_RA_TO_RT                                     ;; copy RA -> RT
          CP_RA_TO_RZ                                     ;; copy RA -> RZ
          CP_RT_TO_RZ                                     ;; copy RT -> RZ
          CP_RT_TO_RP
          CP_RZ_TO_RT

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
                       1767))

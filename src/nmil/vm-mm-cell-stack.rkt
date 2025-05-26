#lang racket/base

#|

all functions around cell-stacks (including evlstk)

cell-stacks are stack organized cells, split into a high-byte page and a low-byte page

|#

(require "../6510.rkt")
(require (only-in "./vm-memory-map.rkt"
                  TAGGED_NIL
                  ZP_RP
                  ZP_RT
                  VM_MEMORY_MANAGEMENT_CONSTANTS))
(require (only-in "../ast/6510-resolver.rkt"
                  add-label-suffix
                  replace-labels))

(provide
           INIT_CELLSTACK_PAGE_X        ;; initialize page A to previous cell stack page (X)
           PUSH_TO_EVLSTK               ;; push value into RT, pushing RT onto the call frame cell stack if not empty
           POP_CELL_EVLSTK_TO_RT        ;; pop cell-stack into RT (discarding RT)
           POP_CELL_EVLSTK_TO_RA
           PUSH_RT_TO_EVLSTK            ;; push RT onto call frame cell stack
           POP_CELL_EVLSTK_TO_CELLy_RT  ;; POP the cell-stack top into CELLy (y=0 cell0, y=2 cell1) pointed to by RT, reducing the stack size by 1, keeping rt as tos
          )

(module+ test
  (require (only-in racket/list make-list))
  (require  "../6510-test-utils.rkt")
  (require "./vm-memory-manager-test-utils.rkt")
  (require (only-in "../tools/6510-interpreter.rkt" peek memory-list))
  (require (only-in "../util.rkt" format-hex-byte format-hex-word))
  (require (only-in "./vm-inspector-utils.rkt"
                    vm-deref-cell-pair-w->string
                    vm-stack->strings
                    vm-regt->string))
  (require (only-in "./vm-mm-register-functions.rkt"
                    WRITE_INT_AY_TO_RT))
  (require (only-in "./vm-mm-pages.rkt"
                    ALLOC_PAGE_TO_X
                    VM_PAGE_SLOT_DATA
                    VM_INITIAL_MM_REGS
                    VM_INITIALIZE_MEMORY_MANAGER))
  (require (only-in "./vm-mm-cell-pairs.rkt"
                    ALLOC_CELLPAIR_TO_RT
                    ALLOC_CELLPAIR_AX_TO_RT
                    WRITE_CELLPAIR_RT_CELLy_TO_RT
                    GET_FRESH_CELLPAIR_TO_AX
                    INIT_CELLPAIR_PAGE_X_TO_AX))

  (define PAGE_AVAIL_0 #x9a)      ;; high byte of first page available for allocation
  (define PAGE_AVAIL_0_W #x9a00)  ;; word (absolute address) of first page available
  (define PAGE_AVAIL_1 #x99)      ;; high byte of second page available for allocation
  (define PAGE_AVAIL_1_W #x9900) ;; word (absolute address) of second page available

  (define test-runtime
    (append
     ALLOC_PAGE_TO_X
     INIT_CELLSTACK_PAGE_X
     PUSH_TO_EVLSTK
     POP_CELL_EVLSTK_TO_RT
     PUSH_RT_TO_EVLSTK
     POP_CELL_EVLSTK_TO_CELLy_RT

     WRITE_CELLPAIR_RT_CELLy_TO_RT
     INIT_CELLPAIR_PAGE_X_TO_AX
     ALLOC_CELLPAIR_TO_RT
     ALLOC_CELLPAIR_AX_TO_RT
     WRITE_INT_AY_TO_RT
     GET_FRESH_CELLPAIR_TO_AX
     VM_MEMORY_MANAGEMENT_CONSTANTS
     VM_INITIALIZE_MEMORY_MANAGER
     (list (label DEC_REFCNT_RT) (RTS))
     (list (org #xcec0))
     VM_INITIAL_MM_REGS
     (list (org #xcf00))
     VM_PAGE_SLOT_DATA)))


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
    (compact-run-code-in-test-
     #:runtime-code test-runtime
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
    (compact-run-code-in-test-
     #:runtime-code test-runtime
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
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR PUSH_INT_1_TO_EVLSTK)
     (JSR PUSH_INT_m1_TO_EVLSTK)
     (JSR PUSH_INT_0_TO_EVLSTK)
     (JSR POP_CELL_EVLSTK_TO_RT)
     (JSR POP_CELL_EVLSTK_TO_RT)))

  (check-equal? (vm-stack->strings vm_cell_stack_pop2_r_state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define vm_cell_stack_pop1_r_state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
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
    (compact-run-code-in-test-
     #:runtime-code test-runtime
      (JSR PUSH_NIL_TO_EVLSTK)))

  (check-equal? (vm-regt->string test-vm_cell_stack_push_nil-a-state-after)
                "pair-ptr NIL")

  (define test-vm_cell_stack_push_nil-b-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
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
    (compact-run-code-in-test-
     #:runtime-code test-runtime
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
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR PUSH_INT_0_TO_EVLSTK)))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_int0_state)
                "int $0000")
  (check-equal? (memory-list vm_cell_stack_push_r_int0_state ZP_RT (add1 ZP_RT))
                (list #x03 #x00))

  (define vm_cell_stack_push_r_int1_state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR PUSH_INT_1_TO_EVLSTK)))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_int1_state)
                "int $0001")
  (check-equal? (memory-list vm_cell_stack_push_r_int1_state ZP_RT (add1 ZP_RT))
                (list #x03 #x01))

  (define vm_cell_stack_push_r_intm1_state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR PUSH_INT_m1_TO_EVLSTK)))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_intm1_state)
                "int $1fff")
  (check-equal? (memory-list vm_cell_stack_push_r_intm1_state ZP_RT (add1 ZP_RT))
                (list #x7f #xff))

  (define vm_cell_stack_push_r_nil_state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR PUSH_NIL_TO_EVLSTK)))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_nil_state)
                "pair-ptr NIL")
  (check-equal? (memory-list vm_cell_stack_push_r_nil_state ZP_RT (add1 ZP_RT))
                (list #x01 #x00))

  (define vm_cell_stack_push_r_cell_ptr_state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDX !$05)
     (LDA !$ce)
     (JSR PUSH_TO_EVLSTK)))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_cell_ptr_state)
                "pair-ptr[0] $ce05")
  (check-equal? (memory-list vm_cell_stack_push_r_cell_ptr_state ZP_RT (add1 ZP_RT))
                (list #x05 #xce)))

(module+ test #| vm_cell_stack_push_r (push rt, and write rt) |#
  (define vm_cell_stack_push_r_push1_state
    (compact-run-code-in-test-
     #:runtime-code test-runtime

     (JSR PUSH_INT_m1_TO_EVLSTK)
     (JSR PUSH_INT_1_TO_EVLSTK)))

  (check-equal? (vm-stack->strings vm_cell_stack_push_r_push1_state)
                (list "stack holds 2 items"
                      "int $0001  (rt)"
                      "int $1fff"))

  (check-equal? (memory-list vm_cell_stack_push_r_push1_state ZP_RT (add1 ZP_RT))
                (list #x03 #x01))

  (define vm_cell_stack_push_r_push2_state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
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
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR WRITE_INTm1_TO_RT)
     (JSR PUSH_RT_TO_EVLSTK)))

  (check-equal? (vm-stack->strings vm-cell-stack-just-push-rt-state)
                (list "stack holds 2 items"
                      "int $1fff  (rt)"
                      "int $1fff")))


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
    (compact-run-code-in-test-
     #:runtime-code test-runtime
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

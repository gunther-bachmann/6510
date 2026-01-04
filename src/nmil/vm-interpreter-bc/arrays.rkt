#lang racket/base

(provide BC_DEC_RBI_NZ_P_BRA            ;; decrement cell array index register RBI and branch if NOT Zero
         BC_DEC_RAI                     ;; decrement cell array index register RAI
         BC_WRITE_TO_RBI                ;; write tos byte cell array index register RBI
         BC_WRITE_TO_RAI                ;; write tos byte into cell array index register RAI
         BC_POP_TO_RAI                  ;; pop tos byte into cell array index register RAI
         BC_BINC_RAI                    ;; increment cell array index register RAI
         BC_ALLOC_ARA                   ;; allocate a cell-array with a number of cells (tos)
         BC_XET_RA_ARRAY_FIELD          ;; -- meta, including the following two
         BC_GET_RA_ARRAY_FIELD          ;; get array field 0..3 (A>>3) from (RA)
         BC_SET_RA_ARRAY_FIELD          ;; set RT into array field 0..3 (A>>3) (RA)
         BC_XET_ARRAY_FIELD             ;; -- meta, including the following two
         BC_GET_ARRAY_FIELD             ;; RT->RA, get array field 0..3 (A>>3) from (RA), overwriting old TOS
         BC_SET_ARRAY_FIELD             ;; RT->RA, set (new) TOS into array field 0..3 (A>>3) (RA)
         BC_WRITE_RA                    ;; write cell-array register RA into tos
         BC_PUSH_RA                     ;; push cell-array register RA itself onto eval stack
         BC_PUSH_RA_AF                  ;; push cell-array RA field A onto the eval stack (inc ref count)
         BC_POP_TO_RA_AF                ;; pop tos into cell-array RA field A onto the eval stack (TODO: ref count old content!)
         BC_PUSH_AF                     ;; push array field (stack: index :: cell-array-ptr)
         BC_POP_TO_AF                   ;; pop tos to array field (stack: index :: cell-ptr->cell-array  :: value )
         BC_SWAP_RA_RB                  ;; swap cell array register RA with RB
         VM_REFCOUNT_DECR_ARRAY_REGS    ;; decrement remaining ptrs in RA, RB, RC
         )

(require "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt" add-label-suffix)
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC
                  VM_INTERPRETER_INC_PC_2_TIMES
                  VM_POP_EVLSTK_AND_INC_PC)
         (only-in "../vm-runtime/vm-m1-slots.rkt"
                  INC_REFCNT_M1_SLOT_RT
                  DEC_REFCNT_M1_SLOT_RZ
                  DEC_REFCNT_M1_SLOT_RA
                  DEC_REFCNT_M1_SLOT_RB
                  DEC_REFCNT_M1_SLOT_RC)
         (only-in "../vm-runtime/vm-memory-map.rkt"
                  ZP_RBI
                  ZP_RAI
                  ZP_RT)
         (only-in "../vm-runtime/vm-cell-array.rkt"
                  ALLOC_CELL_ARRAY_TO_RA
                  POP_EVLSTK_TO_ARR_ATa_RA
                  WRITE_ARR_ATa_RA_TO_RT
                  COPY_ARR_ATa_RA_TO_RZ__IF_PTR)
         (only-in "../vm-runtime/vm-cell-stack.rkt"
                  POP_EVLSTK_TAIL_TO_RT
                  PUSH_RT_TO_EVLSTK_TAIL)
         (only-in "../vm-runtime/vm-register-functions.rkt"
                  SWAP_RA_RB
                  CP_RT_TO_RA)
         (only-in "./branch.rkt"
                  BRANCH_BY_NEXT_BYTE__NO_POP))

(define-vm-function BC_DEC_RBI_NZ_P_BRA
  (list
            (DEC ZP_RBI)
            (BEQ NO_BRA__) ;; == 0 => no branch
            (JMP BRANCH_BY_NEXT_BYTE__NO_POP)
     (label NO_BRA__)
            (JMP VM_INTERPRETER_INC_PC_2_TIMES)))

;; get array field 0..3 (A>>3) from (RA)
(define BC_GET_RA_ARRAY_FIELD '())
;; set RT into array field 0..3 (A>>3) (RA)
(define BC_SET_RA_ARRAY_FIELD '())
(define-vm-function-wol BC_XET_RA_ARRAY_FIELD
  (list
    (label BC_GET_RA_ARRAY_FIELD)               ;; (RA),A -> RT
           (LSR)
           (AND !$03)
           (PHA)
           (JSR PUSH_RT_TO_EVLSTK_TAIL)
           (PLA)
           (JSR WRITE_ARR_ATa_RA_TO_RT)
           (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
           (JMP VM_INTERPRETER_INC_PC)

    (label BC_SET_RA_ARRAY_FIELD)                ;; RT -> (RA),A
           (LSR)
           (AND !$03)
           (JSR COPY_ARR_ATa_RA_TO_RZ__IF_PTR)   ;; keep overwritte field (if ptr)
           (JSR POP_EVLSTK_TO_ARR_ATa_RA)        ;; no refcount adjustment, since value is off the stack (-1), but in array (+1)
           (LDA ZP_RZ)
           (BEQ continue__BC_SET_RA_ARRAY_FIELD) ;; is no ptr (copy fills it with 0 if not a ptr)
           (JSR DEC_REFCNT_M1_SLOT_RZ)           ;; decrement overwritten field (if it was a ptr)
    (label continue__BC_SET_RA_ARRAY_FIELD)
           (JMP VM_INTERPRETER_INC_PC)))

;; allocate a cell-array with A number of cells
;;
(define-vm-function BC_ALLOC_ARA
  (list
          (LDA ZP_RT+1)                 ;; byte/int size
          (JSR ALLOC_CELL_ARRAY_TO_RA)
          (LDA !$00)
          (STA ZP_RAI)

   ;; optional: initialize lowbyte of cells with 0
   ;;        (LDA ZP_RT+1)
   ;;        (ASL A)
   ;;        (TAY)
   ;;        (LDA !$00)
   ;; (label loop__BC_ALLOC_ARA)
   ;;        (STA (ZP_RA),y)
   ;;        (DEY)
   ;;        (DEY)
   ;;        (BNE loop__BC_ALLOC_ARA)

          (JMP VM_POP_EVLSTK_AND_INC_PC)))

(define-vm-function BC_BINC_RAI
  (list
          (INC ZP_RAI)
          (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_POP_TO_RAI
  (list
          (LDA ZP_RT+1)
          (STA ZP_RAI)
          (JMP VM_POP_EVLSTK_AND_INC_PC)))

(define-vm-function BC_WRITE_TO_RAI
  (list
          (LDA ZP_RT+1)
          (STA ZP_RAI)
          (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_WRITE_TO_RBI
  (list
          (LDA ZP_RT+1)
          (STA ZP_RBI)
          (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_DEC_RAI
  (list
          (DEC ZP_RAI)
          (JMP VM_INTERPRETER_INC_PC)))

;; RT->RA, get array field 0..3 (A>>3) from (RA), overwriting old TOS
(define BC_GET_ARRAY_FIELD '())
;; RT->RA, set (new) TOS into array field 0..3 (A>>3) (RA)
(define BC_SET_ARRAY_FIELD '())
(define-vm-function-wol BC_XET_ARRAY_FIELD
   (list
    (label BC_GET_ARRAY_FIELD) ;; replace RT with RT.@A
           (LSR)
           (AND !$03)
           (JSR CP_RT_TO_RA)
           (JSR WRITE_ARR_ATa_RA_TO_RT)
           (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
           (JSR DEC_REFCNT_M1_SLOT_RA)
           (LDA !$00)
           (STA ZP_RA)
           ;; (STA ZP_RA+1)
           (JMP VM_INTERPRETER_INC_PC)

    (label BC_SET_ARRAY_FIELD) ;; Write TOS-1 -> RT.@A, popping
           (LSR)
           (AND !$03)
           (JSR CP_RT_TO_RA)
           (TAX)
           (JSR POP_EVLSTK_TAIL_TO_RT)
           (TXA)
           (JSR COPY_ARR_ATa_RA_TO_RZ__IF_PTR)
           (JSR POP_EVLSTK_TO_ARR_ATa_RA)
           (LDA ZP_RZ)
           (BEQ continue__) ;; if old array entry was no ptr, this is 0
           (JSR DEC_REFCNT_M1_SLOT_RZ)  ;; decrement overwritte (old) array entry
    (label continue__)
           (JSR DEC_REFCNT_M1_SLOT_RA)
           (LDA !$00)
           (STA ZP_RA)
           ;; (STA ZP_RA+1) ;; not necessary
           (JMP VM_INTERPRETER_INC_PC)))

(define BC_WRITE_RA '())
(define-vm-function BC_PUSH_RA
  (list
          (JSR PUSH_RT_TO_EVLSTK_TAIL)
   (label BC_WRITE_RA) ;; TODO: before overwriting rt, it needs to be checked if it is a pointer (no check if pushed before!)
          (JSR CP_RA_TO_RT)
          (JSR INC_REFCNT_M1_SLOT_RT) ;; RA can only be a ptr -> rt is one too
          (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_PUSH_RA_AF
  (list
          (JSR PUSH_RT_TO_EVLSTK_TAIL)
          (LDA ZP_RAI)
          (JSR WRITE_ARR_ATa_RA_TO_RT)
          (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
          (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_POP_TO_RA_AF
  (list
          (LDA ZP_RAI)
          (JSR COPY_ARR_ATa_RA_TO_RZ__IF_PTR)
          (JSR POP_EVLSTK_TO_ARR_ATa_RA)
          (LDA ZP_RZ)
          (BEQ continue__) ;; if old array entry was no ptr, this is 0
          (JSR DEC_REFCNT_M1_SLOT_RZ)     ;; decrement overwritte (old) array entry
    (label continue__)
          (INC ZP_RAI)
          (JMP VM_INTERPRETER_INC_PC)))

;; stack: index (byte) :: cell-ptr -> cell-array
;; ->     value (cell)
(define-vm-function BC_PUSH_AF
   (list
           ;; decrement ra before overwriting
           (JSR DEC_REFCNT_M1_SLOT_RA)
           (JSR POP_EVLSTK_TAIL_TO_RA)            ;; ra = cell-ptr -> cell-array         (stack: index)
           (LDA ZP_RT+1)                          ;; index                               (stack: index)
           (JSR WRITE_ARR_ATa_RA_TO_RT)           ;; rt <- array@a                       (stack: value)
           (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)    ;; now on stack and in array => inc refcnt'd
           ;; (JSR DEC_REFCNT_M1_SLOT_RA)         ;; no decrement, since it is still in RA (and can be reused)
           (JMP VM_INTERPRETER_INC_PC)))

;; stack: index(byte) :: cell-ptr->cell-array  :: value (cell)
;; ->     []
;;        cell-array @ index = value
(define-vm-function BC_POP_TO_AF
   (list
           (LDA ZP_RT+1)                  ;; index                               (stack: index ::cell-ptr ::value )
           (PHA)
           (JSR POP_EVLSTK_TAIL_TO_RA)    ;; ra = cell-ptr -> cell-array         (stack: index ::value )
           (JSR POP_EVLSTK_TAIL_TO_RT)    ;; rt = value                          (stack: value)
           (PLA)                          ;; a = index

           (JSR COPY_ARR_ATa_RA_TO_RZ__IF_PTR)
           (JSR POP_EVLSTK_TO_ARR_ATa_RA)
           (LDA ZP_RZ)
           (BEQ continue__) ;; if old array entry was no ptr, this is 0
           (JSR DEC_REFCNT_M1_SLOT_RZ)  ;; decrement overwritte (old) array entry
    (label continue__)

           (JSR DEC_REFCNT_M1_SLOT_RA)            ;; since array is no longer on stack dec refcnt (value moved => no change)
           (JMP VM_INTERPRETER_INC_PC)))

(define-vm-function BC_SWAP_RA_RB
  (list
          (JSR SWAP_RA_RB)
          (JMP VM_INTERPRETER_INC_PC)))

;; TODO: should be moved to ./vm-cell-array.rkt
(define-vm-function VM_REFCOUNT_DECR_ARRAY_REGS
  (list
          (LDA ZP_RA)
          (BEQ DONE__)
          (JSR DEC_REFCNT_M1_SLOT_RA)
   (label TRY_RB__)
          (LDA ZP_RB)
          (BEQ CLEAR_RA__)
          (JSR DEC_REFCNT_M1_SLOT_RB)
   (label TRY_RC__)
          (LDA ZP_RC)
          (BEQ CLEAR_RAB__)
          (JSR DEC_REFCNT_M1_SLOT_RC)
          (LDA !$00)
          (STA ZP_RC)
          ;; (STA ZP_RC+1) ;; can most probably be optimized away (if dec refcnt checks 0 in low byte)
   (label CLEAR_RAB__)
          (STA ZP_RB)
          ;; (STA ZP_RB+1) ;; can most probably be optimized away (if dec refcnt checks 0 in low byte)
   (label CLEAR_RA__)
          (STA ZP_RA)
          ;; (STA ZP_RA+1) ;; can most probably be optimized away (if dec refcnt checks 0 in low byte)
   (label DONE__)
          (RTS)))

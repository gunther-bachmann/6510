#lang racket/base

(provide BC_DEC_RBI_NZ_P_BRA            ;; decrement cell array index register RBI and branch if NOT Zero
         BC_DEC_RAI                     ;; decrement cell array index register RAI
         BC_WRITE_TO_RBI                ;; write tos byte cell array index register RBI
         BC_WRITE_TO_RAI                ;; write tos byte into cell array index register RAI
         BC_POP_TO_RAI                  ;; pop tos byte into cell array index register RAI
         BC_BINC_RAI                    ;; increment cell array index register RAI
         BC_ALLOC_ARA
         BC_XET_RA_ARRAY_FIELD
         BC_GET_RA_ARRAY_FIELD
         BC_SET_RA_ARRAY_FIELD
         BC_GET_ARRAY_FIELD
         BC_SET_ARRAY_FIELD
         BC_XET_ARRAY_FIELD
         BC_WRITE_RA                    ;; write cell-array register RA into tos
         BC_PUSH_RA                     ;; push cell-array register RA itself onto eval stack
         BC_PUSH_RA_AF                  ;; push cell-array RA field A onto the eval stack (inc ref count)
         BC_POP_TO_RA_AF                ;; pop tos into cell-array RA field A onto the eval stack (TODO: ref count old content!)
         BC_PUSH_AF                     ;; push array field (stack: index :: cell-array-ptr)
         BC_POP_TO_AF                   ;; pop tos to array field (stack: index :: cell-ptr->cell-array  :: value )
         BC_SWAP_RA_RB                  ;; swap cell array register RA with RB
         VM_REFCOUNT_DECR_ARRAY_REGS
         )

(require (only-in racket/list flatten)
         "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt" add-label-suffix)
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
                  PUSH_RT_TO_EVLSTK)
         (only-in "../vm-runtime/vm-register-functions.rkt" SWAP_RA_RB)
         (only-in "./branch.rkt"
                  BRANCH_BY_NEXT_BYTE__NO_POP))

(define BC_DEC_RBI_NZ_P_BRA
  (add-label-suffix
   "__" "__BC_DEC_RBI_NZ_P_BRA"
   (flatten
    (list
     (label BC_DEC_RBI_NZ_P_BRA)
            (DEC ZP_RBI)
            (BEQ NO_BRA__) ;; == 0 => no branch
            (JMP BRANCH_BY_NEXT_BYTE__NO_POP)
     (label NO_BRA__)
            (JMP VM_INTERPRETER_INC_PC_2_TIMES)))))

(define BC_GET_RA_ARRAY_FIELD '())
(define BC_SET_RA_ARRAY_FIELD '())
(define BC_XET_RA_ARRAY_FIELD
  (flatten
   (list
    (label BC_GET_RA_ARRAY_FIELD)               ;; (RA),A -> RT
           (LSR)
           (AND !$03)
           (PHA)
           (JSR PUSH_RT_TO_EVLSTK)
           (PLA)
           (JSR WRITE_ARR_ATa_RA_TO_RT)
           (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
           (JMP VM_INTERPRETER_INC_PC)

    (label BC_SET_RA_ARRAY_FIELD)               ;; RT -> (RA),A
           (LSR)
           (AND !$03)
           (JSR COPY_ARR_ATa_RA_TO_RZ__IF_PTR) ;; keep overwritte field (if ptr)
           (JSR POP_EVLSTK_TO_ARR_ATa_RA)       ;; no refcount adjustment, since value is off the stack (-1), but in array (+1)
           (LDA ZP_RZ)
           (BEQ continue__BC_SET_RA_ARRAY_FIELD)
           (JSR DEC_REFCNT_M1_SLOT_RZ) ;; decrement overwritten field (if it was a ptr)
    (label continue__BC_SET_RA_ARRAY_FIELD)
           (JMP VM_INTERPRETER_INC_PC))))

(define BC_ALLOC_ARA
  (list
   (label BC_ALLOC_ARA)
          (LDA ZP_RT+1)                 ;; byte size
          (JSR ALLOC_CELL_ARRAY_TO_RA)     ;;
          (LDA !$00)
          (STA ZP_RAI)
          ;; init tagged lowbytes with with zeros
          (LDA ZP_RT+1)
          (ASL A)
          (TAY)

   ;; optional: initialize lowbyte of cells with 0
   ;;        (LDA !$00)
   ;; (label loop__BC_ALLOC_ARA)
   ;;        (STA (ZP_RA),y)
   ;;        (DEY)
   ;;        (DEY)
   ;;        (BNE loop__BC_ALLOC_ARA)

          (JMP VM_POP_EVLSTK_AND_INC_PC)))

(define BC_BINC_RAI
  (list
   (label BC_BINC_RAI)
          (INC ZP_RAI)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_POP_TO_RAI
  (list
   (label BC_POP_TO_RAI)
          (LDA ZP_RT+1)
          (STA ZP_RAI)
          (JMP VM_POP_EVLSTK_AND_INC_PC)))

(define BC_WRITE_TO_RAI
  (list
   (label BC_WRITE_TO_RAI)
          (LDA ZP_RT+1)
          (STA ZP_RAI)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_WRITE_TO_RBI
  (list
   (label BC_WRITE_TO_RBI)
          (LDA ZP_RT+1)
          (STA ZP_RBI)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_DEC_RAI
  (list
   (label BC_DEC_RAI)
          (DEC ZP_RAI)
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_GET_ARRAY_FIELD '())
(define BC_SET_ARRAY_FIELD '())
(define BC_XET_ARRAY_FIELD
  (flatten
   (list
    (label BC_GET_ARRAY_FIELD) ;; replace RT with RT.@A
           (LSR)
           (AND !$03)
           (PHA)
           (JSR CP_RT_TO_RA)
           (PLA)
           (JSR WRITE_ARR_ATa_RA_TO_RT)
           (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
           (JSR DEC_REFCNT_M1_SLOT_RA)
           (LDA !$00)
           (STA ZP_RA)
           (STA ZP_RA+1)
           (JMP VM_INTERPRETER_INC_PC)

    (label BC_SET_ARRAY_FIELD) ;; Write TOS-1 -> RT.@A, popping
           (LSR)
           (AND !$03)
           (PHA)
           (JSR CP_RT_TO_RA)
           (JSR POP_CELL_EVLSTK_TO_RT)
           (PLA)
           (JSR COPY_ARR_ATa_RA_TO_RZ__IF_PTR)
           (JSR POP_EVLSTK_TO_ARR_ATa_RA)
           (LDA ZP_RZ)
           (BEQ continue__BC_SET_ARRAY_FIELD) ;; if old array entry was no ptr, this is 0
           (JSR DEC_REFCNT_M1_SLOT_RZ)  ;; decrement overwritte (old) array entry
    (label continue__BC_SET_ARRAY_FIELD)
           (JSR DEC_REFCNT_M1_SLOT_RA)
           (LDA !$00)
           (STA ZP_RA)
           ;; (STA ZP_RA+1) ;; not necessary
           (JMP VM_INTERPRETER_INC_PC))))

(define BC_WRITE_RA '())
(define BC_PUSH_RA
  (list
   (label BC_PUSH_RA)
          (JSR PUSH_RT_TO_EVLSTK)
   (label BC_WRITE_RA)
          (JSR CP_RA_TO_RT)
          (JSR INC_REFCNT_M1_SLOT_RT) ;; RA can only be a ptr -> rt is one too
          (JMP VM_INTERPRETER_INC_PC)))

(define BC_PUSH_RA_AF
  (list
   (label BC_PUSH_RA_AF)
          (JSR PUSH_RT_TO_EVLSTK)
          (LDA ZP_RAI)
          (JSR WRITE_ARR_ATa_RA_TO_RT)
          (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)
          (JMP VM_INTERPRETER_INC_PC)))


(define BC_POP_TO_RA_AF
  (list
   (label BC_POP_TO_RA_AF)
          (LDA ZP_RAI)
          (JSR COPY_ARR_ATa_RA_TO_RZ__IF_PTR)
          (JSR POP_EVLSTK_TO_ARR_ATa_RA)
          (LDA ZP_RZ)
          (BEQ continue__BC_POP_TO_RA_AF) ;; if old array entry was no ptr, this is 0
          (JSR DEC_REFCNT_M1_SLOT_RZ)     ;; decrement overwritte (old) array entry
    (label continue__BC_POP_TO_RA_AF)
          (INC ZP_RAI)
          (JMP VM_INTERPRETER_INC_PC)))

;; stack: index (byte) :: cell-ptr -> cell-array
;; ->     value (cell)
(define BC_PUSH_AF
  (flatten
   (list
    (label BC_PUSH_AF)
           (JSR POP_CELL_EVLSTK_TO_RA)            ;; ra = cell-ptr -> cell-array         (stack: index)
           (LDA ZP_RT+1)                          ;; index                               (stack: index)
           (JSR WRITE_ARR_ATa_RA_TO_RT)           ;; rt <- array@a                       (stack: value)
           (JSR INC_REFCNT_M1_SLOT_RT__IF_PTR)    ;; now on stack and in array => inc refcnt'd
           (JSR DEC_REFCNT_M1_SLOT_RA)            ;; removed from stack => dec refcnt'd
           (JMP VM_INTERPRETER_INC_PC))))

;; stack: index(byte) :: cell-ptr->cell-array  :: value (cell)
;; ->     []
;;        cell-array @ index = value
(define BC_POP_TO_AF
  (flatten
   (list
    (label BC_POP_TO_AF)
           (LDA ZP_RT+1)                  ;; index                               (stack: index ::cell-ptr ::value )
           (PHA)
           (JSR POP_CELL_EVLSTK_TO_RA)    ;; ra = cell-ptr -> cell-array         (stack: index ::value )
           (JSR POP_CELL_EVLSTK_TO_RT)    ;; rt = value                          (stack: value)
           (PLA)                          ;; a = index

           (JSR COPY_ARR_ATa_RA_TO_RZ__IF_PTR)
           (JSR POP_EVLSTK_TO_ARR_ATa_RA)
           (LDA ZP_RZ)
           (BEQ continue__BC_POP_TO_AF) ;; if old array entry was no ptr, this is 0
           (JSR DEC_REFCNT_M1_SLOT_RZ)  ;; decrement overwritte (old) array entry
    (label continue__BC_POP_TO_AF)

           (JSR DEC_REFCNT_M1_SLOT_RA)            ;; since array is no longer on stack dec refcnt (value moved => no change)
           (JMP VM_INTERPRETER_INC_PC))))

(define BC_SWAP_RA_RB
  (list
   (label BC_SWAP_RA_RB)
          (JSR SWAP_RA_RB)
          (JMP VM_INTERPRETER_INC_PC)))

;; TODO: should be moved to ./vm-cell-array.rkt
(define VM_REFCOUNT_DECR_ARRAY_REGS
  (add-label-suffix
   "__" "__VM_REFCOUNT_DECR_ARRAY_REGS"
  (list
   (label VM_REFCOUNT_DECR_ARRAY_REGS)
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
          (STA ZP_RC+1) ;; can most probably be optimized away (if dec refcnt checks 0 in low byte)
   (label CLEAR_RAB__)
          (STA ZP_RB)
          (STA ZP_RB+1) ;; can most probably be optimized away (if dec refcnt checks 0 in low byte)
   (label CLEAR_RA__)
          (STA ZP_RA)
          (STA ZP_RA+1) ;; can most probably be optimized away (if dec refcnt checks 0 in low byte)
   (label DONE__)
          (RTS))))

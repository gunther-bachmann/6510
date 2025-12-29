#lang racket/base

#|

define code and data that is used to execute extended byte code commands
the implementation of these is defined elsewhere, here is the implementation
of the decoder and the jump

|#

(require (only-in racket/list
                  flatten)
         "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "../vm-interpreter-loop.rkt"
                  ZP_VM_PC))

(provide BC_EXT1_CMD
         VM_INTERPRETER_OPTABLE_EXT1_LB
         VM_INTERPRETER_OPTABLE_EXT1_HB)

(define BC_EXT1_CMD
  (list
   (label BC_EXT1_CMD)
          (LDY !$01)
          (LDA (ZP_VM_PC),y) ;; get second command byte
          (TAY)
   ;;        (LDA VM_INTERPRETER_OPTABLE_EXT1_HB,y)
   ;;        (STA CALL_COMMAND__BC_EXT1_CMD+2)
   ;;        (LDA VM_INTERPRETER_OPTABLE_EXT1_LB,y) ;; use as index and read jump address
   ;;        (STA CALL_COMMAND__BC_EXT1_CMD+1)
   ;; (label CALL_COMMAND__BC_EXT1_CMD)
   ;;        (JMP $cf00) ;; is overwritten with table data read before

;; could be optimized
;; !! JUMP TARGETS MUST point 1 byte before actual routine !!
;; !! RTS adds one to the address                          !!
;; save 6 bytes and 1 cycle
          (LDA VM_INTERPRETER_OPTABLE_EXT1_HB,y)
          (PHA)
          (LDA VM_INTERPRETER_OPTABLE_EXT1_LB,y)
          (PHA)
          (RTS)
          ))

;; the jump table lowbyte is filled by codes defined in vm-bc-opcode-definitions.rkt
(define VM_INTERPRETER_OPTABLE_EXT1_LB
  (list
   (label VM_INTERPRETER_OPTABLE_EXT1_LB)))

;; the jump table lowbyte is filled by codes defined in vm-bc-opcode-definitions.rkt
(define VM_INTERPRETER_OPTABLE_EXT1_HB
  (list
   (label VM_INTERPRETER_OPTABLE_EXT1_HB)))

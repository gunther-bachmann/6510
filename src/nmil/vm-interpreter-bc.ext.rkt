#lang racket/base

#|

define code and data that is used to execute extended byte code commands
the implementation of these is defined elsewhere, here is the implementation
of the decoder and the jump

|#

(require "../6510.rkt")
(require (only-in "../ast/6510-resolver.rkt" add-label-suffix))
(require (only-in racket/list flatten))

(require (only-in "./vm-memory-map.rkt"
                  ZP_VM_PC))

(provide BC_EXT1_CMD
         VM_INTERPRETER_OPTABLE_EXT1_LB
         VM_INTERPRETER_OPTABLE_EXT1_HB)

(define BC_EXT1_CMD
  (list
   (label BC_EXT1_CMD)
          (INY)
          (LDA (ZP_VM_PC),y)
          (TAY)
          (LDA VM_INTERPRETER_OPTABLE_EXT1_LB,y)
          (STA CALL_COMMAND__BC_EXT1_CMD+1)
          (LDA VM_INTERPRETER_OPTABLE_EXT1_HB,y)
          (STA CALL_COMMAND__BC_EXT1_CMD+2)
   (label CALL_COMMAND__BC_EXT1_CMD)
          (JMP $cf00) ;; is overwritten with table data read before
          ))

;; the jump table lowbyte is filled by codes defined in vm-bc-opcode-definitions.rkt
(define VM_INTERPRETER_OPTABLE_EXT1_LB
  (list
   (label VM_INTERPRETER_OPTABLE_EXT1_LB)))

;; the jump table lowbyte is filled by codes defined in vm-bc-opcode-definitions.rkt
(define VM_INTERPRETER_OPTABLE_EXT1_HB
  (list
   (label VM_INTERPRETER_OPTABLE_EXT1_HB)))

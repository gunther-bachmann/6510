#lang racket/base

(provide BIOS_PUT_STRING_AT

         vm-bios-code)

(require "../../6510.rkt"
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC_2_TIMES)
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function-wol
                  define-vm-function)
         (only-in "../vm-runtime/vm-cell-stack.rkt"
                  POP_EVLSTK_TAIL_TO_RT
                  UNSAFE_POP_EVLSTK_TAIL_TO_HBA)
         (only-in "../vm-runtime/vm-m1-slots.rkt"
                  DEC_REFCNT_M1_SLOT_RZ))

(module+ test #| require |#
  (require (only-in racket/string
                    string-replace)
           (only-in uuid
                    uuid-string)
           "../../6510-test-utils.rkt"
           (only-in "../../ast/6510-relocator.rkt"
                    estimated-code-len)
           (only-in "../../tools/6510-interpreter.rkt"
                    cpu-state-clock-cycles
                    memory-list)
           "./vm-memory-manager-test-utils.rkt"
           (only-in "./vm-memory-map.rkt"
                    VM_MEMORY_MANAGEMENT_CONSTANTS
                    ZP_RP)
           (only-in "../vm-runtime/vm-screen.rkt"
                    vm-screen-code)
           (only-in "../vm-runtime/vm-bcd.rkt"
                    vm-bcd-code))

  (define test-runtime
    (append
     BIOS_PUT_STRING_AT

     VM_MEMORY_MANAGEMENT_CONSTANTS
     vm-screen-code
     vm-bcd-code
     (list (label VM_INIT_MEMORY_MANAGER) (RTS)))))

;; POP Ptr->MEM
;; POP byte->X
;; POP byte->Y
;; POP byte->MEM
;; POP byte->A
;; POP Ptr->X/Y ?

;; stack: native-array(string)::col(byte)::row(byte):: -> ::
(define-vm-function BIOS_PUT_STRING_AT
  (list
          (LDY ZP_RT)
          (INY)
          (INY) ;; add slot overhead to data, overflow cannot happen (native array always within one page)
          (STY RT_SCREEN_PUT_CHARS_AT__STRING+1)
          (LDA ZP_RT+1)
          (STA RT_SCREEN_PUT_CHARS_AT__STRING+2) ;; ptr to string is set
          (LDY !$01)
          (LDA (ZP_RT),y)
          (AND !$3f) ;; get length of array
          (PHA) ;; # to print
          (JSR UNSAFE_POP_EVLSTK_TAIL_TO_HBA) ;; don't gc this value, it must be valid until after the call!
          (STA ZP_RP) ;; col is set
          (JSR UNSAFE_POP_EVLSTK_TAIL_TO_HBA)
          (TAX) ;; x = row
          (JSR POP_EVLSTK_TAIL_TO_RT)
          (PLA)
          (TAY) ;; y = # to print

   ;; optional check for enough space on line (else it would write into the next)
   ;;        (CLC)
   ;;        (ADC ZP_RP) ;; check for col overflow
   ;;        (CMP !40)
   ;;        (BMI no_adjust__)
   ;;        (LDA !40)
   ;;        (SEC)
   ;;        (SBC ZP_RP)
   ;;        (TAY)       ;; only up to end of line
   ;; (label no_adjust__)

;; input:  x = ROW
;;         ZP_RP = COL
;;         RT_SCREEN_PUT_CHARS_AT__STRING+1 = ptr to screen code data (low at +1, high at +2)
;;         y = # of chars to print -1 (0 for one char, 1 for two ...)
          (DEY) ;; print # -1
          (JSR RT_SCREEN_PUT_CHARS_AT)
          ;; now get pointer and dec refcount (since it was popped)
          (LDY RT_SCREEN_PUT_CHARS_AT__STRING+1)
          (DEY)
          (DEY) ;; subtract slot offset, underflow cannot happen, are on the same page!
          (STY ZP_RZ)
          (LDA RT_SCREEN_PUT_CHARS_AT__STRING+2) ;; ptr to string is set
          (STA ZP_RZ+1)
          (JMP DEC_REFCNT_M1_SLOT_RZ)))

(define vm-bios-code
  (append BIOS_PUT_STRING_AT))

(module+ test #| bios code len |#
  (inform-check-equal? (estimated-code-len vm-bios-code)
                       57
                       "estimated bios code len"))

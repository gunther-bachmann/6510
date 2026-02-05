#lang racket/base

(provide BIOS_PUT_STRING_AT

         vm-bios-code)

(require "../../6510.rkt"
         (only-in "../vm-interpreter-loop.rkt"
                  VM_INTERPRETER_INC_PC_2_TIMES)
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function-wol
                  define-vm-function))

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

;; stack: native-array(string)::col(byte)::row(byte):: -> ::
(define-vm-function BIOS_PUT_STRING_AT
  (list
          (LDA ZP_RT)
          (CLC)
          (ADC !$02)   ;; overflow cannot haben (native array always within one page)
          (STA RT_SCREEN_PUT_CHARS_AT__STRING+1)
          (LDA ZP_RT+1)
          (STA RT_SCREEN_PUT_CHARS_AT__STRING+2) ;; ptr to string is set
          (LDY !$01)
          (LDA (ZP_RT),y)
          (AND !$3f) ;; get length of array
          (PHA) ;; # to print
          (JSR POP_EVLSTK_TAIL_TO_RT)
          (LDA ZP_RT+1)
          (STA ZP_RP) ;; col is set
          (JSR POP_EVLSTK_TAIL_TO_RT)
          (LDA ZP_RT+1)
          (TAX) ;; x = row
          (PLA)
          (TAY) ;; y = # to print
          (CLC)
          (ADC ZP_RP) ;; check for col overflow
          (CMP !40)
          (BMI no_adjust__)
          (LDA !40)
          (SEC)
          (SBC ZP_RP)
          (TAY)       ;; only up to end of line
   (label no_adjust__)
;; input:  x = ROW
;;         ZP_RP = COL
;;         RT_SCREEN_PUT_CHARS_AT__STRING+1 = ptr to screen code data (low at +1, high at +2)
;;         y = # of chars to print -1 (0 for one char, 1 for two ...)
          (DEY) ;; print # -1
          (JSR RT_SCREEN_PUT_CHARS_AT)))

(define vm-bios-code
  (append BIOS_PUT_STRING_AT))

(module+ test #| bios code len |#
  (inform-check-equal? (estimated-code-len vm-bios-code)
                       59
                       "estimated bios code len"))

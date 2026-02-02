#lang racket/base

(provide
 RT_INT8_TO_BCD                  ;; convert byte into bcd (max 3 digits)

 vm-bcd-code
 )

#|

 bcd routines

 | bytes | desc  |  max value | bcd size  |
 |-------+-------+------------+-----------|
 |     1 | int8  |        256 | 1,5 bytes |
 |     2 | int16 |      65536 | 2,5 bytes |
 |     3 | int24 |   16777216 | 4 bytes   |
 |     4 | int32 | 4294967296 | 5 bytes   |


 see http://www.6502.org/users/mycorner/6502/shorts/bin2bcd.html

 |#

(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol))


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
                    ZP_RP))

  (define test-runtime
    (append
     RT_INT8_TO_BCD

     VM_MEMORY_MANAGEMENT_CONSTANTS
     (list (label VM_INIT_MEMORY_MANAGER) (RTS)))))


;; convert int in A to BCD digits into ZP_RP, and ZP_RP+1
(define-vm-function RT_INT8_TO_BCD
  (list

   (label bin_2_bcd__)
          (SED)
          (STA    ZP_RP)
          (LDA    !$00)
          (LDX    !$07)
   (label bit_loop__)
          (LSR    ZP_RP)
          (BCC    skip_add__)

          (ADC    b2b_table-1,x)
   (label skip_add__)
          (DEX)
          (BNE    bit_loop__)

;***********************************************************************
; if you only require conversion of numbers between $00 and $63 (0 to 99
; decimal) then omit all code between the "*"s

          (BCC    skip_100__)    ; branch if no 100's carry
                                ; if Cb set here (and can only be set by the
                                ; last loop add) then there was a carry into
          (INX)            ; the 100's so add 100's carry to the high byte
   (label skip_100__)
                ; now check the 2^7 (128) bit
          (LSR    ZP_RP)        ; bit 7 to carry
          (BCC    skip_fin__)    ; branch if no add

          (INX)            ; else effectively add 100 part of 128
          (ADC    !$27)        ; and then add 128 (-1) part of 128
          (BCC    skip_fin__)    ; branch if no further carry

          (INX)            ; else add 200's carry
  (label  skip_fin__)
          (STX    ZP_RP+1)        ; save result high byte

; end of 100's code
;***********************************************************************

          (STA ZP_RP)        ; save result low byte
          (CLD)
          (RTS)

   ; table of BCD values for each binary bit, put this somewhere.
   ; note! values are -1 as the ADC is always done with the carry set
   (label b2b_table)
          (byte $63 $31 $15 $07 $03 $01 $00)))

(module+ test #| require |#
  (define int8-to-bcd2-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (LDA !243)
     (JSR RT_INT8_TO_BCD)))

  (check-equal? (memory-list int8-to-bcd2-test ZP_RP (+ 1 ZP_RP))
                (list #x43 #x02))
  (inform-check-equal? (cpu-state-clock-cycles int8-to-bcd2-test)
                       129
                       "needed cycles for int8->bcd"))

;; convert byte into bcd (max 3 digits)
#;(define-vm-function RT_INT8_TO_BCD_DEP
  (list
            (STA ZP_TEMP)

            (LDA !0)
            (STA ZP_RP)
            (STA ZP_RP+1)

            ; 8 SHIFTS
            (LDX !8)

     (label LOOP)
            ; SHIFT TARGET LEFT
            (CLC)
            (ASL ZP_TEMP)
            (ROL ZP_RP) ;; shift carry into out
            (ROL ZP_RP+1)   ;; shift carry from out to out+1 // happens first after x >= 6

            (DEX)
            (BEQ DONE)

     (label U1)
            ; SHIFT ONE'S - ADD 3 IF NEEDED
            (LDA ZP_RP)
            (AND !$0F)
            (CMP !$05)
            (BMI U10)
            (LDA !$03)
            (CLC)
            (ADC ZP_RP)
            (STA ZP_RP)

     (label U10)
            ; SHIFT TEN'S - ADD 3 IF NEEDED
            (LDA ZP_RP)
            (AND !$F0)
            (CMP !$50)
            (BMI LOOP)
            (LDA !$30)
            (CLC)
            (ADC ZP_RP)
            (STA ZP_RP)
            (BNE LOOP) ;; always jump

     (label DONE)
            (RTS)))

#;(module+ test #| require |#
  (define int8-to-bcd-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (LDA !243)
     (JSR RT_INT8_TO_BCD_DEP)))

  (check-equal? (memory-list int8-to-bcd-test ZP_RP (+ 1 ZP_RP))
                (list #x43 #x02))
  (inform-check-equal? (cpu-state-clock-cycles int8-to-bcd-test)
                       363
                       "needed cycles for int8->bcd"))

(define vm-bcd-code
  (append RT_INT8_TO_BCD))

(module+ test #| code len |#
  (inform-check-equal? (estimated-code-len vm-bcd-code)
                       48
                       "estimated code len"))

#lang racket/base

#|

 functions to benchmark code on real hardware c64

 |#


(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
         (only-in "./vm-screen.rkt"
                  RT_SCREEN_CLEAR
                  RT_SCREEN_PUT_CHARS_AT
                  RT_SCREEN_PUT_CHARS_AT__STRING
                  RT_BCD_TO_SCREEN_CODE)
         (only-in "./vm-bcd.rkt"
                  RT_INT8_TO_BCD))

;; start the timer on the c64
(define-vm-function BM_START_TIMER
  (list
          (SEI)
          (LDA !0)
          (STA $A2)       ; Real-time jiffy Clock
          (STA $A1)       ; Real-time jiffy Clock
          (STA $A0)       ; Real-time jiffy Clock
          (CLI)
          (RTS)))

;; start the timer on the c64
(define-vm-function BM_STOP_TIMER
  (list
          (SEI)
          (LDA $A2)       ; Real-time jiffy Clock
          (STA TIMER_VAL+2)
          (LDA $A1)       ; Real-time jiffy Clock
          (STA TIMER_VAL+1)
          (LDA $A0)       ; Real-time jiffy Clock
          (STA TIMER_VAL)
          (CLI)
          (RTS)

   (label TIMER_VAL)
          (byte 0 0 0)))

;; report the timer on screen and wait for keypress
(define-vm-function BM_REPORT_TIMER
  (list
          (JSR RT_SCREEN_CLEAR)

          (LDA TIMER_VAL)
          (JSR RT_INT8_TO_BCD)
          (LDX !0)    ;; col
          (LDA !0)    ;; row
          (JSR PRINT_BCD_TO_AX__)

          (LDA TIMER_VAL+1)
          (JSR RT_INT8_TO_BCD)
          (LDX !4)
          (LDA !0)    ;; row
          (JSR PRINT_BCD_TO_AX__)

          (LDA TIMER_VAL+2)
          (JSR RT_INT8_TO_BCD)
          (LDX !8)    ;; col
          (LDA !0)    ;; row
          (JSR PRINT_BCD_TO_AX__)

          (JMP BM_WAIT_FOR_KEYPRESS)

   (label PRINT_BCD_TO_AX__)
          (STX ZP_RP)
          (TAX)
          (LDA !<OUT__RT_INT8_TO_BCD)
          (STA RT_SCREEN_PUT_CHARS_AT__STRING+1)
          (LDA !>OUT__RT_INT8_TO_BCD)
          (STA RT_SCREEN_PUT_CHARS_AT__STRING+2)
          (LDY !2)    ;; 3 chars
          (JMP RT_SCREEN_PUT_CHARS_AT)))

;; wait for key
(define-vm-function BM_WAIT_FOR_KEYPRESS
  (list
          ; Keyboard
          (word-const SCNKEY  $ff9f)  ; Scan keyboard
          (word-const GETIN   $ffe4)  ; Get a character from keyboard
          (word-const XMAX    $0289)  ; Maximum Keyboard Buffer Size
          (word-const RPTFLA  $028a)  ; Keyboard Repeat Flag

          ; KEYBOARD BUFFER SIZE
          (LDA !1)
          (STA XMAX)

          ; NO KEY REPEAT
          (LDA !$40)
          (STA RPTFLA)

          ; LOOP ON KEYPRESS
   (label LOOP__)
          (JSR SCNKEY)
          (JSR GETIN)
          (BEQ LOOP__)

          (RTS)))

;; execute a basic warm start
(define-vm-function BM_BASIC_WARM_START
  (list (JMP ($A002))))

(define benchmark-code
  (append
   BM_STOP_TIMER
   BM_START_TIMER
   BM_REPORT_TIMER
   BM_WAIT_FOR_KEYPRESS
   BM_BASIC_WARM_START))

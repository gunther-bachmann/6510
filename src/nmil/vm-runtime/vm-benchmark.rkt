#lang racket/base

(provide BM_START_TIMER
         BM_STOP_TIMER
         BM_REPORT_TIMER
         BM_WAIT_FOR_KEYPRESS
         BM_RESET

         vm-benchmark-code)

#|

 functions to benchmark code on real hardware c64

 measurements:
   right: 077 jiffies (39*)  73 (video switched off) (still with interrupts)
   down:  048         (24*)  45
   up:    057         (24*)  54
   left : 092         (39*)  87

   optimized:
   right: 063 jiffies
   down:  039
   up:    043
   left : 070


   down
   measured (/ 48 60.0)
     0.8

   up  delta (* 100.0 (- (/ 0.95 0.831) 1)) ~= 14 %
   measured (/ 57 60.0)
     0.95
   calculated: (/ (* 24.0 34658.0) 1000000.0)
     0.831792

   left    delta (* 100.0 (- (/ 1.533333333333334 1.373502) 1)) ~= 12 %
   measured (/ 92 60.0)
     1.533333333333334
   calculated (/ (* 39.0 35218.0) 1000000.0)
     1.373502

   right    delta (* 100.0 (- (/ 1.2833333333333334 1.147068) 1)) ~= 12 %
   measured (/ 77 60.0)
     1.2833333333333334
   calculated (/ (* 39 29412.0) 1000000.0)
     1.147068

 |#


(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
         (only-in "./vm-screen.rkt"
                  RT_SCREEN_CLEAR
                  RT_SCREEN_PUT_CHARS_AT
                  RT_SCREEN_PUT_CHARS_AT__STRING
                  RT_BCD_TO_SCREEN_CODE
                  RT_SCREEN_SCROLL_LEFT_CHARS_AT_BY1
                  RT_SCREEN_SCROLL_RIGHT_CHARS_AT
                  RT_SCREEN_SCROLL_DOWN_BY1
                  RT_SCREEN_SCROLL_UP_BY1)
         (only-in "./vm-bcd.rkt"
                  RT_INT8_TO_BCD))

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
     BM_START_TIMER
     BM_STOP_TIMER
     BM_REPORT_TIMER

     VM_MEMORY_MANAGEMENT_CONSTANTS
     vm-screen-code
     vm-bcd-code
     (list (label VM_INIT_MEMORY_MANAGER) (RTS)))))

;; start the timer on the c64
(define-vm-function BM_START_TIMER
  (list
          (SEI)
          ;; (LDA $D011)     ; switch off video (vic status reg)
          ;; (AND !$EF)
          ;; (STA $D011)
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
          ;; (LDA $D011)     ; switch on video (vic status reg)
          ;; (EOR !$10)
          ;; (STA $D011)
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
          (JSR RT_BCD_TO_SCREEN_CODE)
          (LDX !0)    ;; col
          (LDA !0)    ;; row
          (JSR PRINT_BCD_TO_AX__)

          (LDA TIMER_VAL+1)
          (JSR RT_INT8_TO_BCD)
          (JSR RT_BCD_TO_SCREEN_CODE)
          (LDX !4)
          (LDA !0)    ;; row
          (JSR PRINT_BCD_TO_AX__)

          (LDA TIMER_VAL+2)
          (JSR RT_INT8_TO_BCD)
          (JSR RT_BCD_TO_SCREEN_CODE)
          (LDX !8)    ;; col
          (LDA !0)    ;; row
          ;; print bcd_to_ax__

   (label PRINT_BCD_TO_AX__)
          (STX ZP_RP)
          (TAX)

          (LDA !<OUT__RT_BCD_TO_SCREEN_CODE)
          (STA RT_SCREEN_PUT_CHARS_AT__STRING+1)
          (LDA !>OUT__RT_BCD_TO_SCREEN_CODE)
          (STA RT_SCREEN_PUT_CHARS_AT__STRING+2)
          (LDY !2)    ;; 3 chars
                      ;; row in X
                      ;; col in zp_rp
          (JMP RT_SCREEN_PUT_CHARS_AT)))

(module+ test #| report timer|#
  (define report-timer-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (JSR BM_REPORT_TIMER))))

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
          (STA ZP_RP)

          (RTS)))

(define-vm-function BM_FILL_SCREEN_W_NUMBERS
  (list
          (LDX !$00)
   (label outer_loop__)
          (LDY !$30)
          (TYA)
   (label inner_loop__)
          (DEX)
          (STA $0400,x)
          (STA $0500,x)
          (STA $0600,x)
          (STA $0700,x)
          (STA $d800,x)
          (STA $d900,x)
          (STA $da00,x)
          (STA $db00,x)
          (BEQ done__)
          (INY)
          (TYA)
          (CPY !$39) ;; just up to 8, make scroll better to follow
          (BNE inner_loop__)
          (BEQ outer_loop__)
   (label done__)
          (RTS)))

(define-vm-function BM_SCROLL_RIGHT_40
  (list
          (LDA !40)
          (STA ZP_RT)
   (label LOOP__)
          ;; setup scroll of full screen
          (LDX !24)    ;; start at row 25 (zero indexed) [going up]
          (LDY !0)
          (STY ZP_RP)
          (LDY !38)    ;; copy 39 chars (counter is always 1 -)
          (LDA !25)    ;; scroll 25 rows in total
          (STA ZP_RZ)

          (JSR RT_SCREEN_SCROLL_RIGHT_CHARS_AT_BY1)
          (DEC ZP_RT)
          (BNE LOOP__)
          (RTS)))

(define-vm-function BM_SCROLL_DOWN_25
  (list
          (LDA !25)
          (STA ZP_RT)
   (label LOOP__)
          ;; setup scroll of full screen
          (LDX !23)   ;; start at row 24 (zero indexed) [going up]
          (LDY !0)
          (STY ZP_RP) ;; start col 0
          (LDY !39)   ;; copy 40 chars (counter is always 1 -)
          (LDA !24)   ;; scroll 24 rows in total
          (STA ZP_RZ)

          (JSR RT_SCREEN_SCROLL_DOWN_BY1)
          (DEC ZP_RT)
          (BNE LOOP__)
          (RTS)))

(define-vm-function BM_SCROLL_UP_25
  (list
          (LDA !25)
          (STA ZP_RT)
   (label LOOP__)
          ;; setup scroll of full screen
          (LDX !1)   ;; start at row 2 (zero indexed) [going down]
          (LDY !0)
          (STY ZP_RP) ;; start col 0
          (LDY !40)   ;; copy 40 chars
          (LDA !24)   ;; scroll 24 rows in total
          (STA ZP_RZ)

          (JSR RT_SCREEN_SCROLL_UP_BY1)
          (DEC ZP_RT)
          (BNE LOOP__)
          (RTS)))

(define-vm-function BM_SCROLL_LEFT_40
  (list
          (LDA !40)
          (STA ZP_RT)
   (label LOOP__)
          ;; setup scroll of full screen
          (LDX !0)    ;; start at row 0 (zero indexed) [going down]
          (LDY !1)
          (STY ZP_RP)
          (LDY !39)    ;; copy 39 chars
          (LDA !25)    ;; scroll 25 rows in total
          (STA ZP_RZ)

          (JSR RT_SCREEN_SCROLL_LEFT_CHARS_AT_BY1)
          (DEC ZP_RT)
          (BNE LOOP__)
          (RTS)))

;; execute a machine reset
(define-vm-function BM_RESET
  (list   (JMP $FCE2)))

(define vm-benchmark-code
  (append
   BM_STOP_TIMER
   BM_START_TIMER
   BM_REPORT_TIMER
   BM_WAIT_FOR_KEYPRESS
   BM_RESET
   BM_FILL_SCREEN_W_NUMBERS
   BM_SCROLL_RIGHT_40
   BM_SCROLL_DOWN_25
   BM_SCROLL_UP_25
   BM_SCROLL_LEFT_40))

#lang racket/base

(provide
 RT_SCREEN_PUT_CHARS_AT         ;; write a number of screen codes to y,x position on screen

 screen-code)

#|

 screen output routine runtime for the c64

 useful references:
 - https://github.com/jeff-1amstudios/c64-smooth-scrolling
 - https://github.com/jeff-1amstudios/c64-smooth-scrolling/blob/master/screen.asm
 - https://github.com/cadaver/hessian
 - https://sta.c64.org/cbm64scr.html
 - http://www.6502.org/source/
 - https://www.lemon64.com/page/chapter-3-making-a-front-end-and-putting-everything-together
 - https://github.com/mist64/c64ref
 - https://c64os.com/c64os/programmersguide/usingkernal_screen#screenlayer
 - https://c64os.com/c64os/programmersguide/usingkernal_string
 - https://c64os.com/c64os/programmersguide/usingkernal_memory
 - https://retrocomputing.stackexchange.com/questions/27924/c64-char-screen-plot-routine-not-clear-about-reason-for-logical-or
 - https://www.lemon64.com/page/assembly-chapter-1-building-a-game-prototype

 |#

(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function-wol
                  define-vm-function)
         (only-in "./vm-memory-map.rkt"
                  ZP_TEMP))

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
                    VM_MEMORY_MANAGEMENT_CONSTANTS))

  (define screen-base-address #x0400)
  (define screen-row-bytes 40)

  (define test-runtime
    (append
     PREP_WRITE_SCREEN_CMD
     FAST_SMALL_MEMCOPY
     RT_SCREEN_PUT_CHARS_AT
     RT_SCREEN_CLEAR
     RT_SCREEN_CLEAR_CHARS_AT
     RT_SCREEN_SCROLL_RIGHT_CHARS_AT
     RT_SCREEN_SCROLL_LEFT_CHARS_AT
     RT_SCREEN_SCROLL_UP
     RT_SCREEN_SCROLL_DOWN

     VM_MEMORY_MANAGEMENT_CONSTANTS
     (list (label VM_INIT_MEMORY_MANAGER) (RTS))))

  ;; write characters of this string into screen
  (define (write-string-to-screen--for-test row col str)
    (define uuid-label (string-replace (uuid-string) "-" "_"))
    (define cont-label (string-replace (uuid-string) "-" "_"))
    (list
     (ast-unresolved-opcode-cmd '() '(169) (ast-resolve-byte-scmd uuid-label 'low-byte)) ;;      (LDA !<test_stringy)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+1)
     (ast-unresolved-opcode-cmd '() '(169) (ast-resolve-byte-scmd uuid-label 'high-byte)) ;;      (LDA !>test_stringy)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+2)

     (ast-opcode-cmd '() `(162 ,row)) ;; (LDX !row)
     (ast-opcode-cmd '() `(160 ,col)) ;; (LDY !col)
     (STY ZP_RP)
     (ast-opcode-cmd '() `(160 ,(- (string-length str) 1))) ;; (LDY !n)

     (JSR RT_SCREEN_PUT_CHARS_AT)
     (ast-unresolved-opcode-cmd '() '(76) (ast-resolve-word-scmd cont-label)) ;;      (JMP test_stringy_cont)

     (ast-label-def-cmd '() uuid-label)
     (ast-bytes-cmd '() (map char->integer (string->list str)))
     (ast-label-def-cmd '() cont-label)
     )))

#|

 * fast coordinate calculation:

   table with precalculated row starts (+ page in the lower 3 bits)

      for the scren being located at $0400..$07E8:

      | row | offset | page | complete |
      |-----+--------+------+----------|
      |   0 | $00    | $04  | $04      |
      |   1 | $28    | $04  | $2C      |
      |   2 | $50    | $04  | $54      |
      |   3 | $78    | $04  | $7C      |
      |   4 | $A0    | $04  | $A4      |
      |   5 | $C8    | $04  | $CC      |
      |   6 | $F0    | $04  | $F4      |
      |   7 | $18    | $05  | $1D      |
      |   8 | $40    | $05  | $45      |
      |   9 | $68    | $05  | $6D      |
      |  10 | $90    | $05  | $95      |
      |  11 | $B8    | $05  | $BD      |
      |  12 | $E0    | $05  | $E5      |
      |  13 | $08    | $06  | $0E      |
      |  14 | $30    | $06  | $36      |
      |  15 | $58    | $06  | $5E      |
      |  16 | $80    | $06  | $86      |
      |  17 | $A8    | $06  | $AE      |
      |  18 | $D0    | $06  | $D6      |
      |  19 | $F8    | $06  | $FE      |
      |  20 | $20    | $07  | $27      |
      |  21 | $48    | $07  | $4F      |
      |  22 | $70    | $07  | $77      |
      |  23 | $98    | $07  | $9F      |
      |  24 | $B0    | $07  | $B7      |

 * use cases for scroll operations
   - scroll regions left/up/down/right

     +-----------------------------+
     |          /\                 |
     |      +--------+             |
     |    < |        | >           |
     |      +--------+             |
     |          \/                 |
     +-----------------------------+

     parameter for scroll:
       - width to scroll (number of characters, or last col - first col +1)
       - height to scroll (number of lines, or last line - first line +1)
       - first line/last line to scroll
       - first col/last col to scroll
       - delta (by which) to scroll (e.g 1..n cols/lines)

     convention:
       width        = Y
       first col    = ZP_RP
       delta        = A
       first line   = X
       last line    = ZP_RP+1

     fast-mem-cpy:
 |#

;; X       : screen row src
;; ZP_RP   : source col
;; A       : screen row target
;; ZP_RP+1 : target col
;; Y       : number to copy within row
;; ZP_RZ   : number of lines to copy
;; ZP_RZ+1 : 0 -> copy memory up, != 0 -> copy memory down
(define-vm-function-wol FAST_SMALL_MEMCOPY
  (list
   ;; copy from lower memory to higher memory (used for scrolling down and right)
   (label FAST_SMALL_MEMCOPY_DOWN)
          (STY ZP_TEMP+1)
          (STA ZP_TEMP)            ;; keep target row in temp

          (LDA line_start_table,x) ;; x = source row
          (AND !$F8)
          (CLC)
          (ADC ZP_RP)              ;; source col
          (STA FSMU__MEM_READ_CMD_DOWN+1)

          (LDA line_start_table,x) ;; x = source row
          (AND !$07)
          (ADC !$00)
          (STA FSMU__MEM_READ_CMD_DOWN+2)

          (LDX ZP_TEMP)
          (LDA line_start_table,x) ;; x = target row
          (AND !$F8)
          (CLC)
          (ADC ZP_RP+1)            ;; target col
          (STA FSMU__MEM_WRITE_CMD_DOWN+1)

          (LDA line_start_table,x) ;; x = target row
          (AND !$07)
          (ADC !$00)
          (STA FSMU__MEM_WRITE_CMD_DOWN+2)

          (BNE INNER_LOOP_DOWN__) ;; always branch

   ;; copy from higher memory to lower memory (used for scrolling up and left)
   (label FAST_SMALL_MEMCOPY_UP)
          (STY ZP_TEMP+1)          ;; keep number of chars to copy per row
          (STA ZP_TEMP)            ;; keep target row in temp

          (LDA line_start_table,x) ;; x = source row
          (AND !$F8)
          (CLC)
          (ADC ZP_RP)              ;; source col
          (STA FSMU__MEM_READ_CMD_UP+1)

          (LDA line_start_table,x) ;; x = source row
          (AND !$07)
          (ADC !$00)
          (STA FSMU__MEM_READ_CMD_UP+2)

          (LDX ZP_TEMP)
          (LDA line_start_table,x) ;; x = target row
          (AND !$F8)
          (CLC)
          (ADC ZP_RP+1)            ;; target col
          (STA FSMU__MEM_WRITE_CMD_UP+1)

          (LDA line_start_table,x) ;; x = target row
          (AND !$07)
          (ADC !$00)
          (STA FSMU__MEM_WRITE_CMD_UP+2)

          (BNE INIT_LOOP_UP__) ;; always branch

   (label ROW_LOOP_INC__)
          (SEC)

          (LDA FSMU__MEM_READ_CMD_DOWN+1)
          (SBC !40)
          (STA FSMU__MEM_READ_CMD_DOWN+1)
          (BCS NO_BORROW_NEXT_READ__)
          (DEC FSMU__MEM_READ_CMD_DOWN+2)

          (SEC)
   (label NO_BORROW_NEXT_READ__)

          (LDA FSMU__MEM_WRITE_CMD_DOWN+1)
          (SBC !40)
          (STA FSMU__MEM_WRITE_CMD_DOWN+1)
          (BCS NO_BURROW_NEXT_WRITE__)
          (DEC FSMU__MEM_WRITE_CMD_DOWN+2)

   (label NO_BURROW_NEXT_WRITE__)
          (LDY ZP_TEMP+1) ;; number of chars per row

   (label INNER_LOOP_DOWN__)

   (label FSMU__MEM_READ_CMD_DOWN)
          (LDA $0400,y)
   (label FSMU__MEM_WRITE_CMD_DOWN)
          (STA $0400,y)
          (DEY)
          (BPL INNER_LOOP_DOWN__)

          (DEC ZP_RZ)
          (BNE ROW_LOOP_INC__)

          (RTS)


   (label ROW_LOOP_DEC__)
          (CLC)

          (LDA FSMU__MEM_READ_CMD_UP+1)
          (ADC !40)
          (STA FSMU__MEM_READ_CMD_UP+1)
          (BCC NO_OVFL_NEXT_READ__)
          (INC FSMU__MEM_READ_CMD_UP+2)

          (CLC)
   (label NO_OVFL_NEXT_READ__)

          (LDA FSMU__MEM_WRITE_CMD_UP+1)
          (ADC !40)
          (STA FSMU__MEM_WRITE_CMD_UP+1)
          (BCC NO_OVFL_NEXT_WRITE__)
          (INC FSMU__MEM_WRITE_CMD_UP+2)

   (label NO_OVFL_NEXT_WRITE__)

   (label INIT_LOOP_UP__)
          (LDY !$00)

   (label INNER_LOOP_UP__)

   (label FSMU__MEM_READ_CMD_UP)
          (LDA $0400,y)
   (label FSMU__MEM_WRITE_CMD_UP)
          (STA $0400,y)
          (INY)
          (CPY ZP_TEMP+1)  ;; number of chars per row
          (BMI INNER_LOOP_UP__)

          (DEC ZP_RZ)
          (BNE ROW_LOOP_DEC__)

          (RTS)))

;; A = page of write command
;; X = row <- must stay untouched
;; ZP = COL
;; Y = offset of write command
(define-vm-function PREP_WRITE_SCREEN_CMD
  (list
          (STA ZP_TEMP+1)
          (STY ZP_TEMP)

          (LDA line_start_table,x) ;; x = row
          (AND !$F8)
          (CLC)
          (ADC ZP_RP)
          (LDY !$00)
          (STA (ZP_TEMP),y) ;; write offset

          (LDA line_start_table,x) ;; x = row
          (AND !$07)
          (ADC !$00)

          (INY)
          (STA (ZP_TEMP),y) ;; write page

          (RTS)
          ))

;; write a number of screen codes to y,x position on screen
;;
;; input:  x = ROW
;;         ZP_RP = COL
;;         RT_SCREEN_PUT_CHARS_AT__STRING+1 = ptr to screen code data (low at +1, high at +2)
;;         y = # of chars to print -1 (0 for one char, 1 for two ...)
;; output: screen modified
(define-vm-function RT_SCREEN_PUT_CHARS_AT
  (list

          (STY ZP_RP+1)
          (LDA !>write_screen_cmd__+1)
          (LDY !<write_screen_cmd__+1)
          (JSR PREP_WRITE_SCREEN_CMD)
          (LDY ZP_RP+1)

   (label char_put_loop__)
   (label RT_SCREEN_PUT_CHARS_AT__STRING)
          (LDA $0400,y)
   (label write_screen_cmd__)
          (STA $0400,y)
          (DEY)                      ;; use y as both indices (zp_rp = ptr - col)
          (BPL char_put_loop__)

          (RTS)

   (label line_start_table)
          (byte $04 $2C $54 $7C $A4 ;; row 0..4
                $CC $F4 $1D $45 $6D ;; row 5..9
                $95 $BD $E5 $0E $36 ;; row 10..14
                $5E $86 $AE $D6 $FE ;; row 15..19
                $27 $4F $77 $9F $C7 ;; row 20..24
                )))

(module+ test #| RT_SCREEN_PUT_CHARS_AT |#
  (define screen-put-chars-at-0-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     ;; now put the string
     (LDA !<test_string0)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+1)
     (LDA !>test_string0)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+2)

     (LDX !5)
     (LDY !17)
     (STY ZP_RP)
     (LDY !0)

     (JSR $0100)
     (JSR RT_SCREEN_PUT_CHARS_AT)
     (BRK)
     (label test_string0)
     (asc "O")
     ))

  (check-equal? (memory-list screen-put-chars-at-0-test
                             (+ screen-base-address (* 5 screen-row-bytes) 17))
                (map char->integer (string->list "O"))
                "the char O was written to the right screen area")
  (inform-check-equal? (cpu-state-clock-cycles screen-put-chars-at-0-test)
                       76
                       "cpu cycles for writing string with 1 character to position x,y")

  (define screen-put-chars-at-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     ;; now put the string
     (LDA !<test_string1)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+1)
     (LDA !>test_string1)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+2)

     (LDX !20)
     (LDY !10)
     (STY ZP_RP)
     (LDY !4)

     (JSR $0100)
     (JSR RT_SCREEN_PUT_CHARS_AT)
     (BRK)
     (label test_string1)
     (asc ".SOME.")
     ))

  (check-equal? (memory-list screen-put-chars-at-test
                             (+ screen-base-address (* 20 screen-row-bytes) 10)
                             (+ screen-base-address (* 20 screen-row-bytes) 14))
                (map char->integer (string->list ".SOME"))
                "the string .SOME was written to the right screen area")
  (inform-check-equal? (cpu-state-clock-cycles screen-put-chars-at-test)
                       132
                       "cpu cycles for writing string with 5 characters to position x,y")

  (define screen-put-chars-at-2-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     ;; now put the string
     (LDA !<test_string2)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+1)
     (LDA !>test_string2)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+2)

     (LDX !16)
     (LDY !10)
     (STY ZP_RP)
     (LDY !24)

     (JSR $0100)
     (JSR RT_SCREEN_PUT_CHARS_AT)
     (BRK)
     (label test_string2)
     (asc ".SOME.OTHER.STRING.THAT.IS.A.BIT.LONGER.")
     ))

  (check-equal? (memory-list screen-put-chars-at-2-test
                             (+ screen-base-address (* 16 screen-row-bytes) 10)
                             (+ screen-base-address (* 16 screen-row-bytes) 34))
                (map char->integer (string->list ".SOME.OTHER.STRING.THAT.I"))
                "the string .SOME was written to the right screen area")
  (inform-check-equal? (cpu-state-clock-cycles screen-put-chars-at-2-test)
                       412
                       "cpu cycles for writing string with 25 characters to position x,y"))

;; scroll n lines a number of chars one char to the right
;;
;; X     : screen row (last row)
;; ZP_RP : column (start col)
;; A     : delta (1.. to move right)
;; Y     : number of chars to copy
;; ZP_RZ : number of rows (1.., up from last row, effectively scroll rows X, X-1, ... X-n)
(define-vm-function-wol RT_SCREEN_SCROLL_RIGHT_CHARS_AT
  (list
   (label RT_SCREEN_SCROLL_RIGHT_CHARS_AT_BY1)
          (LDA !$01)
   (label RT_SCREEN_SCROLL_RIGHT_CHARS_AT)
          (CLC)
          (ADC ZP_RP)
          (STA ZP_RP+1) ;; set target col

          (TXA)         ;; row target = row source

          (JMP FAST_SMALL_MEMCOPY_DOWN)
   ))

(module+ test #| scroll right |#
  (define scroll-right-test2
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (write-string-to-screen--for-test 5 17 "OLALA")
     (write-string-to-screen--for-test 4 17 "TIPOP")

     (LDX !5)
     (LDY !17)
     (STY ZP_RP)
     (LDY !4)
     (LDA !$02)
     (STA ZP_RZ)   ;; two rows

     (JSR $0100)
     (JSR RT_SCREEN_SCROLL_RIGHT_CHARS_AT_BY1)))

  (check-equal? (memory-list scroll-right-test2
                             (+ screen-base-address (* 5 screen-row-bytes) 16)
                             (+ screen-base-address (* 5 screen-row-bytes) 23))
                (append (list 0)
                        (map char->integer (string->list "OOLALA"))
                        (list 0))
                "line was scrolled right.")
  (check-equal? (memory-list scroll-right-test2
                             (+ screen-base-address (* 4 screen-row-bytes) 16)
                             (+ screen-base-address (* 4 screen-row-bytes) 23))
                (append (list 0)
                        (map char->integer (string->list "TTIPOP"))
                        (list 0))
                "second line was scrolled right (too).")
  (inform-check-equal? (cpu-state-clock-cycles scroll-right-test2)
                271
                "cpu cycles needed for scrolling 5 chars right")


  (define scroll-right-complete-screen
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (write-string-to-screen--for-test 0 0 "OLALA")
     (write-string-to-screen--for-test 24 0 "TIPOP")
     (write-string-to-screen--for-test 0 35 "OLALA")
     (write-string-to-screen--for-test 24 35 "TIPOP")

     (LDX !24) ;; start at row 25 (zero indexed) [going up]
     (LDY !0)
     (STY ZP_RP)
     (LDY !38) ;; copy 39 chars (counter is always 1 -)
     (LDA !25) ;; scroll 25 rows in total
     (STA ZP_RZ)   ;; all rows

     (JSR $0100)
     (JSR RT_SCREEN_SCROLL_RIGHT_CHARS_AT_BY1)
     ))
  (inform-check-equal? (cpu-state-clock-cycles scroll-right-complete-screen)
                14691
                "cpu cycles needed for scrolling the complete screen 1 char right")
  (check-equal? (memory-list scroll-right-complete-screen
                             (+ screen-base-address (* 0 screen-row-bytes) 0)
                             (+ screen-base-address (* 0 screen-row-bytes) 6))
                (append (map char->integer (string->list "OOLALA"))
                        (list 0))
                "char in col 0 was doubled (O), rest got scrolled right.")
  (check-equal? (memory-list scroll-right-complete-screen
                             (+ screen-base-address (* 0 screen-row-bytes) 35)
                             (+ screen-base-address (* 0 screen-row-bytes) 39))
                (append (list 0)
                        (map char->integer (string->list "OLAL")))
                "leftmost 0 got scrolled in, right most char (A) was cut off")
  (check-equal? (memory-list scroll-right-complete-screen
                             (+ screen-base-address (* 24 screen-row-bytes) 0)
                             (+ screen-base-address (* 24 screen-row-bytes) 6))
                (append (map char->integer (string->list "TTIPOP"))
                        (list 0))
                "char in col 0 was doubled (T), rest got scrolled right.")
  (check-equal? (memory-list scroll-right-complete-screen
                             (+ screen-base-address (* 24 screen-row-bytes) 35)
                             (+ screen-base-address (* 24 screen-row-bytes) 39))
                (append (list 0)
                        (map char->integer (string->list "TIPO")))
                "leftmost 0 got scrolled in, right most char (P) was cut off"))

;; scroll within one line a number of chars one char to the left
;;
;; input:  X = ROW
;;         ZP_RP = COL -> COL-1
;;         Y = # of chars to scroll
;;         A = delta (positive: 1..)
;; output: screen modified
(define-vm-function-wol RT_SCREEN_SCROLL_LEFT_CHARS_AT
  (list
   (label RT_SCREEN_SCROLL_LEFT_CHARS_AT_BY1)
          (LDA !$01)
   (label RT_SCREEN_SCROLL_LEFT_CHARS_AT)
          (STA ZP_TEMP)
          (LDA ZP_RP)
          (SEC)
          (SBC ZP_TEMP)
          (STA ZP_RP+1)

          (TXA)

          (JMP FAST_SMALL_MEMCOPY_UP)))

(module+ test #| scroll left |#
  (define scroll-left-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (write-string-to-screen--for-test 5 17 "OLALA")

     (LDX !5)
     (LDY !18)
     (STY ZP_RP)
     (LDY !4)
     (LDA !$01)
     (STA ZP_RZ)   ;; one row

     (JSR $0100)
     (JSR RT_SCREEN_SCROLL_LEFT_CHARS_AT_BY1)
     ))

  (check-equal? (memory-list scroll-left-test
                             (+ screen-base-address (* 5 screen-row-bytes) 17)
                             (+ screen-base-address (* 5 screen-row-bytes) 21))
                (map char->integer (string->list "LALAA"))
                "screen was clear where once written.")
  (inform-check-equal? (cpu-state-clock-cycles scroll-left-test)
                169
                "cpu cycles needed for scrolling 5 chars left")

  (define scroll-left-2-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (write-string-to-screen--for-test 5 17 "OLALA")
     (write-string-to-screen--for-test 6 17 "OLALA")

     (LDX !5)
     (LDY !18)
     (STY ZP_RP)
     (LDY !4)
     (LDA !$02)
     (STA ZP_RZ)   ;; two rows

     (JSR $0100)
     (JSR RT_SCREEN_SCROLL_LEFT_CHARS_AT_BY1)
     ))

  (check-equal? (memory-list scroll-left-2-test
                             (+ screen-base-address (* 5 screen-row-bytes) 17)
                             (+ screen-base-address (* 5 screen-row-bytes) 21))
                (map char->integer (string->list "LALAA"))
                "string got scrolled one char left.")
  (check-equal? (memory-list scroll-left-2-test
                             (+ screen-base-address (* 6 screen-row-bytes) 17)
                             (+ screen-base-address (* 6 screen-row-bytes) 21))
                (map char->integer (string->list "LALAA"))
                "string got scrolled one char left.")
  (inform-check-equal? (cpu-state-clock-cycles scroll-left-2-test)
                286
                "cpu cycles needed for scrolling 5 chars left")

  (define scroll-left-complete-screen
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (write-string-to-screen--for-test 0 0 "OLALA")
     (write-string-to-screen--for-test 24 0 "TIPOP")
     (write-string-to-screen--for-test 0 35 "OLALA")
     (write-string-to-screen--for-test 24 35 "TIPOP")

     (LDX !0) ;; start at row 0 (zero indexed) [going down]
     (LDY !1)
     (STY ZP_RP)
     (LDY !39) ;; copy 39 chars
     (LDA !25) ;; scroll 25 rows in total
     (STA ZP_RZ)   ;; all rows

     (JSR $0100)
     (JSR RT_SCREEN_SCROLL_LEFT_CHARS_AT_BY1)
     ))
  (inform-check-equal? (cpu-state-clock-cycles scroll-left-complete-screen)
                17600
                "cpu cycles needed for scrolling the complete screen 1 char left")
  (check-equal? (memory-list scroll-left-complete-screen
                             (+ screen-base-address (* 0 screen-row-bytes) 0)
                             (+ screen-base-address (* 0 screen-row-bytes) 4))
                (append (map char->integer (string->list "LALA"))
                        (list 0))
                "leftmost 0 got scrolled in, left most char (T) was cut off")
  (check-equal? (memory-list scroll-left-complete-screen
                             (+ screen-base-address (* 0 screen-row-bytes) 34)
                             (+ screen-base-address (* 0 screen-row-bytes) 39))
                (append (map char->integer (string->list "OLALAA")))
                "char in col 39 was doubled (A), rest got scrolled left.")
  (check-equal? (memory-list scroll-left-complete-screen
                             (+ screen-base-address (* 24 screen-row-bytes) 0)
                             (+ screen-base-address (* 24 screen-row-bytes) 4))
                (append (map char->integer (string->list "IPOP"))
                        (list 0))
                "leftmost 0 got scrolled in, left most char (A) was cut off")
  (check-equal? (memory-list scroll-left-complete-screen
                             (+ screen-base-address (* 24 screen-row-bytes) 34)
                             (+ screen-base-address (* 24 screen-row-bytes) 39))
                (append (map char->integer (string->list "TIPOPP")))
                "char in 39 was doubled (P), rest got scrolled left"))

;; input:  X = ROW
;;         ZP_RP = COL
;;         Y = # of chars to scroll
;;         OPEN: A = delta (positive: 1..)
;; output: screen modified
(define-vm-function-wol RT_SCREEN_SCROLL_DOWN
  (list
   (label RT_SCREEN_SCROLL_DOWN_BY1)
          ;; (LDA !$01)
   (label RT_SCREEN_SCROLL_DOWN)
          (LDA ZP_RP)
          (STA ZP_RP+1)  ;; target col = source col
          (INX)
          (TXA)
          (DEX) ;; A = target row = source row (x) + 1

          (JMP FAST_SMALL_MEMCOPY_DOWN)))

(module+ test #| scroll down |#
  (define scroll-down-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     ;; now put the string
     (write-string-to-screen--for-test 5 17 "OLALA")

     (LDX !5)
     (LDY !17)
     (STY ZP_RP)
     (LDY !4)
     (LDA !$01)
     (STA ZP_RZ) ;; one row

     (JSR $0100)
     (JSR RT_SCREEN_SCROLL_DOWN_BY1)))

  (check-equal? (memory-list scroll-down-test
                             (+ screen-base-address (* 6 screen-row-bytes) 17)
                             (+ screen-base-address (* 6 screen-row-bytes) 21))
                (map char->integer (string->list "OLALA"))
                "string was scrolled down.")
  (inform-check-equal? (cpu-state-clock-cycles scroll-down-test)
                163
                "cpu cycles needed for scrolling 5 chars down"))

;; input:  X = ROW
;;         ZP_RP = COL
;;         Y = # of chars to scrol
;;         OPEN: A = delta (positive: 1..)
(define-vm-function RT_SCREEN_SCROLL_UP
  (list
          (RTS)))

;; clear the whole screen
(define-vm-function RT_SCREEN_CLEAR
  (list
          (LDA !0)
          (TAX)
   (label loop__)
          (STA $0400,x)
          (STA $0500,x)
          (STA $0600,x)
          (STA $0700,x)
          (DEX)
          (BNE loop__)
          (RTS)))

(module+ test #| clear screen |#
  (define screen-clear-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     ;; now put the string
     (LDA !<test_string3)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+1)
     (LDA !>test_string3)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+2)

     (LDX !5)
     (LDY !17)
     (STY ZP_RP)
     (LDY !0)

     (JSR RT_SCREEN_PUT_CHARS_AT)

     (LDA !<test_string3)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+1)
     (LDA !>test_string3)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+2)

     (LDY !0)
     (LDX !0)
     (STX ZP_RP)
     (LDX !0)

     (JSR RT_SCREEN_PUT_CHARS_AT)

     (LDA !<test_string3)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+1)
     (LDA !>test_string3)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+2)

     (LDY !24)
     (LDX !39)
     (STX ZP_RP)
     (LDX !0)

     (JSR RT_SCREEN_PUT_CHARS_AT)

     (JSR $0100)
     (JSR RT_SCREEN_CLEAR)

     (BRK)

     (label test_string3)
     (asc "O")
     ))

  (check-equal? (memory-list screen-clear-test
                             (+ screen-base-address (* 5 screen-row-bytes) 17))
                (list 0)
                "screen was clear where once written.")
  (check-equal? (memory-list screen-clear-test
                             (+ screen-base-address (* 0 screen-row-bytes) 0))
                (list 0)
                "screen was clear where once written.")
  (check-equal? (memory-list screen-clear-test
                             (+ screen-base-address (* 24 screen-row-bytes) 39))
                (list 0)
                "screen was clear where once written.")
  (inform-check-equal? (cpu-state-clock-cycles screen-clear-test)
                       6409
                       "cpu cycles to clear the whole screen"))

;; clear a number of chars in a row y,x position on screen
;;
;; input:  X = ROW
;;         ZP_RP = COL
;;         Y = # of chars to clear -1 (0 for one char, 1 for two ...)
;; output: screen modified
(define-vm-function RT_SCREEN_CLEAR_CHARS_AT
  (list
          (STY ZP_RP+1)
          (LDA !>write_screen_cmd__+1)
          (LDY !<write_screen_cmd__+1)
          (JSR PREP_WRITE_SCREEN_CMD)
          (LDY ZP_RP+1)

          (LDA !$00)
   (label char_put_loop__)
   (label write_screen_cmd__)
          (STA $0400,y)
          (DEY)                      ;; use y as both indices (zp_rp = ptr - col)
          (BPL char_put_loop__)

          (RTS)))

(module+ test #| screen clear chars at |#
  (define screen-clear-chars-at-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     ;; now put the string
     (LDA !<test_string3)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+1)
     (LDA !>test_string3)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+2)

     (LDX !5)
     (LDY !17)
     (STY ZP_RP)
     (LDY !4)

     (JSR RT_SCREEN_PUT_CHARS_AT)

     (LDX !5)
     (LDY !17)
     (STY ZP_RP)
     (LDY !4)

     (JSR RT_SCREEN_CLEAR_CHARS_AT)

     (LDA !<test_string3)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+1)
     (LDA !>test_string3)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+2)

     (LDX !0)
     (LDY !0)
     (STY ZP_RP)
     (LDY !4)

     (JSR RT_SCREEN_PUT_CHARS_AT)

     (LDX !0)
     (LDY !0)
     (STY ZP_RP)
     (LDY !4)

     (JSR RT_SCREEN_CLEAR_CHARS_AT)

     (LDA !<test_string3)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+1)
     (LDA !>test_string3)
     (STA RT_SCREEN_PUT_CHARS_AT__STRING+2)

     (LDX !24)
     (LDY !39)
     (STY ZP_RP)
     (LDY !0)

     (JSR RT_SCREEN_PUT_CHARS_AT)

     (LDX !24)
     (LDY !39)
     (STY ZP_RP)
     (LDY !0)

     (JSR $0100)
     (JSR RT_SCREEN_CLEAR_CHARS_AT)

     (BRK)

     (label test_string3)
     (asc "OLALA")
     ))

  (check-equal? (memory-list screen-clear-chars-at-test
                             (+ screen-base-address (* 5 screen-row-bytes) 17)
                             (+ screen-base-address (* 5 screen-row-bytes) 21))
                (list 0 0 0 0 0)
                "screen was clear where once written.")
  (check-equal? (memory-list screen-clear-chars-at-test
                             (+ screen-base-address (* 0 screen-row-bytes) 0)
                             (+ screen-base-address (* 0 screen-row-bytes) 4))
                (list 0 0 0 0 0)
                "screen was clear where once written.")
  (check-equal? (memory-list screen-clear-chars-at-test
                             (+ screen-base-address (* 24 screen-row-bytes) 39))
                (list 0)
                "screen was clear where once written.")
  (inform-check-equal? (cpu-state-clock-cycles screen-clear-chars-at-test)
                       74
                       "cpu cycles to clear a char"))

(define screen-code
  (append
   FAST_SMALL_MEMCOPY
   PREP_WRITE_SCREEN_CMD
   RT_SCREEN_SCROLL_RIGHT_CHARS_AT
   RT_SCREEN_SCROLL_LEFT_CHARS_AT
   RT_SCREEN_SCROLL_UP
   RT_SCREEN_SCROLL_DOWN
   RT_SCREEN_PUT_CHARS_AT
   RT_SCREEN_CLEAR_CHARS_AT
   RT_SCREEN_CLEAR))

(module+ test #| estimated-code-len |#
  (inform-check-equal? (estimated-code-len screen-code)
                368
                "estimated code length change in screen runtime"))

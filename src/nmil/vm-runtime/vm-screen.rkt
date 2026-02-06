#lang racket/base

(provide
 RT_SCREEN_PUT_CHARS_AT               ;; write a number of screen codes to y,x position on screen
 RT_SCREEN_PUT_CHARS_AT__STRING       ;; label where to put the string pointer (actuall ..+1)

 RT_SCREEN_CLEAR_CHARS_AT             ;; clear a number of chars in one line on the screen
 RT_SCREEN_SCROLL_RIGHT_CHARS_AT      ;; scroll a portion of the screen by delta to the right
 RT_SCREEN_SCROLL_RIGHT_CHARS_AT_BY1  ;; by one
 RT_SCREEN_SCROLL_LEFT_CHARS_AT       ;; scroll a portion of the screen by delta to the left
 RT_SCREEN_SCROLL_LEFT_CHARS_AT_BY1   ;; by one
 RT_SCREEN_SCROLL_UP                  ;; scroll a portion of the screen up by 1 line
 RT_SCREEN_SCROLL_UP_BY1
 RT_SCREEN_SCROLL_DOWN                ;; scroll a portion of the screen down by 1 line
 RT_SCREEN_SCROLL_DOWN_BY1
 RT_SCREEN_CLEAR                      ;; clear the whole screen (and color ram, too)
 RT_SCREEN_PUT_YTIMES_COLOR_AT        ;; put the color in A into char color ram of row X, col ZP_RP
 RT_SCREEN_PUT_COLOR_AT               ;; set color at (lower nibble) with A, row = X, column = ZP_RP

 RT_BCD_TO_SCREEN_CODE                ;; convert bcd to screen codes


 vm-screen-code
 screen-base-address
 color-base-address)

(define screen-base-address #x0400)
(define color-base-address #xd800)
(define screen-row-bytes 40)

#|

 screen output routines for the c64



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


 screen organisation, color and screen modes
 - https://sta.c64.org/cbm64disp.html
 - https://www.c64-wiki.com/wiki/Color
 - https://sta.c64.org/cbm64scr.html


 info about cpu cycles per scanline etc. (https://www.lemon64.com/forum/viewtopic.php?t=2629):
   The PAL C64 has 312 scanlines giving 63*312 = 19656 cycles. If the display is activated (without
   sprites), the VIC will steal 40 cycles for each badline which gives us 63*312 - (25*40) = 18656 cycles.
   Add some sprites and the free cycles will decrease even more.

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
           "../test-utils.rkt"
           "../../6510-test-utils.rkt"
           (only-in "../../ast/6510-relocator.rkt"
                    estimated-code-len)
           (only-in "../../tools/6510-interpreter.rkt"
                    cpu-state-clock-cycles
                    memory-list
                    memory-list-)
           "./vm-memory-manager-test-utils.rkt"
           (only-in "./vm-memory-map.rkt"
                    VM_MEMORY_MANAGEMENT_CONSTANTS))

  (define test-runtime
    (append
     vm-screen-code

     VM_MEMORY_MANAGEMENT_CONSTANTS
     (list (label VM_INIT_MEMORY_MANAGER) (RTS))))

  ;; write color red (02) in row, col for len chars
  (define (write-color-red--for-test row col len)
    (list
     (ast-opcode-cmd '() `(162 ,row)) ;; (LDX !row)
     (ast-opcode-cmd '() `(160 ,col)) ;; (LDY !col)
     (STY ZP_RP)
     (ast-opcode-cmd '() `(160 ,(- len 1))) ;; (LDY !len-1)
     (LDA !$02)
     (JSR RT_SCREEN_PUT_YTIMES_COLOR_AT)))

  ;; write characters of this string into screen (in assembler, using RT_SCREEN_PUT_CHARS_AT)
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

      | row | offset | page | color-page | complete (offset & page) |
      |-----+--------+------+------------+--------------------------|
      |   0 | $00    | $04  | $D8        | $04                      |
      |   1 | $28    | $04  | $D8        | $2C                      |
      |   2 | $50    | $04  | $D8        | $54                      |
      |   3 | $78    | $04  | $D8        | $7C                      |
      |   4 | $A0    | $04  | $D8        | $A4                      |
      |   5 | $C8    | $04  | $D8        | $CC                      |
      |   6 | $F0    | $04  | $D8        | $F4                      |
      |   7 | $18    | $05  | $D9        | $1D                      |
      |   8 | $40    | $05  | $D9        | $45                      |
      |   9 | $68    | $05  | $D9        | $6D                      |
      |  10 | $90    | $05  | $D9        | $95                      |
      |  11 | $B8    | $05  | $D9        | $BD                      |
      |  12 | $E0    | $05  | $D9        | $E5                      |
      |  13 | $08    | $06  | $DA        | $0E                      |
      |  14 | $30    | $06  | $DA        | $36                      |
      |  15 | $58    | $06  | $DA        | $5E                      |
      |  16 | $80    | $06  | $DA        | $86                      |
      |  17 | $A8    | $06  | $DA        | $AE                      |
      |  18 | $D0    | $06  | $DA        | $D6                      |
      |  19 | $F8    | $06  | $DA        | $FE                      |
      |  20 | $20    | $07  | $DB        | $27                      |
      |  21 | $48    | $07  | $DB        | $4F                      |
      |  22 | $70    | $07  | $DB        | $77                      |
      |  23 | $98    | $07  | $DB        | $9F                      |
      |  24 | $B0    | $07  | $DB        | $B7                      |

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

;; modifed: ZP_TEMP    = screen row target
;;          ZP_TEMP+1  = number to copy within row
;;          A, X, Y    = ?
(define FAST_SCREEN_MEMCOPY_DOWN '())
(define FAST_SCREEN_MEMCOPY_UP '())
(define-vm-function-wol FAST_SCREEN_MEMCOPY
  (list
   ;; copy from lower memory to higher memory (used for scrolling down and right)

   (label FAST_SCREEN_MEMCOPY_DOWN_CHARS)
          (STA ZP_TEMP)            ;; keep target row in temp
          (STY ZP_TEMP+1)

          (LDA SCREEN_ROW_MEM_TRANSLATION_TABLE,x) ;; x = source row
          (AND !$F8)
          (CLC)
          (ADC ZP_RP)              ;; source col
          (STA FSMU__MEM_READ_CMD_DOWN+1)
          (STA FSMU__MEM_READ_CMD_DOWN_COLOR+1)

          (LDA SCREEN_ROW_MEM_TRANSLATION_TABLE,x) ;; x = source row
          (AND !$07)
   (label add_page_for_read_cmd_down__)
          (ADC !$00)
          (STA FSMU__MEM_READ_CMD_DOWN+2)
          (ADC !$d4)
          (STA FSMU__MEM_READ_CMD_DOWN_COLOR+2)

          (LDX ZP_TEMP)
          (LDA SCREEN_ROW_MEM_TRANSLATION_TABLE,x) ;; x = target row
          (AND !$F8)
          (CLC)
          (ADC ZP_RP+1)            ;; target col
          (STA FSMU__MEM_WRITE_CMD_DOWN+1)
          (STA FSMU__MEM_WRITE_CMD_DOWN_COLOR+1)

          (LDA SCREEN_ROW_MEM_TRANSLATION_TABLE,x) ;; x = target row
          (AND !$07)
   (label add_page_for_write_cmd_down__)
          (ADC !$00) ;; TODO: use 00 for sreen ram, use d4 for color ram
          (STA FSMU__MEM_WRITE_CMD_DOWN+2)
          (ADC !$d4)
          (STA FSMU__MEM_WRITE_CMD_DOWN_COLOR+2)

          (LDX ZP_RZ)
          (BNE INNER_LOOP_DOWN__) ;; always branch

   (label FAST_SCREEN_MEMCOPY_UP_CHARS)
          (STA ZP_TEMP)            ;; keep target row in temp
          (STY ZP_TEMP+1)          ;; keep number of chars to copy per row

          (LDA SCREEN_ROW_MEM_TRANSLATION_TABLE,x) ;; x = source row
          (AND !$F8)
          (CLC)
          (ADC ZP_RP)              ;; source col
          (STA FSMU__MEM_READ_CMD_UP+1)
          (STA FSMU__MEM_READ_CMD_UP_COLOR+1)

          (LDA SCREEN_ROW_MEM_TRANSLATION_TABLE,x) ;; x = source row
          (AND !$07)
   (label add_page_for_read_cmd_up__)
          (ADC !$00)
          (STA FSMU__MEM_READ_CMD_UP+2)
          (ADC !$d4)
          (STA FSMU__MEM_READ_CMD_UP_COLOR+2)

          (LDX ZP_TEMP)
          (LDA SCREEN_ROW_MEM_TRANSLATION_TABLE,x) ;; x = target row
          (AND !$F8)
          (CLC)
          (ADC ZP_RP+1)            ;; target col
          (STA FSMU__MEM_WRITE_CMD_UP+1)
          (STA FSMU__MEM_WRITE_CMD_UP_COLOR+1)

          (LDA SCREEN_ROW_MEM_TRANSLATION_TABLE,x) ;; x = target row
          (AND !$07)
   (label add_page_for_write_cmd_up__)
          (ADC !$00)
          (STA FSMU__MEM_WRITE_CMD_UP+2)
          (ADC !$d4)
          (STA FSMU__MEM_WRITE_CMD_UP_COLOR+2)
          (LDX ZP_RZ)
          (BNE INIT_LOOP_UP__) ;; always branch

   (label ROW_LOOP_INC__)
          (SEC)
          (LDA FSMU__MEM_READ_CMD_DOWN+1)
          (SBC !40)
          (STA FSMU__MEM_READ_CMD_DOWN+1)
          (STA FSMU__MEM_READ_CMD_DOWN_COLOR+1)
          (BCS NO_BORROW_NEXT_READ__)
          (DEC FSMU__MEM_READ_CMD_DOWN+2)
          (DEC FSMU__MEM_READ_CMD_DOWN_COLOR+2)

          (SEC)
   (label NO_BORROW_NEXT_READ__)

          (LDA FSMU__MEM_WRITE_CMD_DOWN+1)
          (SBC !40)
          (STA FSMU__MEM_WRITE_CMD_DOWN+1)
          (STA FSMU__MEM_WRITE_CMD_DOWN_COLOR+1)
          (BCS NO_BURROW_NEXT_WRITE__)
          (DEC FSMU__MEM_WRITE_CMD_DOWN+2)
          (DEC FSMU__MEM_WRITE_CMD_DOWN_COLOR+2)

   (label NO_BURROW_NEXT_WRITE__)
          (LDY ZP_TEMP+1) ;; number of chars per row

   (label INNER_LOOP_DOWN__)

   (label FSMU__MEM_READ_CMD_DOWN)
          (LDA $0400,y)
   (label FSMU__MEM_WRITE_CMD_DOWN)
          (STA $0400,y)
   (label FSMU__MEM_READ_CMD_DOWN_COLOR)
          (LDA $0400,y)
   (label FSMU__MEM_WRITE_CMD_DOWN_COLOR)
          (STA $0400,y)
          (DEY)
          (BPL INNER_LOOP_DOWN__)

          (DEX)
          (BNE ROW_LOOP_INC__)

          (RTS)


   (label ROW_LOOP_DEC__)
          (CLC)

          (LDA FSMU__MEM_READ_CMD_UP+1)
          (ADC !40)
          (STA FSMU__MEM_READ_CMD_UP+1)
          (STA FSMU__MEM_READ_CMD_UP_COLOR+1)
          (BCC NO_OVFL_NEXT_READ__)
          (INC FSMU__MEM_READ_CMD_UP+2)
          (INC FSMU__MEM_READ_CMD_UP_COLOR+2)

          (CLC)
   (label NO_OVFL_NEXT_READ__)

          (LDA FSMU__MEM_WRITE_CMD_UP+1)
          (ADC !40)
          (STA FSMU__MEM_WRITE_CMD_UP+1)
          (STA FSMU__MEM_WRITE_CMD_UP_COLOR+1)
          (BCC NO_OVFL_NEXT_WRITE__)
          (INC FSMU__MEM_WRITE_CMD_UP+2)
          (INC FSMU__MEM_WRITE_CMD_UP_COLOR+2)

   (label NO_OVFL_NEXT_WRITE__)

   (label INIT_LOOP_UP__)
          (LDY !$00)

   (label INNER_LOOP_UP__)

   (label FSMU__MEM_READ_CMD_UP)
          (LDA $0400,y)
   (label FSMU__MEM_WRITE_CMD_UP)
          (STA $0400,y)
   (label FSMU__MEM_READ_CMD_UP_COLOR)
          (LDA $0400,y)
   (label FSMU__MEM_WRITE_CMD_UP_COLOR)
          (STA $0400,y)
          (INY)
          (CPY ZP_TEMP+1)  ;; number of chars per row
          (BMI INNER_LOOP_UP__)

          (DEX)
          (BNE ROW_LOOP_DEC__)

          (RTS)))

;; MAY BE OBSOLETE
;; A = page of write command
;; X = row <- must stay untouched
;; ZP = COL
;; Y = offset of write command
(define-vm-function PREP_WRITE_SCREEN_CMD
  (list
          (STA ZP_TEMP+1)
          (STY ZP_TEMP)

          (LDA SCREEN_ROW_MEM_TRANSLATION_TABLE,x) ;; x = row
          (AND !$F8)
          (CLC)
          (ADC ZP_RP)
          (LDY !$00)
          (STA (ZP_TEMP),y) ;; write offset

          (LDA SCREEN_ROW_MEM_TRANSLATION_TABLE,x) ;; x = row
          (AND !$07)
          (ADC !$00)

          (INY)
          (STA (ZP_TEMP),y) ;; write page

          (RTS)
          ))


;; prepare (ZP_TEMP) to be a pointer to screen or color ram at row and col
;; input:    X = row
;;           ZP_RP = col
;; modifies: A, ZP_TEMP, ZP_TEMP+1
;; keeps:    X, Y
(define PREP_ZP_TEMP_FOR_COLOR_ACCESS '())
(define PREP_ZP_TEMP_FOR_SCREEN_ACCESS '())
(define-vm-function-wol PREP_ZP_TEMP_FOR_MEM_ACCESS
  (list
   (label PREP_ZP_TEMP_FOR_COLOR_ACCESS)
          (LDA !$d4)
          (BNE CONT__)
   (label PREP_ZP_TEMP_FOR_SCREEN_ACCESS)
          (LDA !$00)
   (label CONT__)
          (STA ZP_TEMP+1)
          (LDA SCREEN_ROW_MEM_TRANSLATION_TABLE,x) ;; x = row
          (AND !$F8)
          (CLC)
          (ADC ZP_RP)
          (STA ZP_TEMP) ;; write offset

          (LDA SCREEN_ROW_MEM_TRANSLATION_TABLE,x) ;; x = row
          (AND !$07)
          (ADC ZP_TEMP+1)
          (STA ZP_TEMP+1)
          (RTS)))

;; set color at (lower nibble) with A, row = X, column = ZP_RP
;;
;; input:    x     = row
;;           ZP_RP = col
;;           A     = color
;; modifies: ZP_TEMP, ZP_TEMP+1
;;           Y = 0
;; keeps:    A, X
(define-vm-function RT_SCREEN_PUT_COLOR_AT
  (list
          (TAY)
          (JSR PREP_ZP_TEMP_FOR_COLOR_ACCESS)
          (TYA)
          (LDY !$00)
          (STA (ZP_TEMP),y)

          (RTS)))

(module+ test #| rt screen put ytimes color at |#
  (define screen-put-color-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (list
      (LDX !5)
      (LDA !17)
      (STA ZP_RP)
      (LDA !2)
      (JSR RT_SCREEN_PUT_COLOR_AT))))

  (check-equal? (memory-list screen-put-color-test
                             (+ color-base-address (* 5 screen-row-bytes) 17)
                             )
                (list 2)))

;; put the color in A into char color ram of row X, col ZP_RP
;;
;; input:    x     = row
;;           ZP_RP = col
;;           A     = color
;;           y     = #of chars color
;; modifies: ZP_TEMP, ZP_TEMP+1
;;           Y = FF
;; keeps:    A, X
(define-vm-function RT_SCREEN_PUT_YTIMES_COLOR_AT
  (list
          (PHA)
          (JSR PREP_ZP_TEMP_FOR_COLOR_ACCESS)
          (PLA)
   (label loop__)
          (STA (ZP_TEMP),y)
          (DEY)
          (BPL loop__)

          (RTS)))

(module+ test #| rt screen put ytimes color at |#
  (define screen-put-ytimes-color-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (list
      (LDX !5)
      (LDA !17)
      (STA ZP_RP)
      (LDY !7)
      (LDA !2)
      (JSR RT_SCREEN_PUT_YTIMES_COLOR_AT))))

  (check-equal? (memory-list screen-put-ytimes-color-test
                             (+ color-base-address (* 5 screen-row-bytes) 17)
                             (+ color-base-address (* 5 screen-row-bytes) 21))
                (list 2 2 2 2 2)))

;; write a number of screen codes to y,x position on screen
;;
;; input:  x = ROW
;;         ZP_RP = COL
;;         RT_SCREEN_PUT_CHARS_AT__STRING+1 = ptr to screen code data (low at +1, high at +2)
;;         y = # of chars to print -1 (0 for one char, 1 for two ...)
;; output: screen modified
(define RT_SCREEN_PUT_CHARS_AT__STRING '()) ;; exported label for string source placement
(define-vm-function RT_SCREEN_PUT_CHARS_AT
  (list
          ;; optimization saves some time!!
          ;; (LDA SCREEN_ROW_MEM_TRANSLATION_TABLE,x) ;; x = row
          ;; (AND !$F8)
          ;; (CLC)
          ;; (ADC ZP_RP)
          ;; (STA write_screen_cmd__+1) ;; write offset

          ;; (LDA SCREEN_ROW_MEM_TRANSLATION_TABLE,x) ;; x = row
          ;; (AND !$07)
          ;; (ADC !$00)

          ;; (STA write_screen_cmd__+2) ;; write page
          (JSR PREP_ZP_TEMP_FOR_SCREEN_ACCESS)

   (label char_put_loop__)
   (label RT_SCREEN_PUT_CHARS_AT__STRING)
          (LDA $0400,y)
   (label write_screen_cmd__)
          (STA (ZP_TEMP),y) ;; STA $0400,y
          (DEY)                      ;; use y as both indices (zp_rp = ptr - col)
          (BPL char_put_loop__)

          (RTS)


   #|
      Translate a row to its screen memory offset and page value (using the row as index into the table):
      - the memory offset is held in the 5 msb, the page in the 3 lsb
      - extracting the page is done by AND #$07
      - extracting the row offset is done by AND #$F8
    |#
   (label SCREEN_ROW_MEM_TRANSLATION_TABLE)
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
                       69
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
                       129
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
                       429
                       "cpu cycles for writing string with 25 characters to position x,y"))

;; scroll n lines a number of chars one char to the right
;;
;; X     : screen row (last row)
;; ZP_RZ : number of rows (1.., up from last row, effectively scroll rows X, X-1, ... X-n)
;; ZP_RP : column (start col) COL->COL+delta
;; Y     : number of chars to copy -1
;; A     : delta (1.. to move right)
(define RT_SCREEN_SCROLL_RIGHT_CHARS_AT_BY1 '())
(define-vm-function-wol RT_SCREEN_SCROLL_RIGHT_CHARS_AT
  (list
   (label RT_SCREEN_SCROLL_RIGHT_CHARS_AT_BY1)
          (LDA !$01)
   (label RT_SCREEN_SCROLL_RIGHT_CHARS_AT)
          (CLC)
          (ADC ZP_RP)
          (STA ZP_RP+1) ;; set target col

          (TXA)         ;; row target = row source

          (JMP FAST_SCREEN_MEMCOPY_DOWN_CHARS)
   ))

(module+ test #| scroll right |#
  (define scroll-right-test2
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (write-string-to-screen--for-test 5 17 "OLALA")
     (write-string-to-screen--for-test 4 17 "TIPOP")
     (write-color-red--for-test 5 17 2)
     (write-color-red--for-test 4 20 2)

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
    (check-equal? (memory-list scroll-right-test2
                             (+ color-base-address (* 5 screen-row-bytes) 16)
                             (+ color-base-address (* 5 screen-row-bytes) 23))
                (append (list 0)
                        (list 2 2 2 0 0 0)
                        (list 0))
                "color was scrolled right.")
  (check-equal? (memory-list scroll-right-test2
                             (+ color-base-address (* 4 screen-row-bytes) 16)
                             (+ color-base-address (* 4 screen-row-bytes) 23))
                (append (list 0)
                        (list 0 0 0 0 2 2)
                        (list 0))
                "color line was scrolled right (too).")
  (inform-check-equal? (cpu-state-clock-cycles scroll-right-test2)
                392
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
                23648
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
;; input:  X = ROW (first row)
;;         ZP_RZ = # of rows
;;         ZP_RP =  (first col) COL -> COL-delta
;;         Y = # of chars to scroll
;;         A = delta (positive: 1..)
;; output: screen modified
(define RT_SCREEN_SCROLL_LEFT_CHARS_AT_BY1 '())
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

          (JMP FAST_SCREEN_MEMCOPY_UP_CHARS)))

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
                "line was scrolled left.")
  (inform-check-equal? (cpu-state-clock-cycles scroll-left-test)
                231
                "cpu cycles needed for scrolling 5 chars left")

  (define scroll-left-2-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (write-string-to-screen--for-test 5 17 "OLALA")
     (write-string-to-screen--for-test 6 17 "OLALA")
     (write-color-red--for-test 5 17 2)
     (write-color-red--for-test 6 20 2)

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
  (check-equal? (memory-list scroll-left-2-test
                             (+ color-base-address (* 5 screen-row-bytes) 17)
                             (+ color-base-address (* 5 screen-row-bytes) 21))
                (list 2 0 0 0 0)
                "string got scrolled one char left.")
  (check-equal? (memory-list scroll-left-2-test
                             (+ color-base-address (* 6 screen-row-bytes) 17)
                             (+ color-base-address (* 6 screen-row-bytes) 21))
                (list 0 0 2 2 2)
                "string got scrolled one char left.")
  (inform-check-equal? (cpu-state-clock-cycles scroll-left-2-test)
                401
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
                26557
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

;; scroll screen area down by A lines
;;   screenarea is: x=last row, zp_rp=first col, y = # of chars per line, zp_rz = number of lines
;;
;; input:  X = ROW (last row to scroll)
;;         ZP_RZ = # of rows to cop
;;         ZP_RP = COL
;;         Y = # of chars to scroll - 1 (per row)
;;         A = delta (positive: 1..)
;; output: screen modified
(define RT_SCREEN_SCROLL_DOWN_BY1 '())
(define-vm-function-wol RT_SCREEN_SCROLL_DOWN
  (list
   (label RT_SCREEN_SCROLL_DOWN_BY1)
          (LDA !$01)
   (label RT_SCREEN_SCROLL_DOWN)
          (STA ZP_TEMP)
          (LDA ZP_RP)
          (STA ZP_RP+1)  ;; target col = source col

          (TXA)
          (CLC)
          (ADC ZP_TEMP)

          (JMP FAST_SCREEN_MEMCOPY_DOWN_CHARS)))

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
                       240
                       "cpu cycles needed for scrolling 5 chars down")


  (define scroll-down-2-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     ;; now put the strings
     (write-string-to-screen--for-test 5 17 "OLALA")
     (write-string-to-screen--for-test 6 17 "TIPOP")
     (write-color-red--for-test 5 17 5)
     (write-color-red--for-test 6 17 5)

     (LDX !6)
     (LDY !17)
     (STY ZP_RP)
     (LDY !4)
     (LDA !$02)
     (STA ZP_RZ) ;; two rows

     (JSR $0100)
     (JSR RT_SCREEN_SCROLL_DOWN_BY1)))

  (check-equal? (memory-list scroll-down-2-test
                             (+ screen-base-address (* 6 screen-row-bytes) 17)
                             (+ screen-base-address (* 6 screen-row-bytes) 21))
                (map char->integer (string->list "OLALA"))
                "string was scrolled down.")
  (check-equal? (memory-list scroll-down-2-test
                             (+ screen-base-address (* 7 screen-row-bytes) 17)
                             (+ screen-base-address (* 7 screen-row-bytes) 21))
                (map char->integer (string->list "TIPOP"))
                "string was scrolled down.")
  (check-equal? (memory-list scroll-down-2-test
                             (+ color-base-address (* 6 screen-row-bytes) 17)
                             (+ color-base-address (* 6 screen-row-bytes) 21))
                (list 2 2 2 2 2)
                "string was scrolled down.")

  (check-equal? (memory-list scroll-down-2-test
                             (+ color-base-address (* 7 screen-row-bytes) 17)
                             (+ color-base-address (* 7 screen-row-bytes) 21))
                (list 2 2 2 2 2)
                "string was scrolled down.")
  (inform-check-equal? (cpu-state-clock-cycles scroll-down-2-test)
                       411
                       "cpu cycles needed for scrolling 5 chars down")

  (define scroll-down-3-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     ;; now put the strings
     (write-string-to-screen--for-test 5 17 "OLALA")
     (write-string-to-screen--for-test 6 17 "TIPOP")

     (LDX !6)
     (LDY !17)
     (STY ZP_RP)
     (LDY !4)
     (LDA !$02)
     (STA ZP_RZ) ;; two rows

     (LDA !$04) ;; scroll down by 4
     (JSR $0100)
     (JSR RT_SCREEN_SCROLL_DOWN)))

  (check-equal? (memory-list scroll-down-3-test
                             (+ screen-base-address (* 9 screen-row-bytes) 17)
                             (+ screen-base-address (* 9 screen-row-bytes) 21))
                (map char->integer (string->list "OLALA"))
                "string was scrolled down.")
  (check-equal? (memory-list scroll-down-3-test
                             (+ screen-base-address (* 10 screen-row-bytes) 17)
                             (+ screen-base-address (* 10 screen-row-bytes) 21))
                (map char->integer (string->list "TIPOP"))
                "string was scrolled down.")
  (inform-check-equal? (cpu-state-clock-cycles scroll-down-3-test)
                       409
                       "cpu cycles needed for scrolling 5 chars down"))

;; scroll an area of the screen up by A lines
;;   area is x=first row, zp_rp=first col, y = number of chars per line, zp_rz = number of rows
;;
;; input:  X = ROW (first row)
;;         ZP_RP = COL
;;         ZP_RZ = number of rows to scroll
;;         Y = # of chars to scroll
;;         A = delta (positive: 1..)
(define RT_SCREEN_SCROLL_UP_BY1 '())
(define-vm-function-wol RT_SCREEN_SCROLL_UP
  (list
   (label RT_SCREEN_SCROLL_UP_BY1)
          (LDA !$01)
   (label RT_SCREEN_SCROLL_UP)
          (STA ZP_TEMP)
          (LDA ZP_RP)
          (STA ZP_RP+1)  ;; target col = source col

          (TXA)
          (SEC)
          (SBC ZP_TEMP)

          (JMP FAST_SCREEN_MEMCOPY_UP_CHARS)))

(module+ test #| scroll up |#
  (define scroll-full-page-up-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     ;; fill the corners
     (write-string-to-screen--for-test 0 0 "O")
     (write-string-to-screen--for-test 0 39 "E")
     (write-string-to-screen--for-test 1 0 "N")
     (write-string-to-screen--for-test 1 39 "T")
     (write-string-to-screen--for-test 24 0 "U")
     (write-string-to-screen--for-test 24 39 "2")
     (write-color-red--for-test 0 0 1)
     (write-color-red--for-test 0 39 1)
     (write-color-red--for-test 1 0 1)
     (write-color-red--for-test 1 39 1)
     (write-color-red--for-test 24 0 1)
     (write-color-red--for-test 24 39 1)

     (LDX !1)
     (LDY !0)
     (STY ZP_RP)
     (LDY !40)
     (LDA !24)
     (STA ZP_RZ) ;; 24 rows (25 - 1)

     (JSR $0100)
     (JSR RT_SCREEN_SCROLL_UP_BY1)))
  (check-equal? (memory-list scroll-full-page-up-test
                             (+ screen-base-address (* 0 screen-row-bytes) 0))
                (map char->integer (string->list "N"))
                "line was scrolled up.")
  (check-equal? (memory-list scroll-full-page-up-test
                             (+ screen-base-address (* 0 screen-row-bytes) 39))
                (map char->integer (string->list "T"))
                "line was scrolled up.")
  (check-equal? (memory-list scroll-full-page-up-test
                             (+ screen-base-address (* 23 screen-row-bytes) 0))
                (map char->integer (string->list "U"))
                "line was scrolled up.")
  (check-equal? (memory-list scroll-full-page-up-test
                             (+ screen-base-address (* 23 screen-row-bytes) 39))
                (map char->integer (string->list "2"))
                "line was scrolled up.")
  (check-equal? (memory-list scroll-full-page-up-test
                             (+ color-base-address (* 0 screen-row-bytes) 0))
                (list 2)
                "line was scrolled up.")
  (check-equal? (memory-list scroll-full-page-up-test
                             (+ color-base-address (* 0 screen-row-bytes) 39))
                (list 2)
                "line was scrolled up.")
  (check-equal? (memory-list scroll-full-page-up-test
                             (+ color-base-address (* 23 screen-row-bytes) 0))
                (list 2)
                "line was scrolled up.")
  (check-equal? (memory-list scroll-full-page-up-test
                             (+ color-base-address (* 23 screen-row-bytes) 39))
                (list 2)
                "line was scrolled up.")
  (inform-check-equal? (cpu-state-clock-cycles scroll-full-page-up-test)
                       26125
                       "full page scroll up")


  (define scroll-up-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     ;; now put the string
     (write-string-to-screen--for-test 5 17 "OLALA")

     (LDX !5)
     (LDY !17)
     (STY ZP_RP)
     (LDY !5)
     (LDA !$01)
     (STA ZP_RZ) ;; one row

     (JSR $0100)
     (JSR RT_SCREEN_SCROLL_UP_BY1)))

  (check-equal? (memory-list scroll-up-test
                             (+ screen-base-address (* 4 screen-row-bytes) 17)
                             (+ screen-base-address (* 4 screen-row-bytes) 21))
                (map char->integer (string->list "OLALA"))
                "string was scrolled up.")
  (inform-check-equal? (cpu-state-clock-cycles scroll-up-test)
                       257
                       "cpu cycles needed for scrolling 5 chars up")


  (define scroll-up-2-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     ;; now put the strings
     (write-string-to-screen--for-test 5 17 "OLALA")
     (write-string-to-screen--for-test 6 17 "TIPOP")

     (LDX !5)
     (LDY !17)
     (STY ZP_RP)
     (LDY !5)
     (LDA !$02)
     (STA ZP_RZ) ;; two rows

     (JSR $0100)
     (JSR RT_SCREEN_SCROLL_UP_BY1)))

  (check-equal? (memory-list scroll-up-2-test
                             (+ screen-base-address (* 4 screen-row-bytes) 17)
                             (+ screen-base-address (* 4 screen-row-bytes) 21))
                (map char->integer (string->list "OLALA"))
                "string was scrolled up.")
  (check-equal? (memory-list scroll-up-2-test
                             (+ screen-base-address (* 5 screen-row-bytes) 17)
                             (+ screen-base-address (* 5 screen-row-bytes) 21))
                (map char->integer (string->list "TIPOP"))
                "string was scrolled up.")
  (inform-check-equal? (cpu-state-clock-cycles scroll-up-2-test)
                       442
                       "cpu cycles needed for scrolling 5 chars up")

  (define scroll-up-3-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     ;; now put the strings
     (write-string-to-screen--for-test 5 17 "OLALA")
     (write-string-to-screen--for-test 6 17 "TIPOP")

     (LDX !5)
     (LDY !17)
     (STY ZP_RP)
     (LDY !5)
     (LDA !$02)
     (STA ZP_RZ) ;; two rows

     (LDA !$04) ;; scroll up by 4
     (JSR $0100)
     (JSR RT_SCREEN_SCROLL_UP)))

  (check-equal? (memory-list scroll-up-3-test
                             (+ screen-base-address (* 1 screen-row-bytes) 17)
                             (+ screen-base-address (* 1 screen-row-bytes) 21))
                (map char->integer (string->list "OLALA"))
                "string was scrolled up.")
  (check-equal? (memory-list scroll-up-3-test
                             (+ screen-base-address (* 2 screen-row-bytes) 17)
                             (+ screen-base-address (* 2 screen-row-bytes) 21))
                (map char->integer (string->list "TIPOP"))
                "string was scrolled up.")
  (inform-check-equal? (cpu-state-clock-cycles scroll-up-3-test)
                       440
                       "cpu cycles needed for scrolling 5 chars up"))

;; clear the whole screen (and color ram)
(define-vm-function RT_SCREEN_CLEAR
  (list
          (PHA)
          (LDA !32)
          (LDX !$00)
   (label loop__)
          (STA $0400,x)
          (STA $0500,x)
          (STA $0600,x)
          (STA $0700,x)
          (DEX)
          (BNE loop__)
          (PLA)
          (LDX !$00)
   (label loop_color__)
          (STA $D800,x)
          (STA $D900,x)
          (STA $DA00,x)
          (STA $DB00,x)
          (DEX)
          (BNE loop_color__)
          (RTS)))

(module+ test #| clear screen |#
  (define screen-clear-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     ;; now put the string
     (write-string-to-screen--for-test 5 17 "O")
     (write-string-to-screen--for-test 0 0 "O")
     (write-string-to-screen--for-test 24 39 "O")

     (LDA !$01)
     (JSR $0100)
     (JSR RT_SCREEN_CLEAR)))

  (check-equal? (memory-list screen-clear-test
                             (+ screen-base-address (* 5 screen-row-bytes) 17))
                (list 32)
                "screen was clear where once written.")
  (check-equal? (memory-list screen-clear-test
                             (+ color-base-address (* 5 screen-row-bytes) 17))
                (list 1)
                "screen was clear where once written.")
  (check-equal? (memory-list screen-clear-test
                             (+ screen-base-address (* 0 screen-row-bytes) 0))
                (list 32)
                "screen was clear where once written.")
  (check-equal? (memory-list screen-clear-test
                             (+ screen-base-address (* 24 screen-row-bytes) 39))
                (list 32)
                "screen was clear where once written.")
  (inform-check-equal? (cpu-state-clock-cycles screen-clear-test)
                       12823
                       "cpu cycles to clear the whole screen"))

;; clear a number of chars in a row y,x position on screen
;;
;; input:  X = ROW
;;         ZP_RP = COL
;;         Y = # of chars to clear -1 (0 for one char, 1 for two ...)
;;         A = color (for clearing the color ram)
;; modifies: Y=0, ZP_TEMP, ZP_TEMP+1, ZP_TEMP3
;; output: screen modified
(define-vm-function RT_SCREEN_CLEAR_CHARS_AT
  (list
          (PHA)
          (STY ZP_TEMP3)
          (JSR PREP_ZP_TEMP_FOR_SCREEN_ACCESS)
          (LDA !$20) ;; screen code for space
   (label char_put_loop__)
          (STA (ZP_TEMP),y)
          (DEY)
          (BPL char_put_loop__)

          (CLC)
          (LDA !$D4)
          (ADC ZP_TEMP+1)
          (STA ZP_TEMP+1)
          (LDY ZP_TEMP3)
          (PLA)

   (label color_put_loop__)
          (STA (ZP_TEMP),y)
          (DEY)
          (BPL color_put_loop__)

          (RTS)))

(module+ test #| screen clear chars at |#
  (define screen-clear-chars-at-test
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

     (JSR RT_SCREEN_CLEAR_CHARS_AT)

     (write-string-to-screen--for-test 0 0 "OLALA")

     (LDX !0)
     (LDY !0)
     (STY ZP_RP)
     (LDY !4)
     (LDA !$01)

     (JSR RT_SCREEN_CLEAR_CHARS_AT)

     (write-string-to-screen--for-test 24 39 "O")

     (LDX !24)
     (LDY !39)
     (STY ZP_RP)
     (LDY !0)
     (LDA !$01)

     (JSR $0100)
     (JSR RT_SCREEN_CLEAR_CHARS_AT)
     ))

  (regression-test
   screen-clear-chars-at-test
   "clear part of a line of screen (including color)"

   (check-equal? (memory-list- screen-clear-chars-at-test
                               (+ screen-base-address (* 5 screen-row-bytes) 17)
                               5)
                 (list 32 32 32 32 32)
                 "screen was clear where once written.")
   (check-equal? (memory-list- screen-clear-chars-at-test
                               (+ color-base-address (* 5 screen-row-bytes) 17)
                               5)
                 (list 1 1 1 1 1)
                 "color was clear where once written.")
   (check-equal? (memory-list- screen-clear-chars-at-test
                               (+ screen-base-address (* 0 screen-row-bytes) 0)
                               5)
                 (list 32 32 32 32 32)
                 "screen was clear where once written.")
   (check-equal? (memory-list- screen-clear-chars-at-test
                               (+ color-base-address (* 0 screen-row-bytes) 0)
                               5)
                 (list 1 1 1 1 1)
                 "color was clear where once written.")
   (check-equal? (memory-list screen-clear-chars-at-test
                              (+ screen-base-address (* 24 screen-row-bytes) 39))
                 (list 32)
                 "screen was clear where once written.")
   (check-equal? (memory-list screen-clear-chars-at-test
                              (+ color-base-address (* 24 screen-row-bytes) 39))
                 (list 1)
                 "color was clear where once written.")
   (inform-check-equal? (cpu-state-clock-cycles screen-clear-chars-at-test)
                        100
                        "cpu cycles to clear a char")))

(define screen-code-0 48)
(define screen-code-A 1)
(define screen-code-space 32)

;; convert bcd value in zp_rp into screen codes (that can directly written to screen)
;;   zp_rp is not a bcd as encoded by the virtual machine <- TODO if wanted as general method
;;
;; input: ZP_RP bcd1 bcd0,
;;        ZP_RP+1 xxxx bcd3
(define-vm-function RT_BCD_TO_SCREEN_CODE
  (list   (LDA ZP_RP+1)
          (AND !$0f)
          (CLC)
          (ADC !48)
          (STA OUT__)

          (LDA ZP_RP)
          (AND !$f0)
          (LSR)
          (LSR)
          (LSR)
          (LSR)
          (CLC)
          (ADC !48)
          (STA OUT__+1)

          (LDA ZP_RP)
          (AND !$0f)
          (CLC)
          (ADC !48)
          (STA OUT__+2)
          (RTS)

   (label OUT__)
          (byte 48 48 48)))

(module+ test #| bcd to screen code |#
  (define bcd-to-screen-code-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (LDA !$02)
     (STA ZP_RP+1)
     (LDA !$43)
     (STA ZP_RP)        ;; zp := bcd (243)

     (JSR RT_BCD_TO_SCREEN_CODE)

     (LDA OUT__RT_BCD_TO_SCREEN_CODE)
     (STA $0400)
     (LDA OUT__RT_BCD_TO_SCREEN_CODE+1)
     (STA $0401)
     (LDA OUT__RT_BCD_TO_SCREEN_CODE+2)
     (STA $0402)))

  (check-equal? (memory-list bcd-to-screen-code-test #x0400 #x0402)
                (map (lambda (n) (+ screen-code-0 n))
                     (list 2 4 3))))

(define vm-screen-code
  (append
   FAST_SCREEN_MEMCOPY
   ;; PREP_WRITE_SCREEN_CMD
   RT_SCREEN_SCROLL_RIGHT_CHARS_AT
   RT_SCREEN_SCROLL_LEFT_CHARS_AT
   RT_SCREEN_SCROLL_UP
   RT_SCREEN_SCROLL_DOWN
   RT_SCREEN_PUT_CHARS_AT
   RT_SCREEN_CLEAR_CHARS_AT
   RT_SCREEN_CLEAR
   RT_SCREEN_PUT_YTIMES_COLOR_AT
   RT_SCREEN_PUT_COLOR_AT
   RT_BCD_TO_SCREEN_CODE
   PREP_ZP_TEMP_FOR_MEM_ACCESS))

(module+ test #| estimated-code-len |#
  (inform-check-equal? (estimated-code-len vm-screen-code)
                548
                "estimated code length change in screen runtime"))

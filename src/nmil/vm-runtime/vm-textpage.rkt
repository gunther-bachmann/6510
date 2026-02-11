#lang racket/base

(provide
 WRITE_TEXT_PAGE_LINE
 NEXT_TEXT_SECTION_RZ
 PREV_TEXT_SECTION_RZ

 create-text-page-data-for
 vm-textpage-code)

#|

 page type for holding long texts

 a text is organized as a list of text pages.
 each text page holds a number of text sections (which follow directly on one another)
 text is held

 | offset | description             |
 |--------+-------------------------|
 |     00 | page type = 0001 0100   |
 |     01 | prev page               | ;; allows for reverse scanning pages
 |     02 | offset to start of text | ;; allows forward scanning lines
 |     03 | offset to end of text   | ;; allows for reverse scanning lines
 |        | . . .                   |
 |     ff | next page               | ;; allows forward scanning pages


 text section w/i text page, allow for lines spanning over pages. another option would be to not allow page spanning lines.
 then rendering would have reduced options (simpler code), but more in memory waste.
 - [ ] check how complex the more compact solution will incurr

 | offset | description                                                                                      |
 |--------+--------------------------------------------------------------------------------------------------|
 |     00 | number of characters (forward nav)  (0 = empty line, > 0 = line w/ chars, >=128 no more entries) |
 |     01 | indentation level + LF Flag [fiii  iiii] (only present if n > 0)                                 |
 |        | n chars                                  (only present if n > 0)                                 |
 |    n+2 | number of characters (backward nav)      (only present if n > 0)                                 |


 operations:
   get offset to text section of line #n from start 0 = first section
   get # of lines on this page

   write text of line t_y col t_x to screen s_x s_y, width w (space for non existent)
   - spaces before because of indent: indent - t_x       (only if t_x < indent)
   - offset into a: t_x - indent (only if t_x >= indent and t_x < indent + n_a)
            #chars to write from a
   - offset into b: (only if f_a = 0 and ...)
            #chars to write from b
   - spaces after:  if f_a = 0: indent - t_x + n_a + n_b (only positive values are relevant)
                    if f_a = 1: indent - t_x + n_a       (only positive values are relevant)

   - option: ex ab wi (extended containing a and b, completely within)
               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+----+-----+-----+---+-----+    t_x < indent
            | indent |aaaaa|bbbbb| spaces  |    indent - t_x + n_a + n_b < width (spaces needed at end)
            +---+----+-----+-----+---------+    f_a = 0 (no line feed in a)
                |
               t_x
   - option: ex ab cf (extended containing a and b, cutting front)
               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+----------+-----+---+-----+    t_x > indent
            |aaaaaaaaaaaaaa|bbbbb| spaces  |    indent + n_a - t_x + n_b < width (spaces needed at end)
            +---+----------+-----+---------+    f_a = 0 (no line feed in a)
                |
               t_x
   - option: ex b cf (extended containing only b, cutting front)
               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+----------------+---+-----+    t_x > indent + n_a
            |bbbbbbbbbbbbbbbbbbbb| spaces  |    indent + n_a - t_x + n_b < width (spaces needed at end)
            +---+----------------+---------+    f_a = 0 (no line feed in a)
                |
               t_x
   - option: ex b cft (extended containing only b, cutting front and tail)
               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+--------------------+-----+    t_x > indent + n_a
            |bbbbbbbbbbbbbbbbbbbbbbbbbbbbbb|    indent + n_a - t_x + n_b >= width (no spaces needed at end)
            +---+--------------------------+    f_a = 0 (no line feed in a)
                |
               t_x
   - option: ex ab ct (extended containing a and b cutting tail)
               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+----+---------+-----+--+       t_x < indent
            | indent |aaaaaaaaa|bbbbbbbb|       indent + n_a + n_b - t_x >= width (no spaces needed)
            +---+----+---------+--------+       indent + n_a - t_x < width (b is needed!)
                |                               f_a = 0 (no line feed in a)
               t_x
   - option: ex ab cft (extended containing a and b, cutting front and tail)
               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+--------------+-----+--+       t_x > indent
            |aaaaaaaaaaaaaaaaaa|bbbbbbbb|       indent + n_a + n_b - t_x >= width (no spaces needed)
            +---+--------------+--------+       indent + n_a - t_x < width (b is needed)
                |                               f_a = 0 (no line feed in a)
               t_x
   - option: a wi (basic containing only a, completely within)
               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+----+-----------+---+-----+    t_x < indent
            | indent |aaaaaaaaaaa| spaces  |    indent + n_a - t_x < width (spaces needed at end)
            +---+----+-----------+---------+    f_a = 1 (line feed in a)
                |
               t_x
   - option: a cf (basic containing only a, cutting front)
               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+----------------+---+-----+    t_x > indent
            |aaaaaaaaaaaaaaaaaaaa| spaces  |    indent + n_a - t_x < width (spaces needed at end)
            +---+----------------+---------+    f_a = 1 (line feed in a)
                |
               t_x
   - option: a ct (basic containing only a, cutting tail)
               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+----+---------------+-----+    t_x < indent
            | indent |aaaaaaaaaaaaaaaaaaaaa|    indent + n_a - t_x > width (no spaces needed at end)
            +---+----+---------------------+    f_a = irrelevant
                |
               t_x
  - option: a cft (basic containing only a, cutting front and tail)
               s_x
                | <- width        -> |
      s_y    -> +--------------------+
            +---+--------------------+-----+    t_x >= indent
            |aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|    indent + n_a - t_x > width (no spaces needed at end)
            +---+--------------------------+    f_a = irrelevant
                |
               t_x


   - summary:
     - ex ab wi:
         t_x < indent
         indent - t_x + n_a + n_b < width (spaces needed at end)
         f_a = 0 (no line feed in a)
     - ex ab cf:
         t_x > indent
         indent + n_a - t_x + n_b < width (spaces needed at end)
         f_a = 0 (no line feed in a)
     - ex b cf:
         t_x > indent + n_a
         indent + n_a - t_x + n_b < width (spaces needed at end)
         f_a = 0 (no line feed in a)
     - ex b cft
         t_x > indent + n_a
         indent + n_a - t_x + n_b >= width (no spaces needed at end)
         f_a = 0 (no line feed in a)
     - ex ab ct
         t_x < indent
         indent + n_a + n_b - t_x >= width (no spaces needed)
         indent + n_a - t_x < width (b is needed!)
         f_a = 0 (no line feed in a)
     - ex ab cft
         t_x > indent
         indent + n_a + n_b - t_x >= width (no spaces needed)
         indent + n_a - t_x < width (b is needed)
         f_a = 0 (no line feed in a)
     - a wi
         t_x < indent
         indent + n_a - t_x < width (spaces needed at end)
         f_a = 1 (line feed in a)
     - a cf
         t_x > indent
         indent + n_a - t_x < width (spaces needed at end)
         f_a = 1 (line feed in a)
     - a ct
         t_x < indent
         indent + n_a - t_x > width (no spaces needed at end)
         f_a = irrelevant
     - a cft
         t_x >= indent
         indent + n_a - t_x > width (no spaces needed at end)
         f_a = irrelevant

     decision tree:
       f_a = 1:  (awi, acf, act, acft)
         t_x <= indent: (awi, act)
           indent + n_a - t_x < width (spaces needed at end): awi
           indent + n_a - t_x >= width (no spaces needed at end): act

         t_x > indent: (acf, acft)
           indent + n_a - t_x < width (spaces needed at end): acf
           indent + n_a - t_x >= width (no spaces needed at end): acft

       f_a = 0: (exabwi, exabcf, exbcf, exbcft, exabct, exabcft, act, acft)
         t_x <= indent: exabwi, exabct, act
           indent - t_x + n_a + n_b < width (spaces needed at end): exabwi
           indent - t_x + n_a + n_b >= width (no spaces needed)
             && indent - t_x + n_a < width (b is needed!): exabct
           indent - t_x + n_a > width (no spaces needed at end): act

         t_x > indent, t_x < indent + n_a: exabcf, exabcft, acft
           indent  - t_x+ n_a + n_b < width (spaces needed at end): exabcf
           indent - t_x + n_a + n_b >= width (no spaces needed)
             && indent - t_x + n_a < width (b is needed!): exabcft
           indent - t_x + n_a > width (no spaces needed at end): acft

         t_x >= indent + n_a: exbcf, exbcft
           indent - t_x + n_a + n_b < width (spaces needed at end): exbcf
           indent - t_x + n_a + n_b >= width (no spaces needed at end): exbcft

 |#

(require "../../6510.rkt"
         (only-in racket/string string-split string-trim string-join)
         (only-in racket/list range take empty?)
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
         (only-in "./vm-screen.rkt"
                  RT_SCREEN_PUT_CHARS_AT
                  RT_SCREEN_PUT_CHARS_AT__STRING
                  RT_SCREEN_CLEAR_CHARS_AT)
         (only-in "../../ast/6510-relocator.rkt"
                  estimated-code-len))

(module+ test #| require |#
  (require (only-in racket/string
                    string-replace)
           (only-in uuid
                    uuid-string)
           (only-in racket/list
                    range)
           (only-in "../test-utils.rkt"
                    regression-test)
           "../../6510-test-utils.rkt"
           (only-in "../../ast/6510-relocator.rkt"
                    estimated-code-len)
           (only-in "../../tools/6510-interpreter.rkt"
                    cpu-state-clock-cycles
                    peek
                    memory-list-)
           "./vm-memory-manager-test-utils.rkt"
           (only-in "./vm-memory-map.rkt"
                    VM_MEMORY_MANAGEMENT_CONSTANTS
                    ZP_RP
                    ZP_RZ)
           (only-in "./vm-screen.rkt"
                    vm-screen-code
                    screen-base-address))

  (define test-runtime
    (append
     vm-textpage-code
     vm-screen-code

     VM_MEMORY_MANAGEMENT_CONSTANTS
     (list (label VM_INIT_MEMORY_MANAGER) (RTS)))))

;; given curren text-section pointer in ZP_RZ move on to the next text-section
;; result zero-flag = 1, means no more text was available (BEQ)
;;        zero-flag = 0, means ZP_RZ holds the pointer to the next text-section (BNE)
;;
;; input:    ZP_RZ: pointer to text-section
;;           ZP_PAGE_REG: pointer to current text-page!
;; modifies: Y, A
;; result:   ZP_RZ: pointer to text-section (next if successful)
;;           zero-flag: indicator whether move to next line was successful
(define-vm-function NEXT_TEXT_SECTION_RZ
  (list
          (CLC)
          (LDY !$00)
          (LDA (ZP_RZ),y) ;; current len of line just printed
          (BEQ empty_line_inc__)
          (ADC ZP_RZ)
          (ADC !$02)
   (label empty_line_inc__)
          ;; check whether this is the last offset on this page, else move on to next page
          (LDY ZP_RZ+1)
          (STY ZP_PAGE_REG+1)
          (LDY !$02) ;; offset of end of last entry on this text page
          (CMP (ZP_PAGE_REG),y)
          (BNE continue_without_page_chage__)

          ;; switch to next text page
          (LDY !$ff)
          (LDA (ZP_RZ),y) ;; get next page
          (BEQ next__) ;; EQ

          (STA ZP_RZ+1)
          (STA ZP_PAGE_REG+1)

          ;; get offset to first text section
          (LDY !$01)
          (LDA (ZP_PAGE_REG),y)
          (STA ZP_RZ)
          (BNE next__) ;; NE

   (label continue_without_page_chage__)
          (CLC)
          (ADC !$01)
          (STA ZP_RZ)   ;; always != 0 -> NE

   (label next__)
          (RTS) ;; beq == no more text available
                ;; bne == successfully advanced zp_rz to next text-section
))

;; given curren text-section pointer in ZP_RZ move on to the next text-section
;; result zero-flag = 1, means no more text was available (BEQ) (happens if ZP_RZ points to the first text-section on the first textpage)
;;        zero-flag = 0, means ZP_RZ holds the pointer to the previous text-section (BNE)
;;
;; input:    ZP_RZ: pointer to text-section
;;           ZP_PAGE_REG: pointer to current text-page!
;; modifies: Y, A
;; result:   ZP_RZ: pointer to text-section (previous if successful)
;;           zero-flag: indicator whether move to prev line was successful
(define-vm-function PREV_TEXT_SECTION_RZ
  (list
          (DEC ZP_RZ) ;; point to #of chars of previous section (or behind first entry)
          (LDA ZP_RZ)
   (label on_end_of_prev__)
          (LDY !2)
          (CMP (ZP_PAGE_REG),y)
          (BMI look_for_previous_page__)
          (LDY !$00)
          (LDA (ZP_RZ),y)           ;; get number of chars in prev section
          (BEQ done__)              ;; if this is en empty line, we are done already

          (EOR !$ff)                ;; reverse
          (SEC)                     ;; set carry such that adc works as if (ZP_RZ),y was counted negative!
          (ADC ZP_RZ)
          (SEC)
          (SBC !2)                  ;; A = org-offset - 1 - n-chars - 2
          (STA ZP_RZ)
          (RTS)

   (label look_for_previous_page__)
          (LDY !1)
          (LDA (ZP_PAGE_REG),y) ;; load previous page
          (BEQ donenc__)
          (STA ZP_RZ+1)
          (STA ZP_PAGE_REG+1)
          (LDY !3)
          (LDA (ZP_PAGE_REG),y) ;; a = last offset
          (STA ZP_RZ)
          (BNE on_end_of_prev__)

   (label done__)
          (LDA !1) ;; NE
   (label donenc__)
          (RTS)))

(module+ test #| prev text section|#
  (define prev-text-section-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (list
      (LDA !<text_line_1__benchmark)
      (STA ZP_RZ)
      (LDA !>text_line_1__benchmark)
      (STA ZP_RZ+1)
      (STA ZP_PAGE_REG+1)

      (JSR PREV_TEXT_SECTION_RZ)

      (LDA !<text_line_0__benchmark)
      (STA ZP_RP)
      (LDA !>text_line_0__benchmark)
      (STA ZP_RP+1)
      (BRK)

      (org-align #x100)
      (create-text-page-data-for
       (string-join
        (list
         "HELLO MR WORLD"
         "  TODAY I WANT TO SHOW YOU A WINDOWED"
         "DISPLAY OF TEXT"
         )
        "\n")
       "__benchmark"))))

  (check-equal? (memory-list- prev-text-section-test ZP_RZ 02)
                (memory-list- prev-text-section-test ZP_RP 02)
                "rz = rp, which is a pointer to the previous text section")

  (define prev-text-section-test-no-more
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (list
             (LDA !<text_line_0__benchmark)
             (STA ZP_RZ)
             (LDA !>text_line_0__benchmark)
             (STA ZP_RZ+1)
             (STA ZP_PAGE_REG+1)

             (JSR PREV_TEXT_SECTION_RZ)
             (BEQ no_more_data_found__)
             (LDA !$01)
             (STA ZP_RP)
             (BRK)
      (label no_more_data_found__)
             (LDA !$00)
             (STA ZP_RP)
             (BRK)

      (org-align #x100)
      (create-text-page-data-for
       (string-join
        (list
         "HELLO MR WORLD"
         "  TODAY I WANT TO SHOW YOU A WINDOWED"
         "DISPLAY OF TEXT"
         )
        "\n")
       "__benchmark"))))

  (check-equal? (memory-list- prev-text-section-test-no-more ZP_RP)
                (list 0)
                "there is no previous text section to navigate to"))


;; input: screen_x
;;        X = screen_y
;;        scroll_x (or t_x, scroll_index, 0 = not left/right scrolled)
;;        window_width
;;        ZP_RZ = text-ptr (pointer into text buffer, start of text section) (ZP_RZ), format [payload-len][indent][ ... payload ...][payload-len]
;; modified: A, Y
;;           zp_temp3 = # of blanks written (up front)
;;           zp_rp+1  = # of characters written from text section
;;           zp_rp = col for calls to text output functions
;;           zp_temp + zp_temp+1 is used by text output functions
;;           RT_SCREEN_PUT_CHARS_AT__STRING+1..2 = location for text output functions (overwritten during output)
;; output:   screen memory and color memory is overwritten
;; TODO: color of text output?
;; TODO: fill parameter from current window ?
(define-vm-function WRITE_TEXT_PAGE_LINE
  (list
          (LDY !$00)
          (STY ZP_TEMP3)        ;; init blanks written with 0
          (LDA (ZP_RZ),y)       ;; #of chars in line
          (BEQ empty_line__)    ;; if 0 -> empty line
          (LDY !$01)
          (LDA (ZP_RZ),y)       ;; read indent
          (SEC)
          (SBC scroll_x)
          (BPL spaces_up_front__)
          ;; neg_index_in_a__
          ;; negate A, then jump to text__
          (EOR !$ff)
          (ADC !1)              ;; carry is clear (burrow took place)
          ;; a = index into A
          (BCC text__)          ;; always jump (carry is still clear)

   (label spaces_up_front__)
          ;; write spaces starting at screen_x num (min A window_width)
          (CMP window_width)
          (BPL empty_line__)    ;; branch if only a completely empty line needs printing
          ;; write A times spaces, then some of the string needs printing too (from the start)
          ;; A = #blanks, screen_x, screen_y
          (STA ZP_TEMP3)
          (TAY)
          (LDA screen_x)
          (STA ZP_RP)
          [LDA !1] ;; white

          (JSR RT_SCREEN_CLEAR_CHARS_AT) ;; zp_rp = col, x = row, y = #, a = color (INCOMPLETE)
          (LDA !$00)
   (label text__)
          (STA ZP_TEMP)         ;; index into text to start printing
          (LDY !$00)
          (SEC)
          (LDA (ZP_RZ),y)       ;; length of string
          (SBC ZP_TEMP)
          (BMI empty_line__)    ;; line is completely empty
          (CLC)
          (ADC ZP_TEMP3)
          ;; check len to print with screen width
          ;; A = len to print
          ;; ZP_TEMP = index into text to start printing
          (CMP window_width)
          (BMI use_a_for_num_chars__) ;; screen-width < num chars -> print len # of chars
          (LDA window_width)    ;; print screen width # of chars
          (SEC)
          (SBC ZP_TEMP3)
          (JMP write_len_a_to_screen__)
          ;; done, no spaces at end

   (label use_a_for_num_chars__)
          (SEC)
          (SBC ZP_TEMP3)
          (STA ZP_RP+1)
          (JSR write_len_a_to_screen__)
          (LDA window_width)
          (SEC)
          (SBC ZP_RP+1)
          (SEC)
          (SBC ZP_TEMP3)
          ;; a = number of spaces to print

   (label spaces_at_end__)
          (STA ZP_TEMP)
          (LDA screen_x)
          (CLC)
          (ADC window_width)
          (SEC)
          (SBC ZP_TEMP)
          (STA ZP_RP)           ;; col = screen_x + width - spaces to print
          (LDY ZP_TEMP)         ;; Y = # of spaces
          (DEY)                 ;; function wants # of spaces-1
          (LDA !1)              ;; white
          (JMP RT_SCREEN_CLEAR_CHARS_AT) ;; zp_rp = col, x = row, y = #, a = color (INCOMPLETE)

   (label empty_line__)
          ;; write a complete empty line
          (LDA screen_x)
          (STA ZP_RP)
          (LDY window_width)
          (DEY)
          (LDA !1) ;; white
          (JMP RT_SCREEN_CLEAR_CHARS_AT)

   (label write_len_a_to_screen__)
          (TAY)
          (DEY)

          ;; y = num of chars to print
          ;; zp_temp = index into text
          (LDA ZP_TEMP)
          (CLC)
          (ADC !$02)            ;; move over header of test section
          (ADC ZP_RZ)           ;; no carry, since chars are on one page!
          (STA RT_SCREEN_PUT_CHARS_AT__STRING+1)
          (LDA ZP_RZ+1)
          (STA RT_SCREEN_PUT_CHARS_AT__STRING+2)
          (LDA screen_x)
          (CLC)
          (ADC ZP_TEMP3)
          (STA ZP_RP)

          ;; input:  x = ROW
          ;;         ZP_RP = COL
          ;;         RT_SCREEN_PUT_CHARS_AT__STRING+1 = ptr to screen code data (low at +1, high at +2)
          ;;         y = # of chars to print -1 (0 for one char, 1 for two ...)
          (JMP RT_SCREEN_PUT_CHARS_AT)

   (label scroll_x)
          (byte $00)
   (label screen_x)
          (byte $00)
   (label window_width)
          (byte $00)))

(module+ test #| write_text_page_line |#
  (define write-text-page-line-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
            (LDA !<text_section)
            (STA ZP_RZ)
            (LDA !>text_section)
            (STA ZP_RZ+1)

            (LDA !10)                   ;;            s_x
            (STA screen_x)              ;;             | <- width        -> |
            (LDX !7)                    ;;   s_y    -> +--------------------+
            (LDA !0)                    ;;             +--------+-----------+---------+
            (STA scroll_x)              ;;             | indent |aaaaaaaaaaaaaaaaaaaaa|
            (LDA !8)                    ;;             +--------+---------------------+
            (STA window_width)          ;;             |
            (JSR WRITE_TEXT_PAGE_LINE)  ;;            t_x

            (LDA !10)                   ;;           s_x
            (STA screen_x)              ;;            | <- width        -> |
            (LDX !8)                    ;;  s_y    -> +--------------------+
            (LDA !3)                    ;;        +---+----+---------------+-----+
            (STA scroll_x)              ;;        | indent |aaaaaaaaaaaaaaaaaaaaa|
            (LDA !8)                    ;;        +---+----+---------------------+
            (STA window_width)          ;;            |
            (JSR WRITE_TEXT_PAGE_LINE)  ;;           t_x

            (LDA !10)                   ;;           s_x
            (STA screen_x)              ;;            | <- width        -> |
            (LDX !9)                    ;;  s_y    -> +--------------------+
            (LDA !5)                    ;;   +--------+--------------------++
            (STA scroll_x)              ;;   | indent |aaaaaaaaaaaaaaaaaaaaa|
            (LDA !8)                    ;;   +--------+---------------------+
            (STA window_width)          ;;            |
            (JSR WRITE_TEXT_PAGE_LINE)  ;;           t_x

            (LDA !10)                   ;;           s_x
            (STA screen_x)              ;;            | <- width        -> |
            (LDX !10)                   ;;  s_y    -> +--------------------+
            (LDA !6)                    ;;        +---+--------------------+-----+
            (STA scroll_x)              ;;        |aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|
            (LDA !8)                    ;;        +---+--------------------------+
            (STA window_width)          ;;            |
            (JSR WRITE_TEXT_PAGE_LINE)  ;;           t_x

            (LDA !10)                   ;;           s_x
            (STA screen_x)              ;;            | <- width        -> |
            (LDX !11)                   ;;  s_y    -> +--------------------+
            (LDA !10)                   ;;  +---------+--------------------+
            (STA scroll_x)              ;;  |aaaaaaaaaaaaaaaaaaaaaaaaaaaaaa|
            (LDA !8)                    ;;  +---------+--------------------+
            (STA window_width)          ;;            |
            (JSR WRITE_TEXT_PAGE_LINE)  ;;           t_x

            (LDA !10)                   ;;           s_x
            (STA screen_x)              ;;            | <- width        -> |
            (LDX !12)                   ;;  s_y    -> +--------------------+
            (LDA !11)                   ;;        +---+----------------+---+-----+
            (STA scroll_x)              ;;        |aaaaaaaaaaaaaaaaaaaa| spaces  |
            (LDA !8)                    ;;        +---+----------------+---------+
            (STA window_width)          ;;            |
            (JSR WRITE_TEXT_PAGE_LINE)  ;;           t_x

            (LDA !10)                   ;;           s_x
            (STA screen_x)              ;;            | <- width        -> |
            (LDX !13)                   ;;  s_y    -> +--------------------+
            (LDA !17)                   ;;        +---+-+------------------+-----+
            (STA scroll_x)              ;;        |aaaaa|        spaces          |
            (LDA !8)                    ;;        +---+-+------------------------+
            (STA window_width)          ;;            |
            (JSR WRITE_TEXT_PAGE_LINE)  ;;           t_x

            (LDA !10)                   ;;           s_x
            (STA screen_x)              ;;            | <- width        -> |
            (LDX !14)                   ;;  s_y    -> +--------------------+
            (LDA !19)                   ;;        +---+--------------------+-+
            (STA scroll_x)              ;;        |aaa|       spaces         |
            (LDA !8)                    ;;        +---+----------------------+
            (STA window_width)          ;;            |
            (JSR WRITE_TEXT_PAGE_LINE)  ;;           t_x

            (LDA !10)                   ;;           s_x
            (STA screen_x)              ;;            | <- width        -> |
            (LDX !15)                   ;;  s_y    -> +--------------------+
            (LDA !20)                   ;;       +---+---------------------++
            (STA scroll_x)              ;;       |aaa|       spaces         |
            (LDA !8)                    ;;       +---+----------------------+
            (STA window_width)          ;;            |
            (JSR WRITE_TEXT_PAGE_LINE)  ;;           t_x

            (LDA !<text_section_large_indent)
            (STA ZP_RZ)
            (LDA !>text_section_large_indent)
            (STA ZP_RZ+1)

            (LDA !10)                   ;;            s_x
            (STA screen_x)              ;;             | <- width        -> |
            (LDX !16)                   ;;   s_y    -> +--------------------+
            (LDA !0)                    ;;             +--------------------+--+-------+
            (STA scroll_x)              ;;             |         indent        |aaaaaaa|
            (LDA !8)                    ;;             +-----------------------+-------+
            (STA window_width)          ;;             |
            (JSR WRITE_TEXT_PAGE_LINE)  ;;            t_x

            (LDA !10)                   ;;            s_x
            (STA screen_x)              ;;             | <- width        -> |
            (LDX !17)                   ;;   s_y    -> +--------------------+
            (LDA !08)                   ;;          +--+--------------------+-------+
            (STA scroll_x)              ;;          |         indent        |aaaaaaa|
            (LDA !8)                    ;;          +--+--------------------+-------+
            (STA window_width)          ;;             |
            (JSR WRITE_TEXT_PAGE_LINE)  ;;            t_x

            (LDA !10)                   ;;            s_x
            (STA screen_x)              ;;             | <- width        -> |
            (LDX !18)                   ;;   s_y    -> +--------------------+
            (LDA !09)                   ;;         +---+-------------------++------+
            (STA scroll_x)              ;;         |         indent        |aaaaaaa|
            (LDA !8)                    ;;         +---+-------------------+-------+
            (STA window_width)          ;;             |
            (JSR WRITE_TEXT_PAGE_LINE)  ;;            t_x

            (LDA !<text_section_empty_line)
            (STA ZP_RZ)
            (LDA !>text_section_empty_line)
            (STA ZP_RZ+1)

            (LDA !10)
            (STA screen_x)
            (LDX !19)
            (LDA !03) ;; irrelevant
            (STA scroll_x)
            (LDA !8)
            (STA window_width)
            (JSR WRITE_TEXT_PAGE_LINE)

            (BRK)

            (org-align #x100)
     (label text_section)
            (byte $0d   ;; len
                  $05   ;; indent
                  $01 $02 $03 $04 $05 $06 $07 $08 $09 $0a $0b $0c $0d
                  $09   ;; len
                  )
     (label text_section_large_indent)
            (byte $0d   ;; len
                  $10   ;; indent
                  $01 $02 $03 $04 $05 $06 $07 $08 $09 $0a $0b $0c $0d
                  $09   ;; len
                  )
     (label text_section_empty_line)
            (byte $00) ;; len = 0 => empty line
            ))

  (regression-test
   write-text-page-line-test
   "write string from text page onto a screen line window"
   (check-equal? (memory-list-
                  write-text-page-line-test
                  (+ (* 40 7) screen-base-address 9)
                  10)
                 (list 0 32 32 32 32 32 1 2 3 0)
                 "0 = not written, write indent spaces first then 3 characters")
   (check-equal? (memory-list-
                  write-text-page-line-test
                  (+ (* 40 8) screen-base-address 9)
                  10)
                 (list 0 32 32 1 2 3 4 5 6 0)
                 "0 = not written, write indent-3 spaces first then 6 characters")
   (check-equal? (memory-list-
                  write-text-page-line-test
                  (+ (* 40 9) screen-base-address 9)
                  10)
                 (list 0 1 2 3 4 5 6 7 8 0)
                 "0 = not written, write starting at offset 0 only 8 characters")
   (check-equal? (memory-list-
                  write-text-page-line-test
                  (+ (* 40 11) screen-base-address 9)
                  10)
                 (list 0 6 7 8 9 10 11 12 13 0)
                 "0 = not written, write starting at offset 5 only 8 characters")
   (check-equal? (memory-list-
                  write-text-page-line-test
                  (+ (* 40 12) screen-base-address 9)
                  10)
                 (list 0 7 8 9 10 11 12 13 32 0)
                 "0 = not written, write starting at offset 6 only 7 characters then space at the end")
   (check-equal? (memory-list-
                  write-text-page-line-test
                  (+ (* 40 13) screen-base-address 9)
                  10)
                 (list 0 13 32 32 32 32 32 32 32 0)
                 "0 = not written, write starting at offset 12 only 1 character, rest spaces")
   (check-equal? (memory-list-
                  write-text-page-line-test
                  (+ (* 40 14) screen-base-address 9)
                  10)
                 (list 0 32 32 32 32 32 32 32 32 0)
                 "0 = not written, only  spaces")
   (check-equal? (memory-list-
                  write-text-page-line-test
                  (+ (* 40 15) screen-base-address 9)
                  10)
                 (list 0 32 32 32 32 32 32 32 32 0)
                 "0 = not written, only  spaces")
   (check-equal? (memory-list-
                  write-text-page-line-test
                  (+ (* 40 16) screen-base-address 9)
                  10)
                 (list 0 32 32 32 32 32 32 32 32 0)
                 "0 = not written, only  spaces")
   (check-equal? (memory-list-
                  write-text-page-line-test
                  (+ (* 40 17) screen-base-address 9)
                  10)
                 (list 0 32 32 32 32 32 32 32 32 0)
                 "0 = not written, only  spaces")
   (check-equal? (memory-list-
                  write-text-page-line-test
                  (+ (* 40 18) screen-base-address 9)
                  10)
                 (list 0 32 32 32 32 32 32 32 1 0)
                 "0 = 7 spaces in front, then 1 char from string")
   (check-equal? (memory-list-
                  write-text-page-line-test
                  (+ (* 40 19) screen-base-address 9)
                  10)
                 (list 0 32 32 32 32 32 32 32 32 0)
                 "only spaces since it is an empty line")

   (inform-check-equal? (cpu-state-clock-cycles write-text-page-line-test)
                        5640
                        "clock cycles needed for windowed write text page to screen"))

  (define write-one-column-over-24-lines
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
            (LDA !25)
            (STA LOOP_COUNTER)

     (label LOOP)
            (LDA !<text_section)
            (STA ZP_RZ)
            (LDA !>text_section)
            (STA ZP_RZ+1)

            (LDA !10)
            (STA screen_x)
            (LDX LOOP_COUNTER)
            (LDA !04)
            (STA scroll_x)
            (LDA !1)
            (STA window_width)
            (JSR WRITE_TEXT_PAGE_LINE)

            (DEC LOOP_COUNTER)
            (BNE LOOP)
            (BRK)

            (org-align #x100)
      (label text_section)
             (byte $14)
             (byte $02)
             (byte 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20)
             (byte $14)
      (label LOOP_COUNTER)
             (byte 0)))

  (regression-test
   write-one-column-over-24-lines
   "check 24 rows writing one col each"
   (check-equal? (map (lambda (adr) (peek write-one-column-over-24-lines adr ))
                      (map (lambda (line) (+ (* 40 line) screen-base-address 10))
                           (range 1 25)))
                 (build-list 24 (lambda (_) 3)))
   (check-equal? (map (lambda (adr) (peek write-one-column-over-24-lines adr ))
                      (map (lambda (line) (+ (* 40 line) screen-base-address 11))
                           (range 1 25)))
                 (build-list 24 (lambda (_) 0)))
   (check-equal? (map (lambda (adr) (peek write-one-column-over-24-lines adr ))
                      (map (lambda (line) (+ (* 40 line) screen-base-address 9))
                           (range 1 25)))
                 (build-list 24 (lambda (_) 0)))
   (inform-check-equal? (cpu-state-clock-cycles write-one-column-over-24-lines)
                        6433)))


;; flow:
;;   cursor movement does not change anything
;;   on idle: merge current open edit session pages
;;   on edit: 1. do merge if open
;;            2. prepare page with lines before edit position
;;            3. prepare page with lines after edit position
;;            4. prepare page with current line only
;;            5. insert character (move tail if necessary)
;;               cursor movement within the line keeps you in step 5
;;               cursor movement off this line => goto top of steps
;; idea: how to insert empty lines fast


;; create data for text pages of the given string
(define (create-text-page-data-for str (postfix ""))
  (define splitted-lines (string-split str "\n" #:trim? #f))
  (define lines (if (empty? splitted-lines) (list "") splitted-lines))
  (define encoded-lines
    (map (lambda (line idx)
           (define indented-line (string-trim line))
           (if (= 0 (string-length indented-line))
               (list (ast-label-def-cmd '() (format (string-join (list "text_line_~a" postfix) "") idx))
                     (ast-bytes-cmd '() (list 0)))
               (list (ast-label-def-cmd '() (format (string-join (list "text_line_~a" postfix) "") idx))
                     (ast-bytes-cmd '() (list (string-length indented-line)))
                     (ast-bytes-cmd '() (list (- (string-length line) (string-length indented-line))))
                     (ast-bytes-cmd '() (map (lambda (char) (if (>= (char->integer char) 65) (- (char->integer char) 64) (char->integer char))) (string->list indented-line)))
                     (ast-bytes-cmd '() (list (string-length indented-line))))))
         lines
         (range (length lines))))
  (define encoded-text (apply append encoded-lines))
  (append
   (list (byte $14 $00 $04) ;; page type $14, previous page 0, first offset at 3
         (ast-bytes-cmd '() (list (+ 3 (estimated-code-len encoded-text))))) ;; last offset at ...
   encoded-text
   (map (lambda (idx) (byte $00)) (range (- 251 (estimated-code-len encoded-text))))
   (list (byte $00)) ;; next page is 0
   ))

(module+ test #| create-text-page |#
  (check-equal? (estimated-code-len (create-text-page-data-for ""))
                256)
  (check-equal? (take (create-text-page-data-for "") 4)
                (list
                 (ast-bytes-cmd '() '(20 0 4))
                 (ast-bytes-cmd '() '(4))
                 (ast-label-def-cmd '() "text_line_0")
                 (ast-bytes-cmd '() '(0))))
  (check-equal? (take (create-text-page-data-for "H") 7)
                (list
                 (ast-bytes-cmd '() '(20 0 4))
                 (ast-bytes-cmd '() '(7)) ;; ptr to tail
                 (ast-label-def-cmd '() "text_line_0")
                 (ast-bytes-cmd '() '(1)) ;; len
                 (ast-bytes-cmd '() '(0)) ;; indent
                 (ast-bytes-cmd '() '(8)) ;; H
                 (ast-bytes-cmd '() '(1)) ;; len
                 ))

  (check-equal? (take (create-text-page-data-for "   H") 7)
                (list
                 (ast-bytes-cmd '() '(20 0 4))
                 (ast-bytes-cmd '() '(7)) ;; ptr to tail
                 (ast-label-def-cmd '() "text_line_0")
                 (ast-bytes-cmd '() '(1)) ;; len
                 (ast-bytes-cmd '() '(3)) ;; indent
                 (ast-bytes-cmd '() '(8)) ;; H
                 (ast-bytes-cmd '() '(1)) ;; len
                 ))
  (check-equal? (take (create-text-page-data-for "   HELLO") 7)
                (list
                 (ast-bytes-cmd '() '(20 0 4))
                 (ast-bytes-cmd '() '(11)) ;; ptr to tail
                 (ast-label-def-cmd '() "text_line_0")
                 (ast-bytes-cmd '() '(5)) ;; len
                 (ast-bytes-cmd '() '(3)) ;; indent
                 (ast-bytes-cmd '() '(8 5 12 12 15)) ;; HELLO
                 (ast-bytes-cmd '() '(5)) ;; len
                 ))
  (check-equal? (take (create-text-page-data-for "   HELLO\n THERE") 12)
                (list
                 (ast-bytes-cmd '() '(20 0 4))
                 (ast-bytes-cmd '() '(19)) ;; ptr to tail
                 (ast-label-def-cmd '() "text_line_0")
                 (ast-bytes-cmd '() '(5)) ;; len
                 (ast-bytes-cmd '() '(3)) ;; indent
                 (ast-bytes-cmd '() '(8 5 12 12 15)) ;; H
                 (ast-bytes-cmd '() '(5)) ;; len
                 (ast-label-def-cmd '() "text_line_1")
                 (ast-bytes-cmd '() '(5)) ;; len
                 (ast-bytes-cmd '() '(1)) ;; indent
                 (ast-bytes-cmd '() '(20 8 5 18 5)) ;; THERE
                 (ast-bytes-cmd '() '(5))
                 )))

;; prepare text buffer for editing
;;
;; (split text page into text-page before, text-page after and text-page for editing)

(define vm-textpage-code
  (append
   WRITE_TEXT_PAGE_LINE
   NEXT_TEXT_SECTION_RZ
   PREV_TEXT_SECTION_RZ))

(module+ test #| code len |#
  (inform-check-equal? (estimated-code-len vm-textpage-code)
                       284
                       "estimated code len of text page code"))

#lang racket/base

(provide

 WINDOW_RENDER_COMPLETE ;; zp_rt is the pointer to the window structure

 WINDOW_INIT            ;; initialize window structure with defaults and parameters
 WINDOW_SCROLL_LEFT     ;; scroll window one char left (done, when cursor is moving right and scroll_x < MAX_SCROLL_X)
 WINDOW_SCROLL_RIGHT    ;; scroll window one char right (done, when cursor is moving left, and scroll_x > 0)
 WINDOW_SCROLL_UP       ;; scroll window one line up (done when cursor is moving down, and vis_line_start != vis_line_last)
 WINDOW_SCROLL_DOWN     ;; scroll window one line down (done when cursor is moving up, and scroll_y > 0)

 ;; indices into vm_window structure (in native array, so add +2)
 vm_window__screen_x                    ;; position on screen 0..39
 vm_window__screen_y                    ;; position on screen 0..24
 vm_window__width                       ;; width 1..40
 vm_window__height                      ;; height 1..25
 vm_window__cursor_x                    ;; cursor position in window 0..width-1 (for screen coordinate add screen_x)
 vm_window__cursor_y                    ;; cursor position in window 0..height-1 (for screen coordinate add screen_y)
 vm_window__char_pos                    ;; unused (position in textbuffer by numbers of characters from start)
 vm_window__vis_line_start              ;; pointer to the first visible text-section in line 0
 vm_window__scroll_y                    ;; word holding the current text line number of the first visible text 0..
 vm_window__vis_line_last               ;; pointer to the last visible text section
 vm_window__scroll_x                    ;; number of chars scrolled to the left (0..MAX_SCROLL_X-1)
 vm_window__first_textpage              ;; first text page for display w/i this window
 vm_window__num_trailing_empty_lines    ;; number of empty lines after vis_line_last that is displayed on screen must be < height

 vm-window-defs                         ;; definitions for other assembler routines
 vm-window-code)                        ;; complete code (including definitions)

#|

 window is a visible object on a buffer

 TODO:
  - add test coverage (especially for adjustments to scroll_y, scroll_x, cursor_x, cursor_y, num_trailing_empty_lines)
  - make code more compact
  - scroll page wise up/down (redisplay with other scroll_y)

 it holds:
    | offset | description                                                    |
    |--------+----------------------------------------------------------------|
    |      0 | screen-position-x                                              |
    |      1 | screen-position-y                                              |
    |      2 | scroll-position-x (0 based)                                    |
    |   3..4 | scroll-posiition-y = line # of first visible row (0 based)     |
    |      5 | width (1..40)                                                  |
    |      6 | height (1..25)                                                 |
    |        | cursor-position:                                               |
    |      7 | cursor-x -> logical-X = cursor-x + scroll-position-x (0 based) |
    |      8 | cursor-y -> logical-Y = cursor-y + scroll-position-y (0 based) |
    |  9..10 | char-position (in file) (0 based)                              |
    | 11..12 | ptr to first visible row (to text section)                     |
    | 13..14 | ptr to last visible row (to text section)                      |
    |     15 | first text page                                                |
    |     16 | number of lines after the last line, currently displayed blank |


 actions:
   cursor movement (up*, down*, left*, right*, page-down/up, eol, bol, eof, bof)
   scroll (up*, down*, left*, right*)
   init-window (initialize window based on textpages passed)
   render-window (increment||full)
   insert newline

 |#

(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
         (only-in "./vm-screen.rkt"
                  RT_SCREEN_PUT_CHARS_AT
                  RT_SCREEN_PUT_CHARS_AT__STRING
                  RT_SCREEN_SCROLL_RIGHT_CHARS_AT_BY1
                  RT_SCREEN_SCROLL_LEFT_CHARS_AT_BY1
                  RT_SCREEN_SCROLL_UP_BY1
                  RT_SCREEN_SCROLL_DOWN_BY1
                  RT_SCREEN_CLEAR_CHARS_AT)
         (only-in "./vm-textpage.rkt"
                  WRITE_TEXT_PAGE_LINE
                  NEXT_TEXT_SECTION_RZ
                  PREV_TEXT_SECTION_RZ
                  create-text-page-data-for)
         (only-in racket/string string-join)
         (only-in "./vm-m1-slots.rkt"
                  ALLOC_M1_SLOT_TO_RT))


(define vm_window__screen_x                  0) #| byte |#
(define vm_window__screen_y                  1) #| byte |#
(define vm_window__scroll_x                  2) #| byte |#
(define vm_window__scroll_y                  3) #| word |#
(define vm_window__width                     5) #| byte |#
(define vm_window__height                    6) #| byte |#
(define vm_window__cursor_x                  7) #| byte |#
(define vm_window__cursor_y                  8) #| byte |#
(define vm_window__char_pos                  9) #| word |#
(define vm_window__vis_line_start           11) #| word |#
(define vm_window__vis_line_last            13) #| word |#
(define vm_window__first_textpage           15) #| byte |#
(define vm_window__num_trailing_empty_lines 16) #| byte |#

(define MAX_SCROLL_X 80) #| byte |#

(define vm-window-defs
  (list
   (byte-const vm_window__screen_x                  0) #| byte |#
   (byte-const vm_window__screen_y                  1) #| byte |#     
   (byte-const vm_window__scroll_x                  2) #| byte |#     
   (byte-const vm_window__scroll_y                  3) #| word |#     
   (byte-const vm_window__width                     5) #| byte |#        
   (byte-const vm_window__height                    6) #| byte |#       
   (byte-const vm_window__cursor_x                  7) #| byte |#     
   (byte-const vm_window__cursor_y                  8) #| byte |#     
   (byte-const vm_window__char_pos                  9) #| word |#     
   (byte-const vm_window__vis_line_start           11) #| word |#
   (byte-const vm_window__vis_line_last            13) #| word |#
   (byte-const vm_window__first_textpage           15) #| byte |#
   (byte-const vm_window__num_trailing_empty_lines 16) #| byte |#

   (byte-const MAX_SCROLL_X 80) #| byte |#
))

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
                    ZP_RA
                    ZP_RT
                    ZP_RZ)
           (only-in "./vm-memory-manager.rkt"
                    vm-memory-manager-code)
           (only-in "./vm-m1-slots.rkt"
                    vm-m1-slot-code)
           (only-in "./vm-pages.rkt"
                    vm-pages-code)
           (only-in "./vm-register-functions.rkt"
                    vm-register-functions-code)
           (only-in "./vm-textpage.rkt"
                    vm-textpage-code)
           (only-in "./vm-screen.rkt"
                    vm-screen-code
                    screen-base-address))

  (define PAGE_AVAIL_0 #xcb)

  (define test-runtime
    (append
     vm-textpage-code
     vm-screen-code
     vm-window-code

     ;; VM_MEMORY_MANAGEMENT_CONSTANTS
     vm-memory-manager-code
     (list (byte-const ZP_VM_PC $85)))))


;; input: screen-x, screen-y, width, ptr->textpage
;; modified:
;; output: ZP_RA -> native array with structure with initialized window data
(define-vm-function WINDOW_INIT
  (list
          ;; allocate m1-slot native array          
          (LDA !17)
          (JSR ALLOC_NATARR_TO_RA)

          (LDY !19)
          (LDA !0)
   (label init_loop__)
          (STA (ZP_RA),y)
          (DEY)
          (CPY !1)
          (BNE init_loop__)

          (LDA screen_x__)
          (LDY !vm_window__screen_x+2)
          (STA (ZP_RA),y)
          (LDA screen_y__)
          (LDY !vm_window__screen_y+2)
          (STA (ZP_RA),y)
          (LDA width__)
          (LDY !vm_window__width+2)
          (STA (ZP_RA),y)
          (LDA height__)
          (LDY !vm_window__height+2)
          (STA (ZP_RA),y)

          ;; set page for vis line start/last and first textpage
          (LDA textpage__)
          (STA ZP_PAGE_REG+1)
          (LDY !vm_window__vis_line_start+3)
          (STA (ZP_RA),y)
          (LDY !vm_window__vis_line_last+3)
          (STA (ZP_RA),y)
          (LDY !vm_window__first_textpage+2)
          (STA (ZP_RA),y)
          (STA ZP_RZ+1)
          
          (LDY !2)
          (LDA (ZP_PAGE_REG),y) ;; offset of first line
          (LDY !vm_window__vis_line_start+2)
          (STA (ZP_RA),y)
          (LDY !vm_window__vis_line_last+2)
          (STA (ZP_RA),y)
          (LDX height__)
          (STA ZP_RZ)

          ;; ptr to last line in ZP_RZ (additionally)
   (label loop_for_last_line__)
          (DEX)
          (BEQ done__)


          (JSR NEXT_TEXT_SECTION_RZ)
          (BNE loop_for_last_line__) ;; branch when found new line
          
   (label done__)
          ;; store number of empty lines currently displayed after last line
          (LDY !vm_window__num_trailing_empty_lines)
          (TXA) ;; # of empty lines below last line in this window
          (STA (ZP_RA),y)

          (LDY !vm_window__vis_line_last+2)
          (LDA ZP_RZ)
          (STA (ZP_RA),y)
          (INY)
          (LDA ZP_RZ+1)
          (STA (ZP_RA),y)
          
          (RTS)

          (label screen_x__) (byte $00)
          (label screen_y__) (byte $00)
          (label width__)    (byte $00)
          (label height__)   (byte $00)
          (label textpage__) (byte $00)))

(module+ test #| window init |#
  (define window-init-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (list
             (LDA !8)
             (STA screen_x__WINDOW_INIT)
             (LDA !5)
             (STA screen_y__WINDOW_INIT)
             (LDA !20)
             (STA width__WINDOW_INIT)
             (LDA !3)
             (STA height__WINDOW_INIT)
             (LDA !>test_textpage)
             (STA textpage__WINDOW_INIT)
             (JSR WINDOW_INIT)
             (BRK)

             (org-align #x100)
      (label test_textpage)
             (create-text-page-data-for
               (string-join
                (list
                 "HELLO MR WORLD"
                 "  TODAY I WANT TO SHOW YOU A WINDOWED"
                 "DISPLAY OF TEXT"
                 )
                "\n")
               "__benchmark")
      )))

  (check-equal? (memory-list- window-init-test ZP_RA 02)
                (list 02 PAGE_AVAIL_0)
                "RA points to a freshly allocated m1 slot")
  (check-equal? (memory-list- window-init-test (bytes->int 02 PAGE_AVAIL_0) 19)
                (list #x01 #x91  ;; rc=1 textype = native array ($80) + length 17 ($11)
                      #x08 #x05 #x00 #x00 #x00 #x14 #x03 ;; screenx=9, screeny=5, scrollx=0, scrolly=$0000, width=$14, height=$03
                      #x00 #x00 #x00 #x00 ;; cursorx=0, cursory=0, charpos=$0000 
                      #x04 #xa1 #x3b #xa1 #xa1 0) ;; firstline = $__04, last line = $__3b, first textpage=__, emptylines at end =0
                "RA points to an initialized window buffer"))

;; input: ZP_RA window
(define-vm-function WINDOW_RENDER_COMPLETE
  (list

          ;; init data for first list
          (LDY !vm_window__vis_line_start+2)
          (LDA (ZP_RA),y)
          (STA ZP_RZ)
          (INY)
          (LDA (ZP_RA),y)
          (STA ZP_RZ+1)           ;; zp_rz = ptr to text section line

          (LDY !vm_window__screen_x+2)
          (LDA (ZP_RA),y)
          (STA screen_x)

          (LDY !vm_window__screen_y+2)
          (LDA (ZP_RA),y)
          (TAX)                 ;; x = screen position y

          (LDY !vm_window__width+2)
          (LDA (ZP_RA),y)
          (STA window_width)

          (LDY !vm_window__height+2)
          (LDA (ZP_RA),y)
          (STA remaining_lines__) ;; put height into

          (LDY !vm_window__scroll_x+2)
          (LDA (ZP_RA),y)
          (STA scroll_x)

   (label loop__)
          ;; provide all call parameter for this line

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
          (JSR WRITE_TEXT_PAGE_LINE)
          ;; move on to next line

          (JSR NEXT_TEXT_SECTION_RZ)
          (BEQ no_more_text__)

   (label next__)
          ;; loop
          (INX)
          (DEC remaining_lines__)
          (BNE loop__)

   (label done__)
          (RTS)

   (label no_more_text__)
          ;; any lines left?
          (INX)
          (DEC remaining_lines__)
          (BEQ done__)


          ;; fill the rest of the window with spaces!
          ;; x = line
          (LDY !vm_window__screen_x+2)
          (LDA (ZP_RA),y)
          (STA ZP_RP) ;; col from which to start

   (label loop_empty_line__)
          (LDY !vm_window__width+2)
          (LDA (ZP_RA),y)
          (TAY) ;;
          (DEY) ;; number of characters to clear (-1)

          (LDA !1);; color white

          (JSR RT_SCREEN_CLEAR_CHARS_AT)

          (INX)
          (DEC remaining_lines__)
          (BNE loop_empty_line__)
          (RTS)

   (label remaining_lines__)
          (byte $01)))

(module+ test #| window-render-complete |#
  (define window-render-complete-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
            (LDA !<window)
            (STA ZP_RA)
            (LDA !>window)
            (STA ZP_RA+1)
            (JSR WINDOW_RENDER_COMPLETE)

            (LDA !7)
            (STA window+4) ;; set scroll x position
            (LDA !9)
            (STA window+3) ;; set screen_y
            (JSR WINDOW_RENDER_COMPLETE)

            (LDA !9)
            (STA window+4) ;; set scroll x position
            (LDA !12)
            (STA window+3) ;; set screen_y
            (JSR WINDOW_RENDER_COMPLETE)

            (LDA !10)
            (STA window+4) ;; set scroll x position
            (LDA !15)
            (STA window+3) ;; set screen_y
            (JSR WINDOW_RENDER_COMPLETE)

            (BRK)

            (org-align #x02) ;; to function as a ptr
     (label window)
            (byte $01 $0f) ;; native array header
            (byte $08)     ;; screen-x
            (byte $05)     ;; screen-y
            (byte $01) ;; scroll-position x (on char scrolled left)
            (word $0002) ;; line number and scroll position y
            (byte $14)     ;; width
            (byte $03)     ;; height
            (byte $00)     ;; cursor-x
            (byte $00)     ;; cursor-y
            (word $0000)   ;; char position
            (word-ref text_line_2) ;; first visible line
            (word-ref text_line_4) ;; last visible line
            ;; (word $0004)

            (org-align #x100) ;; ensure page boundary
     (label text_page)
            (byte $00) ;; page type
            (byte $03) ;; start of page section
            (byte $29) ;; end of last page section (points to trailing num char of last text section)
            ;; line 0
            (byte $00) ;; empty
            ;; line 1
            (byte $04) ;; 4 chars
            (byte $00) ;; indent 0
            (byte $01 $02 $03 $04) ;; ABCD
            (byte $04)
     (label text_line_2)
            (byte $06)
            (byte $04) ;; indent 4
            (byte $05 $06 $07 $08 $09 $0a) ;; EFGHIJ
            (byte $06)
            ;; line 3
            (byte $04) ;; 4 chars
            (byte $05) ;; indent 5
            (byte $01 $02 $03 $04) ;; ABCD
            (byte $04)
     (label text_line_4)
            (byte $06) ;; 6 chars
            (byte $10) ;; indent 16
            (byte $11 $12 $13 $14 $15 $16) ;; QRSTUV
            (byte $06)
            ;; line 5
            (byte $04) ;; 4 chars
            (byte $08) ;; indent 8
            (byte $17 $18 $19 $1a) ;; WXYZ
            (byte $04)

            (org-align #xff)
     (label last-byte)
            (byte $00) ;; next text-page
            ))

  ;; scroll_x = 1
  (check-equal? (memory-list- window-render-complete-test (+ (* 5 40) screen-base-address 7) 22)
                (list 0 32 32 32 05 06 07 08 09 10 32 32 32 32 32 32 32 32 32 32 32 0))
  (check-equal? (memory-list- window-render-complete-test (+ (* 6 40) screen-base-address 7) 22)
                (list 0 32 32 32 32 01 02 03 04 32 32 32 32 32 32 32 32 32 32 32 32 0))
  (check-equal? (memory-list- window-render-complete-test (+ (* 7 40) screen-base-address 7) 22)
                (list 0 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 17 18 19 20 21 0))
  ;; scroll_x = 7
  (check-equal? (memory-list- window-render-complete-test (+ (* 09 40) screen-base-address 7) 22)
                (list 0 08 09 10 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 0))
  (check-equal? (memory-list- window-render-complete-test (+ (* 10 40) screen-base-address 7) 22)
                (list 0 03 04 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 0))
  (check-equal? (memory-list- window-render-complete-test (+ (* 11 40) screen-base-address 7) 22)
                (list 0 32 32 32 32 32 32 32 32 32 17 18 19 20 21 22 32 32 32 32 32 0))
  ;; scroll_x = 9
  (check-equal? (memory-list- window-render-complete-test (+ (* 12 40) screen-base-address 7) 22)
                (list 0 10 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 0))
  (check-equal? (memory-list- window-render-complete-test (+ (* 13 40) screen-base-address 7) 22)
                (list 0 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 0))
  (check-equal? (memory-list- window-render-complete-test (+ (* 14 40) screen-base-address 7) 22)
                (list 0 32 32 32 32 32 32 32 17 18 19 20 21 22 32 32 32 32 32 32 32 0))

  ;; scroll_x = 10
  (check-equal? (memory-list- window-render-complete-test (+ (* 15 40) screen-base-address 7) 22)
                (list 0 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 0))
  (check-equal? (memory-list- window-render-complete-test (+ (* 16 40) screen-base-address 7) 22)
                (list 0 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 0))
  (check-equal? (memory-list- window-render-complete-test (+ (* 17 40) screen-base-address 7) 22)
                (list 0 32 32 32 32 32 32 17 18 19 20 21 22 32 32 32 32 32 32 32 32 0))

  (inform-check-equal? (cpu-state-clock-cycles window-render-complete-test)
                       10421
                       "time it takes to write 4x with different x-scroll position 3 strings in a window starting at 8,5 of width 20 and height 3 "))

(module+ test #| write text |#
  (define write-text-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (flatten
      (list
              (LDA !<window)
              (STA ZP_RA)
              (LDA !>window)
              (STA ZP_RA+1)

              (JSR WINDOW_RENDER_COMPLETE)
              (BRK)

       ;; no org align necessary, since rt is not gcd
       (label window)
              (byte $01 $0f) ;; native array header
              (byte $08)     ;; screen-x
              (byte $05)     ;; screen-y
              (byte $00) ;; scroll-position x
              (word $0000) ;; line number and scroll position y
              (byte $14)     ;; width
              (byte $03)     ;; height
              (byte $00)     ;; cursor-x
              (byte $00)     ;; cursor-y
              (word $0000)   ;; char position
              (word-ref text_line_0__benchmark) ;; first visible line
              (word-ref text_line_2__benchmark) ;; last visible line
              ;; (word $0002)

              (org-align #x100)
       (label text_page)
              (create-text-page-data-for
               (string-join
                (list
                 "HELLO MR WORLD"
                 "  TODAY I WANT TO SHOW YOU A WINDOWED"
                 "DISPLAY OF TEXT"
                 )
                "\n")
               "__benchmark")))))

  (check-equal? (memory-list- write-text-test (+ (* 40 5) screen-base-address 7) 22)
                (append
                 (list 0)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "HELLO"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "MR"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "WORLD"))
                 (list 32 32 32 32 32 32 0)))
  (check-equal? (memory-list- write-text-test (+ (* 40 6) screen-base-address 7) 22)
                (append
                 (list 0 32 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TODAY"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "I"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "WANT"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TO"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "SH"))
                 (list 0)))
  (check-equal? (memory-list- write-text-test (+ (* 40 7) screen-base-address 7) 22)
                (append
                 (list 0)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "DISPLAY"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "OF"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TEXT"))
                 (list 32 32 32 32 32 0))))

#|

 reasoning behind
 - first line visible :: ptr to the data of first visible line on screen
   => fast way to scroll x (since line is already there),
      fast way to scroll y (move prev/next from hereon)
 - first line number
   => allow fast get line  -> line display
      fast jump to line? (relative from here?, form start?)
 - last line visible :: ptr to the data of last visible line on screen
   => fast way to scroll y (move prev/next from hereon)
 - empty-lines after last (how many empty lines after the last real line are displayed, because scrolled to far
 - char position :: position of cursor to actual char # from start
   => fast goto char ?
      fast get char pos (relative from here?, from start?)
 |#


;; scroll window left, fill up last column with new data
;; keep cursor within window
;; adjust window data structure to represent new scroll status
;;   scroll-x := min (scroll-x + 1, MAX_SCROLL_X)
;;   cursor-x := max (screen-x, cursor-x)
;;
;; input: ZP_RA window
(define-vm-function WINDOW_SCROLL_LEFT
  (list
          (LDY !vm_window__scroll_x+2)
          (LDA (ZP_RA),y)
          (CMP !MAX_SCROLL_X)
          (BPL now_return__) ;; can't scroll over MAX_SCROLL_X
          (CLC)
          (ADC !1)
          (STA (ZP_RA),y)

          (LDY !vm_window__cursor_x+2)
          (LDA (ZP_RA),y)
          (BEQ dont_adjust_cursor__)
          (SEC)
          (SBC !1)
          (STA (ZP_RA),y)

   (label dont_adjust_cursor__)
          (LDY !vm_window__screen_y+2)
          (LDA (ZP_RA),y)
          (TAX)
          (LDY !vm_window__height+2)
          (LDA (ZP_RA),y)
          (STA ZP_RZ)
          (STA lines__)
          (LDY !vm_window__screen_x+2)
          (LDA (ZP_RA),y)
          (STA ZP_RP)
          (INC ZP_RP)
          (LDY !vm_window__width+2)
          (LDA (ZP_RA),y)
          (TAY)
          (DEY)
          ;; input:  X = ROW (first row)
          ;;         ZP_RZ = # of rows
          ;;         ZP_RP =  (first col) COL -> COL-delta
          ;;         Y = # of chars to scroll
          ;;         A = delta (positive: 1..)
          (JSR RT_SCREEN_SCROLL_LEFT_CHARS_AT_BY1)

          ;; now fill the empty column with data
          (LDY !vm_window__vis_line_start+2)
          (LDA (ZP_RA),y)
          (STA ZP_RZ)
          (INY)
          (LDA (ZP_RA),y)
          (STA ZP_RZ+1)

          (LDA !1)
          (STA window_width)

          (LDY !vm_window__scroll_x+2)
          (LDA (ZP_RA),y)
          (LDY !vm_window__width+2)
          (CLC)
          (ADC (ZP_RA),y)
          (STA scroll_x)
          (DEC scroll_x)

          (LDY !vm_window__screen_x+2)
          (LDA (ZP_RA),y)
          (LDY !vm_window__width+2)
          (CLC)
          (ADC (ZP_RA),y)
          (STA screen_x)
          (DEC screen_x)

          (LDY !vm_window__screen_y+2)
          (LDA (ZP_RA),y)
          (TAX)
          ;; input: screen_x
          ;;        X = screen_y
          ;;        scroll_x (or t_x, scroll_index, 0 = not left/right scrolled)
          ;;        window_width
          ;;        ZP_RZ = text-ptr (pointer into text buffer, start of text section) (ZP_RZ), format [payload-len][indent][ ... payload ...][payload-len]
   (label loop__)
          (JSR WRITE_TEXT_PAGE_LINE)
          (INX)
          (DEC lines__)
          (BEQ now_return__)
          (JSR NEXT_TEXT_SECTION_RZ)
          (BNE loop__)

   (label now_return__)
          (RTS)

   (label lines__)
          (byte $01)))

(module+ test #| scroll left |#
  (define window-scroll-left-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (flatten
      (list
              (LDA !<window)
              (STA ZP_RA)
              (LDA !>window)
              (STA ZP_RA+1)

              (JSR WINDOW_RENDER_COMPLETE)
              (JSR WINDOW_SCROLL_LEFT)
              (BRK)

       ;; no org align necessary, since rt is not gcd
       (label window)
              (byte $01 $0f) ;; native array header
              (byte $08)     ;; screen-x
              (byte $05)     ;; screen-y
              (byte $00)     ;; scroll-position x
              (word $0000)   ;; line number and scroll position y
              (byte $14)     ;; width
              (byte $03)     ;; height
              (byte $00)     ;; cursor-x
              (byte $00)     ;; cursor-y
              (word $0000)   ;; char position
              (word-ref text_line_0__benchmark) ;; first visible line
              (word-ref text_line_2__benchmark) ;; last visible line

              (org-align #x100)
       (label text_page)
              (create-text-page-data-for
               (string-join
                (list
                 "HELLO MR WORLD"
                 "  TODAY I WANT TO SHOW YOU A WINDOWED"
                 "DISPLAY OF TEXT"
                 )
                "\n")
               "__benchmark")))))

  (check-equal? (memory-list- window-scroll-left-test (+ (* 40 5) screen-base-address 7) 22)
                (append
                 (list 0)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "ELLO"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "MR"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "WORLD"))
                 (list 32 32 32 32 32 32 32 0)))
  (check-equal? (memory-list- window-scroll-left-test (+ (* 40 6) screen-base-address 7) 22)
                (append
                 (list 0 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TODAY"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "I"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "WANT"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TO"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "SHO"))
                 (list 0)))
  (check-equal? (memory-list- window-scroll-left-test (+ (* 40 7) screen-base-address 7) 22)
                (append
                 (list 0)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "ISPLAY"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "OF"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TEXT"))
                 (list 32 32 32 32 32 32 0)))

  (define window-scroll-left-test2
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (flatten
      (list
              (LDA !<window)
              (STA ZP_RA)
              (LDA !>window)
              (STA ZP_RA+1)

              (JSR WINDOW_RENDER_COMPLETE)
              (JSR WINDOW_SCROLL_LEFT)
              (JSR $0100)
              (JSR WINDOW_SCROLL_LEFT)
              (BRK)

       ;; no org align necessary, since rt is not gcd
       (label window)
              (byte $01 $0f) ;; native array header
              (byte $08)     ;; screen-x
              (byte $05)     ;; screen-y
              (byte $00)     ;; scroll-position x
              (word $0000)   ;; line number and scroll position y
              (byte $0d)     ;; width
              (byte $03)     ;; height
              (byte $00)     ;; cursor-x
              (byte $00)     ;; cursor-y
              (word $0000)   ;; char position
              (word-ref text_line_0__benchmark) ;; first visible line
              (word-ref text_line_2__benchmark) ;; last visible line

              (org-align #x100)
       (label text_page)
              (create-text-page-data-for
               (string-join
                (list
                 "HELLO MR WORLD"
                 "  TODAY I WANT TO SHOW YOU A WINDOWED"
                 "DISPLAY OF TEXT"
                 )
                "\n")
               "__benchmark")))))

  (check-equal? (memory-list- window-scroll-left-test2 (+ (* 40 5) screen-base-address 7) 15)
                (append
                 (list 0)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "LLO"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "MR"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "WORLD"))
                 (list 32 0)))
  (check-equal? (memory-list- window-scroll-left-test2 (+ (* 40 6) screen-base-address 7) 15)
                (append
                 (list 0)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TODAY"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "I"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "WANT"))
                 (list 32 0)))
  (check-equal? (memory-list- window-scroll-left-test2 (+ (* 40 7) screen-base-address 7) 15)
                (append
                 (list 0)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "SPLAY"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "OF"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TEXT"))
                 (list 0)))
  (inform-check-equal? (cpu-state-clock-cycles window-scroll-left-test2)
                       2097))

;; scroll window right, fill up firstcolumn with new data
;; keep cursor within window
;; adjust window data structure to represent new scroll status
;;   scroll-x := max (scroll-x - 1, 0)
;;   cursor-x := max (scroll-x, cursor-x)
;;
;; input: ZP_RT window
(define-vm-function WINDOW_SCROLL_RIGHT
  (list
          (LDY !vm_window__scroll_x+2)
          (LDA (ZP_RA),y)
          (BEQ now_return__) ;; can't scroll over 0
          (SEC)
          (SBC !1)
          (STA (ZP_RA),y)

          (LDY !vm_window__cursor_x+2)
          (LDA (ZP_RA),y)
          (CLC)
          (ADC !1)
          (LDY !vm_window__width+2)
          (CMP (ZP_RA),y)
          (BNE dont_adjust_cursor__)
          (SEC)
          (SBC !2)
          (STA (ZP_RA),y)

   (label dont_adjust_cursor__)
          (LDY !vm_window__screen_y+2)
          (LDA (ZP_RA),y)
          (LDY !vm_window__height+2)
          (CLC)
          (ADC (ZP_RA),y)
          (TAX)
          (DEX)
          (LDA (ZP_RA),y)
          (STA ZP_RZ)
          (STA lines__)
          (LDY !vm_window__screen_x+2)
          (LDA (ZP_RA),y)
          (STA ZP_RP)
          (LDY !vm_window__width+2)
          (LDA (ZP_RA),y)
          (TAY)
          (DEY)
          (DEY)
          ;; X     : screen row (last row)
          ;; ZP_RZ : number of rows (1.., up from last row, effectively scroll rows X, X-1, ... X-n)
          ;; ZP_RP : column (start col) COL->COL+delta
          ;; Y     : number of chars to copy -1
          (JSR RT_SCREEN_SCROLL_RIGHT_CHARS_AT_BY1)

          ;; now fill the empty column with data
          (LDY !vm_window__vis_line_start+2)
          (LDA (ZP_RA),y)
          (STA ZP_RZ)
          (INY)
          (LDA (ZP_RA),y)
          (STA ZP_RZ+1)

          (LDA !1)
          (STA window_width)

          (LDY !vm_window__scroll_x+2)
          (LDA (ZP_RA),y)
          (STA scroll_x)

          (LDY !vm_window__screen_x+2)
          (LDA (ZP_RA),y)
          (STA screen_x)

          (LDY !vm_window__screen_y+2)
          (LDA (ZP_RA),y)
          (TAX)
          ;; input: screen_x
          ;;        X = screen_y
          ;;        scroll_x (or t_x, scroll_index, 0 = not left/right scrolled)
          ;;        window_width
          ;;        ZP_RZ = text-ptr (pointer into text buffer, start of text section) (ZP_RZ), format [payload-len][indent][ ... payload ...][payload-len]
   (label loop__)
          (JSR WRITE_TEXT_PAGE_LINE)
          (INX)
          (DEC lines__)
          (BEQ now_return__)
          (JSR NEXT_TEXT_SECTION_RZ)
          (BNE loop__)

   (label now_return__)
          (RTS)

   (label lines__)
          (byte $01)))

(module+ test #| window scroll right |#
  (define window-scroll-right-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (flatten
      (list
              (LDA !<window)
              (STA ZP_RA)
              (LDA !>window)
              (STA ZP_RA+1)

              (JSR WINDOW_RENDER_COMPLETE)
              (JSR $0100)
              (JSR WINDOW_SCROLL_RIGHT)
              (BRK)

       ;; no org align necessary, since rt is not gcd
       (label window)
              (byte $01 $0f) ;; native array header
              (byte $08)     ;; screen-x
              (byte $05)     ;; screen-y
              (byte $05) ;; scroll-position x
              (word $0000) ;; line number and scroll position y
              (byte $14)     ;; width
              (byte $03)     ;; height
              (byte $00)     ;; cursor-x
              (byte $00)     ;; cursor-y
              (word $0000)   ;; char position
              (word-ref text_line_0__benchmark) ;; first visible line
              (word-ref text_line_2__benchmark) ;; last visible line
              ;; (word $0002)

              (org-align #x100)
       (label text_page)
              (create-text-page-data-for
               (string-join
                (list
                 "HELLO MR WORLD"
                 "  TODAY I WANT TO SHOW YOU A WINDOWED"
                 "DISPLAY OF TEXT"
                 )
                "\n")
               "__benchmark")))))

  (check-equal? (memory-list- window-scroll-right-test (+ (* 40 5) screen-base-address 7) 22)
                (append
                 (list 0)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "O"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "MR"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "WORLD"))
                 (list 32 32 32 32 32 32 32 32 32 32 0)))
  (check-equal? (memory-list- window-scroll-right-test (+ (* 40 6) screen-base-address 7) 22)
                (append
                 (list 0)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "DAY"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "I"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "WANT"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TO"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "SHOW"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "Y"))
                 (list 0)))
  (check-equal? (memory-list- window-scroll-right-test (+ (* 40 7) screen-base-address 7) 22)
                (append
                 (list 0)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "LAY"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "OF"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TEXT"))
                 (list 32 32 32 32 32 32 32 32 32 0)))
  (inform-check-equal? (cpu-state-clock-cycles window-scroll-right-test)
                       2497))

;; scroll window content down, fill first row with new data
;; keep cursor within window
;; adjust window data structure to represent new scroll status
;;   cursor-y := min(cursor-y + 1, screen-position-y + height - 1)
;;   vis-line-start := previous line
;;   scroll-y := max(scroll-y - 1, 0) 
;;   vis-line-end := previous line, iff it was on the last line, else no modification
;;
;; input: ZP_RT window
(define-vm-function WINDOW_SCROLL_DOWN
  (list

          ;; recalc vis-line start (and check whether scroll is possible)
          (LDY !vm_window__vis_line_start+2)
          (LDA (ZP_RA),y)
          (STA ZP_RZ)
          (INY)
          (LDA (ZP_RA),y)
          (STA ZP_RZ+1)
          (JSR PREV_TEXT_SECTION_RZ)
          (BNE cont__) ;; scrolling possible
          (RTS)

   (label cont__)
          (LDY !vm_window__vis_line_start+2)
          (LDA ZP_RZ)
          (STA (ZP_RA),y)
          (INY)
          (LDA ZP_RZ+1)
          (STA (ZP_RA),y)

          (LDY !vm_window__num_trailing_empty_lines+2)
          (LDA (ZP_RA),y)
          (BEQ recalc_vis_line_last__)
          (SEC)
          (SBC !1)
          (STA (ZP_RA),y)
          (BPL vis_lines_done__) ;; always jump

   (label recalc_vis_line_last__)
          ;; recalc vis-line last!
          (LDY !vm_window__vis_line_last+2)
          (LDA (ZP_RA),y)
          (STA ZP_RZ)
          (INY)
          (LDA (ZP_RA),y)
          (STA ZP_RZ+1)
          (JSR PREV_TEXT_SECTION_RZ)
          (LDA ZP_RZ+1)
          (STA (ZP_RA),y)
          (DEY)
          (LDA ZP_RZ)
          (STA (ZP_RA),y)

   (label vis_lines_done__)
          ;; decrement scroll y num
          (LDY !vm_window__scroll_y+2)
          (LDA (ZP_RA),y)
          (SEC)
          (SBC !1)
          (STA (ZP_RA),y)
          (BCS no_dec_scroll_y__)
          (INY)
          (LDA (ZP_RA),y)
          (SBC !0)
          (STA (ZP_RA),y)
   (label no_dec_scroll_y__)

          ;; check cursor adjustment
          (LDY !vm_window__cursor_y+2)
          (LDA (ZP_RA),y)
          (CLC)
          (ADC !1)
          (LDY !vm_window__height+2)
          (CMP (ZP_RA),y)
          (BNE dont_adjust_cursor__)
          (LDY !vm_window__cursor_y+2)
          (STA (ZP_RA),y)

   (label dont_adjust_cursor__)
          (LDY !vm_window__screen_y+2)
          (LDA (ZP_RA),y)
          (LDY !vm_window__height+2)
          (CLC)
          (ADC (ZP_RA),y)
          (TAX)
          (DEX) ;; now on last row
          (DEX) ;; row that needs to be copied
          (LDA (ZP_RA),y)
          (STA ZP_RZ)
          (LDY !vm_window__screen_x+2)
          (LDA (ZP_RA),y)
          (STA ZP_RP)
          (LDY !vm_window__width+2)
          (LDA (ZP_RA),y)
          (TAY)
          (DEY)
          ;; input:  X = ROW (last row to scroll)
          ;;         ZP_RZ = # of rows to cop
          ;;         ZP_RP = COL
          ;;         Y = # of chars to scroll - 1 (per row)
          (JSR RT_SCREEN_SCROLL_DOWN_BY1)

          ;; now fill the empty rowwith data

          (LDY !vm_window__width+2)
          (LDA (ZP_RA),y)
          (STA window_width)

          (LDY !vm_window__scroll_x+2)
          (LDA (ZP_RA),y)
          (STA scroll_x)

          (LDY !vm_window__screen_x+2)
          (LDA (ZP_RA),y)
          (STA screen_x)

          (LDY !vm_window__screen_y+2)
          (LDA (ZP_RA),y)
          (TAX)
          (LDY !vm_window__vis_line_start+2)
          (LDA (ZP_RA),y)
          (STA ZP_RZ)
          (INY)
          (LDA (ZP_RA),y)
          (STA ZP_RZ+1)
          ;; input: screen_x
          ;;        X = screen_y
          ;;        scroll_x (or t_x, scroll_index, 0 = not left/right scrolled)
          ;;        window_width
          ;;        ZP_RZ = text-ptr (pointer into text buffer, start of text section) (ZP_RZ), format [payload-len][indent][ ... payload ...][payload-len]
   (label loop__)
          (JMP WRITE_TEXT_PAGE_LINE)))

(module+ test #| window scroll down |#
  (define window-scroll-down-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (flatten
      (list
              (LDA !<window)
              (STA ZP_RA)
              (LDA !>window)
              (STA ZP_RA+1)

              (JSR WINDOW_RENDER_COMPLETE)
              (JSR $0100)
              (JSR WINDOW_SCROLL_DOWN)
              (BRK)

       ;; no org align necessary, since rt is not gcd
       (label window)
              (byte $01 $0f) ;; native array header
              (byte $08)     ;; screen-x
              (byte $05)     ;; screen-y
              (byte $00) ;; scroll-position x
              (word $0001) ;; line number and scroll position y
              (byte $14)     ;; width
              (byte $02)     ;; height
              (byte $00)     ;; cursor-x
              (byte $00)     ;; cursor-y
              (word $0000)   ;; char position
              (word-ref text_line_1__benchmark) ;; first visible line
              (word-ref text_line_2__benchmark) ;; last visible line
              (byte $a1) ;; first text page
              (byte $00) ;; trailing empty lines

              (org-align #x100)
       (label text_page)
              (create-text-page-data-for
               (string-join
                (list
                 "HELLO MR WORLD"
                 "  TODAY I WANT TO SHOW YOU A WINDOWED"
                 "DISPLAY OF TEXT"
                 )
                "\n")
               "__benchmark")))))

  (check-equal? (memory-list- window-scroll-down-test (+ (* 40 5) screen-base-address 7) 22)
                (append
                 (list 0)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "HELLO"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "MR"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "WORLD"))
                 (list 32 32 32 32 32 32 0)))
  (check-equal? (memory-list- window-scroll-down-test (+ (* 40 6) screen-base-address 7) 22)
                (append
                 (list 0 32 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TODAY"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "I"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "WANT"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TO"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "SH"))
                 (list 0))))

;; scroll window content up, fill last row with new data
;; keep cursor within window
;; adjust window data structure to represent new scroll status
;;   cursor-y := max(cursor-y - 1, screen-position-y)
;;   char-position := ? (not for now)
;;   vis-line-start := next line
;;   scroll-y := min(scroll-y + 1, MAX_LINES + height) 
;;   vis-line-end := next line, iff there is one
;;   last-line-num := last-line-num + 1, iff there is a next line
;;
;; input: ZP_RT window
(define-vm-function WINDOW_SCROLL_UP
  (list

   (label recalc_vis_line_last__)
          ;; recalc vis-line last!
          (LDY !vm_window__vis_line_last+2)
          (LDA (ZP_RA),y)
          (STA ZP_RZ)
          (INY)
          (LDA (ZP_RA),y)
          (STA ZP_RZ+1)
          (JSR NEXT_TEXT_SECTION_RZ)
          (BEQ no_next_last_available__)
          (LDY !vm_window__vis_line_last+2)
          (LDA ZP_RZ)
          (STA (ZP_RA),y)
          (INY)
          (LDA ZP_RZ+1)
          (STA (ZP_RA),y)
          (BNE cont__) ;; always jump

   (label no_next_last_available__)
          (LDY !vm_window__num_trailing_empty_lines+2)
          (LDA (ZP_RA),y)
          (CLC)
          (ADC !1)
          (LDY !vm_window__height+2)
          (CMP (ZP_RA),y)
          (BMI scrolling_necessary__)
          (RTS)

   (label scrolling_necessary__)
          (LDY !vm_window__num_trailing_empty_lines+2)
          (STA (ZP_RA),y) ;; store incremented empty lines

   (label cont__)
          ;; recalc vis-line start (and check whether scroll is possible)
          (LDY !vm_window__vis_line_start+2)
          (LDA (ZP_RA),y)
          (STA ZP_RZ)
          (INY)
          (LDA (ZP_RA),y)
          (STA ZP_RZ+1)
          (JSR NEXT_TEXT_SECTION_RZ)
          (LDY !vm_window__vis_line_start+2)
          (LDA ZP_RZ)
          (STA (ZP_RA),y)
          (INY)
          (LDA ZP_RZ+1)
          (STA (ZP_RA),y)

   (label vis_lines_done__)
          ;; increment scroll y num
          (LDY !vm_window__scroll_y+2)
          (LDA (ZP_RA),y)
          (CLC)
          (ADC !1)
          (STA (ZP_RA),y)
          (BCC no_inc_scroll_y__)
          (INY)
          (LDA (ZP_RA),y)
          (ADC !0)
          (STA (ZP_RA),y)
   (label no_inc_scroll_y__)

          ;; check cursor adjustment
          (LDY !vm_window__cursor_y+2)
          (LDA (ZP_RA),y)
          (BEQ dont_adjust_cursor__)
          (SEC)
          (SBC !1)
          (STA (ZP_RA),y)

   (label dont_adjust_cursor__)
          (LDY !vm_window__screen_y+2)
          (LDA (ZP_RA),y)
          (TAX)
          (INX)
          (mark-breakpoint)
          (LDY !vm_window__height+2)
          (LDA (ZP_RA),y)
          (STA ZP_RZ)
          (DEC ZP_RZ)
          (LDY !vm_window__screen_x+2)
          (LDA (ZP_RA),y)
          (STA ZP_RP)
          (LDY !vm_window__width+2)
          (LDA (ZP_RA),y)
          (TAY)
          ;; input:  X = ROW (first row)
          ;;         ZP_RP = COL
          ;;         ZP_RZ = number of rows to scroll
          ;;         Y = # of chars to scroll
          (JSR RT_SCREEN_SCROLL_UP_BY1)

          ;; now fill the empty last row with data

          (LDY !vm_window__screen_y+2)
          (LDA (ZP_RA),y)
          (LDY !vm_window__height+2)
          (CLC)
          (ADC (ZP_RA),y)
          (TAX)
          (DEX)

          (LDY !vm_window__num_trailing_empty_lines+2)
          (LDA (ZP_RA),y)
          (BEQ write_text_data_in_last_line__)

          (LDY !vm_window__screen_x+2)
          (LDA (ZP_RA),y)
          (STA ZP_RP)

          (LDY !vm_window__width+2)
          (LDA (ZP_RA),y)
          (TAY)
          (DEY)

          (LDA !1)

          ;; input:  X = ROW
          ;;         ZP_RP = COL
          ;;         Y = # of chars to clear -1 (0 for one char, 1 for two ...)
          ;;         A = color (for clearing the color ram)
          ;; modifies: Y=0, ZP_TEMP, ZP_TEMP+1, ZP_TEMP3
          ;; output: screen modified
          (JMP RT_SCREEN_CLEAR_CHARS_AT)

  (label  write_text_data_in_last_line__)

          (LDY !vm_window__width+2)
          (LDA (ZP_RA),y)
          (STA window_width)

          (LDY !vm_window__scroll_x+2)
          (LDA (ZP_RA),y)
          (STA scroll_x)

          (LDY !vm_window__screen_x+2)
          (LDA (ZP_RA),y)
          (STA screen_x)

          (LDY !vm_window__vis_line_last+2)
          (LDA (ZP_RA),y)
          (STA ZP_RZ)
          (INY)
          (LDA (ZP_RA),y)
          (STA ZP_RZ+1)
          ;; input: screen_x
          ;;        X = screen_y
          ;;        scroll_x (or t_x, scroll_index, 0 = not left/right scrolled)
          ;;        window_width
          ;;        ZP_RZ = text-ptr (pointer into text buffer, start of text section) (ZP_RZ), format [payload-len][indent][ ... payload ...][payload-len]
   (label loop__)
          (JMP WRITE_TEXT_PAGE_LINE)))

(module+ test #| window scroll up |#
  (define window-scroll-up-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (flatten
      (list
              (LDA !<window)
              (STA ZP_RA)
              (LDA !>window)
              (STA ZP_RA+1)

              (JSR WINDOW_RENDER_COMPLETE)
              (JSR $0100)
              (JSR WINDOW_SCROLL_UP)
              (BRK)

       ;; no org align necessary, since rt is not gcd
       (label window)
              (byte $01 $0f) ;; native array header
              (byte $08)     ;; screen-x
              (byte $05)     ;; screen-y
              (byte $00) ;; scroll-position x
              (word $0000) ;; line number and scroll position y
              (byte $14)     ;; width
              (byte $02)     ;; height
              (byte $00)     ;; cursor-x
              (byte $00)     ;; cursor-y
              (word $0000)   ;; char position
              (word-ref text_line_0__benchmark) ;; first visible line
              (word-ref text_line_1__benchmark) ;; last visible line
              (byte $a1)     ;; first text page
              (byte $00)     ;; num of empty lines shown

              (org-align #x100)
       (label text_page)
              (create-text-page-data-for
               (string-join
                (list
                 "HELLO MR WORLD"                         ;; initially on screen line 1, finally gone
                 "  TODAY I WANT TO SHOW YOU A WINDOWED"  ;; initially on screen line 2, finally screen line 1
                 ;; ---- initial window
                 "DISPLAY OF TEXT"                        ;; initially invisible, finalyy in screen line 2
                 )
                "\n")
               "__benchmark")))))

  (check-equal? (memory-list- window-scroll-up-test (+ (* 40 5) screen-base-address 7) 22)
                (append
                 (list 0 32 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TODAY"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "I"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "WANT"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TO"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "SH"))
                 (list 0)))
  (check-equal? (memory-list- window-scroll-up-test (+ (* 40 6) screen-base-address 7) 22)
                (append
                 (list 0)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "DISPLAY"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "OF"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TEXT"))
                 (list 32 32 32 32 32 0)))

  (define window-scroll-up-test2
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (flatten
      (list
              (LDA !<window)
              (STA ZP_RA)
              (LDA !>window)
              (STA ZP_RA+1)

              (JSR WINDOW_RENDER_COMPLETE)
              (JSR WINDOW_SCROLL_UP)
              (JSR WINDOW_SCROLL_UP)
              (BRK)

       ;; no org align necessary, since rt is not gcd
       (label window)
              (byte $01 $0f) ;; native array header
              (byte $08)     ;; screen-x
              (byte $05)     ;; screen-y
              (byte $00) ;; scroll-position x
              (word $0000) ;; line number and scroll position y
              (byte $14)     ;; width
              (byte $02)     ;; height
              (byte $00)     ;; cursor-x
              (byte $00)     ;; cursor-y
              (word $0000)   ;; char position
              (word-ref text_line_0__benchmark) ;; first visible line
              (word-ref text_line_1__benchmark) ;; last visible line
              (byte $a1)     ;; first text page
              (byte $00)     ;; num of empty lines shown

              (org-align #x100)
       (label text_page)
              (create-text-page-data-for
               (string-join
                (list
                 "HELLO MR WORLD"                         ;; initially on screen line 1, finally gone
                 "  TODAY I WANT TO SHOW YOU A WINDOWED"  ;; initially on screen line 2, finally gone
                 ;;--- initial window
                 "DISPLAY OF TEXT"                        ;; finally on screen line 1
                 )
                "\n")
               "__benchmark")))))

  (check-equal? (memory-list- window-scroll-up-test2 (+ (* 40 5) screen-base-address 7) 22)
                (append
                 (list 0)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "DISPLAY"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "OF"))
                 (list 32)
                 (map (lambda (char) (- (char->integer char) 64)) (string->list "TEXT"))
                 (list 32 32 32 32 32 0)))
  (check-equal? (memory-list- window-scroll-up-test2 (+ (* 40 6) screen-base-address 7) 22)
                (list 0 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 0)))

(define vm-window-code
  (append
   vm-window-defs
   WINDOW_INIT
   WINDOW_RENDER_COMPLETE
   WINDOW_SCROLL_LEFT
   WINDOW_SCROLL_RIGHT
   WINDOW_SCROLL_UP
   WINDOW_SCROLL_DOWN))

(module+ test #| code len |#
  (inform-check-equal? (estimated-code-len vm-window-code)
                       847
                       "estimated code len of window module"))

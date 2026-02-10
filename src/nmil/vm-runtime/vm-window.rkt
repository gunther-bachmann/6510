#lang racket/base

(provide WINDOW_RENDER_COMPLETE ;; zp_rt is the pointer to the window structure

         vm_window__screen_x      
         vm_window__screen_y      
         vm_window__width         
         vm_window__height        
         vm_window__cursor_x      
         vm_window__cursor_y      
         vm_window__char_pos      
         vm_window__vis_line_start
         vm_window__scroll_y      
         vm_window__vis_line_last 
         vm_window__scroll_x      
         vm_window__first_textpage
         vm_window__num_trailing_empty_lines

         vm-window-defs
         vm-window-code)

#|

 window is a visible object on a buffer

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
                  RT_SCREEN_CLEAR_CHARS_AT)
         (only-in "./vm-textpage.rkt"
                  WRITE_TEXT_PAGE_LINE
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
                    ZP_RT)
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

;; given curren text-section pointer in ZP_RZ move on to the next text-section
;; result zero-flag = 1, means no more text was available (BEQ)
;;        zero-flag = 0, means ZP_RZ holds the pointer to the next text-section (BNE)
;;
;; input:    ZP_RZ: pointer to text-section
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

;; input: ZP_RT window
(define-vm-function WINDOW_RENDER_COMPLETE
  (list

          ;; init data for first list
          (LDY !vm_window__vis_line_start+2)
          (LDA (ZP_RT),y)
          (STA ZP_RZ)
          (INY)
          (LDA (ZP_RT),y)
          (STA ZP_RZ+1)           ;; zp_rz = ptr to text section line

          (LDY !vm_window__screen_x+2)
          (LDA (ZP_RT),y)
          (STA screen_x)

          (LDY !vm_window__screen_y+2)
          (LDA (ZP_RT),y)
          (TAX)                 ;; x = screen position y

          (LDY !vm_window__width+2)
          (LDA (ZP_RT),y)
          (STA window_width)

          (LDY !vm_window__height+2)
          (LDA (ZP_RT),y)
          (STA remaining_lines__) ;; put height into

          (LDY !vm_window__scroll_x+2)
          (LDA (ZP_RT),y)
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
          (LDA (ZP_RT),y)
          (STA ZP_RP) ;; col from which to start

   (label loop_empty_line__)
          (LDY !vm_window__width+2)
          (LDA (ZP_RT),y)
          (TAY) ;;
          (DEY) ;; number of characters to clear (-1)

          (LDA !1);; color white

          (JSR RT_SCREEN_CLEAR_CHARS_AT)

          (INX)
          (DEC remaining_lines__)
          (BNE loop_empty_line__)
          (RTS)

   (label remaining_lines__)
          (byte $01)
))

(module+ test #| window-render-complete |#
  (define window-render-complete-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
            (LDA !<window)
            (STA ZP_RT)
            (LDA !>window)
            (STA ZP_RT+1)
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
                       10775
                       "time it takes to write 4x with different x-scroll position 3 strings in a window starting at 8,5 of width 20 and height 3 "))

(module+ test #| write text |#
  (define write-text-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     (flatten
      (list
              (LDA !<window)
              (STA ZP_RT)
              (LDA !>window)
              (STA ZP_RT+1)

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
;;   cursor-x := max (scroll-x, cursor-x)
;;
;; input: ZP_RT window
(define-vm-function WINDOW_SCROLL_LEFT
  (list   (RTS)))

;; scroll window right, fill up firstcolumn with new data
;; keep cursor within window
;; adjust window data structure to represent new scroll status
;;   scroll-x := max (scroll-x - 1, 0)
;;   cursor-x := max (scroll-x, cursor-x)
;;
;; input: ZP_RT window
(define-vm-function WINDOW_SCROLL_RIGHT
  (list   (RTS)))

;; scroll window content down, fill first row with new data
;; keep cursor within window
;; adjust window data structure to represent new scroll status
;;   cursor-y := min(cursor-y + 1, screen-position-y + height - 1)
;;   char-position := ? (not for now)
;;   vis-line-start := previous line
;;   scroll-y := max(scroll-y - 1, 0) 
;;   vis-line-end := previous line, iff it was on the last line, else no modification
;;   last-line-num := last-line-num - 1, iff it was on the last line, else no modification
;;
;; input: ZP_RT window
(define-vm-function WINDOW_SCROLL_DOWN
  (list   (RTS)))

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
  (list   (RTS)))

(define vm-window-code
  (append
   vm-window-defs
   NEXT_TEXT_SECTION_RZ
   WINDOW_INIT
   WINDOW_RENDER_COMPLETE))

(module+ test #| code len |#
  (inform-check-equal? (estimated-code-len vm-window-code)
                       260
                       "estimated code len of window module"))

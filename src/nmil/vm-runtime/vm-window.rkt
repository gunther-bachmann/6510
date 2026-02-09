#lang racket/base

(provide WINDOW_RENDER_COMPLETE

         vm-window-code)

#|

 window is a visible object on a buffer

 it holds:
    | offset | description                                                    |
    |--------+----------------------------------------------------------------|
    |      0 | screen-position-x                                              |
    |      1 | screen-position-y                                              |
    |      2 | width (1..40)                                                  |
    |      3 | height (1..25)                                                 |
    |        | cursor-position:                                               |
    |      4 | cursor-x -> logical-X = cursor-x + scroll-position-x (0 based) |
    |      5 | cursor-y -> logical-Y = cursor-y + scroll-position-y (0 based) |
    |   6..7 | char-position (in file) (0 based)                              |
    |   8..9 | ptr to first visible row (to text section)                     |
    | 10..11 | scroll-posiition-y = line # of first visible row (0 based)                          |
    | 12..13 | ptr to last visible row (to text section)                      |
    | 14..15 | line # of last visible row (0 based)                           |
    |     16 | scroll-position-x (0 based)                                    |


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
         (only-in racket/string string-join))

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
                    ZP_RP)
           (only-in "./vm-textpage.rkt"
                    vm-textpage-code)
           (only-in "./vm-screen.rkt"
                    vm-screen-code
                    screen-base-address))

  (define test-runtime
    (append
     vm-textpage-code
     vm-screen-code
     vm-window-code

     VM_MEMORY_MANAGEMENT_CONSTANTS
     (list (label VM_INIT_MEMORY_MANAGER) (RTS)))))


;; input: screen-x, screen-y, width, ptr->textpage
;; modified:
;; output: structure with initialized window data
(define-vm-function WINDOW_INIT
  (RTS))

;; input: ZP_RT window
(define-vm-function WINDOW_RENDER_COMPLETE
  (list

          ;; init data for first list
          (LDY !$08)
          (LDA (ZP_RT),y)
          (STA ZP_RZ)
          (INY)
          (LDA (ZP_RT),y)
          (STA ZP_RZ+1)           ;; zp_rz = ptr to text section line

          (LDY !$00)
          (LDA (ZP_RT),y)
          (STA screen_x)

          (LDY !$01)
          (LDA (ZP_RT),y)
          (TAX)                 ;; x = screen position y

          (LDY !$02)
          (LDA (ZP_RT),y)
          (STA window_width)

          (LDY !$03)
          (LDA (ZP_RT),y)
          (STA remaining_lines__) ;; put height into

          (LDY !$10)
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
          (BEQ no_more_text__)

          (STA ZP_RZ+1)
          (STA ZP_PAGE_REG+1)

          ;; get offset to first text section
          (LDY !$01)
          (LDA (ZP_PAGE_REG),y)
          (STA ZP_RZ)
          (BNE next__)

   (label continue_without_page_chage__)
          (CLC)
          (ADC !$01)
          (STA ZP_RZ)   ;;

   (label next__)
          ;; loop
          (INX)
          (DEC remaining_lines__)
          (BNE loop__)

   (label done)
          (RTS)
   (label no_more_text__)
          ;; fill the rest of the window with spaces!
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
            (STA window+16) ;; set scroll x position
            (LDA !9)
            (STA window+1) ;; set screen_y
            (JSR WINDOW_RENDER_COMPLETE)

            (LDA !9)
            (STA window+16) ;; set scroll x position
            (LDA !12)
            (STA window+1) ;; set screen_y
            (JSR WINDOW_RENDER_COMPLETE)

            (LDA !10)
            (STA window+16) ;; set scroll x position
            (LDA !15)
            (STA window+1) ;; set screen_y
            (JSR WINDOW_RENDER_COMPLETE)

            (BRK)

            (org-align #x02) ;; to function as a ptr
     (label window)
            (byte $08)     ;; screen-x
            (byte $05)     ;; screen-y
            (byte $14)     ;; width
            (byte $03)     ;; height
            (byte $00)     ;; cursor-x
            (byte $00)     ;; cursor-y
            (word $0000)   ;; char position
            (word-ref text_line_2) ;; first visible line
            (word $0002) ;; line number and scroll position y
            (word-ref text_line_4) ;; last visible line
            (word $0004)
            (byte $01) ;; scroll-position x (on char scrolled left)

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
                       10607
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
              (byte $08)     ;; screen-x
              (byte $05)     ;; screen-y
              (byte $14)     ;; width
              (byte $03)     ;; height
              (byte $00)     ;; cursor-x
              (byte $00)     ;; cursor-y
              (word $0000)   ;; char position
              (word-ref text_line_0__benchmark) ;; first visible line
              (word $0000) ;; line number and scroll position y
              (word-ref text_line_2__benchmark) ;; last visible line
              (word $0002)
              (byte $00) ;; scroll-position x

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


(define vm-window-code
  (append WINDOW_INIT
          WINDOW_RENDER_COMPLETE))

(module+ test #| code len |#
  (inform-check-equal? (estimated-code-len vm-window-code)
                       110
                       "estimated code len of window module"))

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
  (require "../../6510-test-utils.rkt"
           "./vm-memory-manager-test-utils.rkt"
           (only-in "../../ast/6510-relocator.rkt" estimated-code-len))

  (define test-runtime
    (append
     (list (label VM_INIT_MEMORY_MANAGER) (RTS)))))

#|

 fast coordinate calculation:

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

 |#

;; write a number of screen codes to y,x position on screen
;;
;; input:  Y = ROW
;;         X = COL
;;         ZP_RP = ptr to screen code data
;;         A = END_COL+1
;; output: screen modified
(define-vm-function RT_SCREEN_PUT_CHARS_AT
  (list
          (STA ZP_TEMP)              ;; temp = end col + 1
          (LDA line_start_table__,y) ;; y = row
          (TAY)
          (AND !$07)
          (STA write_screen_cmd__+2) ;; write page
          (TYA)
          (AND !$F8)
          (STA write_screen_cmd__+1) ;; write offset

          (LDY !$00)
   (label char_put_loop__)
          (LDA (ZP_RP),y)
   (label write_screen_cmd__)
          (STA $0400,x)
          (INY)                      ;; use y as both indices (zp_rp = ptr - col)
          (INX)
          (CPX ZP_TEMP)              ;; use dec, precalc offset, no compare, use BNE
          (BNE char_put_loop__)

          (RTS)

   (label line_start_table__)
          (byte $04 $2C $54 $7C $A4 ;; row 0..4
                $CC $F4 $1D $45 $6D ;; row 5..9
                $95 $BD $E5 $0E $36 ;; row 10..14
                $5E $86 $AE $D6 $FE ;; row 15..19
                $27 $4F $77 $9F $B7 ;; row 20..24
                )))

(module+ test #| RT_SCREEN_PUT_CHARS_AT |#
  (define screen-put-chars-at-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
     ;; now put the string
     ;; (JSR RT_SCREEN_PUT_CHARS_AT)
     )))

(define screen-code
  (append
   RT_SCREEN_PUT_CHARS_AT))

(module+ test #| estimated-code-len |#
  (inform-check-equal? (estimated-code-len screen-code)
                58
                "estimated code length change in screen runtime"))

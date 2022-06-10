#! /usr/bin/env racket
#lang reader "6510-reader.rkt"

        *=$C000        ; origin

; in zero page $80+x $90+x is stored high byte, low byte of the function
; in zero page $a0, $a1 is the lisp program counter

       brk

;; jump to function with index in x register
:inter ; index to function is in x register 0 <= x <= 15
       lda $80,x
       pha
       lda $90,x
       pha
       rts
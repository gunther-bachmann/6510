#! /usr/bin/env racket
#lang reader "6510-reader.rkt"

       *=$C000 ; origin

:some
        lda #$41   ; load character A (dec 65)
        jsr :out   ; print this character to screen
        adc #1     ; load character B (dec 66)
        jsr :out   ; print this character to screen
        lda #$0a
        jsr :out
 :end   rts        ; end of execution

 :out   jsr $ffd2
        rts
        brk

        .data $01, $2F

 inc $10
 inc $10,x
 inc $1000
 inc $1000,x

 dec $10
 dec $10,x
 dec $1000
 dec $1000,x

 adc $231A,y ; indexed y
 adc $213C,X ; indexed x
 adc #$10    ; immediate
 Adc $07     ; absolute zero page
 adc $21,x   ; zero page indexed x
 adc $FFFf   ; absolute
 adc ($c000),y
 adc ($a000,x)

 sta $231A,y ; indexed y
 sta $213C,x ; indexed x
 sta $07     ; absolute zero page
 sta $21,x   ; zero page indexed x
 sta $FFFf   ; absolute
 sta ($c000),y
 sta ($a000,x)

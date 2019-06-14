#! /usr/bin/env racket
#lang reader "6510-reader.rkt"

        *=$C000        ; origin

:some
        lda #$41       ; load character A (dec 65)
        jsr :cout      ; print this character to screen
        adc #1         ; load character B (dec 66)
        jsr :cout      ; print this character to screen
        lda #%00001010 ; $0a
        jsr :cout
 :end   rts            ; end of execution

 :cout  jsr $ffd2
        rts
        brk

        .data $01, $2F, 255, %1001

 asl a
 asl $10
 asl $1000
 asl $10,x
 asl $1000,x
 bcc $10
 bcs $ff
 beq :end
 bmi $1
 bne :end
 bpl $FF
 bvc :end
 bvs $00

 inc %00010000
 inc $10,x
 inc $1000
 inc $1000,x

 dec $10
 dec $10,x
 dec $1000
 dec $1000,x

 adc $231A,y ; indexed y
 adc $213C,X ; indexed x
 ADC #$10    ; immediate
 Adc $07     ; absolute zero page
 adc $21,x   ; zero page indexed x
 adc $FFFf   ; absolute
 adc ($c000),y
 adc (:cout,x)
 adc ($a000,x)

 sta $231A,y ; indexed y
 sta $213C,x ; indexed x
 sta $07     ; absolute zero page
 sta $21,x   ; zero page indexed x
 sta $FFFf   ; absolute
 sta ($c000),y
 sta ($a000,x)

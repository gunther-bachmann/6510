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
:end   brk        ; end of execution

:out   jsr $FFFF
       rts
       brk

       lda ($a000,x)
       lda ($c000),y
       adc ($c000),y
       adc ($a000,x)



;; jsr :end

;; adc $231A,y ; indexed y
;; adc $213C,X ; indexed x
;; adc #$10    ; immediate
;; Adc $07     ; absolute zero page
;; adc $21,x   ; zero page indexed x
;; adc $FFFf   ; absolute

;; lda $2000,x

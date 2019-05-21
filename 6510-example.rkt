#lang reader "6510-reader.rkt"

       *=$0000 ; origin

;       adc $231A,y ; indexed y
;       adc $213C,X ; indexed x
;       adc #$10    ; immediate
;       Adc $07     ; absolute zero page
;       adc $21,x   ; zero page indexed x
;       adc $FFFf   ; absolute

;       lda $2000,x

       lda #$41
       jsr $FFFF

       brk

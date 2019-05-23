#lang reader "6510-reader.rkt"

       *=$C000 ; origin
:some

;       adc $231A,y ; indexed y
;       adc $213C,X ; indexed x
;       adc #$10    ; immediate
;       Adc $07     ; absolute zero page
;       adc $21,x   ; zero page indexed x
;       adc $FFFf   ; absolute

;       lda $2000,x

       lda #$41   ; load character A (dec 65)
;       jsr $FFFF  ; print this character to screen
       jsr $FFFF

:end   brk        ; end of execution

       jsr :some
       jsr :end

#lang reader "6510-reader.rkt"

       adc $231A,y ; indexed y
       adc $213C,X ; indexed x
       adc #$10    ; immediate
       Adc $07     ; absolute zero page
       adc $21,x   ; zero page indexed x
       adc $FFFf   ; absolute

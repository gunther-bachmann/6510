#lang reader "6510-reader.rkt"

       adc $231A,y ; indexed y
       adc $213C,x ; indexed x
       adc #$10    ; immediate
       adc $07     ; absolute zero page
       adc $21,x   ; zero page indexed x
       adc $1704   ; absolute

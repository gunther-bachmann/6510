#! /usr/bin/env racket
#lang reader "6510-reader.rkt"

        *=$C000        ; origin

        ldx #$05       ; repeat .. times
:some
        lda #$41       ; load character A (dec 65)
        jsr :cout      ; print this character to screen
        adc #1         ; load character B (dec 66)
        jsr :cout      ; print this character to screen
        adc #1
        jsr :cout      ; print this character to screen
        lda #%00001101 ; $0d
        jsr :cout
 :end   dex
        bne :some
        rts            ; end of execution

 :cout  jsr $ffd2
        rts

        .data $01, $2F, 255, %1001



; --- just to test the assembler

brk
ora ($10,x)
ora $10
asl $10
php
ora #$10
asl a
ora $ffff
asl $ffff
bpl $a0
ora ($10),y
ora $10,x
asl $10,x
clc
ora $ffff,y
ora $ffff,x
asl $ffff,x
jsr $fffd
and ($10,x)
bit $10
and $10
rol $10
plp
and #$10
rol
bit $fffd
and $fffd
rol $fffd
bmi $a9
and ($10),y
and $10,x
rol $10,x
sec
and $fffd,y
and $fffd,x
rol $fffd,x
rti
eor ($10,x)
eor $10
lsr $10
pha
eor #$10
lsr
jmp $fff2
eor $fffd
lsr $ffff
bvc $b9
eor ($10),y
eor $10,x
lsr $10,x
cli
eor $ffff,y
eor $fffd,x
lsr $fffd,x
rts
adc ($10,x)
adc $10
ror $10
pla
adc #$ff
ror
jmp ($1000)
adc $1000
ror $1000
bvs $10
adc ($10),y
adc $10,x
ror $10,x
sei
adc $ffff,y
adc $ffff,x
ror $ffff,x
sta ($10,x)
sty $10
sta $10
stx $10
dey
txa
sty $1000
sta $1000
stx $1000
bcc $20
sta ($10),y
sty $10,x
sta $10,x
stx $10,y
tya
sta $ffff,y
txs ;; 9a

; --- some additional tests
 jmp ($1000)
 jmp $1011
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

 ;; using different cases in some places!
 adc $231A,y ; indexed y
 adc $213C,X ; indexed x
 ADC #$10    ; immediate
 Adc $07     ; absolute zero page
 adc $21,x   ; zero page indexed x
 adc $FFFf   ; absolute
 adc ($c0),y ; indirect y (load pointer in C1 C0, read from pointer + y)
 adc ($a0,x) ; indirect x (load pointer in A1+x A0+x, read from pointer)

 sta $231A,y ; indexed y
 sta $213C,x ; indexed x
 sta $07     ; absolute zero page
 sta $21,x   ; zero page indexed x
 sta $FFFf   ; absolute
 sta ($c0),y
 sta ($a0,x)

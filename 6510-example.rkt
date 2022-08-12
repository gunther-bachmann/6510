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

adc #$ff
adc $10
adc $10,x
adc $1000
adc $ffff,x
adc $ffff,y
adc ($10),y
adc ($10,x)
and #$10
and $10
and $10,x
and $fffd
and $fffd,x
and $fffd,y
and ($10),y
and ($10,x)
asl $10
asl $10,x
asl $ffff
asl $ffff,x
asl a
bcc $20
bcs $18
beq $c7
bit $10
bit $fffd
bmi $a9
bne $79
bpl $a0
brk
bvc $b9
bvs $10
clc
cld
cli
clv
cmp #$18
cmp $1011,y
cmp $11
cmp $12,x
cmp $1213
cmp $1213,x
cmp ($10),y
cmp ($10,x)
cpx #$a0
cpx $1011
cpx $11
cpy #$23
cpy $10
cpy $1011
dec $13
dec $13,x
dec $1415
dec $1415,x
dex
dey
eor #$10
eor $10
eor $10,x
eor $fffd
eor $fffd,x
eor $ffff,y
eor ($10),y
eor ($10,x)
inc $10,x
inc $13
inc $1415
inc $1415,x
inx
iny
jmp $fff2
jmp ($1000)
jsr $fffd
lda #$10
lda $1011,y
lda $12
lda $12,x
lda $1213
lda $1311,x
lda ($10),y
lda ($10,x)
ldx #$12
ldx $13
ldx $14,y
ldx $1415
ldx $abcd,y
ldy #$10
ldy $1011
ldy $1012,x
ldy $11
ldy $11,x
lsr
lsr $10
lsr $10,x
lsr $fffd,x
lsr $ffff
nop
ora #$10
ora $10
ora $10,x
ora $ffff
ora $ffff,x
ora $ffff,y
ora ($10),y
ora ($10,x)
pha
php
pla
plp
rol
rol $10
rol $10,x
rol $fffd
rol $fffd,x
ror
ror $10
ror $10,x
ror $1000
ror $ffff,x
rti
rts
sbc #$c9
sbc $10,x
sbc $1011,y
sbc $12
sbc $1213
sbc $1213,x
sbc ($10),y
sbc ($10,x)
sec
sed
sei
sta $10
sta $10,x
sta $1000
sta $1000,x
sta $ffff,y
sta ($10),y
sta ($10,x)
stx $10
stx $10,y
stx $1000
sty $10
sty $10,x
sty $1000
tax
tay
tsx
txa
txs
tya


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

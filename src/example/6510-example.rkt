#! /usr/bin/env racket
#lang reader "../asm/6510-reader.rkt"


; this program is an example of how the assembler can be used

        *=$0810        ; origin (basic start, to make loading and executing easier)

; currently require/provide cannot be parsed or resolved here

ROM-COUT  = $ffd2
REPEAT    = 2
LINE-FEED = %00001101 ; $0d


        ;; print hello word (reverse encoded)
        ;; hello: #of bytes to print
        ;; hello+1: last byte to print
        ; ... hello+n: first byte to print
        ldx hello
sout:   lda hello,x ;; a long comment that might be overridden by the disassembled code and the bytes encoding
        jsr ROM-COUT
        dex
        bne sout


        ;; some other print out function (pring REPEAT times ABC)
        ldx #REPEAT    ; repeat .. times
some:
        lda #$41       ; load character A (dec 65)
        jsr cout       ; print this character to screen
        adc #1         ; load character B (dec 66)
        jsr cout       ; print this character to screen
        adc #1
        jsr cout       ; print this character to screen
        lda #LINE-FEED
        jsr cout
end:    dex
        bne some

        ;; hello world non reverse encoded (taking 1 byte more in printing method)
        ;; hellon: zero terminated string
        ldx #00
rep:    lda hellon,x
        beq done
        jsr ROM-COUT
        inx             ; max 255 chars
        bne rep

done:

        ;; using string out of the c64 rom
        ;; hellon2: zero terminated string
        lda #<hellon2
        ldy #>hellon2
        jsr $ab1e
        rts             ; end of execution

cout:   jmp (table)

table:  .data $d2, $ff

hello:  .data 23  ; number of bytes to print (string length)
        .data $92 ; inverse off
        .data $0d ; line feed
        .data $a7 ; right one eigth
        .data $12 ; reverse
        .data $0d
        .data $a7 ; left half block
        .data $0d
        .asc "DLROw WEN OLLEh"
        .data $0e ; switch to lower letter mode

hellon: .asc "hELLO wORLD"
        .data $0d
        .data $00

hellon2: .asc "hELLO wORLD FROM AB1E"
         .data $0d
         .data $00

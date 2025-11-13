#! /usr/bin/env racket
#lang reader "../asm/6510-reader.rkt"
        ; testcode for presentation on persistent data structures

        *=$0810        ; origin (right behind basic loader "10 SYS 2064")

;; implement a 16-bit integer stack machine (only add)

;; start of main routine
;;--------------------------------------------------------------------------------
;;   push the integer 386, duplicate, add, print

        lda #$82       ; lb $82
        ldx #$01       ; hb $01
        jsr push       ; 1 * 256 + 8 * 16 + 2 = 386

        jsr dup        ; stack holds the int 386 twice

        jsr add        ; sum of the top two ints

        jsr pint       ; print int number on tos = 772
        rts

;; data locations for the stack
;;--------------------------------------------------------------------------------
tos:    .data 0   ;; top of stack index (*2), always points to the next free
acc:    .data 0   ;; low byte of 16-bit accu
accp1:  .data 0   ;; high byte of 16-bit accu
stack:  .data 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
        .data 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0

;; push a/x (low/high) onto the stack
;;----------------------------------------
push:   ldy tos
        sta stack,y
        iny
        txa
        sta stack,y
        iny
        sty tos
        rts

;; pop stack into a/x (low/high)
;;----------------------------------------
pop:    ldy tos
        dey
        lda stack,y
        tax
        dey
        lda stack,y
        sty tos
        rts

;; duplicate value on stack
;;----------------------------------------
dup:    ldy tos         ; load top of stack index
        dey             ; point to high byte
        lda stack,y     ; load
        tax             ; high byte -> x
        dey             ; point to low byte
        lda stack,y     ; load 
        jsr push        ; push A/X on stack
        rts             ; done

;; int add top two values on the stack, pop them and push the result
;;----------------------------------------
add:    jsr pop         ; pop tos into a/x
        sta acc         ; store low byte in acc
        stx accp1       ; store high byte in acc+1
        jsr pop         ; pop second value into a/x
        clc             
        adc acc         ; add two low-bytes
        sta acc         ; store into acc 
        txa             ; get high byte -> a
        adc accp1       ; add two high bytes (heed carry)
        sta accp1       ; store into acc+1
        tax             ; high byte -> x
        lda acc         ; low byte -> a
        jsr push        ; push result (A/X)
        rts             ; done


INT_OUT  = $bdcd       ; rom routine to print an integer XA (with a = high byte)
;; print integer in a/x (low/high)
;;----------------------------------------
pint:   ldy tos
        dey
        dey
        lda stack,y
        tax
        iny
        lda stack,y
        jsr INT_OUT
        rts


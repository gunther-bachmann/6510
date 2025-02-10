#! /usr/bin/env racket
#lang reader "../asm/6510-reader.rkt"
        ; testcode for presentation on persistent data structures

        *=$0810        ; origin (basic start, to make loading and executing easier)

INT_OUT  = $bdcd       ; rom routine to print an integer XA (with a = high byte)
CHAR_OUT = $ffd2       ; rom routine to print character


;; start of main routine
;;--------------------------------------------------------------------------------
        lda #$82
        ldx #$01       ; 1 * 256 + 8 * 16 + 2 = 386
        jsr push

        jsr dup        ; stack holds the integer 386 twice

        jsr add        ; sum of the top two integers

        jsr pint       ; print number on top of the stack = 772
        rts

;; data locations for the stack
;;--------------------------------------------------------------------------------
tos:    .data 0   ;; top of stack index (*2)
acc:    .data 0   ;; lower byte of 16-bit accu
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
        ; and print the pushed value 
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
dup:    ldy tos
        dey
        lda stack,y
        tax
        ;; dey ;; TODO uncomment
        lda stack,y
        jsr push
        rts

;; int add top two values on the stack, pop them and push the result
;;----------------------------------------
add:    jsr pop
        sta acc
        stx accp1
        jsr pop
        clc
        adc acc
        sta acc
        txa
        adc accp1
        sta accp1
        tax
        lda acc
        jsr push
        rts

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


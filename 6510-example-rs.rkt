#! /usr/bin/env racket
#lang racket

(require "6510.rkt")
(require "6510-interpreter.rkt")
(require "6510-debugger.rkt")

(define org #xc000)

(define program
  (list
   (LDX "#$05")
   (LABEL ("<label>") ":NEXT")
   (LDA "#$41" )
   (JSR ":COUT")
   (ADC "#$01")
   (JSR ":COUT")
   (LDA "#%00001010") ; line feed
   (JSR ":COUT")
   (DEX)
   (BNE ":NEXT")
   (BRK)

   ;; (IMPORT-BYTE ":IMPORTEDBVAL")
   ;; (IMPORT-WORD ":IMPORTEDWORD")
   ;; (EXPORT-WORD ":EXPORTED" ":COUT"
   ;; (CONSTANT-BYTE  ":MYVAL" "$42") ;; define label with byte constant: :myval = $42
   ;; (CONSTANT-WORD  ":M16BVAL" "$C842") ;; define label with word constant: :m16bval = $c842
   ;; (ASC "Hello World") ;; define bytes by ascii encoding: .asc "Hello World"

   (LABEL ("<label>") ":COUT")
   (JSR "$FFD2")
   (RTS)
   (JMP ":COUT")
   (JMP < ":COUT" >)
   (ADC ":COUT-L")
   (ADC "#:COUT-L")
   (STA ":COUT-H")
   (STA ":COUT-H" ,x)
   (LDX ":COUT-H" ,y)
   (STA < ":COUT-H" ,x >)
   (STA < ":COUT-H" > ,y)
   (STA ":COUT")
   (STA ":COUT" ,x)
   (STA ":COUT" ,y)
))

(define addressing-program
  (list
   (LDA "#$40")           ;; immediate
   (LDA "$40")            ;; zero-page
   (LDA "$40",x)          ;; zero-page-x (add reg x to $40, read memory from there)
   (LDA "$4000")          ;; absolute
   (LDA "$4000",x)        ;; absolute x
   (LDA "$4000",y)        ;; absolute y
   (LDA < "$40",x >)      ;; indirect x (add x to $40, read pointer (low+high) from memory, read pointed to byte)
   (LDA < "$40" >,y)      ;; indirect y (read pointer from $40, add y to pointer, read pointed to byte)
   (LABEL ("<label>") ":NEXT")
   (ASL A)                ;; work on accumulator
   (BEQ "$10")            ;; relative to current program counter
   (BEQ ":NEXT")))


(define resolved-program (replace-labels program org))
(define raw-bytes (commands->bytes org program))
(define data (6510-load (initialize-cpu) org raw-bytes))
(define executable-program (with-program-counter data org))

(displayln "program execution:")
(run-debugger org raw-bytes)
;; (print-state (run executable-program))
;; (let ([_ (run executable-program)])
;;   (void))

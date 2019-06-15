#lang racket

(require "6510.rkt")
(require "6510-interpreter.rkt")

(define org #xc000)

(define program
  (list
   (LDA "#$41" )
   (JSR ":COUT")
   (ADC "#$01")
   (JSR ":COUT")
   (LDA "#%00001010") ; line feed
   (JSR ":COUT")
   (RTS)
   (LABEL ":COUT")
   (JSR "$FFD2")
   (RTS)))

(define addressing-program
  (list
   (LDA "#$40")           ;; immediate
   (LDA "$40")            ;; zero-page
   (LDA "$40",x)          ;; zero-page-x (add reg x to $40, read memory from there)
   (LDA "$4000")          ;; absolute
   (LDA "$4000",x)        ;; absolute x
   (LDA "$4000",y)        ;; absolute y
   (LDA < "$40",x >)    ;; indirect x (add x to $40, read pointer (low+high) from memory, read pointed to byte)
   (LDA < "$40" >,y)    ;; indirect y (read pointer from $40, add y to pointer, read pointed to byte)
   (LABEL ":NEXT")
   (ASL A)                ;; work on accumulator
   (BEQ "$10")            ;; relative to current program counter
   (BEQ ":NEXT")))


(define resolved-program (replace-labels program org))
(define raw-bytes (commands->bytes org program))
(define data (6510-load (initialize-cpu) org raw-bytes))
(define executable-program (set-pc-in-state data org))

;; (displayln "program execution:")
;; (let ([_ (run executable-program))])
;;   (void))

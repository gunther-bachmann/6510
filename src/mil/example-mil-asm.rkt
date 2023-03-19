#! /usr/bin/env racket
#lang racket

#|

this file is an example of how the native compilation of a mil could look like

|#

(require "../6510.rkt")
(require "../ast/6510-resolver.rkt")
(require "../ast/6510-relocator.rkt")

(require "../tools/6510-interpreter.rkt")
(require "../tools/6510-debugger.rkt")
(require "../ast/6510-constants.rkt")

(module+ test
  (require rackunit))

(define program
  (list
   (byte-const "STRING_ID_HELLO-WORLD" 1)

   (byte-const "MILRT_STRING_TYPE" 1)
   (word-const "MILRT_VAL_HEAP" #x9eff)
   (byte-const "MILRT_ZP_VAL_HEAP_PTR" #x80)
   (byte-const "MILRT_ZP_VAL_HEAP_PTRP1" #x81)
   (byte-const "MILRT_ZP_STRING_PTR" #x82)
   (byte-const "MILRT_ZP_STRING_PTRP1" #x83)

   (label "MILRT_SETUP")             (LDA "!<MILRT_VAL_HEAP")
                                     (STA "MILRT_ZP_VAL_HEAP_PTR")
                                     (LDA "!>MILRT_VAL_HEAP")
                                     (STA "MILRT_ZP_VAL_HEAP_PTRP1")
                                     (JMP "MAIN")

   (label "MILRT_PUSH_STRING")       (LDY "!2")
                                     (JSR "MILRT_DEC_VAL_HEAP_BY_Y")
                                     (LDY "!2")
                                     (STA ("MILRT_ZP_VAL_HEAP_PTR"),y)
                                     (DEY)
                                     (LDA "!MILRT_STRING_TYPE")
                                     (STA ("MILRT_ZP_VAL_HEAP_PTRP1"),y)
                                     (DEY) ;; return w/ Y=0
                                     (RTS)

   (label "MILRT_DEC_VAL_HEAP_BY_Y") (DEC "MILRT_ZP_VAL_HEAP_PTR")
                                     (BNE "MILRT_DVHBY_DONE")
                                     (DEC "MILRT_ZP_VAL_HEAP_PTRP1")
   (label "MILRT_DVHBY_DONE")        (DEY)
                                     (BNE "MILRT_DEC_VAL_HEAP_BY_Y")
                                     (RTS)

   (label "MILRT_INC_VAL_HEAP_BY_Y") (INC "MILRT_ZP_VAL_HEAP_PTR")
                                     (BNE "MILRT_IVHBY_DONE")
                                     (INC "MILRT_ZP_VAL_HEAP_PTRP1")
   (label "MILRT_IVHBY_DONE")        (DEY)
                                     (BNE "MILRT_INC_VAL_HEAP_BY_Y")
                                     (RTS)

   (label "MILRT_DISPLAY")           (LDY "!2")  ;; expecting bytes on the stack
                                     (LDA ("MILRT_ZP_VAL_HEAP_PTR"),y) ;; get string id
                                     (JSR "MILRT_STRING_ID2PTR")
                                     (JSR "MILRT_INC_VAL_HEAP_BY_Y")
                                     (LDA ("MILRT_ZP_STRING_PTR"),y) ;; y should be 0, a= strlen
                                     (TAY);;  y = strlen
   (label "MILRT_DISPLAY_NEXT")      (LDA ("MILRT_ZP_STRING_PTR"),y)
                                     (JSR "$FFD2")
                                     (DEY)
                                     (BNE "MILRT_DISPLAY_NEXT")
                                     (RTS)

   (label "MILRT_STRING_ID2PTR")     (LDA "!<STRING_PTR") ;; work around
                                     (STA "MILRT_ZP_STRING_PTR")
                                     (LDA "!>STRING_PTR")
                                     (STA "MILRT_ZP_STRING_PTRP1")
                                     (RTS)

   (label "MAIN")                    (JMP "HELLO_WORLD")
   (label "HELLO_WORLD")             (LDA "!STRING_ID_HELLO-WORLD")
                                     (JSR "MILRT_PUSH_STRING")
                                     (JMP "MILRT_DISPLAY")

   (label "STRING_PTR")              (byte 11)
                                     (asc "DLROW OLLEH")
))

(define org 2064)

(define program-p1 (->resolved-decisions (label-instructions program) program))
(define program-p2 (->resolve-labels org (label-string-offsets org program-p1) program-p1 '()))
(define program-p3 (resolve-constants (constant-definitions-hash program-p1) program-p2))
(define raw-bytes (resolved-program->bytes program-p3))

(define data (6510-load (initialize-cpu) org raw-bytes))
(define executable-program (with-program-counter data org))

(run-debugger org raw-bytes)

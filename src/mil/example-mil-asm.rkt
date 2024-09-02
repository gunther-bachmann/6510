#! /usr/bin/env racket
#lang racket

#|

this file is an example of how the native compilation of a mil could look like

|#

(require "../6510.rkt")
(require "../ast/6510-resolver.rkt")
(require "../ast/6510-relocator.rkt")
(require "../tools/6510-prg-generator.rkt")

;; (require "../tools/6510-interpreter.rkt")
(require "../tools/6510-debugger.rkt")
(require "../ast/6510-constants.rkt")
(require (only-in "../tools/6510-source-map.rkt" create-source-map))

(module+ test
  (require rackunit))

(define program
  (list
   (byte-const STRING_ID_HELLO_WORLD  0)
   (byte-const STRING_ID_FORMAT_ERROR 1)

   (byte-const MILRT_STRING_TYPE 1)
   (byte-const MILRT_UINT8_TYPE 2)

   (word-const MILRT_VAL_HEAP #x9eff)
   (byte-const MILRT_ZP_VAL_HEAP_PTR #x80)
   (byte-const MILRT_ZP_VAL_HEAP_PTRP1 #x81)
   (byte-const MILRT_ZP_STRING_PTR #x82)
   (byte-const MILRT_ZP_STRING_PTRP1 #x83)
   (byte-const MILRT_ZP_STRING_PTR2 #x84)
   (byte-const MILRT_ZP_STRING_PTR2P1 #x85)

   (word-const MILRT_STRING_ID_TABLE #xc000)
   (word-const MILRT_STRING_ID_TABLE_P2 #xc100)
   (word-const MILRT_STRING_TABLE #xc200)

   (label MILRT_SETUP)               (LDA !<STRING_TABLE)
                                     (STA MILRT_ZP_VAL_HEAP_PTR)
                                     (LDA !>STRING_TABLE)
                                     (STA MILRT_ZP_VAL_HEAP_PTRP1)

                                     (LDA !<MILRT_STRING_ID_TABLE)
                                     (STA MILRT_ZP_STRING_PTR)
                                     (LDA !>MILRT_STRING_ID_TABLE)
                                     (STA MILRT_ZP_STRING_PTRP1)

                                     (LDA !<MILRT_STRING_TABLE)
                                     (STA MILRT_ZP_STRING_PTR2)
                                     (LDA !>MILRT_STRING_TABLE)
                                     (STA MILRT_ZP_STRING_PTR2P1)

   (label MLRT_SETUP__COPY_STRINGS)  (LDY !0)
                                     (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                     (TAX) ;; put len into x
                                     (BEQ MILRT_SETUP_VALUE_HEAP)

                                     ;; store current ptr into MLRT_STRING_TABLE into MLRT_STRING_ID_TABLE
                                     (LDA MILRT_ZP_STRING_PTR2)
                                     (STA (MILRT_ZP_STRING_PTR),y)
                                     (LDA MILRT_ZP_STRING_PTR2P1)
                                     (INY)
                                     (STA (MILRT_ZP_STRING_PTR),y)

                                     (INX) ;; copy len = strlen + len info itself
                                     (LDY !0)
   (label MLRT_SETUP__COPY)          (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                     (STA (MILRT_ZP_STRING_PTR2),y)
                                     (INY)
                                     (DEX)
                                     (BNE MLRT_SETUP__COPY)

                                     ;; increment string table ptr by Y
                                     (TYA)
                                     (CLC)
                                     (ADC MILRT_ZP_STRING_PTR2)
                                     (STA MILRT_ZP_STRING_PTR2)
                                     (BCC MILRT_SETUP__COPY_INC_NEXT)
                                     (INC MILRT_ZP_STRING_PTR2P1)

   (label MILRT_SETUP__COPY_INC_NEXT)
                                     ;; increment value expression ptr (src of strings)
                                     (TYA)
                                     (CLC)
                                     (ADC MILRT_ZP_VAL_HEAP_PTR)
                                     (STA MILRT_ZP_VAL_HEAP_PTR)
                                     (BCC MILRT_SETUP__COPY_INC_NEXT3)
                                     (INC MILRT_ZP_VAL_HEAP_PTRP1)

   (label MILRT_SETUP__COPY_INC_NEXT3)
                                     ;; increment location in string id table
                                     (CLC)
                                     (LDA !2)
                                     (ADC MILRT_ZP_STRING_PTR)
                                     (STA MILRT_ZP_STRING_PTR)
                                     (BCC MILRT_SETUP__COPY_INC_NEXT2)
                                     (INC MILRT_ZP_STRING_PTRP1)
   (label MILRT_SETUP__COPY_INC_NEXT2)
                                     (JMP MLRT_SETUP__COPY_STRINGS)

                                     ;; setup up value heap ptr for application
   (label MILRT_SETUP_VALUE_HEAP)    (LDA !<MILRT_VAL_HEAP)
                                     (STA MILRT_ZP_VAL_HEAP_PTR)
                                     (LDA !>MILRT_VAL_HEAP)
                                     (STA MILRT_ZP_VAL_HEAP_PTRP1)
                                     (JMP MAIN)

                                     ;; push the STRING_ID in A onto the value stack
                                     ;; A = MILRT_STRING_TYPE, Y = 0
   (label MILRT_PUSH_STRING)         (LDY !2)
                                     (JSR MILRT_DEC_VAL_HEAP_BY_Y)
                                     (LDY !2)
                                     (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                     (DEY)
                                     (LDA !MILRT_STRING_TYPE)
                                     (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                     (DEY) ;; return w/ Y=0
                                     (RTS)

                                     ;; push the STRING_ID in A onto the value stack
                                     ;; A = MILRT_STRING_TYPE, Y = 0
   (label MILRT_PUSH_UINT8)          (LDY !2)
                                     (JSR MILRT_DEC_VAL_HEAP_BY_Y)
                                     (LDY !2)
                                     (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                     (DEY)
                                     (LDA !MILRT_UINT8_TYPE)
                                     (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                     (DEY) ;; return w/ Y=0
                                     (RTS)

                                      ;; decrement ptr to free tos of value stack (push) by Y
                                      ;; Y = 0  
   (label MILRT_DEC_VAL_HEAP_BY_Y)   (DEC MILRT_ZP_VAL_HEAP_PTR)
                                     (BNE MILRT_DVHBY_DONE)
                                     (DEC MILRT_ZP_VAL_HEAP_PTRP1)
   (label MILRT_DVHBY_DONE)          (DEY)
                                     (BNE MILRT_DEC_VAL_HEAP_BY_Y)
                                     (RTS)

                                     ;; increment ptr to free tos of value stack (pop) by Y
                                     ;; Y = 0
   (label MILRT_INC_VAL_HEAP_BY_Y)   (INC MILRT_ZP_VAL_HEAP_PTR)
                                     (BNE MILRT_IVHBY_DONE)
                                     (INC MILRT_ZP_VAL_HEAP_PTRP1)
   (label MILRT_IVHBY_DONE)          (DEY)
                                     (BNE MILRT_INC_VAL_HEAP_BY_Y)
                                     (RTS)

                                     ;; DISPLAY string with STRING_ID on the value stack
                                     ;; Y = 0, A = last char of string
   (label MILRT_DISPLAY)             (LDY !2)  ;; expecting bytes on the stack
                                     (TYA)
                                     (PHA)
                                     (LDA (MILRT_ZP_VAL_HEAP_PTR),y) ;; get string id                                     
                                     (JSR MILRT_STRING_ID2PTR)
                                     (PLA)
                                     (TAY)
                                     (JSR MILRT_INC_VAL_HEAP_BY_Y) ;; drop string from value stack
                                     (LDA (MILRT_ZP_STRING_PTR),y) ;; y should be 0, a= strlen
                                     (TAY);;  y = strlen
   (label MILRT_DISPLAY_NEXT)        (LDA (MILRT_ZP_STRING_PTR),y)
                                     ;; if A = #\^, then special (up arrow)
                                     (CMP !$5E)
                                     (BNE MILRT_DISPLAY_REGULAR)
                                     (DEY)
                                     (LDA (MILRT_ZP_STRING_PTR),y)
                                     (CMP !$5E)
                                     (BNE MILRT_DISPLAY_OTHER)
   (label MILRT_DISPLAY_REGULAR)     (JSR $FFD2)
   (label MILRT_DISPLAY_REGULAR_C)   (DEY)
                                     (BNE MILRT_DISPLAY_NEXT)
                                     (RTS)
                                     
  (label MILRT_DISPLAY_OTHER)        (CMP !97) ;; 'a'
                                     (BNE STR_FORMAT_ERROR)
                                     ;; keep y
                                     (TYA)
                                     (PHA)
                                     (JSR MILRT_DISPLAY_OBJECT)
                                     (PLA)
                                     (TAY)
                                     (JMP MILRT_DISPLAY_REGULAR_C) ;; continue with rest of string
  (label STR_FORMAT_ERROR)           (LDA !STRING_ID_FORMAT_ERROR)
                                     (JSR MILRT_PUSH_STRING)
                                     (JSR MILRT_DISPLAY)
                                     (BRK) ; stop

                                     ;; print object tos as string and pop
                                     ;; uint8 => 0..255, string => string, char => char, bool => true/false
                                     ;; cons-cell => (a . b), list => (a b ... )
                                     ;; Y = *, A = *
   (label MILRT_DISPLAY_OBJECT)      (LDY !1)  ;;
                                     (LDA (MILRT_ZP_VAL_HEAP_PTR),y) ;; get type descriptor
                                     (CMP !MILRT_UINT8_TYPE)
                                     (BNE MILRT_DISPLAY_NONUINT)
   (label MILRT_DISPLAY_UINT)        (INY)
                                     (LDA !36) ;; prefix with '$'
                                     (JSR $FFD2)
                                     (LDA (MILRT_ZP_VAL_HEAP_PTR),y) ;; get uint value
                                     (JSR MILRT_INC_VAL_HEAP_BY_Y) ;; pop uint incl. type descriptor from stack

                                     ;; basic routine to write byte !

                                     ;; print byte in A as Hex
                                     ;; A = last Digit, need 1 stack slot
   (label MILRT_DISPLAY_UINT8)       (PHA)                            ; Save A
                                     (LSR) (LSR) (LSR) (LSR)          ; Move top nybble to bottom nybble
                                     (JSR MILRT_DISPLAY_NIBBLE)     ; Print this nybble
                                     (PLA)                            ; Get A back and print bottom nybble
   (label MILRT_DISPLAY_NIBBLE)      (AND !15)                        ; Keep bottom four bits
                                     (CMP !10) 
                                     (BCC MILRT_DISPLAY_DIGIT)      ; If 0-9, jump to print
                                     (ADC !6)                         ; Convert ':' to 'A'
   (label MILRT_DISPLAY_DIGIT)       (ADC !48) ; 0 -> 0
                                     (JMP $FFD2)  

   (label MILRT_DISPLAY_NONUINT)     ;; TODO: implement printing strings, cons-cells, lists and boolean
                                     (RTS)

                                     ;; load ptr to string with A = STRING ID
                                     ;; into zero page MILRT_ZP_STRING_PTR, MILRT_ZP_STRING_PTRP1 (low, high)
                                     ;; A = *, Y = *
   (label MILRT_STRING_ID2PTR)       (ASL A)
                                     (TAY)
                                     (BCS MILRT_STRING_ID2PTR_2)

                                     (LDA MILRT_STRING_ID_TABLE,y)
                                     (STA MILRT_ZP_STRING_PTR)
                                     (INY)
                                     (LDA MILRT_STRING_ID_TABLE,y)
                                     (STA MILRT_ZP_STRING_PTRP1)
                                     (RTS)
                                     
   (label MILRT_STRING_ID2PTR_2)     (LDA MILRT_STRING_ID_TABLE_P2,y)
                                     (STA MILRT_ZP_STRING_PTR)
                                     (INY)
                                     (LDA MILRT_STRING_ID_TABLE_P2,y)
                                     (STA MILRT_ZP_STRING_PTRP1)
                                     (RTS)

                                     ;; (LDA !<STRING_PTR) ;; work around
                                     ;; (STA MILRT_ZP_STRING_PTR)
                                     ;; (LDA !>STRING_PTR)
                                     ;; (STA MILRT_ZP_STRING_PTRP1)
                                     ;; (RTS)
   
                                     ;; copy bytes from [MILRT_ZP_MEMCPY_SPTR] -> [MILRT_ZP_MEMCPY_TPTR]
                                     ;; # = [MILRT_ZP_MEMCPY_LEN_LOW][MILRT_ZP_MEMCPY_LEN_HIGH+1]
   (label MILRT_MEM_COPY)          
   (label MILRT_MEM_COPY_IN_LOOP)  
                                     (RTS)

   (label MAIN)                      (JMP HELLO_WORLD)
   (label HELLO_WORLD)               (LDA !$a3)
                                     (JSR MILRT_PUSH_UINT8)
                                     (LDA !STRING_ID_HELLO_WORLD)
                                     (JSR MILRT_PUSH_STRING)
                                     (JMP MILRT_DISPLAY)

   (label STRING_TABLE)
   (label STRING_PTR)                (byte 15)
                                     ;; (byte $5E)
                                     (asc "!a")
                                     (byte $5E)
                                     (asc " DLROW OLLEH")
   ;; (label STRING_FORMAT_ERROR) ; this label is not needed
                                     (byte 19)
                                     (asc "RORRE TAMROF GNIRTS")
                                     (byte 0) ;; marks the end of the string table
                                     ))

(define org 2064)

(define program-p1 (->resolved-decisions (label-instructions program) program))
(define program-p2 (->resolve-labels org (label-string-offsets org program-p1) program-p1 '()))
(define program-p3 (resolve-constants (constant-definitions-hash program-p1) program-p2))
(define raw-bytes (resolved-program->bytes program-p3))

;; (define data (6510-load (initialize-cpu) org raw-bytes))
;; (define executable-program (with-program-counter data org))

(module+ main
  (create-source-map raw-bytes org "example-mil-asm.rkt" program-p3 )
  (create-prg raw-bytes org "example-mil-asm.prg")
  (create-image-with-program raw-bytes org "example-mil-asm.prg" "example-mil-asm.d64" ".")

  (run-debugger org raw-bytes "example-mil-asm.rkt"))

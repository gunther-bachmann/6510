#lang racket

(require "../6510.rkt")
(require "./mil-6510-commands.rkt")

(provide mil-runtime)


(define MILRT_CONSTANTS
  (list
     (byte-const STRING_ID_FORMAT_ERROR 0)

     ;; type id for values on the expression stack : ADJUST MILRT_POP and MILRT_DISPLAY!
     (byte-const MILRT_SYMBOL_TYPE     0)
     (byte-const MILRT_STRING_TYPE     1)
     (byte-const MILRT_UINT8_TYPE      2)
     (byte-const MILRT_BOOL_TYPE       3)

     (byte-const MILRT_CONS_CELL_TYPE  4)

     (byte-const MILRT_LIST_TYPE_START 5)
     (byte-const MILRT_LIST_TYPE_END   6)


     (word-const MILRT_VAL_HEAP #x9eff) ;; growing down

     (byte-const MILRT_ZP_VAL_HEAP_PTR #x71) ;; ptr to val-heap next free cell
     (byte-const MILRT_ZP_VAL_HEAP_PTRP1 #x72) ;; 2nd part

     (byte-const MILRT_ZP_STRING_PTR #xfb) ;; ptr for string/symbol copy 1
     (byte-const MILRT_ZP_STRING_PTRP1 #xfc)

     (byte-const MILRT_ZP_STRING_PTR2 #xfd) ;; ptr for string/symbol copy 2
     (byte-const MILRT_ZP_STRING_PTR2P1 #xfe)

     (byte-const MILRT_LIST_LEVEL #xff)

     (word-const MILRT_STRING_ID_TABLE #xc000) ;; growing up
     (word-const MILRT_STRING_ID_TABLE_P2 #xc100)
     (word-const MILRT_STRING_TABLE #xc200) ;; growing up

     (word-const MILRT_SYMBOL_ID_TABLE #xcf00) ;; growing up
     (word-const MILRT_SYMBOL_TABLE #xceff) ;; growing down
))

(define MILRT_ZERO_PAGE_SETUP
  (list
     (label MILRT_SETUP)               (LDA !<STRING-TABLE)
                                       (STA MILRT_ZP_VAL_HEAP_PTR)
                                       (LDA !>STRING-TABLE)
                                       (STA MILRT_ZP_VAL_HEAP_PTRP1)

                                       (LDA !<MILRT_STRING_ID_TABLE)
                                       (STA MILRT_ZP_STRING_PTR)
                                       (LDA !>MILRT_STRING_ID_TABLE)
                                       (STA MILRT_ZP_STRING_PTRP1)))

(define MILRT_COPY_STRINGS_TO_IDTABLE
  (list
                                       (LDA !<MILRT_STRING_TABLE)
                                       (STA MILRT_ZP_STRING_PTR2)
                                       (LDA !>MILRT_STRING_TABLE)
                                       (STA MILRT_ZP_STRING_PTR2P1)

      (label MLRT_SETUP__COPY_STRINGS)

                                        (LDA-immediate (char->integer #\.))
                                        (JSR $FFD2)

                                        (LDY !0)
                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (TAX) ;; put len into x
                                        (BEQ MILRT_SETUP__DONE)

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

      (label MILRT_SETUP__DONE)
                                        (LDA !13)
                                        (JSR $FFD2)
))

(define MILRT_SETUP_VAL_HEAP
  (list
   (label MILRT_SETUP_VALUE_HEAP)
                                        (LDA !<MILRT_VAL_HEAP)
                                        (STA MILRT_ZP_VAL_HEAP_PTR)
                                        (LDA !>MILRT_VAL_HEAP)
                                        (STA MILRT_ZP_VAL_HEAP_PTRP1)))

(define MILRT_PUSH_STRING
  (list
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
                                        (RTS)))

(define MILRT_DEC_VAL_HEAP_BY_Y
  (list
      ;; decrement ptr to free tos of value stack (push) by Y
      ;; Y = 0
      (label MILRT_DEC_VAL_HEAP_BY_Y)   (DEC MILRT_ZP_VAL_HEAP_PTR)
                                        (BNE MILRT_DVHBY_DONE)
                                        (DEC MILRT_ZP_VAL_HEAP_PTRP1)
      (label MILRT_DVHBY_DONE)          (DEY)
                                        (BNE MILRT_DEC_VAL_HEAP_BY_Y)
                                        (RTS)))

(define MILRT_INC_VAL_HEAP_BY_Y
  (list
      ;; increment ptr to free tos of value stack (pop) by Y
      ;; Y = 0
      (label MILRT_INC_VAL_HEAP_BY_Y)   (INC MILRT_ZP_VAL_HEAP_PTR)
                                        (BNE MILRT_IVHBY_DONE)
                                        (INC MILRT_ZP_VAL_HEAP_PTRP1)
      (label MILRT_IVHBY_DONE)          (DEY)
                                        (BNE MILRT_INC_VAL_HEAP_BY_Y)
                                        (RTS)))

(define MILRT_PUSH_LIST_END_MARKER
  (list
      (label MILRT_PUSH_LIST_END_MARKER)
                                        (LDY !2)
                                        (JSR MILRT_DEC_VAL_HEAP_BY_Y)
                                        (INY)
                                        (LDA !MILRT_LIST_TYPE_END)
                                        (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (DEY) ;; return w/ Y=0
                                        (RTS)))
(define MILRT_PUSH_LIST_START_MARKER
  (list
      (label MILRT_PUSH_LIST_START_MARKER)
                                        (LDY !2)
                                        (JSR MILRT_DEC_VAL_HEAP_BY_Y)
                                        (INY)
                                        (LDA !MILRT_LIST_TYPE_START)
                                        (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (DEY) ;; return w/ Y=0
                                        (RTS)))

(define MILRT_GENERIC_POP
  (list
   (label MILRT_INTERNAL_POP)
                                        (LDY !1)
                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (CMP !MILRT_UINT8_TYPE)
                                        (BEQ MILRT_GENERIC_POP__POPUINT8)
                                        (CMP !MILRT_BOOL_TYPE)
                                        (BEQ MILRT_GENERIC_POP__POPBOOL)
                                        (CMP !MILRT_STRING_TYPE)
                                        (BEQ MILRT_GENERIC_POP__POPSTRING)
                                        (CMP !MILRT_SYMBOL_TYPE)
                                        (BEQ MILRT_GENERIC_POP__POPSYMBOL)
                                        (CMP !MILRT_LIST_TYPE_START)
                                        (BEQ MILRT_GENERIC_POP__POPLIST)
                                        (CMP !MILRT_CONS_CELL_TYPE)
                                        (BEQ MILRT_GENERIC_POP__POPCONS)

    (label MILRT_GENERIC_POP__POPUINT8) (JMP MILRT_POP_UINT8)
    (label MILRT_GENERIC_POP__POPBOOL)  (JMP MILRT_POP_BOOL)
    (label MILRT_GENERIC_POP__POPSTRING) (JMP MILRT_POP_STRING)
    (label MILRT_GENERIC_POP__POPSYMBOL) (JMP MILRT_POP_SYMBOL)
    (label MILRT_GENERIC_POP__POPLIST)  (JMP MILRT_POP_LIST)
    (label MILRT_GENERIC_POP__POPCONS)  (JMP MILRT_POP_CONS_CELL)
))

(define MILRT_POP_LIST
  (list
   (label MILRT_POP_LIST)
                                        (LDX !1)
   (label MILRT_POP_LIST__CONT)
                                        (LDY !1)
                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (CMP !MILRT_LIST_TYPE_START)
                                        (BEQ MILRT_POP_LIST__INC_LEVEL)
                                        (CMP !MILRT_LIST_TYPE_END)
                                        (BEQ MILRT_POP_LIST__DEC_LEVEL)
                                        (JSR MILRT_GENERIC_POP)
                                        
   (label MILRT_POP_LIST__INC_LEVEL) 
                                        (INX)
                                        (INX) ;; since another dex is executed
   (label MILRT_POP_LIST__DEC_LEVEL)
                                        (LDY !2)
                                        (JSR MILRT_DEC_VAL_HEAP_BY_Y)
                                        (DEX)
                                        (BNE MILRT_POP_LIST__CONT)

                                        (RTS)))

;; combines the two top elements on the stack to a cons cell
(define MILRT_PUSH_CONS_CELLT
  (list
      (label MILRT_PUSH_CONS_CELLT)     (LDY !2)
                                        (JSR MILRT_DEC_VAL_HEAP_BY_Y)
                                        (INY)
                                        (LDA !MILRT_CONS_CELL_TYPE)
                                        (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (DEY) ;; return w/ Y=0
                                        (RTS)))

(define MILRT_POP_CONS_CELL
  (list
      (label MILRT_POP_CONS_CELL)       (LDY !6)
                                        (JMP MILRT_INC_VAL_HEAP_BY_Y)))

(define MILRT_PUSH_UINT8
  (list
      ;; push the UINT8 in A onto the value stack
      ;; A = MILRT_UINT8_TYPE, Y = 0
      (label MILRT_PUSH_UINT8)          (LDY !2)
                                        (JSR MILRT_DEC_VAL_HEAP_BY_Y)
                                        (LDY !2)
                                        (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (DEY)
                                        (LDA !MILRT_UINT8_TYPE)
                                        (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (DEY) ;; return w/ Y=0
                                        (RTS)))

(define MILRT_POP_UINT8
  (list
      (label MILRT_POP_UINT8)           (LDY !1)
                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (CMP !MILRT_UINT8_TYPE)
                                        (BEQ MILRT_POP_UINT8__CONT)
                                        (JMP MILRT_TYPE_ERROR)
      (label MILRT_POP_UINT8__CONT)
                                        (INY)
                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (JSR MILRT_INC_VAL_HEAP_BY_Y)
                                        (RTS)))

(define MILRT_PUSH_BOOL
  (list
      ;; push the UINT8 in A onto the value stack
      ;; A = MILRT_BOOL_TYPE, Y = 0
      (label MILRT_PUSH_BOOL)           (LDY !2)
                                        (JSR MILRT_DEC_VAL_HEAP_BY_Y)
      (label MILRT_SETTOS_BOOL)
                                        (LDY !2)
                                        (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (DEY)
                                        (LDA !MILRT_BOOL_TYPE)
                                        (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (DEY) ;; return w/ Y=0
                                        (RTS)))

(define MILRT_POP_BOOL
  (list
      (label MILRT_POP_BOOL)            (LDY !1)
                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (CMP !MILRT_BOOL_TYPE)
                                        (BEQ MILRT_POP_BOOL__CONT)
                                        (JMP MILRT_TYPE_ERROR)
      (label MILRT_POP_BOOL__CONT)
                                        (INY)
                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (JSR MILRT_INC_VAL_HEAP_BY_Y)
                                        (CMP !0)
                                        (RTS)))

(define MILRT_GREATER
  (list
      (label MILRT_GREATER)
                                        (JSR MILRT_POP_UINT8)
                                        (PHA)
                                        (LDY !1)
                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (CMP !MILRT_UINT8_TYPE)
                                        (BEQ MILRT_GREATER__CONT)
                                        (PLA) ;; drop pushed a from stack
                                        (JMP MILRT_TYPE_ERROR)
      (label MILRT_GREATER__CONT)
                                        (PLA)
                                        (INY)
                                        (CMP (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (BEQ MILRT_GREATER__FALSE)
                                        (BCC MILRT_GREATER__FALSE)
                                        (LDA !$FF) ;; true
                                        (JMP MILRT_SETTOS_BOOL)
      (label MILRT_GREATER__FALSE)
                                        (LDA !0)
                                        (JMP MILRT_SETTOS_BOOL)))

(define MILRT_SMALLER
  (list
      (label MILRT_SMALLER)
                                        (JSR MILRT_POP_UINT8)
                                        (PHA)
                                        (LDY !1)
                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (CMP !MILRT_UINT8_TYPE)
                                        (BEQ MILRT_SMALLER__CONT)
                                        (PLA) ;; drop pushed a from stack
                                        (JMP MILRT_TYPE_ERROR)
      (label MILRT_SMALLER__CONT)
                                        (PLA)
                                        (INY)
                                        (CMP (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (BCS MILRT_SMALLER__FALSE)

                                        (LDA !$FF) ;; true
                                        (JMP MILRT_SETTOS_BOOL)
      (label MILRT_SMALLER__FALSE)
                                        (LDA !0) ;; false
                                        (JMP MILRT_SETTOS_BOOL)))

(define MILRT_PLUS
  (list
      (label MILRT_PLUS)
                                        (JSR MILRT_POP_UINT8)
                                        (PHA)
                                        (LDY !1)
                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (CMP !MILRT_UINT8_TYPE)
                                        (BEQ MILRT_PLUS__CONT)
                                        (PLA) ;; drop pushed a from stack
                                        (JMP MILRT_TYPE_ERROR)
      (label MILRT_PLUS__CONT)
                                        (PLA)
                                        (INY)
                                        (CLC)
                                        (ADC (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (RTS)))

(define MILRT_MINUS
  (list
      (label MILRT_MINUS)
                                        (JSR MILRT_POP_UINT8)
                                        (PHA)
                                        (LDY !1)
                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (CMP !MILRT_UINT8_TYPE)
                                        (BEQ MILRT_MINUS__CONT)
                                        (PLA) ;; drop pushed a from stack
                                        (JMP MILRT_TYPE_ERROR)
      (label MILRT_MINUS__CONT)
                                        (PLA)
                                        (INY)
                                        (CLC)
                                        (SBC (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (RTS)))

(define MILRT_CDR
  (list
   (label MILRT_CDR)
                                        (RTS)))

(define MILRT_CAR
  (list
   (label MILRT_CAR)
                                        (RTS)))

(define MILRT_CONS
  (list
   (label MILRT_CONS)
                                        (RTS)))

(define MILRT_EQUAL
  (list
   (label MILRT_EQUAL)
                                        (RTS)))

(define MILRT_NOT
  (list
   (label MILRT_NOT)
                                        (RTS)))

(define MILRT_ZERO
  (list
   (label MILRT_ZERO)
                                        (RTS)))

(define MILRT_TYPE_ERROR
  (list
      (label MILRT_TYPE_ERROR)
                                        (LDA-immediate (char->integer #\X))
                                        (JMP $FFD2)))

(define MILRT_DISPLAY_STRING
  (list
   (label MILRT_DISPLAY_STRING)
                                        (LDA-immediate (char->integer #\"))
                                        (JSR $FFD2)
                                        (LDY !2)  ;; expecting bytes on the stack
                                        (TYA)
                                        (PHA)
                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y) ;; get string id
                                        (JSR MILRT_STRING_ID2PTR)
                                        (PLA)
                                        (TAY)
                                        (JSR MILRT_INC_VAL_HEAP_BY_Y) ;; drop string from value stack
                                        (LDA (MILRT_ZP_STRING_PTR),y) ;; y should be 0, a= strlen
                                        (TAY);;  y = strlen
      (label MILRT_DISPLAY_STRING__NEXT)       
                                        (LDA (MILRT_ZP_STRING_PTR),y)
                                        (CMP-immediate (char->integer #\"))
                                        (BNE MILRT_DISPLAY_STRING__REG)
                                        ;; (LDA-immediate (char->integer #\#))
                                        (JSR $FFD2)
                                        ;; (LDA (MILRT_ZP_STRING_PTR),y)
      (label MILRT_DISPLAY_STRING__REG)
                                        (JSR $FFD2)
                                        (DEY)
                                        (BNE MILRT_DISPLAY_STRING__NEXT)
                                        (LDA-immediate (char->integer #\"))
                                        (JSR $FFD2)
                                        (RTS)))

(define MILRT_DISPLAY
  (list
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
      (label MILRT_DISPLAY__NEXT)        (LDA (MILRT_ZP_STRING_PTR),y)
                                        ;; if A = #\^, then special (up arrow)
                                        (CMP !$5E)
                                        (BNE MILRT_DISPLAY__REG)
                                        (DEY)
                                        (LDA (MILRT_ZP_STRING_PTR),y)
                                        (CMP !$5E)
                                        (BNE MILRT_DISPLAY__OTHER)
      (label MILRT_DISPLAY__REG)        (JSR $FFD2)
      (label MILRT_DISPLAY__REG_CONT)   (DEY)
                                        (BNE MILRT_DISPLAY__NEXT)
                                        (RTS)

      (label MILRT_DISPLAY__OTHER)      (CMP !97) ;; 'a'
                                        (BNE MILRT_DISPLAY__STR_FORMAT_ERROR)
                                        ;; keep y
                                        (TYA)
                                        (PHA)
                                        (JSR MILRT_DISPLAY_OBJECT)
                                        (PLA)
                                        (TAY)
                                        (JMP MILRT_DISPLAY__REG_CONT) ;; continue with rest of string
      (label MILRT_DISPLAY__STR_FORMAT_ERROR)
                                        (LDA !STRING_ID_FORMAT_ERROR)
                                        (JSR MILRT_PUSH_STRING)
                                        (JMP MILRT_DISPLAY)))

(define MILRT_DISPLAY_OBJECT
  (list

      ;; print object tos as string and pop
      ;; uint8 => 0..255, string => string, char => char, bool => true/false
      ;; cons-cell => (a . b), list => (a b ... )
      ;; Y = *, A = *
      (label MILRT_DISPLAY_OBJECT)
                                        (LDY !1)  ;;
                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y) ;; get type descriptor
      (label MILRT_DISPLAY_OBJECT_A)
                                        (CMP !MILRT_UINT8_TYPE)
                                        (BNE MILRT_DISPLAY_NONUINT)
      (label MILRT_DISPLAY_UINT8)
                                        (INY)
                                        (LDA !36) ;; prefix with '$'
                                        (JSR $FFD2)

                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y) ;; get uint value
                                        (JSR MILRT_INC_VAL_HEAP_BY_Y) ;; pop uint incl. type descriptor from stack
                                        (JMP MILRT_DISPLAY_UINT8-RAW)

      (label MILRT_DISPLAY_NONUINT)     ;; TODO: implement printing strings, cons-cells, lists and boolean
                                        (CMP !MILRT_LIST_TYPE_START)
                                        (BNE MILRT_DISPLAY_OBJECT__NOLIST)
                                        (JMP MILRT_DISPLAY_LIST)
                                        
      (label MILRT_DISPLAY_OBJECT__NOLIST)
                                        (CMP !MILRT_STRING_TYPE)
                                        (BNE MILRT_DISPLAY_OBJECT_NOSTRING)
                                        (JMP MILRT_DISPLAY_STRING)

      (label MILRT_DISPLAY_OBJECT_NOSTRING)
                                        (LDA-immediate (char->integer #\?))
                                        (JMP $FFD2)))

(define MILRT_DISPLAY_LIST
  (list
   (label MILRT_DISPLAY_LIST)
                                        (LDA !0)
                                        (STA MILRT_LIST_LEVEL)

   (label MILRT_DISPLAY_LIST__NEXTLEVEL)
                                        (INC MILRT_LIST_LEVEL)
                                        (LDY !2)
                                        (JSR MILRT_INC_VAL_HEAP_BY_Y)
   
                                        (LDA-immediate (char->integer #\())
                                        (JSR $FFD2)

   (label MILRT_DISPLAY_LIST__CONT)
                                        (LDY !1)
                                        (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                        (CMP !MILRT_LIST_TYPE_END)
                                        (BEQ MILRT_DISPLAY_LIST__LE)

                                        (PHA)
                                        (LDA-immediate (char->integer #\ ))
                                        (JSR $FFD2)
                                        (PLA)

                                        (CMP !MILRT_LIST_TYPE_START)
                                        (BEQ MILRT_DISPLAY_LIST__NEXTLEVEL)
                                        
                                        (JSR MILRT_DISPLAY_OBJECT_A)
                                        (JMP MILRT_DISPLAY_LIST__CONT)

   (label MILRT_DISPLAY_LIST__LE)
                                        (LDY !2)     
                                        (JSR MILRT_INC_VAL_HEAP_BY_Y)
                                        (LDA-immediate (char->integer #\)))
                                        (JSR $FFD2)
                                        (DEC MILRT_LIST_LEVEL)
                                        (BNE MILRT_DISPLAY_LIST__CONT)
                                        (RTS)
))

(define MILRT_DISPLAY_UINT8-RAW
  (list
                                        ;; basic routine to write byte !

                                        ;; print byte in A as Hex
                                        ;; A = last Digit, need 1 stack slot
      (label MILRT_DISPLAY_UINT8-RAW)
                                        (PHA)                            ; Save A
                                        (LSR) (LSR) (LSR) (LSR)          ; Move top nybble to bottom nybble
                                        (JSR MILRT_DISPLAY_NIBBLE)     ; Print this nybble
                                        (PLA)                            ; Get A back and print bottom nybble

      (label MILRT_DISPLAY_NIBBLE)
                                        (AND !15)                        ; Keep bottom four bits
                                        (CMP !10)
                                        (BCC MILRT_DISPLAY_DIGIT)      ; If 0-9, jump to print
                                        (ADC !6)                         ; Convert ':' to 'A'
      (label MILRT_DISPLAY_DIGIT)       (ADC !48) ; 0 -> 0
                                        (JMP $FFD2)))

(define MILRT_STRING_ID2PTR
  (list
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
                                        (RTS)))


(define mil-runtime
    (append
     MILRT_CONSTANTS
     MILRT_ZERO_PAGE_SETUP
     MILRT_COPY_STRINGS_TO_IDTABLE
     MILRT_SETUP_VAL_HEAP
     (list (JMP MAIN))
     MILRT_DEC_VAL_HEAP_BY_Y
     MILRT_INC_VAL_HEAP_BY_Y
     MILRT_PUSH_STRING
     MILRT_PUSH_UINT8
     MILRT_POP_UINT8
     MILRT_PUSH_BOOL
     MILRT_POP_BOOL
     MILRT_GREATER
     MILRT_SMALLER
     MILRT_PLUS
     MILRT_MINUS
     MILRT_CDR
     MILRT_CAR
     MILRT_CONS
     MILRT_EQUAL
     MILRT_NOT
     MILRT_ZERO
     MILRT_TYPE_ERROR
     MILRT_DISPLAY
     MILRT_DISPLAY_OBJECT
     MILRT_DISPLAY_UINT8-RAW
     MILRT_DISPLAY_LIST
     MILRT_STRING_ID2PTR
     MILRT_PUSH_LIST_START_MARKER
     MILRT_PUSH_LIST_END_MARKER
     MILRT_DISPLAY_STRING
     (list
      (label MAIN)
      ;; (LDA-immediate (char->integer #\Z))
      ;; (JMP $FFD2)
      )))

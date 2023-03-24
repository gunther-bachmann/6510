#lang racket

(require "./mil-structures.rkt")
(require "./mil-compile-structures.rkt")
(require "../6510.rkt")
(require "../ast/6510-resolver.rkt")
(require "../ast/6510-relocator.rkt")
(require "../ast/6510-constants.rkt")
(require "../tools/6510-prg-generator.rkt")
(require "./mil-6510-commands.rkt")

(provide compile-expression)

(module+ test #| require test utils |#
  (require "../6510-test-utils.rkt"))

(define user-function-prefix "FUN-")

(define/contract (translate-function-symbol sym)
  (-> symbol? symbol?)
  (cond ((eq? sym '+)       'MILRT_PLUS)
        ((eq? sym '-)       'MILRT_MINUS)
        ((eq? sym 'display) 'MILRT_DISPLAY)
        ((eq? sym 'cdr)     'MILRT_CDR)
        ((eq? sym 'car)     'MILRT_CAR)
        ((eq? sym 'cons)    'MILRT_CONS)
        ((eq? sym '>)       'MILRT_GREATER)
        ((eq? sym '<)       'MILRT_SMALLER)
        ((eq? sym 'eq?)     'MILRT_EQUAL)
        ((eq? sym 'not)       'MILRT_NOT)
        ((eq? sym 'zero?)   'MILRT_ZERO)
        (#t                 (string->symbol
                             (regexp-replace* #rx"[^a-zA-Z_0-9-]"
                                              (string-append user-function-prefix
                                                             (symbol->string sym)) "_")))))

(module+ test #| translate-function-symbol |#
  (check-equal? (translate-function-symbol '+)
                'MILRT_PLUS)
  (check-equal? (translate-function-symbol 'Some?+Other?)
                'FUN-Some__Other_))

(define/contract (encode-string str)
  (-> string? (listof ast-command?))
  (list (ast-bytes-cmd (list (string-length str)))
        (ast-bytes-cmd (map char->integer (reverse (string->list str))))))

(module+ test #| encode-string |#
  (check-equal? (encode-string "ABC")
                (list
                 (ast-bytes-cmd (list 3))
                 (ast-bytes-cmd (list 67 66 65)))))

(define/contract (gen-string-table ctx)
  (-> compile-ctx? (listof ast-command?))
  (flatten (list (label STRING-TABLE)
                 (map encode-string (compile-ctx-strings ctx)))))

(module+ test #| gen-string-table |#
  (check-equal? (gen-string-table (compile-ctx (list "SOME" "OTHER")))
                (list (label STRING-TABLE)
                      (byte 4) (asc "EMOS")
                      (byte 5) (asc "REHTO"))))

(define/contract (compile-bool expr ctx)
  (-> mil-bool? compile-ctx? (values (listof ast-command?) compile-ctx?))
  (values
   (list
    (if (mil-bool-value expr)
        (LDA !$FF)
        (LDA !$00))
    (JSR MILRT_PUSH_BOOL))
   ctx))

(module+ test #| compile-bool |#
  (check-equal? (let-values (((opcodes ctx) (compile-bool (mil-bool #t) (compile-ctx (list)))))
                  opcodes)
                (list
                 (LDA !$FF)
                 (JSR MILRT_PUSH_BOOL)))
  (check-equal? (let-values (((opcodes ctx) (compile-bool (mil-bool #f) (compile-ctx (list)))))
                  opcodes)
                (list
                 (LDA !$00)
                 (JSR MILRT_PUSH_BOOL))))

(define/contract (compile-uint8 expr ctx)
  (-> mil-uint8? compile-ctx? (values (listof ast-command?) compile-ctx?))
  (values
   (list
    (LDA-immediate (mil-uint8-value expr))
    (JSR MILRT_PUSH_UINT8))
   ctx))

(module+ test #| compile |#

  (check-equal? (let-values (((opcodes ctx) (compile-uint8 (mil-uint8 #x18) (compile-ctx (list)))))
                  opcodes)
                (list (LDA !$18)
                      (JSR MILRT_PUSH_UINT8))))

(define/contract (compile-string expr ctx)
  (-> mil-string? compile-ctx? (values (listof ast-command?) compile-ctx?))
  (values 
   (list
    (LDA-immediate (length (compile-ctx-strings ctx))) 
    (JSR MILRT_PUSH_STRING))
   (struct-copy compile-ctx ctx
                [strings (append (compile-ctx-strings ctx)
                                 (list (mil-string-value expr)))])))

(module+ test #| compile-string |#
  (check-equal? (let-values (((opcodes ctx) (compile-string (mil-string "SOME") (compile-ctx (list)))))
                  opcodes)
                (list (LDA !0)
                      (JSR MILRT_PUSH_STRING))))

(define/contract (compile-list expr ctx)
  (-> mil-list? compile-ctx? (values (listof ast-command?) compile-ctx?))
  (if (empty? (mil-list-elements expr))
      (values (list) ctx)
      (compile--elements
       (append (reverse (cdr (mil-list-elements expr)))
               (list (list (JSR-absolute (translate-function-symbol (mil-symbol-value (car (mil-list-elements expr))))))))
       ctx)))

(module+ test #| compile-list |#
  (check-equal? (let-values (((opcodes ctx) (compile-list (mil-l) (compile-ctx (list)))))
                  opcodes)
                (list))

  (check-equal? (let-values (((opcodes ctx) (compile-list (mil-l (mil-symbol 'my-func)) (compile-ctx (list)))))
                  opcodes)
                (list (JSR FUN-my-func)))

  (check-equal? (let-values (((opcodes ctx)
                              (compile-list (mil-l (mil-symbol 'my-func) (mil-uint8 #x20))
                                            (compile-ctx (list)))))
                  opcodes)
                (list
                 (LDA !$20)
                 (JSR MILRT_PUSH_UINT8)
                 (JSR FUN-my-func)))

  (check-equal? (let-values (((opcodes ctx)
                              (compile-list (mil-l (mil-symbol 'my-func)
                                                   (mil-string "SOME")
                                                   (mil-string "OTHER")
                                                   (mil-uint8 #x20))
                                            (compile-ctx (list)))))
                  opcodes)
                (list
                 (LDA !$20)
                 (JSR MILRT_PUSH_UINT8)
                 (LDA !0)
                 (JSR MILRT_PUSH_STRING)
                 (LDA !1)
                 (JSR MILRT_PUSH_STRING)
                 (JSR FUN-my-func)))
  (check-equal? (let-values (((opcodes ctx)
                              (compile-list (mil-l (mil-symbol 'my-func)
                                                   (mil-string "SOME")
                                                   (mil-string "OTHER")
                                                   (mil-uint8 #x20))
                                            (compile-ctx (list)))))
                  (gen-string-table ctx))
                (list
                 (label STRING-TABLE)
                 (byte 5) (asc "REHTO")
                 (byte 4) (asc "EMOS"))))

;; compile a list of mil-expressions (interspersed with list of opcodes), passing ctx to each next compile
(define/contract (compile--elements elements ctx (opcodes (list)))
  (->* ((listof (or/c mil-expression? (listof ast-command?))) compile-ctx?) ((listof ast-command?)) (values (listof ast-command?) compile-ctx?))
  (if (empty? elements)
      (values opcodes ctx)
      (let ((element (car elements)))
        (if (mil-expression? element)
            (let-values (((compiled-opcodes compiled-ctx) (compile-expression element ctx)))
              (compile--elements (cdr elements) compiled-ctx (append opcodes compiled-opcodes)))
            (compile--elements (cdr elements) ctx (append opcodes element))))))

(module+ test #| compile--elements |#
  (check-equal? (let-values (((opcodes ctx) (compile--elements (list (mil-uint8 15)
                                                                     (list (JSR test))
                                                                     (mil-bool #f)
                                                                     (list (ADC !20))) (compile-ctx (list)))))
                  opcodes)
                (list (LDA !15)
                      (JSR MILRT_PUSH_UINT8)
                      (JSR test)
                      (LDA !0)
                      (JSR MILRT_PUSH_BOOL)
                      (ADC !20))))

(define/contract (compile-if expr ctx (true_target (gensym "if_true")) (end_target (gensym "if_end")))
  (->* (mil-if? compile-ctx?) (symbol? symbol?) (values (listof ast-command?) compile-ctx?))  
  (compile--elements
   (list
    (mil-if-predicate expr)
    (list 
     (JSR MILRT_POP_BOOL)
     (BNE-relative true_target))
    (mil-if-false-body expr) 
    (list 
     (JMP-absolute end_target)
     (label-def true_target))
    (mil-if-true-body expr)
    (list 
     (label-def end_target)))
   ctx))

(module+ test #| compile-if |#
  (check-equal? (let-values (((opcodes ctx) (compile-if (mil-if (mil-bool #f) (mil-string "SOME") (mil-string "OTHER")) (compile-ctx (list)) 'if_true 'if_end)))
                  opcodes)
                (list
                 (LDA !0)
                 (JSR MILRT_PUSH_BOOL)
                 (JSR MILRT_POP_BOOL)
                 (BNE if_true)
                 (LDA !0)
                 (JSR MILRT_PUSH_STRING)
                 (JMP if_end)
                 (label if_true)
                 (LDA !1)
                 (JSR MILRT_PUSH_STRING)
                 (label if_end))))

(define/contract (compile-expression expr ctx)
  (-> mil-expression? compile-ctx? (values (listof ast-command?) compile-ctx?))  
  (cond
    ((mil-uint8? expr)  (compile-uint8 expr ctx))
    ((mil-string? expr) (compile-string expr ctx))
    ((mil-list? expr)   (compile-list expr ctx))
    ((mil-bool? expr)   (compile-bool expr ctx))
    ((mil-if? expr)     (compile-if expr ctx))
    (#t (raise-user-error "cannot compile expression ~a" expr))))

(define (mil->asm expr)
  (define milrt
    (list
     (byte-const STRING_ID_FORMAT_ERROR 0)

     (byte-const MILRT_STRING_TYPE 1)
     (byte-const MILRT_UINT8_TYPE 2)
     (byte-const MILRT_BOOL_TYPE 3)

     (word-const MILRT_VAL_HEAP #x9eff)
     (byte-const MILRT_ZP_VAL_HEAP_PTR #x71)
     (byte-const MILRT_ZP_VAL_HEAP_PTRP1 #x72)
     (byte-const MILRT_ZP_STRING_PTR #xfb)
     (byte-const MILRT_ZP_STRING_PTRP1 #xfc)
     (byte-const MILRT_ZP_STRING_PTR2 #xfd)
     (byte-const MILRT_ZP_STRING_PTR2P1 #xfe)

     (word-const MILRT_STRING_ID_TABLE #xc000)
     (word-const MILRT_STRING_ID_TABLE_P2 #xc100)
     (word-const MILRT_STRING_TABLE #xc200)

     (label MILRT_SETUP)               (LDA !<STRING-TABLE)
                                       (STA MILRT_ZP_VAL_HEAP_PTR)
                                       (LDA !>STRING-TABLE)
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
                                       (RTS)

                                       ;; push the UINT8 in A onto the value stack
                                       ;; A = MILRT_BOOL_TYPE, Y = 0
     (label MILRT_PUSH_BOOL)           (LDY !2)
                                       (JSR MILRT_DEC_VAL_HEAP_BY_Y)
                                       (LDY !2)
                                       (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                       (DEY)
                                       (LDA !MILRT_BOOL_TYPE)
                                       (STA (MILRT_ZP_VAL_HEAP_PTR),y)
                                       (DEY) ;; return w/ Y=0
                                       (RTS)

     (label MILRT_POP_BOOL)            (LDY !1)
                                       (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                       (CMP !MILRT_BOOL_TYPE)
                                       (BNE MILRT_TYPE_ERROR)
                                       (INY)
                                       (LDA (MILRT_ZP_VAL_HEAP_PTR),y)
                                       (JSR MILRT_INC_VAL_HEAP_BY_Y)
                                       (CMP !0)
                                       (RTS)

     (label MILRT_TYPE_ERROR)          (BRK)

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
                                     
     (label MILRT_DISPLAY_OTHER)       (CMP !97) ;; 'a'
                                       (BNE STR_FORMAT_ERROR)
                                       ;; keep y
                                       (TYA)
                                       (PHA)
                                       (JSR MILRT_DISPLAY_OBJECT)
                                       (PLA)
                                       (TAY)
                                       (JMP MILRT_DISPLAY_REGULAR_C) ;; continue with rest of string
     (label STR_FORMAT_ERROR)
                                       (LDA !STRING_ID_FORMAT_ERROR)
                                       (JSR MILRT_PUSH_STRING)
                                       (JMP MILRT_DISPLAY)
                                       ;; (BRK) ; stop

                                       ;; print object tos as string and pop
                                       ;; uint8 => 0..255, string => string, char => char, bool => true/false
                                       ;; cons-cell => (a . b), list => (a b ... )
                                       ;; Y = *, A = *
     (label MILRT_DISPLAY_OBJECT)
                                       (LDY !1)  ;;
                                       (LDA (MILRT_ZP_VAL_HEAP_PTR),y) ;; get type descriptor
                                       (CMP !MILRT_UINT8_TYPE)
                                       (BNE MILRT_DISPLAY_NONUINT)
     (label MILRT_DISPLAY_UINT)
                                       (INY)
                                       (LDA !36) ;; prefix with '$'
                                       (JSR $FFD2)

                                       (LDA (MILRT_ZP_VAL_HEAP_PTR),y) ;; get uint value
                                       (JSR MILRT_INC_VAL_HEAP_BY_Y) ;; pop uint incl. type descriptor from stack

                                       ;; basic routine to write byte !

                                       ;; print byte in A as Hex
                                       ;; A = last Digit, need 1 stack slot
     (label MILRT_DISPLAY_UINT8)
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
                                       (JMP $FFD2)

     (label MILRT_DISPLAY_NONUINT)     ;; TODO: implement printing strings, cons-cells, lists and boolean
                                       (LDA-immediate (char->integer #\Y))
                                       (JSR $FFD2)
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

     (label MAIN)))
  (let-values (((opcodes ctx) (compile-expression expr (compile-ctx (list "STRING FORMAT ERROR")))))
    (define program (append milrt opcodes (list (RTS)) (gen-string-table ctx)))
    

    (define program-p1 (->resolved-decisions (label-instructions program) program))
    (define program-p2 (->resolve-labels org (label-string-offsets org program-p1) program-p1 '()))
    (define program-p3 (resolve-constants (constant-definitions-hash program-p1) program-p2))
    (define raw-bytes (resolved-program->bytes program-p3))

    (create-prg raw-bytes org "compiled.prg")
    ;; (create-image-with-program raw-bytes org "compiled.prg" "compiled.d64" ".")

    raw-bytes))

(define org 2064)
(module+ main
  (require "../tools/6510-debugger.rkt")
  (run-debugger
   org
   (mil->asm
    ;; (mil-l (mil-symbol 'display)
    ;;        (mil-string "OH"))
    ;; (mil-l (mil-symbol 'display)
    ;;        (mil-string "HELLO WORLD ^a!") ;; in mil its printed in hex
    ;;        (mil-uint8 #xa3)
    ;;        )
    (mil-if (mil-bool #f)
            (mil-l (mil-symbol 'display)
                   (mil-string "HELLO WORLD ^a!") ;; in mil its printed in hex
                   (mil-uint8 #xa3))
            (mil-l (mil-symbol 'display)
                   (mil-string "OH")))
    )))


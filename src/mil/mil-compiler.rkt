#lang racket

(require "./mil-structures.rkt")
(require "./mil-compile-structures.rkt")
(require "../6510.rkt")
(require "../tools/6510-prg-generator.rkt")
(require "../ast/6510-calc-opcode-facades.rkt")
(require "./mil-runtime.rkt")
(require "../ast/6510-assembler.rkt")

(provide compile-expression)

(module+ test #| require test utils |#
  (require "../6510-test-utils.rkt"))

(define user-function-prefix "FUN-")

(define/contract (translate-function-symbol sym)
  (-> symbol? symbol?)
  (cond ((eq? sym '+)       'MILRT_PLUS)    ;; ok
        ((eq? sym '-)       'MILRT_MINUS)   ;; ok
        ((eq? sym 'display) 'MILRT_DISPLAY) ;; ok
        ((eq? sym 'cdr)     'MILRT_CDR)
        ((eq? sym 'car)     'MILRT_CAR)
        ((eq? sym 'cons)    'MILRT_CONS)
        ((eq? sym '>)       'MILRT_GREATER) ;; ok
        ((eq? sym '<)       'MILRT_SMALLER) ;; ok
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
  (list (ast-bytes-cmd '()  (list (string-length str)))
        (ast-bytes-cmd '() (map char->integer (reverse (string->list str))))))

(module+ test #| encode-string |#
  (check-equal? (encode-string "ABC")
                (list
                 (ast-bytes-cmd '() (list 3))
                 (ast-bytes-cmd '() (list 67 66 65)))))

(define/contract (gen-string-table ctx)
  (-> compile-ctx? (listof ast-command?))
  (append (flatten (list (label STRING-TABLE)
                         (map encode-string (compile-ctx-strings ctx))))
          (list (ast-bytes-cmd '() (list 0)))))

(module+ test #| gen-string-table |#
  (check-equal? (gen-string-table (compile-ctx (list "SOME" "OTHER")))
                (list (label STRING-TABLE)
                      (byte 4) (asc "EMOS")
                      (byte 5) (asc "REHTO")
                      (byte 0))))

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
                 (byte 4) (asc "EMOS")
                 (byte 0))))

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

;; compile a list of mil-expressions (interspersed with list of opcodes), passing ctx to each next compile
(define/contract (quote--elements elements ctx (opcodes (list)))
  (->* ((listof (or/c mil-expression? (listof ast-command?))) compile-ctx?) ((listof ast-command?)) (values (listof ast-command?) compile-ctx?))
  (if (empty? elements)
      (values opcodes ctx)
      (let ((element (car elements)))
        (if (mil-expression? element)
            (let-values (((compiled-opcodes compiled-ctx) (quote-expression element ctx)))
              (quote--elements (cdr elements) compiled-ctx (append opcodes compiled-opcodes)))
            (quote--elements (cdr elements) ctx (append opcodes element))))))

(define/contract (quote-expression quoted ctx)
  (-> mil-expression? compile-ctx? (values (listof ast-command?) compile-ctx?))  
  (cond ((mil-atomic-value? quoted) (compile-expression quoted ctx))
        ((mil-list? quoted) (quote--elements
                             (append (list (list (JSR MILRT_PUSH_LIST_END_MARKER)))
                                     (reverse (mil-list-elements quoted))
                                     (list (list (JSR MILRT_PUSH_LIST_START_MARKER)))) 
                             ctx))
        ((mil-cell? quoted) (quote--elements
                             (list (mil-cell-tail quoted)
                                   (mil-cell-head quoted)
                                   (JSR MILRT_PUSH_CONS_CELLT))
                             ctx))
        (#t (raise-user-error "unknown expression type ~a" quoted))))

(define/contract (compile-quote expr ctx)
  (-> mil-quote? compile-ctx? (values (listof ast-command?) compile-ctx?))  
  (define quoted (mil-quote-quoted expr))
  (quote-expression quoted ctx))

(module+ test #| compile-quote |#
  (check-equal? (let-values (((opcodes ctx) (compile-quote (mil-quote (mil-l)) (compile-ctx (list)) )))
                  opcodes)
                (list
                 (JSR MILRT_PUSH_LIST_END_MARKER)
                 (JSR MILRT_PUSH_LIST_START_MARKER)))
  (check-equal? (let-values (((opcodes ctx) (compile-quote (mil-quote (mil-l (mil-uint8 10) (mil-uint8 20))) (compile-ctx (list))) ))
                  opcodes)
                (list
                 (JSR MILRT_PUSH_LIST_END_MARKER)
                 (LDA !20)
                 (JSR MILRT_PUSH_UINT8)
                 (LDA !10)
                 (JSR MILRT_PUSH_UINT8)
                 (JSR MILRT_PUSH_LIST_START_MARKER))))

(define/contract (compile-expression expr ctx)
  (-> mil-expression? compile-ctx? (values (listof ast-command?) compile-ctx?))  
  (cond
    ((mil-uint8? expr)  (compile-uint8 expr ctx))
    ((mil-string? expr) (compile-string expr ctx))
    ((mil-list? expr)   (compile-list expr ctx))
    ((mil-bool? expr)   (compile-bool expr ctx))
    ((mil-if? expr)     (compile-if expr ctx))
    ((mil-quote? expr)  (compile-quote expr ctx))
    (#t (raise-user-error "cannot compile expression ~a" expr))))


(define (mil->asm expr)
  
  (let-values (((opcodes ctx) (compile-expression expr (compile-ctx (list "STRING FORMAT ERROR")))))
    (define program (append mil-runtime (list) opcodes (list (RTS)) (gen-string-table ctx)))
    (define raw-bytes (assemble org program))

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
    ;; (mil-l (mil-symbol '-)
    ;;               (mil-uint8 10)
    ;;               (mil-uint8 100))
    ;; (mil-l (mil-symbol 'display)
    ;;        (mil-string "HELLO WORLD ^a!") ;; in mil its printed in hex
    ;;        (mil-l (mil-symbol '+)
    ;;               (mil-uint8 #x80)
    ;;               (mil-uint8 #x10))
    ;;        )
    ;; (mil-if (mil-bool #f)
    ;;         (mil-l (mil-symbol 'display)
    ;;                (mil-string "HELLO WORLD ^a!") ;; in mil its printed in hex
    ;;                (mil-uint8 #xa3))
    ;;         (mil-l (mil-symbol 'display)
    ;;                (mil-string "OH")))
    ;; (mil-uint8 #x10)
    ;; (mil-if (mil-l (mil-symbol '>)
    ;;                (mil-l (mil-symbol '+) (mil-uint8 #x10) (mil-uint8 #x03))
    ;;                (mil-uint8 #x12))
    ;;         (mil-l (mil-symbol 'display)
    ;;                (mil-string "HELLO WORLD ^a!") ;; in mil its printed in hex
    ;;                (mil-uint8 #xa3))
    ;;         (mil-l (mil-symbol 'display)
    ;;                (mil-string "OH")))

    (mil-l (mil-symbol 'display)
           (mil-string "LIST: ^a")
           (mil-quote (mil-l (mil-uint8 10) (mil-l (mil-uint8 20) (mil-string "O")) (mil-l) (mil-string "HEL\"LO")))
           )))

  )

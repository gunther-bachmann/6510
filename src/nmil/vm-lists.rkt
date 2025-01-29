#lang racket/base

#|

implementation of list primitives (car, cdr, cons) using 6510 assembler routines

|#

(require "../6510.rkt")
(require "../util.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader))
(require (only-in racket/list flatten take))
(require (only-in "../ast/6510-relocator.rkt" command-len))

(require (only-in "./vm-memory-manager.rkt" vm-memory-manager vm-stack->strings vm-page->strings vm-regt->string vm-deref-cell-pair-w->string))
(require (only-in "./vm-call-frame.rkt" vm-call-frame))
(require (only-in "./vm-memory-manager-test-utils.rkt" run-code-in-test-on-code))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in racket/port open-output-nowhere))
  (require (only-in "../tools/6510-disassembler.rkt" disassemble-bytes))
  (require (only-in "../tools/6510-debugger.rkt" run-debugger-on))

  (define (wrap-code-for-test bc)
    (append (list (org #xc000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INITIALIZE_CALL_FRAME))
            bc
            (list (BRK))
            vm-lists))

  (define (run-code-in-test bc (debug #f))
    (define wrapped-code (wrap-code-for-test bc))
    (run-code-in-test-on-code wrapped-code  debug)))

(module+ test #| after mem init |#
  (define PAGE_AVAIL_0 #x97)
  (define PAGE_AVAIL_0_W #x9700)
  (define PAGE_AVAIL_1 #x96)
  (define PAGE_AVAIL_1_W #x9600))


(require (only-in "../tools/6510-interpreter.rkt" 6510-load 6510-load-multiple initialize-cpu run-interpreter run-interpreter-on memory-list cpu-state-accumulator))

(provide vm-lists)

;; code
;;   VM_NIL_P :: TOS := (TOS = nil?)
;;   VM_CAR   :: TOS := (car TOS)
;;   VM_CDR   :: TOS := (cdr TOS)
;;   VM_CONS  :: TOS := (TOS . TOS-1)

(define VM_NIL_P_R
  (list
   (label STACK_EMPTY__VM_NIL_P_R)
   (label NO_CELL_PAIR_PTR__VM_NIL_P_R)
          (BRK)

   ;; ----------------------------------------
   (label VM_NIL_P_R)
          ;; check if stack is not empty
          (LDA ZP_RT)
          (BEQ STACK_EMPTY__VM_NIL_P_R)

          ;; check if tos is cell-pair-ptr
          (AND !$03)
          (CMP !$01)
          (BNE NOT_NIL__VM_NIL_P_R)

   (label VM_NIL_P_R__UC) ;; no checks
          (LDA ZP_RT) ;; get tagged byte
          (CMP !<TAGGED_NIL) ;;
          (BNE NOT_NIL__VM_NIL_P_R)
          ;; this additional check should not be necessary, since tagged low byte of a non-nil cell-pair-ptr may never be #x02
          ;; (LDA ZP_RT+1) ;; get high byte
          ;; (BNE NOT_NIL__VM_NIL_P_R) ;; high byte of nil is 0! => branch if != 0
          (JMP VM_WRITE_INT1_TO_RT) ;; true
   (label NOT_NIL__VM_NIL_P_R)
          (JMP VM_WRITE_INT0_TO_RT) ;; false
          ))

(module+ test #| VM_NIL_P_R |#
  (define use-case-nil_p-a-code
    (list
     (JSR VM_WRITE_NIL_TO_RT)
     (JSR VM_NIL_P_R)
     ))

  (define use-case-nil_p-a-state-after  ;; (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on use-case-nil_p-a-state-before))
    (run-code-in-test use-case-nil_p-a-code))

  (check-equal? (vm-stack->strings use-case-nil_p-a-state-after)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define use-case-nil_p-b-code
    (list
     (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
     (JSR VM_NIL_P_R)))

  (define use-case-nil_p-b-state-after
    (run-code-in-test use-case-nil_p-b-code))

  (check-equal? (vm-stack->strings use-case-nil_p-b-state-after)
                (list "stack holds 1 item"
                      "int $0000  (rt)")
                "which is false"))

(define VM_CAR_R
  (list
   (label STACK_EMPTY__VM_CAR_R)
   (label RT_IS_NIL__VM_CAR_R)
   (label NO_CELL_PAIR_PTR__VM_CAR_R)
          (BRK)
   (label VM_CAR_R)
          ;; check operand present
          (LDA ZP_RT)
          (BEQ STACK_EMPTY__VM_CAR_R)

          ;; check RT is not nil
          (CMP !$01)
          (BEQ RT_IS_NIL__VM_CAR_R)

          ;; check RT is a cell-pair-ptr
          (AND !$03)
          (CMP !$01)
          (BNE NO_CELL_PAIR_PTR__VM_CAR_R)

          (JMP VM_WRITE_RT_CELL0_TO_RT)))

(define VM_CDR_R
  (list
   (label STACK_EMPTY__VM_CDR_R)
   (label RT_IS_NIL__VM_CDR_R)
   (label NO_CELL_PAIR_PTR__VM_CDR_R)
          (BRK)
   (label VM_CDR_R)
          ;; check operand present
          (LDA ZP_RT)
          (BEQ STACK_EMPTY__VM_CDR_R)

          ;; check RT is not nil
          (CMP !$01)
          (BEQ RT_IS_NIL__VM_CDR_R)

          ;; check RT is a cell-pair-ptr
          (AND !$03)
          (CMP !$01)
          (BNE NO_CELL_PAIR_PTR__VM_CDR_R)

          (JMP VM_WRITE_RT_CELL1_TO_RT)))

;; short command for doing caar, cadr, cdar, cddr
;; caar =  (car (car x))
;; cadr =  (car (cdr x))
;; cdar =  (cdr (car x))
;; cddr =  (cdr (cdr x))
(define VM_CxxR_R
  (list 
   (label VM_CxxR_R)
          (LSR)
          (BCS CONSES__VM_CxxR_R)
          (TAX)
          (LDA BRANCH_TARGETS__VM_CxxR_R,x)
          (STA BRANCH_COMMAND__VM_CxxR_R+1)
   (label BRANCH_COMMAND__VM_CxxR_R)
          (BNE $00)
          (JSR VM_CAR_R) ;; caar
          (JMP VM_CAR_R)
          (JSR VM_CDR_R) ;; cadr
          (JMP VM_CAR_R)
          (JSR VM_CAR_R) ;; cdar
          (JMP VM_CDR_R)
          (JSR VM_CDR_R) ;; cddr
          (JMP VM_CDR_R)
   (label CONSES__VM_CxxR_R)
          (RTS) ;; no implementation for the other 4 posssible commands

   (label BRANCH_TARGETS__VM_CxxR_R)
          (byte $00 $06 $0c $12)))

(define VM_CONS_R
  (list
   (label STACK_HAS_LESS_THAN_TWO__VM_CONS_R)
          (BRK)
   (label VM_CONS_R)
          (LDY ZP_CELL_STACK_TOS)
          (CPY !$01) ;; 01 = one element + RT = two elements
          (BMI STACK_HAS_LESS_THAN_TWO__VM_CONS_R)

   (label VM_CONS_R__UC) ;; no checks
          (LDA ZP_RT)
          (STA ZP_RA)
          (LDA ZP_RT+1)
          (STA ZP_RA+1)
          (JSR VM_ALLOC_CELL_PAIR_PTR_TO_RT)
          (JSR VM_WRITE_RA_TO_CELL0_RT)
          (JMP VM_POP_FSTOS_TO_CELL1_RT)))

(module+ test #| VM_CONS |#
  (define use-case-cons-code
    (list
     (JSR VM_CELL_STACK_PUSH_NIL_R)
     (JSR VM_CELL_STACK_PUSH_INT_1_R)
     (JSR VM_CONS_R)
     ))

  (define use-case-cons-state-after
    (run-code-in-test use-case-cons-code ))

  (check-equal? (vm-stack->strings use-case-cons-state-after)
                (list "stack holds 1 item"
                      (format "pair-ptr[0] $~a05  (rt)" (format-hex-byte PAGE_AVAIL_0))))
  (check-equal? (vm-page->strings use-case-cons-state-after PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09"))
  (check-equal? (vm-deref-cell-pair-w->string use-case-cons-state-after (+ PAGE_AVAIL_0_W #x05))
                "(int $0001 . pair-ptr NIL)"))

(define vm-lists
  (append VM_CONS_R
          VM_CAR_R
          VM_CDR_R
          VM_NIL_P_R
          VM_CxxR_R
          vm-call-frame))

(module+ test #| vm-lists |#
  (inform-check-equal? (foldl + 0 (map command-len (flatten vm-call-frame)))
                       1412))

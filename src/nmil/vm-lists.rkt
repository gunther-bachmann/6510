#lang racket/base

#|

implementation of list primitives (car, cdr, cons) using 6510 assembler routines

|#

(require "../6510.rkt")
(require "../util.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader))
(require (only-in racket/list flatten take))
(require (only-in "../ast/6510-relocator.rkt" command-len))
(require (only-in "./vm-inspector-utils.rkt"
                  vm-stack->strings
                  vm-page->strings
                  vm-regt->string
                  vm-deref-cell-pair-w->string))

(require (only-in "./vm-call-frame.rkt" vm-call-frame))
(require (only-in "./vm-memory-manager-test-utils.rkt" run-code-in-test-on-code))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in racket/port open-output-nowhere))
  (require (only-in "../tools/6510-disassembler.rkt" disassemble-bytes))
  (require (only-in "../tools/6510-debugger.rkt" run-debugger-on))

  (define (wrap-code-for-test bc)
    (append (list (org #xa000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INITIALIZE_CALL_FRAME))
            bc
            (list (BRK))
            vm-lists))

  (define (run-code-in-test bc (debug #f))
    (define wrapped-code (wrap-code-for-test bc))
    (run-code-in-test-on-code wrapped-code  debug)))

(module+ test #| after mem init |#
  (define PAGE_AVAIL_0 #x8a)
  (define PAGE_AVAIL_0_W #x8a00)
  (define PAGE_AVAIL_1 #x89)
  (define PAGE_AVAIL_1_W #x8900))


(require (only-in "../tools/6510-interpreter.rkt" 6510-load 6510-load-multiple initialize-cpu run-interpreter run-interpreter-on memory-list cpu-state-accumulator))

(provide vm-lists)

;; code
;;   VM_NIL_P :: TOS := (TOS = nil?)
;;   VM_CAR   :: TOS := (car TOS)
;;   VM_CDR   :: TOS := (cdr TOS)
;;   VM_CONS  :: TOS := (TOS . TOS-1)

;; @DC-FUN: VM_NIL_P, group: predicates
;; is tos currently NIL?
(define VM_NIL_P
  (list
   (label STACK_EMPTY__VM_NIL_P)
   (label NO_CELL_PAIR_PTR__VM_NIL_P)
          (BRK)

   ;; ----------------------------------------
   (label VM_NIL_P)
          ;; check if stack is not empty
          (LDA ZP_RT)
          (BEQ STACK_EMPTY__VM_NIL_P)

          ;; check if tos is cell-pair-ptr
          (AND !$03)
          (CMP !$01)
          (BNE NOT_NIL__VM_NIL_P)

   (label VM_NIL_P__UC) ;; no checks
          (LDA ZP_RT) ;; get tagged byte
          (CMP !<TAGGED_NIL) ;;
          (BNE NOT_NIL__VM_NIL_P)
          ;; this additional check should not be necessary, since tagged low byte of a non-nil cell-pair-ptr may never be #x02
          ;; (LDA ZP_RT+1) ;; get high byte
          ;; (BNE NOT_NIL__VM_NIL_P) ;; high byte of nil is 0! => branch if != 0
          (JMP WRITE_INT1_TO_RT) ;; true
   (label NOT_NIL__VM_NIL_P)
          (JMP WRITE_INT0_TO_RT) ;; false
          ))

(module+ test #| VM_NIL_P |#
  (define use-case-nil_p-a-code
    (list
     (JSR WRITE_NIL_TO_RT)
     (JSR VM_NIL_P)
     ))

  (define use-case-nil_p-a-state-after  ;; (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on use-case-nil_p-a-state-before))
    (run-code-in-test use-case-nil_p-a-code))

  (check-equal? (vm-stack->strings use-case-nil_p-a-state-after)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define use-case-nil_p-b-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR VM_NIL_P)))

  (define use-case-nil_p-b-state-after
    (run-code-in-test use-case-nil_p-b-code))

  (check-equal? (vm-stack->strings use-case-nil_p-b-state-after)
                (list "stack holds 1 item"
                      "int $0000  (rt)")
                "which is false"))

;; @DC-FUN: VM_CAR, group: list
;; replace cell-pair on tos with CAR element
(define VM_CAR
  (list
   (label STACK_EMPTY__VM_CAR)
   (label RT_IS_NIL__VM_CAR)
   (label NO_CELL_PAIR_PTR__VM_CAR)
          (BRK)
   (label VM_CAR)
          ;; check operand present
          (LDA ZP_RT)
          (BEQ STACK_EMPTY__VM_CAR)

          ;; check RT is not nil
          (CMP !$01)
          (BEQ RT_IS_NIL__VM_CAR)

          ;; check RT is a cell-pair-ptr
          (AND !$03)
          (CMP !$01)
          (BNE NO_CELL_PAIR_PTR__VM_CAR)

          (JMP WRITE_CELLPAIR_RT_CELL0_TO_RT)))

;; @DC-FUN: VM_CDR, group: list
;; replace cell-pair on tos with CDR element
(define VM_CDR
  (list
   (label STACK_EMPTY__VM_CDR)
   (label RT_IS_NIL__VM_CDR)
   (label NO_CELL_PAIR_PTR__VM_CDR)
          (BRK)
   (label VM_CDR)
          ;; check operand present
          (LDA ZP_RT)
          (BEQ STACK_EMPTY__VM_CDR)

          ;; check RT is not nil
          (CMP !$01)
          (BEQ RT_IS_NIL__VM_CDR)

          ;; check RT is a cell-pair-ptr
          (AND !$03)
          (CMP !$01)
          (BNE NO_CELL_PAIR_PTR__VM_CDR)

          (JMP WRITE_CELLPAIR_RT_CELL1_TO_RT)))

;; short command for doing caar, cadr, cdar, cddr
;; caar =  (car (car x))
;; cadr =  (car (cdr x))
;; cdar =  (cdr (car x))
;; cddr =  (cdr (cdr x))
;; @DC-FUN: VM_CxxR, group: list
;; execute caar, cadr, cdar or cddr depending on value in A
;; A & $1f = 00 -> caar
;; A & $1f = 06 -> cadr
;; A & $1f = 0c -> cdar
;; A & $1f = 12 -> cddr
;; input:  A, evlstk
;; usage:  A X Y
;; output: cell-pair on the stack replaced with its cxxr
(define VM_CxxR
  (list 
   (label VM_CxxR)
          (AND !$1f)
          (STA BRANCH_COMMAND__VM_CxxR+1)
   (label BRANCH_COMMAND__VM_CxxR)
          (BNE $00)
          (JSR VM_CAR) ;; caar
          (JMP VM_CAR)

          (JSR VM_CDR) ;; cadr
          (JMP VM_CAR)

          (JSR VM_CAR) ;; cdar
          (JMP VM_CDR)

          (JSR VM_CDR) ;; cddr
          (JMP VM_CDR)))

;; @DC-FUN: VM_CONS__REFCNTD, group: list
;; cons value to cell-pair on the stack
;; input: stack = value :: cell-pair
;; output: stack = new cell-pair
;;         new cell-pair = [value:old cell-pair]
(define VM_CONS__REFCNTD
  (list
   (label STACK_HAS_LESS_THAN_TWO__VM_CONS__REFCNTD)
          (BRK)
   (label VM_CONS__REFCNTD)
          (LDY ZP_CELL_STACK_TOS)
          (CPY !$01) ;; 01 = one element + RT = two elements
          (BMI STACK_HAS_LESS_THAN_TWO__VM_CONS__REFCNTD)

   (label VM_CONS__REFCNTD__UC) ;; no checks
          (LDA ZP_RT)
          (STA ZP_RP)
          (LDA ZP_RT+1)
          (STA ZP_RP+1)
          (JSR ALLOC_CELLPAIR_TO_RT)             ;; this cellpair is new
          (JSR INC_REFCNT_RT)
          (JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)    ;; overwrite rt, but rt is put into cell0 of freshly allocated cell-pair => no refcnt mod needed here
          (JMP POP_CELL_EVLSTK_TO_CELL1_RT)))    ;; and and write into cell1 => no refcnt mod needed here

(module+ test #| VM_CONS |#
  (define use-case-cons-code
    (list
     (JSR PUSH_NIL_TO_EVLSTK)
     (JSR PUSH_INT_1_TO_EVLSTK)
     (JSR VM_CONS__REFCNTD)
     ))

  (define use-case-cons-state-after
    (run-code-in-test use-case-cons-code ))

  (check-equal? (vm-stack->strings use-case-cons-state-after)
                (list "stack holds 1 item"
                      (format "pair-ptr[1] $~a05  (rt)" (format-hex-byte PAGE_AVAIL_0))))
  (check-equal? (vm-page->strings use-case-cons-state-after PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09"))
  (check-equal? (vm-deref-cell-pair-w->string use-case-cons-state-after (+ PAGE_AVAIL_0_W #x05))
                "(int $0001 . pair-ptr NIL)"))

(define just-vm-list
  (append VM_CONS__REFCNTD
          VM_CAR
          VM_CDR
          VM_NIL_P
          VM_CxxR))
(define vm-lists
  (append just-vm-list vm-call-frame))

(module+ test #| vm-lists |#
  (inform-check-equal? (foldl + 0 (map command-len (flatten just-vm-list)))
                       126
                       "estimated list code length"))

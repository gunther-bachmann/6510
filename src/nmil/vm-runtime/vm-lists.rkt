#lang racket/base

#|

implementation of list primitives (car, cdr, cons) using 6510 assembler routines

|#

(require (only-in racket/list
                  flatten
                  take)
         "../../6510.rkt"
         (only-in "../../ast/6510-relocator.rkt"
                  command-len
                  code-len)
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
         (only-in "../vm-inspector-utils.rkt"
                  vm-stack->strings
                  vm-page->strings
                  vm-regt->string
                  vm-deref-cell-pair-w->string)
         (only-in "./vm-cell-array.rkt"
                  WRITE_RP_TO_ARR_AT1_RT
                  POP_CELL_EVLSTK_TO_ARR_AT0_RT
                  ALLOC_CELL_ARRAY_P0_TO_RT)
         "./vm-memory-manager-test-utils.rkt")

(provide vm-list-code
         VM_CxxR
         VM_CAR
         VM_CDR
         VM_CONS__REFCNTD)

(module+ test
  (require "../../6510-test-utils.rkt"
           (only-in "../vm-interpreter-loop.rkt" VM_INTERPRETER_ZP)
           (only-in "./vm-cell-array.rkt" vm-cell-array-code)
           (only-in "./vm-m1-slots.rkt" vm-m1-slot-code)
           (only-in "./vm-pages.rkt" vm-pages-code)
           (only-in "./vm-cell-stack.rkt" vm-cell-stack-code)
           (only-in "./vm-register-functions.rkt" vm-register-functions-code )
           (only-in "./vm-memory-map.rkt" VM_MEMORY_MANAGEMENT_CONSTANTS))

  (define (wrap-code-for-test bc)
    (append (list (org #xA000)
                  (JSR VM_INITIALIZE_PAGE_MEMORY_MANAGER_N20)
                  (LDA !$01) ;; just mimick an empty cell-stack
                  (STA ZP_CELL_STACK_TOS))
            (list (label TEST_ENTRY))
            bc
            (list (BRK))
            vm-m1-slot-code
            vm-pages-code
            vm-cell-array-code
            vm-list-code
            vm-cell-stack-code
            VM_MEMORY_MANAGEMENT_CONSTANTS
            vm-register-functions-code
            (list (label VM_INTERPRETER_OPTABLE)) ;; needed by interpreter_zp
            VM_INTERPRETER_ZP)) ;; needed because of references to ZP_VM_PC

  (define (run-code-in-test bc (debug #f))
    (define wrapped-code (wrap-code-for-test bc))
    (run-code-in-test-on-code wrapped-code  debug)))

(module+ test #| after mem init |#
  (define PAGE_AVAIL_0 #xcf)
  (define PAGE_AVAIL_0_W #xcf00)
  (define PAGE_AVAIL_1 #xce)
  (define PAGE_AVAIL_1_W #xce00))


;; code
;;   VM_NIL_P :: TOS := (TOS = nil?)
;;   VM_CAR   :: TOS := (car TOS)
;;   VM_CDR   :: TOS := (cdr TOS)
;;   VM_CONS  :: TOS := (TOS . TOS-1)

;; @DC-FUN: VM_NIL_P, group: predicates
;; is tos currently NIL?
(define-vm-function-wol VM_NIL_P
  (list
   (label STACK_EMPTY__VM_NIL_P)
   (label NO_CELL_PAIR_PTR__VM_NIL_P)
          (BRK)

   ;; ----------------------------------------
   (label VM_NIL_P)
          ;; check if stack is not empty

   (label VM_NIL_P__UC) ;; no checks
          (LDA ZP_RT) ;; get tagged byte
          (BNE NOT_NIL__)
          ;; this additional check should not be necessary, since tagged low byte of a non-nil cell-pair-ptr may never be #x02
          ;; (LDA ZP_RT+1) ;; get high byte
          ;; (BNE NOT_NIL__VM_NIL_P) ;; high byte of nil is 0! => branch if != 0
          (JMP WRITE_INT1_TO_RT) ;; true
   (label NOT_NIL__)
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

  (check-equal? (vm-regt->string use-case-nil_p-a-state-after)
                "int $0001"
                "returns true")

  (define use-case-nil_p-b-code
    (list
     (JSR ALLOC_CELL_ARRAY_P0_TO_RT)
     (JSR VM_NIL_P)))

  (define use-case-nil_p-b-state-after
    (run-code-in-test use-case-nil_p-b-code))

  (check-equal? (vm-regt->string use-case-nil_p-b-state-after)
                "int $0000"
                "return false"))

;; @DC-FUN: VM_CAR, group: list
;; replace cell-pair on tos with CAR element
;; no refcount adjustments
(define-vm-function-wol VM_CAR
  (list
   (label RT_IS_NIL__VM_CAR)
   (label NO_PTR__VM_CAR)
          (BRK)
   (label VM_CAR)
          ;; check RT is not nil
          (LDA ZP_RT)
          (BEQ RT_IS_NIL__VM_CAR)

          ;; check RT is a ptr
          (LSR)
          (BCS NO_PTR__VM_CAR)

          ;; A = 0
          (LDY !$02)
          (JMP WRITE_ARR_ATyl_RT_TO_RT)))

;; @DC-FUN: VM_CDR, group: list
;; replace cell-pair on tos with CDR element
;; no refcount adjustments
(define-vm-function-wol VM_CDR
  (list
   (label RT_IS_NIL__VM_CDR)
   (label NO_PTR__VM_CDR)
          (BRK)
   (label VM_CDR)
          ;; check operand present

          ;; check RT is not nil
          (LDA ZP_RT)
          (BEQ RT_IS_NIL__VM_CDR)

          ;; check RT is a cell-pair-ptr
          (LSR)
          (BCS NO_PTR__VM_CDR)

          (LDY !$04)
          (JMP WRITE_ARR_ATyl_RT_TO_RT)))

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
(define-vm-function VM_CxxR
  (list 
          (AND !$1f)
          (STA BRANCH_COMMAND__+1)
   (label BRANCH_COMMAND__)
          (BNE $00)    ;; x00 = caar (no branch), x06 = cadr, x0c = cdar, x12 = cddr
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
(define-vm-function-wol VM_CONS__REFCNTD
  (list
   (label STACK_HAS_LESS_THAN_TWO__)
          (BRK)

   (label VM_CONS__REFCNTD)
          (LDY ZP_CELL_STACK_TOS)
          (CPY !$01) ;; 01 = one element + RT = two elements
          (BMI STACK_HAS_LESS_THAN_TWO__)

   (label VM_CONS__REFCNTD__UC) ;; no checks
          (JSR CP_RT_TO_RP) ;; more compact
          (LDA !$04) ;; payload of size 4
          (JSR ALLOC_CELL_ARRAY_P0_TO_RT)        ;; this cellpair is new
          ;; (JSR INC_REFCNT_RT) ;; already done by allocation
          (JSR WRITE_RP_TO_ARR_AT0_RT)  ;;(JSR WRITE_RP_TO_CELL0_CELLPAIR_RT)    ;; overwrite rt, but rt is put into cell0 of freshly allocated cell-pair => no refcnt mod needed here
          (JMP POP_CELL_EVLSTK_TO_ARR_AT1_RT))) ;; (JMP POP_CELL_EVLSTK_TO_CELL1_RT)    ;; and and write into cell1 => no refcnt mod needed here

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
                  (list "stack holds 2 items"
                        (format "ptr[1] $~a02  (rt)" (format-hex-byte PAGE_AVAIL_0))
                        "ptr NIL"))
 (check-equal? (vm-page->strings use-case-cons-state-after PAGE_AVAIL_0)
                 (list "page-type:      m1 page p0"
                       "previous page:  $00"
                       "slots used:     1"
                       "next free slot: $08"))
  (check-equal? (vm-deref-cell-pair-w->string use-case-cons-state-after (+ PAGE_AVAIL_0_W #x02))
                  "(int $0001 . ptr NIL)"))

(define vm-list-code
  (append VM_CONS__REFCNTD
          VM_CAR
          VM_CDR
          VM_NIL_P
          VM_CxxR))

(module+ test #| vm-lists |#
  (inform-check-equal? (code-len (flatten vm-list-code))
                       93
                       "estimated list code length"))

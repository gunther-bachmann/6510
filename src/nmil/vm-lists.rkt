#lang racket/base

(require "../6510.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader))
(require (only-in racket/list flatten take))

(require (only-in "./vm-memory-manager.rkt" vm-memory-manager))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in racket/port open-output-nowhere))
  (require (only-in "../tools/6510-disassembler.rkt" disassemble-bytes))
  (require (only-in "../tools/6510-debugger.rkt" run-debugger-on)))
(require (only-in "../tools/6510-interpreter.rkt" 6510-load 6510-load-multiple initialize-cpu run-interpreter run-interpreter-on memory-list cpu-state-accumulator))

(provide vm-lists)


;; code
;;   VM_NIL_P :: TOS := (TOS = nil?)
;;   VM_CAR   :: TOS := (car TOS)
;;   VM_CDR   :: TOS := (cdr TOS)
;;   VM_CONS  :: TOS := (TOS . TOS-1)

(define VM_NIL_P
  (list
   (label STACK_EMPTY__VM_NIL_P)
   (label NO_CELL_PAIR_PTR__VM_NIL_P)
          (BRK)

   ;; ----------------------------------------
   (label VM_NIL_P)
          ;; check if stack is not empty
          (LDX ZP_CELL_TOS)
          (BMI STACK_EMPTY__VM_NIL_P)
          ;; check if tos is cell-pair-ptr
          (LDA ZP_CELL0-1,x)
          (AND !$02)
          (BEQ NO_CELL_PAIR_PTR__VM_NIL_P)

   (label VM_NIL_P__UC) ;; no checks
          (LDX ZP_CELL_TOS)
          (LDA ZP_CELL0-1,x) ;; get tagged byte
          (CMP !<TAGGED_NIL) ;;
          (BNE NOT_NIL__VM_NIL_P)
          ;; this additional check should not be necessary, since tagged low byte of a non-nil cell-pair-ptr may never be #x02
          ;; (LDA ZP_CELL0_LOW,x) ;; get high byte
          ;; (BNE NOT_NIL__VM_NIL_P) ;; high byte of nil is 0! => branch if != 0
          (JMP VM_CELL_STACK_WRITE_INT_1_TO_TOS) ;; true
   (label NOT_NIL__VM_NIL_P)
          (JMP VM_CELL_STACK_WRITE_INT_0_TO_TOS) ;; false
          ))

(module+ test #| VM_NIL_P |#
  (define use-case-nil_p-a
    (list
     (JSR VM_INITIALIZE_MEMORY_MANAGER)
     (JSR VM_CELL_STACK_PUSH_NIL)
     (JSR VM_NIL_P)))

  (define use-case-nil_p-a-code
    (append (list (org #xc000))
            use-case-nil_p-a
            (list (BRK))
            vm-lists))

  (define use-case-nil_p-a-state-before (6510-load-multiple (initialize-cpu) (assemble-to-code-list use-case-nil_p-a-code)))
  ;; (run-debugger-on use-case-nil_p-a-state-before)
  (define use-case-nil_p-a-state-after  (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on use-case-nil_p-a-state-before)))
  (check-equal? (memory-list use-case-nil_p-a-state-after #xd9 #xdc)
                '(#x01
                  #x00 #x00 #x01) ;; int 1 = bool true
                "case nil_p: stack holds only int 1")

  (define use-case-nil_p-b
    (list
     (JSR VM_INITIALIZE_MEMORY_MANAGER)
     (JSR VM_ALLOC_CELL_PAIR)
     (JSR VM_CELL_STACK_PUSH_ZP_PTR)
     (JSR VM_NIL_P)))

  (define use-case-nil_p-b-code
    (append (list (org #xc000))
            use-case-nil_p-b
            (list (BRK))
            vm-lists))

  (define use-case-nil_p-b-state-before (6510-load-multiple (initialize-cpu) (assemble-to-code-list use-case-nil_p-b-code)))
  ;; (run-debugger-on use-case-nil_p-b-state-before)
  (define use-case-nil_p-b-state-after  (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on use-case-nil_p-b-state-before)))
  (check-equal? (memory-list use-case-nil_p-b-state-after #xd9 #xdc)
                '(#x01
                  #x00 #x00 #x00) ;; int 0 = bool false
                "case nil_p: stack holds only int 0"))

(define VM_CAR
  (list
   (label STACK_EMPTY__VM_CAR)
   (label TOS_IS_NIL__VM_CAR)
   (label NO_CELL_PAIR_PTR__VM_CAR)
          (BRK)

   ;; ----------------------------------------
   (label VM_CAR)
          ;; check if stack is not empty
          (LDX ZP_CELL_TOS)
          (BMI STACK_EMPTY__VM_CAR)

          ;; check if tos not nil
          (LDA ZP_CELL0-1,x)
          (CMP !$02)
          (BEQ TOS_IS_NIL__VM_CAR)

          ;; check if tos is cell-pair-ptr
          (AND !$02)
          (BEQ NO_CELL_PAIR_PTR__VM_CAR)

   (label VM_CDR__UC) ;; no checks
          (JSR VM_CELL_STACK_WRITE_TOS_TO_ZP_PTR)
          (JMP VM_CELL_STACK_WRITE_CELL0_OF_ZP_PTR_TO_TOS)))

(define VM_CDR
  (list
   (label STACK_EMPTY__VM_CDR)
   (label TOS_IS_NIL__VM_CDR)
   (label NO_CELL_PAIR_PTR__VM_CDR)
          (BRK)

   ;; ----------------------------------------
   (label VM_CDR)
          ;; check if stack is not empty
          (LDX ZP_CELL_TOS)
          (BMI STACK_EMPTY__VM_CDR)

          ;; check if tos not nil
          (LDA ZP_CELL0-1,x)
          (CMP !$02)
          (BEQ TOS_IS_NIL__VM_CDR)

          ;; check if tos is cell-pair-ptr
          (AND !$02)
          (BEQ NO_CELL_PAIR_PTR__VM_CDR)

   (label VM_CDR__UC) ;; no checks
          (JSR VM_CELL_STACK_WRITE_TOS_TO_ZP_PTR)
          (JMP VM_CELL_STACK_WRITE_CELL1_OF_ZP_PTR_TO_TOS)))

;; cons TOS into list at TOS-1
(define VM_CONS
  (list
   (label STACK_HAS_LESS_THAN_TWO__VM_CONS)
          (BRK)

   (label VM_CONS)
          ;; check stack size >= 2
          (LDX ZP_CELL_TOS)
          (CPX !$04) ;; 01 = one element 04 = two elements
          (BMI STACK_HAS_LESS_THAN_TWO__VM_CONS)

   (label VM_CONS__UC) ;; no checks
          (JSR VM_ALLOC_CELL_PAIR)
          (JSR VM_REFCOUNT_INCR_CELL_PAIR)
          (JSR VM_CELL_STACK_WRITE_TOS_TO_CELL1_OF_ZP_PTR)
          (JSR VM_CELL_STACK_POP__NO_GC)
          (JSR VM_CELL_STACK_WRITE_TOS_TO_CELL0_OF_ZP_PTR)
          (JMP VM_CELL_STACK_WRITE_ZP_PTR_TO_TOS)))

(module+ test #| VM_CONS |#
  (define use-case-cons
    (list
     (JSR VM_INITIALIZE_MEMORY_MANAGER)
     (JSR VM_CELL_STACK_PUSH_INT_1)
     (JSR VM_CELL_STACK_PUSH_NIL)
     (JSR VM_CONS)))

  (define use-case-cons-code
    (append (list (org #xc000))
            use-case-cons
            (list (BRK))
            vm-lists))

  (define use-case-cons-state-before (6510-load-multiple (initialize-cpu) (assemble-to-code-list use-case-cons-code)))
  ;; (run-debugger-on use-case-cons-state-before)
  (define use-case-cons-state-after  (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on use-case-cons-state-before)))
  (check-equal? (memory-list use-case-cons-state-after #xfb #xfc)
                '(#x04 #xcd)
                "case cons: zp_ptr -> $cd04")
  (check-equal? (memory-list use-case-cons-state-after #xd9 #xdc)
                '(#x01
                  #x06 #x04 #xcd)
                "case cons: stack holds single element pointing to cd04")
  (check-equal? (memory-list use-case-cons-state-after #xcd00 #xcd07)
                '(#x00 #x01 #x00 #x00
                  #x00 #x01 #x02 #x00) ;; (int1 . nil)
                "case cons: cell-pair is "))

(define vm-lists
  (append VM_CONS
          VM_CAR
          VM_CDR
          VM_NIL_P
          vm-memory-manager))

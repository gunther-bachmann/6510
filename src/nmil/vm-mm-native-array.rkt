#lang racket/base

#|

memory management for native arrays

|#

(require "../6510.rkt")
(require (only-in "./vm-memory-map.rkt"
                  TAGGED_NIL
                  TAG_BYTE_NATIVE_ARRAY
                  ZP_RA
                  VM_MEMORY_MANAGEMENT_CONSTANTS))
(require (only-in "../ast/6510-resolver.rkt"
                  add-label-suffix
                  replace-labels))

(provide ALLOC_NATARR_TO_RA)


(module+ test
  (require (only-in racket/list make-list))
  (require  "../6510-test-utils.rkt")
  (require "./vm-memory-manager-test-utils.rkt")
  (require (only-in "../tools/6510-interpreter.rkt" peek memory-list))
  (require (only-in "../util.rkt" format-hex-byte format-hex-word))
  (require (only-in "./vm-inspector-utils.rkt"
                    vm-cell-at-nil?
                    vm-rega->string
                    vm-regt->string
                    vm-cell-pair-free-tree->string
                    vm-deref-cell-pair-w->string
                    vm-deref-cell-w->string
                    vm-refcount-cell-pair-ptr
                    vm-refcount-cell-ptr
                    vm-regp->string
                    vm-page->strings))
  (require (only-in "./vm-mm-register-functions.rkt" WRITE_NIL_TO_RP))
  (require (only-in "./vm-mm-pages.rkt"
                    ALLOC_PAGE_TO_X
                    VM_PAGE_SLOT_DATA
                    VM_INITIAL_MM_REGS
                    VM_INITIALIZE_MEMORY_MANAGER))
  (require (only-in "./vm-mm-m1-slots.rkt"
                    ALLOC_M1_SLOT_TO_RA
                    INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX
                    VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS
                    ADD_M1_SLOT_RZ_TO_PFL
                    DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A
                    PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ))

  (define PAGE_AVAIL_0 #x9a)      ;; high byte of first page available for allocation
  (define PAGE_AVAIL_0_W #x9a00)  ;; word (absolute address) of first page available
  (define PAGE_AVAIL_1 #x99)      ;; high byte of second page available for allocation
  (define PAGE_AVAIL_1_W #x9900) ;; word (absolute address) of second page available

  (define test-runtime
    (append
     ALLOC_PAGE_TO_X
     ALLOC_NATARR_TO_RA

     ALLOC_M1_SLOT_TO_RA
     VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS
     ADD_M1_SLOT_RZ_TO_PFL
     DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A
     PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ
     INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX
     VM_INITIALIZE_MEMORY_MANAGER
     VM_MEMORY_MANAGEMENT_CONSTANTS
     (list (label INIT_CELLSTACK_PAGE_X) (RTS))
     (list (org #xcec0))
     VM_INITIAL_MM_REGS
     (list (org #xcf00))
     VM_PAGE_SLOT_DATA))
)

;; allocate an array of bytes (native) (also useful for strings)
;; overwrite RA no matter whether RA was filled
;; input:  A = number of bytes (1..81)
;; usage:  A, X, Y, RA
;; output: RA -> points to an allocated array (not initialized)
;; funcs:
;;   ALLOC_M1_SLOT_TO_RA
(define ALLOC_NATARR_TO_RA
  (list
   (label ALLOC_NATARR_TO_RA)
          (PHA)
          (CLC)
          (ADC !$02) ;; add to total slot size

          (JSR ALLOC_M1_SLOT_TO_RA)

          ;; write header cell
          (LDY !$00)
          (LDA !TAG_BYTE_NATIVE_ARRAY)
          (STA (ZP_RA),y) ;; store tag byte

          (INY)
          (PLA)
          (STA (ZP_RA),y) ;; store number of array elements

   ;; no initializing with 0 (might be useful for debugging, though)
   ;;        (TAX) ;; use number of array elements as loop counter

   ;;        ;; initialize slots/array with 0
   ;;        (LDA !$00)
   ;; (label LOOP_INIT__VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)
   ;;        (INY)
   ;;        (STA (ZP_RA),y)
   ;;        (DEX)
   ;;        (BNE LOOP_INIT__VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)

          (RTS)))

(module+ test #| vm_allocate_native_array |#
  (define test-alloc-native-array-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDA !$10)
     (JSR ALLOC_NATARR_TO_RA)))

  (check-equal? (vm-page->strings test-alloc-native-array-state-after PAGE_AVAIL_0)
                (list
                 "page-type:      m1 page p2"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $2e"))
  (check-equal? (memory-list test-alloc-native-array-state-after ZP_RA (add1 ZP_RA))
                (list #x10 PAGE_AVAIL_0))
  (check-equal? (memory-list test-alloc-native-array-state-after (+ PAGE_AVAIL_0_W #x10) (+ PAGE_AVAIL_0_W #x11))
                (list TAG_BYTE_NATIVE_ARRAY #x10)))

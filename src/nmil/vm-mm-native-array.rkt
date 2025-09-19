#lang racket/base

#|

memory management for native arrays

|#

(require "../6510.rkt"
         (only-in "../ast/6510-resolver.rkt"
                  add-label-suffix
                  replace-labels)
         (only-in "./vm-memory-map.rkt"
                  TAGGED_NIL
                  TAG_BYTE_NATIVE_ARRAY
                  TAG_BYTE_BYTE_CELL
                  ZP_RA
                  ZP_RAI
                  ZP_RB
                  ZP_RBI
                  VM_MEMORY_MANAGEMENT_CONSTANTS))

(provide ALLOC_NATARR_TO_RA
         ALLOC_NATARR_TO_RB
         WRITE_RT_TO_NATARR_RAI
         POP_TO_NATARR_RAI
         WRITE_NATARR_RAI_TO_RT
         PUSH_NATARR_RAI                ;; just ref, no code (included in WRITE_NATARR_RAI_TO_RT)
         CP_NATARR_RA_TO_RB
         CP_NATARR_RANGE_RA_TO_RB)

(module+ test
  (require "../6510-test-utils.rkt"
           (only-in "../tools/6510-interpreter.rkt"
                    peek
                    memory-list)
           (only-in "./vm-inspector-utils.rkt"
                    vm-cell-at-nil?
                    vm-rega->string
                    vm-regt->string
                    vm-stack->strings
                    vm-cell-pair-free-tree->string
                    vm-deref-cell-pair-w->string
                    vm-deref-cell-w->string
                    vm-refcount-cell-pair-ptr
                    vm-refcount-cell-ptr
                    vm-regp->string
                    vm-page->strings)
           "./vm-memory-manager-test-utils.rkt"
           (only-in "./vm-mm-cell-stack.rkt"
                    PUSH_XA_TO_EVLSTK
                    PUSH_RT_TO_EVLSTK
                    POP_CELL_EVLSTK_TO_RT)
           (only-in "./vm-mm-m1-slots.rkt"
                    ALLOC_M1_SLOT_TO_RA
                    ALLOC_M1_SLOT_TO_RB
                    INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX
                    VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS
                    ADD_M1_SLOT_RZ_TO_PFL
                    DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A
                    PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ)
           (only-in "./vm-mm-pages.rkt"
                    ALLOC_PAGE_TO_X
                    VM_PAGE_SLOT_DATA
                    VM_INITIAL_MM_REGS
                    VM_INITIALIZE_MEMORY_MANAGER)
           (only-in "./vm-mm-register-functions.rkt"
                    CP_RA_TO_RB
                    SWAP_RA_RB))

  (define PAGE_AVAIL_0 #x8d)      ;; high byte of first page available for allocation
  (define PAGE_AVAIL_0_W #x8d00)  ;; word (absolute address) of first page available
  (define PAGE_AVAIL_1 #x8c)      ;; high byte of second page available for allocation
  (define PAGE_AVAIL_1_W #x8c00) ;; word (absolute address) of second page available

  (define test-runtime
    (append
     ALLOC_NATARR_TO_RA
     ALLOC_NATARR_TO_RB
     WRITE_RT_TO_NATARR_RAI
     POP_TO_NATARR_RAI
     WRITE_NATARR_RAI_TO_RT
     CP_NATARR_RA_TO_RB
     CP_NATARR_RANGE_RA_TO_RB

     CP_RA_TO_RB
     SWAP_RA_RB
     PUSH_XA_TO_EVLSTK
     POP_CELL_EVLSTK_TO_RT
     PUSH_RT_TO_EVLSTK
     ALLOC_PAGE_TO_X
     ALLOC_M1_SLOT_TO_RA
     ALLOC_M1_SLOT_TO_RB
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
     VM_PAGE_SLOT_DATA)))

;; @DC-FUN: ALLOC_NATARR_TO_RA, group: native_array
;; allocate an array of bytes (native) (also useful for strings)
;; overwrite RA no matter whether RA was filled
;; input:  A = number of bytes (1..81)
;; usage:  A, X, Y, RA
;; output: RA -> points to an allocated array (not initialized)
;;         RAI = 0
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
          (STY ZP_RAI)
          (LDA !TAG_BYTE_NATIVE_ARRAY)
          (STA (ZP_RA),y) ;; store tag byte

          (INY)
          (PLA)
          (STA (ZP_RA),y) ;; store number of array elements

          ;; no initializing with 0 (might be useful for debugging, though)

          (INC ZP_RA)
          (INC ZP_RA) ;; ensure RA points to first entry of array
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
                (list #x12 PAGE_AVAIL_0)
                "points two byte behind slot address, at arrays first entry")
  (check-equal? (peek test-alloc-native-array-state-after ZP_RAI)
                0
                "RAI is initialized to 0")
  (check-equal? (memory-list test-alloc-native-array-state-after (+ PAGE_AVAIL_0_W #x10) (+ PAGE_AVAIL_0_W #x11))
                (list TAG_BYTE_NATIVE_ARRAY #x10)
                "the native array in this slot starts with its appropriate tag_byte and its length #x10"))

(define ALLOC_NATARR_TO_RB
  (list
   (label ALLOC_NATARR_TO_RB)
          (PHA)
          (CLC)
          (ADC !$02) ;; add to total slot size

          (JSR ALLOC_M1_SLOT_TO_RB)

          ;; write header cell
          (LDY !$00)
          (STY ZP_RBI)
          (LDA !TAG_BYTE_NATIVE_ARRAY)
          (STA (ZP_RB),y) ;; store tag byte

          (INY)
          (PLA)
          (STA (ZP_RB),y) ;; store number of array elements

          ;; no initializing with 0 (might be useful for debugging, though)

          (INC ZP_RB)
          (INC ZP_RB) ;; ensure RA points to first entry of array
          (RTS)))

;; @DC-FUN: WRITE_RT_TO_NATARR_RAI, group: native_array
;; write byte value of rt (high byte) into nat array
;; referenced by RA at index RAI and increment RAI
;; input:  RT, RA, RAi
;; usage:  A, Y
;; output: (RA),RAi++ := (RT+1)
(define WRITE_RT_TO_NATARR_RAI
  (list
   (label WRITE_RT_TO_NATARR_RAI__CHECKED)
          (LDA ZP_RT)
          (CMP !TAG_BYTE_BYTE_CELL)
          (BEQ POP_TO_NATARR_RAI)
          (BRK) ;; error: this is no byte on the cell stack

   (label WRITE_RT_TO_NATARR_RAI)
          (LDA ZP_RT+1)
          (LDY ZP_RAI)
          (STA (ZP_RA),y)
          (INC ZP_RAI)
          (RTS)))

(module+ test #| write_rt_to_natarr_rai |#
  (define write-rt-to-natarr-rai-t0
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDA !$04)                         ;; alocates profile for 4 bytes content
     (JSR ALLOC_NATARR_TO_RA)
     (LDX !$58)
     (JSR PUSH_BYTE_X_TO_EVLSTK)
     (JSR WRITE_RT_TO_NATARR_RAI)
     (LDA !$62)
     (STA ZP_RT+1)
     (JSR WRITE_RT_TO_NATARR_RAI)))

  (check-equal? (vm-page->strings write-rt-to-natarr-rai-t0 PAGE_AVAIL_0)
                (list
                 "page-type:      m1 page p0"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $0e"))
  (check-equal? (vm-stack->strings write-rt-to-natarr-rai-t0)
                (list "stack holds 1 item"
                      "byte $62  (rt)"))
  (check-equal? (memory-list write-rt-to-natarr-rai-t0 (+ PAGE_AVAIL_0_W #x04) (+ PAGE_AVAIL_0_W #x07))
                (list TAG_BYTE_NATIVE_ARRAY #x04 #x58 #x62)
                "native array is made of tag-byte, length=4, @0 = #x58, @1 = #x62")
  (check-equal? (peek write-rt-to-natarr-rai-t0 ZP_RAI)
                2
                "index for array a points to the next"))

;; pop byte value of rt (high byte) into nat array
;; referenced by RA at index RAI and increment RAI
;; input:  RT, RA, RAi, EVLSTK
;; usage:  A, Y
;; output: (RA),RAi++ := (RT+1), RT<<EVLSTK<<
(define POP_TO_NATARR_RAI
  (list
   (label POP_TO_NATARR_RAI__CHECKED)
          (LDA ZP_RT)
          (CMP !TAG_BYTE_BYTE_CELL)
          (BEQ POP_TO_NATARR_RAI)
          (BRK) ;; error: this is no byte on the cell stack

   (label POP_TO_NATARR_RAI)
          (LDA ZP_RT+1)
          (LDY ZP_RAI)
          (STA (ZP_RA),y)
          (INC ZP_RAI)
          (JMP POP_CELL_EVLSTK_TO_RT)))

(module+ test #| pop-to-natarra-rai |#
  (define pop-to-natarra-rai-t0
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDA !$04)                         ;; alocates profile for 4 bytes content
     (JSR ALLOC_NATARR_TO_RA)
     (LDX !$58)
     (JSR PUSH_BYTE_X_TO_EVLSTK)
     (JSR POP_TO_NATARR_RAI)
     ))

  (check-equal? (vm-page->strings pop-to-natarra-rai-t0 PAGE_AVAIL_0)
                (list
                 "page-type:      m1 page p0"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $0e"))
  (check-equal? (vm-stack->strings pop-to-natarra-rai-t0)
                (list "stack is empty"))
  (check-equal? (memory-list pop-to-natarra-rai-t0 (+ PAGE_AVAIL_0_W #x04) (+ PAGE_AVAIL_0_W #x06))
                (list TAG_BYTE_NATIVE_ARRAY #x04 #x58)
                "native array is made of tag-byte, length=4, @0 = #x58")
  (check-equal? (peek pop-to-natarra-rai-t0 ZP_RAI)
                1
                "index for array a points to the next"))

;; @DC-FUN: WRITE_NATARR_RAI_TO_RT, group: native_array
;; write/push byte referenced by RA at index RAI
;; onto the eval stack and increment RAI
;; input:  RT, RA, RAi, EVLSTK
;; usage:  A, Y
;; output: EVLSTK<<RT, RT := (RA),RAi++,
(define PUSH_NATARR_RAI #t)
(define WRITE_NATARR_RAI_TO_RT
  (list
   (label PUSH_NATARR_RAI)
          (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
   (label WRITE_NATARR_RAI_TO_RT)
          (LDA !TAG_BYTE_BYTE_CELL)
          (STA ZP_RT)
          (LDY ZP_RAI)
          (LDA (ZP_RA),y)
          (STA ZP_RT+1)
          (INC ZP_RAI)
          (RTS)))

(module+ test #| push-natarr-rai |#

  (define push-natarr-rai-t0
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDA !$04)
     (JSR ALLOC_NATARR_TO_RA)
     (LDX !$32)
     (JSR PUSH_BYTE_X_TO_EVLSTK)
     (JSR POP_TO_NATARR_RAI)
     (DEC ZP_RAI)
     (JSR PUSH_NATARR_RAI)))

  (check-equal? (vm-page->strings push-natarr-rai-t0 PAGE_AVAIL_0)
                (list
                 "page-type:      m1 page p0"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $0e"))
  (check-equal? (vm-stack->strings push-natarr-rai-t0)
                (list "stack holds 1 item"
                      "byte $32  (rt)"))
  (check-equal? (memory-list pop-to-natarra-rai-t0 (+ PAGE_AVAIL_0_W #x04) (+ PAGE_AVAIL_0_W #x06))
                (list TAG_BYTE_NATIVE_ARRAY #x04 #x58)
                "native array is made of tag-byte, length=4, @0 = #x32")
  (check-equal? (peek pop-to-natarra-rai-t0 ZP_RAI)
                1
                "index for array a points to the next"))

;; @DC-FUN: CP_NATARR_RA_TO_RB, group: native_array
;; copy the whole lenght of the array ra into the array rb (same position)
;; input:  RA, RB
;; usage:  A, Y
;; output: start of RB is equal to RA (length of RB needs to >= RA)
(define CP_NATARR_RA_TO_RB
  (add-label-suffix
   "__" "CP_NATARR_RA_TO_RB"
   (list
    (label CP_NATARR_RA_TO_RB)
           (LDY !$00)
           (DEC ZP_RA)
           (LDA (ZP_RA),y)       ;; A = length of array
           (INC ZP_RA)
           (TAY)
           (DEY)

    (label LOOP__)
           (LDA (ZP_RA),y)
           (STA (ZP_RB),y)
           (DEY)
           (BPL LOOP__)
           (RTS))))

(module+ test #| cp-natarr-ra-to-rb |#
  (define cp-natarr-ra-to-rb-t0
    (compact-run-code-in-test-
     #:runtime-code test-runtime
            (LDA !$04)
            (JSR ALLOC_NATARR_TO_RA)
            (LDY ZP_RAI)
     (label LOOP__)
            (TYA)
            (STA (ZP_RA),y)
            (INY)
            (CPY !$04)
            (BNE LOOP__)
            (LDA !$08)
            (JSR ALLOC_NATARR_TO_RB)
            (JSR CP_NATARR_RA_TO_RB)))

  (check-equal? (memory-list cp-natarr-ra-to-rb-t0 ZP_RA (+ 1 ZP_RA))
                (list #x06 PAGE_AVAIL_0)
                "ra is allocated first on page 0")
  (check-equal? (vm-page->strings cp-natarr-ra-to-rb-t0 PAGE_AVAIL_0)
                (list "page-type:      m1 page p0"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $0e")
                "page 0 is of profile 0, since length is 4")
  (check-equal? (memory-list cp-natarr-ra-to-rb-t0 ZP_RB (+ 1 ZP_RB))
                (list #x06 PAGE_AVAIL_1)
                "rb is allocated first on page 1")
  (check-equal? (vm-page->strings cp-natarr-ra-to-rb-t0 PAGE_AVAIL_1)
                (list "page-type:      m1 page p1"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $16")
                "page 1 is of profile 1, since length is 8")
  (check-equal? (memory-list cp-natarr-ra-to-rb-t0 (+ PAGE_AVAIL_0_W 4) (+ PAGE_AVAIL_0_W 9))
                (list TAG_BYTE_NATIVE_ARRAY 4 0 1 2 3)
                "ra points to page0, containing a native array of len 4 filled with 0 1 2 3")
  (check-equal? (memory-list cp-natarr-ra-to-rb-t0 (+ PAGE_AVAIL_1_W 4) (+ PAGE_AVAIL_1_W 9))
                (list TAG_BYTE_NATIVE_ARRAY 8 0 1 2 3)
                "rb points to page1, containing a native array of len 8 (first 4 bytes) filled with 0 1 2 3"))

;; @DC-FUN: CP_NATARR_RANGE_RA_TO_RB, group: native_array
;; copy from nat array ra, range index x..y to the start of array rb
;; input:  RA, RB, X, Y
;; usage:  A, X, Y
;; output: start of RB is equal to RA@X..Y (0-indexed), RB needs to have sufficient size (>=Y-X)
(define CP_NATARR_RANGE_RA_TO_RB
  (add-label-suffix
   "__" "CP_NATARR_RANGE_RA_TO_RB"
   (list
    (label CP_NATARR_RANGE_RA_TO_RB)
           (SEC)
           (STX ZP_TEMP)
           (LDA ZP_RB)
           (SBC ZP_TEMP)
           (STA ZP_RB)           ;; set start of rb such that range is copy to start of rb

           (STX LOOP_COMPARE__+1)

    (label LOOP__)
           (LDA (ZP_RA),y)
           (STA (ZP_RB),y)
           (DEY)
    (label LOOP_COMPARE__)
           (CPY !$ff)
           (BPL LOOP__)

           (TXA)
           (CLC)
           (ADC ZP_RB)
           (STA ZP_RB) ;; restore original ZP_RB
           (RTS))))

(module+ test #| cp-natarr-range-ra-to-rb |#
  (define cp-natarr-range-ra-to-rb-t0
    (compact-run-code-in-test-
     ;; #:debug #t
     #:runtime-code test-runtime
            (LDA !$04)
            (JSR ALLOC_NATARR_TO_RA)
            (LDY ZP_RAI)
     (label LOOP__)
            (TYA)
            (STA (ZP_RA),y)
            (INY)
            (CPY !$04)
            (BNE LOOP__)
            (LDA !$08)
            (JSR ALLOC_NATARR_TO_RB)

            (LDX !$02) ;; not including 1
            (LDY !$03)
            (JSR CP_NATARR_RANGE_RA_TO_RB)))


  (check-equal? (memory-list cp-natarr-range-ra-to-rb-t0 ZP_RA (+ 1 ZP_RA))
                (list #x06 PAGE_AVAIL_0)
                "ra is allocated first on page 0")
  (check-equal? (vm-page->strings cp-natarr-range-ra-to-rb-t0 PAGE_AVAIL_0)
                (list "page-type:      m1 page p0"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $0e")
                "page 0 is of profile 0, since length is 4")
  (check-equal? (memory-list cp-natarr-range-ra-to-rb-t0 ZP_RB (+ 1 ZP_RB))
                (list #x06 PAGE_AVAIL_1)
                "rb is allocated first on page 1")
  (check-equal? (vm-page->strings cp-natarr-range-ra-to-rb-t0 PAGE_AVAIL_1)
                (list "page-type:      m1 page p1"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $16")
                "page 1 is of profile 1, since length is 8")
  (check-equal? (memory-list cp-natarr-range-ra-to-rb-t0 (+ PAGE_AVAIL_0_W 4) (+ PAGE_AVAIL_0_W 9))
                (list TAG_BYTE_NATIVE_ARRAY 4 0 1 2 3)
                "ra points to page0, containing a native array of len 4 filled with 0 1 2 3")
  (check-equal? (memory-list cp-natarr-range-ra-to-rb-t0 (+ PAGE_AVAIL_1_W 4) (+ PAGE_AVAIL_1_W 7))
                (list TAG_BYTE_NATIVE_ARRAY 8 2 3)
                "rb points to page1, containing a native array of len 8 (first 4 bytes) filled with 2 3"))

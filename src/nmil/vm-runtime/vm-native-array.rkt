#lang racket/base

(provide ALLOC_NATARR_TO_RA
         ALLOC_NATARR_TO_RB
         WRITE_RT_TO_NATARR_RAI
         POP_TO_NATARR_RAI
         WRITE_NATARR_RAI_TO_RT
         PUSH_NATARR_RAI                ;; just ref, no code (included in WRITE_NATARR_RAI_TO_RT)
         CP_NATARR_RA_TO_RB
         CP_NATARR_RANGE_RA_TO_RB
         vm-native-array-code)

#|

 memory management for native arrays

 |#

(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
         (only-in "./vm-m1-slots.rkt"
                  ALLOC_M1_SLOT_TO_RA)
         (only-in "./vm-memory-map.rkt"
                  TAGGED_NIL
                  TAG_BYTE_NATIVE_ARRAY
                  TAG_BYTE_BYTE_CELL
                  ZP_RA
                  ZP_RAI
                  ZP_RB
                  ZP_RBI
                  VM_MEMORY_MANAGEMENT_CONSTANTS))

(module+ test
  (require "../../6510-test-utils.rkt"
           (only-in "../../ast/6510-relocator.rkt"
                    code-len)
           (only-in "../../tools/6510-interpreter.rkt"
                    peek
                    memory-list)
           (only-in "../vm-inspector-utils.rkt"
                    vm-stack->strings
                    vm-page->strings)
           (only-in "./vm-cell-stack.rkt"
                    PUSH_XA_TO_EVLSTK
                    PUSH_RT_TO_EVLSTK_TAIL
                    POP_EVLSTK_TAIL_TO_RT)
           (only-in "./vm-cell-stack.rkt"
                    vm-cell-stack-code)
           (only-in "./vm-m1-slots.rkt"
                    ALLOC_M1_SLOT_TO_RA
                    ALLOC_M1_SLOT_TO_RB
                    vm-m1-slot-code)
           "./vm-memory-manager-test-utils.rkt"
           (only-in "./vm-pages.rkt"
                    vm-pages-code)
           (only-in "./vm-register-functions.rkt"
                    vm-register-functions-code
                    CP_RA_TO_RB
                    CP_RA_TO_RT
                    SWAP_RA_RB))

  (define PAGE_AVAIL_0 #xcf)      ;; high byte of first page available for allocation
  (define PAGE_AVAIL_0_W #xcf00)  ;; word (absolute address) of first page available
  (define PAGE_AVAIL_1 #xce)      ;; high byte of second page available for allocation
  (define PAGE_AVAIL_1_W #xce00) ;; word (absolute address) of second page available
  (define PAGE_AVAIL_2 #xcd)      ;; high byte of second page available for allocation
  (define PAGE_AVAIL_2_W #xcd00) ;; word (absolute address) of second page available

  (define test-runtime
    [append
     ALLOC_NATARR_TO_RA
     ALLOC_NATARR_TO_RB
     WRITE_RT_TO_NATARR_RAI
     POP_TO_NATARR_RAI
     WRITE_NATARR_RAI_TO_RT
     CP_NATARR_RA_TO_RB
     CP_NATARR_RANGE_RA_TO_RB

     vm-register-functions-code
     VM_MEMORY_MANAGEMENT_CONSTANTS
     vm-pages-code
     vm-m1-slot-code
     vm-cell-stack-code]))

;; @DC-FUN: ALLOC_NATARR_TO_RA, group: native_array
;; allocate an array of bytes (native) (also useful for strings)
;; overwrite RA no matter whether RA was filled
;; input:  A = number of bytes (1..81)
;; usage:  A, X, Y, RA
;; output: RA -> points to an allocated array (not initialized)
;;         RAI = 0
;; funcs:
;;   ALLOC_M1_SLOT_TO_RA
(define-vm-function ALLOC_NATARR_TO_RA
  (list
          (PHA)
          (JSR ALLOC_M1_SLOT_TO_RA)

          ;; write header cell
          (LDY !$00)
          (LDA !$01)
          (STA (ZP_RA),y) ;; recount initialized to 1
          (PLA)
          (ORA !$80) ;; set highest bit
          (INY)
          (STA (ZP_RA),y) ;; store number of array elements

          (INY)
          (STY ZP_RAI) ;; rai = 2

          (RTS)))

(module+ test #| vm_allocate_native_array |#
  (define test-alloc-native-array-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     #:init-label "VM_INITIALIZE_PAGE_MEMORY_MANAGER_N20"
     #:debug #f
     (LDA !$10)
     (JSR ALLOC_NATARR_TO_RA)))

  (check-equal? (vm-page->strings test-alloc-native-array-state-after PAGE_AVAIL_0)
                (list
                 "page-type:      m1 page p3"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $1a"))
  (check-equal? (memory-list test-alloc-native-array-state-after ZP_RA (add1 ZP_RA))
                (list #x02 PAGE_AVAIL_0)
                "points at slot address, arrays first entry is at +2")
  (check-equal? (peek test-alloc-native-array-state-after ZP_RAI)
                2
                "RAI is initialized to 0")
  (check-equal? (memory-list test-alloc-native-array-state-after (+ PAGE_AVAIL_0_W #x03))
                (list #x90)
                "native array has highest bit set, following 7 bits mark the actual length of the array"))

(define-vm-function ALLOC_NATARR_TO_RB
  (list
          (PHA)
          (JSR ALLOC_M1_SLOT_TO_RB)

          ;; write header cell
          (LDY !$00)
          (LDA !$01)
          (STA (ZP_RB),y) ;; recount initialized to 1
          (PLA)
          (ORA !$80) ;; set highest bit
          (INY)
          (STA (ZP_RB),y) ;; store number of array elements

          (INY)
          (STY ZP_RBI) ;; rbi = 2

          (RTS)))

;; @DC-FUN: WRITE_RT_TO_NATARR_RAI, group: native_array
;; write byte value of rt (high byte) into nat array
;; referenced by RA at index RAI and increment RAI
;; input:  RT, RA, RAi
;; usage:  A, Y
;; output: (RA),RAi++ := (RT+1)
(define-vm-function-wol WRITE_RT_TO_NATARR_RAI
  (list
   (label WRITE_RT_TO_NATARR_RAI__CHECKED)
          (LDA ZP_RT)
          (CMP !TAG_BYTE_BYTE_CELL)
          (BEQ WRITE_RT_TO_NATARR_RAI)
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
     #:init-label "VM_INITIALIZE_PAGE_MEMORY_MANAGER_N20"
     (JSR INIT_EVLSTK_TAIL)

     (LDA !$04)                         ;; alocates profile for 4 bytes content
     (JSR ALLOC_NATARR_TO_RA)
     (LDX !$58)
     (JSR PUSH_BYTE_X_TO_EVLSTK)
     (JSR WRITE_RT_TO_NATARR_RAI)
     (LDA !$62)
     (STA ZP_RT+1)
     (JSR WRITE_RT_TO_NATARR_RAI)))

  (check-equal? (vm-page->strings write-rt-to-natarr-rai-t0 PAGE_AVAIL_2)
                (list
                 "page-type:      m1 page p0"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $08"))
  (check-equal? (vm-stack->strings write-rt-to-natarr-rai-t0)
                (list "stack holds 2 items"
                      "byte $62  (rt)"
                      "ptr NIL"))
  (check-equal? (memory-list write-rt-to-natarr-rai-t0 (+ PAGE_AVAIL_2_W #x02) (+ PAGE_AVAIL_2_W #x05))
                (list #x01 #x84 #x58 #x62)
                "refcount, length=4 (+highest bit set), @0 = #x58, @1 = #x62")
  (check-equal? (peek write-rt-to-natarr-rai-t0 ZP_RAI)
                4
                "index for array a points to the next"))

;; pop byte value of rt (high byte) into nat array
;; referenced by RA at index RAI and increment RAI
;; input:  RT, RA, RAi, EVLSTK
;; usage:  A, Y
;; output: (RA),RAi++ := (RT+1), RT<<EVLSTK<<
(define-vm-function-wol POP_TO_NATARR_RAI
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
          (JMP POP_EVLSTK_TAIL_TO_RT)))

(module+ test #| pop-to-natarra-rai |#
  (define pop-to-natarra-rai-t0
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     #:init-label "VM_INITIALIZE_PAGE_MEMORY_MANAGER_N20"
     (JSR INIT_EVLSTK_TAIL)

     (LDA !$04)                         ;; alocates profile for 4 bytes content
     (JSR ALLOC_NATARR_TO_RA)
     (LDX !$58)
     (JSR PUSH_BYTE_X_TO_EVLSTK)
     (JSR POP_TO_NATARR_RAI)
     ))

  (check-equal? (vm-page->strings pop-to-natarra-rai-t0 PAGE_AVAIL_2)
                (list
                 "page-type:      m1 page p0"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $08"))
  (check-equal? (vm-stack->strings pop-to-natarra-rai-t0)
                (list "stack is empty or tos=nil"))
  (check-equal? (memory-list pop-to-natarra-rai-t0 (+ PAGE_AVAIL_2_W #x02) (+ PAGE_AVAIL_2_W #x04))
                (list #x01 #x84 #x58)
                "native array is made of refcount, length=4, @0 = #x58")
  (check-equal? (peek pop-to-natarra-rai-t0 ZP_RAI)
                3
                "index for array a points to the next"))

;; @DC-FUN: WRITE_NATARR_RAI_TO_RT, group: native_array
;; write/push byte referenced by RA at index RAI
;; onto the eval stack and increment RAI
;; input:  RT, RA, RAi, EVLSTK
;; usage:  A, Y
;; output: EVLSTK<<RT, RT := (RA),RAi++,
(define PUSH_NATARR_RAI '())
(define-vm-function-wol WRITE_NATARR_RAI_TO_RT
  (list
   (label PUSH_NATARR_RAI)
          (JSR PUSH_RT_TO_EVLSTK_TAIL)
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
     #:init-label "VM_INITIALIZE_PAGE_MEMORY_MANAGER_N20"
     (JSR INIT_EVLSTK_TAIL)

     (LDA !$04)
     (JSR ALLOC_NATARR_TO_RA)
     (LDX !$32)
     (JSR PUSH_BYTE_X_TO_EVLSTK)
     (JSR POP_TO_NATARR_RAI)
     (DEC ZP_RAI)
     (JSR PUSH_NATARR_RAI)))

  (check-equal? (vm-page->strings push-natarr-rai-t0 PAGE_AVAIL_2)
                (list
                 "page-type:      m1 page p0"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $08"))
  (check-equal? (vm-stack->strings push-natarr-rai-t0)
                (list "stack holds 2 items"
                      "byte $32  (rt)"
                      "ptr NIL"))
  (check-equal? (memory-list pop-to-natarra-rai-t0 (+ PAGE_AVAIL_2_W #x02) (+ PAGE_AVAIL_2_W #x04))
                (list #x01 #x84 #x58)
                "native array is made of refcount, length=4 (+high bit set), @0 = #x32")
  (check-equal? (peek pop-to-natarra-rai-t0 ZP_RAI)
                3
                "index for array a points to the next"))

;; @DC-FUN: CP_NATARR_RA_TO_RB, group: native_array
;; copy the whole lenght of the array ra into the array rb (same position)
;; input:  RA, RB
;; usage:  A, Y
;; output: start of RB is equal to RA (length of RB needs to >= RA)
(define-vm-function CP_NATARR_RA_TO_RB
   (list
           (LDY !$01)
           (LDA (ZP_RA),y)
           (AND !$7f)           ;; A = length of array
           (TAY)
           (INY)

    (label LOOP__)
           (LDA (ZP_RA),y)
           (STA (ZP_RB),y)
           (DEY)
           (CMP !$01)
           (BNE LOOP__)
           (RTS)))

(module+ test #| cp-natarr-ra-to-rb |#
  (define cp-natarr-ra-to-rb-t0
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     #:init-label "VM_INITIALIZE_PAGE_MEMORY_MANAGER_N20"
            (LDA !$04)
            (JSR ALLOC_NATARR_TO_RA)
            (LDY ZP_RAI)
     (label LOOP__)
            (TYA)
            (SEC)
            (SBC !$02)
            (STA (ZP_RA),y)
            (INY)
            (CPY !$06)
            (BNE LOOP__)
            (LDA !$08)
            (JSR ALLOC_NATARR_TO_RB)
            (JSR CP_NATARR_RA_TO_RB)))

  (check-equal? (memory-list cp-natarr-ra-to-rb-t0 ZP_RA (+ 1 ZP_RA))
                (list #x02 PAGE_AVAIL_0)
                "ra is allocated first on page 0")
  (check-equal? (vm-page->strings cp-natarr-ra-to-rb-t0 PAGE_AVAIL_0)
                (list "page-type:      m1 page p0"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page 0 is of profile 0, since length is 4")
  (check-equal? (memory-list cp-natarr-ra-to-rb-t0 ZP_RB (+ 1 ZP_RB))
                (list #x02 PAGE_AVAIL_1)
                "rb is allocated first on page 1")
  (check-equal? (vm-page->strings cp-natarr-ra-to-rb-t0 PAGE_AVAIL_1)
                (list "page-type:      m1 page p2"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $0e")
                "page 1 is of profile 2, since payload length is 8")
  (check-equal? (memory-list cp-natarr-ra-to-rb-t0 (+ PAGE_AVAIL_0_W 2) (+ PAGE_AVAIL_0_W 7))
                (list #x01 #x84 0 1 2 3)
                "ra points to page0, containing a native array of len 4 filled with 0 1 2 3")
  (check-equal? (memory-list cp-natarr-ra-to-rb-t0 (+ PAGE_AVAIL_1_W 2) (+ PAGE_AVAIL_1_W 7))
                (list #x01 #x88 0 1 2 3)
                "rb points to page1, containing a native array of len 8 (first 4 bytes) filled with 0 1 2 3"))

;; @DC-FUN: CP_NATARR_RANGE_RA_TO_RB, group: native_array
;; copy from nat array ra, range index x..y to the start of array rb
;; input:  RA, RB, X, Y
;; usage:  A, X, Y
;; output: start of RB is equal to RA@X..Y (0-indexed), RB needs to have sufficient size (>=Y-X)
(define-vm-function CP_NATARR_RANGE_RA_TO_RB
   (list
           (SEC)
           (STX ZP_TEMP)
           (LDA ZP_RB)
           (STA ZP_TEMP+1)
           (SBC ZP_TEMP)
           (STA ZP_RB)           ;; set start of rb such that range is copy to start of rb

           (INX)
           (INX)
           (STX LOOP_COMPARE__+1)
           (INY)
           (INY)

    (label LOOP__)
           (LDA (ZP_RA),y)
           (STA (ZP_RB),y)
           (DEY)
    (label LOOP_COMPARE__)
           (CPY !$ff)
           (BPL LOOP__)

           (LDA ZP_TEMP+1)
           (STA ZP_RB) ;; restore original ZP_RB
           (RTS)))

(module+ test #| cp-natarr-range-ra-to-rb |#
  (define cp-natarr-range-ra-to-rb-t0
    (compact-run-code-in-test-
     ;; #:debug #t
     #:runtime-code test-runtime
     #:init-label "VM_INITIALIZE_PAGE_MEMORY_MANAGER_N20"
            (LDA !$04)
            (JSR ALLOC_NATARR_TO_RA)
            (LDY ZP_RAI)
     (label LOOP__)
            (TYA)
            (SEC)
            (SBC !$02)
            (STA (ZP_RA),y)
            (INY)
            (CPY !$06)
            (BNE LOOP__)
            (LDA !$08)
            (JSR ALLOC_NATARR_TO_RB)

            (LDX !$02) ;; not including 1
            (LDY !$03)
            (JSR CP_NATARR_RANGE_RA_TO_RB)))


  (check-equal? (memory-list cp-natarr-range-ra-to-rb-t0 ZP_RA (+ 1 ZP_RA))
                (list #x02 PAGE_AVAIL_0)
                "ra is allocated first on page 0")
  (check-equal? (vm-page->strings cp-natarr-range-ra-to-rb-t0 PAGE_AVAIL_0)
                (list "page-type:      m1 page p0"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page 0 is of profile 0, since length is 4")
  (check-equal? (memory-list cp-natarr-range-ra-to-rb-t0 ZP_RB (+ 1 ZP_RB))
                (list #x02 PAGE_AVAIL_1)
                "rb is allocated first on page 1")
  (check-equal? (vm-page->strings cp-natarr-range-ra-to-rb-t0 PAGE_AVAIL_1)
                (list "page-type:      m1 page p2"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $0e")
                "page 1 is of profile 1, since payloda length is 8")
  (check-equal? (memory-list cp-natarr-range-ra-to-rb-t0 (+ PAGE_AVAIL_0_W 2) (+ PAGE_AVAIL_0_W 7))
                (list #x01 #x84 0 1 2 3)
                "ra points to page0, containing a native array of len 4 filled with 0 1 2 3")
  (check-equal? (memory-list cp-natarr-range-ra-to-rb-t0 (+ PAGE_AVAIL_1_W 2) (+ PAGE_AVAIL_1_W 5))
                (list #x01 #x88 2 3)
                "rb points to page1, containing a native array of len 8 (first 2 bytes) filled with 2 3"))

(define vm-native-array-code
  (append
    ALLOC_NATARR_TO_RA
    ALLOC_NATARR_TO_RB
    WRITE_RT_TO_NATARR_RAI
    POP_TO_NATARR_RAI
    WRITE_NATARR_RAI_TO_RT
    PUSH_NATARR_RAI                ;; just ref, no code (included in WRITE_NATARR_RAI_TO_RT)
    CP_NATARR_RA_TO_RB
    CP_NATARR_RANGE_RA_TO_RB))

(module+ test #| code len |#
  (inform-check-equal? (code-len vm-native-array-code)
                       161))

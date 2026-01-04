#lang racket/base

(provide
 INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX    ;; initialize m1 page (page = RZ+1) of profile x, returning first free slot in A/X
 ALLOC_M1_SLOT_TO_RA                  ;; allocate an m1 slot of size A into RA
 ALLOC_M1_SLOT_TO_RB                  ;; allocate an m1 slot of size A into RB
 ALLOC_M1_P0_SLOT_TO_RA               ;; allocate an m1 slot profile 0 (of size 4) into RA
 ALLOC_M1_PX_SLOT_TO_RA               ;; allocate an m1 slot profile X into RA
 ALLOC_M1_SLOT_TO_RT                  ;; allocate an m1 slot of size A into RT
 ALLOC_M1_P0_SLOT_TO_RT               ;; allocate m1 slot of profile 0 to RT
 DEC_REFCNT_M1_SLOT_RZ                ;; decrement pointed to by RZ (without checks)
 DEC_REFCNT_M1_SLOT_RZ__IF_PTR        ;; decrement pointed to by RZ only iff it is a ptr
 DEC_REFCNT_M1_SLOT_RT__IF_PTR        ;; decrement pointed to by RT only iff it is a ptr
 DEC_REFCNT_M1_SLOT_RA                ;; decrement pointed to by RZ (no checks needed, RA may only hold ptrs)
 DEC_REFCNT_M1_SLOT_RB                ;; decrement pointed to by RZ (no checks needed, RB may only hold ptrs)
 DEC_REFCNT_M1_SLOT_RC                ;; decrement pointed to by RZ (no checks needed, RC may only hold ptrs)
 INC_REFCNT_M1_SLOT_RT                ;; decrement pointed to by RZ (without checks)

 vm-m1-slot-code                      ;; complete list of code of this module

 )

#|

  functions for m1 pages and slots


  following profiles are available:

  |M| profile | payload | slots on page | payload slots | waste |   each slot has a reference count field and a type field (payload = profile-2)
  |-|---------|---------|---------------|---------------|-------|   each page has 4 bytes of metadata (leaving 252 bytes for data)
  |*| 6       | 4       | 42            | 2             | 0     |     page type
  |*| 8       | 6       | 31            | 3             | 4     |     a reference to the prev page of this type
  | | 10      | 8       | 25            | 4             | 2     |     number of free slots
  |*| 12      | 10      | 21            | 5             | 0     |     offset to first free slot on this page
  | | 18      | 16      | 14            | 8             | 0     |
  |*| 24      | 22      | 10            | 11            | 8     |
  | | 28      | 26      | 9             | 13            | 0     |
  |*| 36      | 34      | 7             | 17            | 0     |
  | | 42      | 40      | 6             | 20            | 0     |
  |*| 50      | 48      | 5             | 24            | 2     |
  | | 84      | 82      | 3             | 41            | 0     |

  - selected profiles;  6 8 12 24 36 50
  - alternative option: 6 8 12 18 28 42
  - alternative option: 6 8 12 18 24 36 50

  page layout
  | offset | content
  |--------|--------------------
  | +00    | page-type
  | +01    | # of free slots / # of used slots (optional, since this offset is not available on specialized cell-pair pages)
  | +02    | beginning of first slot
  |  :     |
  | +fe    | offset to first free slot (0 if there is no free slot)
  | +ff    | ptr to prev page of this type (0 if this page is full || head and tail of the PPAGE_FREE_LIST)

  if a slot is part of the on page free list of slots, its first payload byte points to the next free slot!

  | page-type | description
  |-----------|--------------------
  | 0000 0000 | unknown or uninitialized
  | 0001 0001 | page with code
  | 0001 0010 | cell stack page
  | 0001 0011 | call stack page
  | 0010 xxxx | m1 page with profile xxxx

  slot layout
  | offset | content
  |--------|----------------
  | +0     | reference count or (if this slot is free) offset to the next free slot
  | +1     | slot type (may include #of cells in this slot)
  | +2 ...  | payload

  | slot type | description
  |-----------|---------------------
  | 0100 0000 | not initialized (unknown slot type)
  | 0100 0001 | reserved (1)
  | 0100 001x | reserved (2)
  | 0100 01xx | reserved (4)
  | 0100 1xxx | reserved (8)
  | 0101 xxxx | reserved (16)
  | 011x xxxx | reserved (32)
  | 00xx xxxx | cell-array (x = length of array), all profiles
  |           | cell and cell-pairs are cell-arrays of profile 6 (with length 1 and 2 respectively), no extra code necessary
  | 1xxx xxxx | native-array (x = length of array), all profiles

  INC_COLLECTIBLES_LIST head of a list of slots that are incrementally collected and incrementally collectible
    the list is made up of word pointers to slots (lb hb)
    each slot uses the slot cell 0 (first slot cell) to reference the next in list
    => slot layout of a collectible:
       | offset | content
       |--------|----
       | 0      | 0 = refcount
       | 1      | slot type + # of cells to left checking (n+1)
       | 2..3   | ptr to next slot that is collectible (lb=0 if no more slots in list)
       | cell1  |
       | ...    |
       | celln  | next cell to be checked during incremental gc
       | ...    | already collected cells

  INITALIZED_PAGE_FREE_LIST (an initialized completely unused page of a certain profile)
    n-lists with profile -> page head of list
    usage 1 byte per profile
    completely initialized (no slot allocated yet, but page free list available etc.)

  PPAGE_FREE_LISTS (maybe partially free page list, head may have free slots of this profile)
    n-lists with profile->page head of list
    usage 1 byte per profile
    for each profile there is a byte that points to the page with (potentially) free slots of that type

  PSLOT_FREE_LISTS (partially reclaimed/freed cell-array slots organized in a single linked list)
    n-lists with profile->slot head of list
    usage 2 bytes per profile
    for each profile there is a pointer (16bit) to a slot that is partially freed
        this is used if cell-arrays are incrementally freed, but cells (may) remain in the array that have not been collected yet
        the first 2 bytes of a partially reclaimed slot are used for linking this list => list structure is independent of profile! => uniform code
        a partially reclaimed slot keeps the number of unscanned cells in its size => partially reclaimed arrays are processed by uniform code
        LDA slot_type
        AND #$3F = #of cells remaining (except the first cell, which is always freed first!)

  Allocation steps
    start:
      - if PSLOT_FREE_LIST for this profile is empty => goto no-incremental-free
      - execute one incremental free on slots of this profile
    no-incremental-free:
      - if head of PPAGE_FREE_LIST is empty (not pointing to a page) => goto maybe allocate new page
      - if head of PPAGE_FREE_LIST of this profile NOT full => goto allocate slot on page
    find page with free slots:
      - pop PPAGE_FREE_LIST head (and make sure that page popped has no next any more, to mark it as full)
      - goto start
    maybe allocate new page:
      - is there an initialized page of this profile in INITALIZED_PAGE_FREE_LIST that is unused => pop that page and use it, goto allocate slot on page
      - if there is no slot in the PSLOT_FREE_LIST of this profile => goto allocate new page
      - run incremental-free max x-times
      - if no free slot available in PSLOT_FREE_LIST after that => goto allocate new page
      - goto allocate slot on page (using the slot now found)
    allocate new page:
      - allocate new page
      - initialize page for profile x slots
    allocate slot on page:
      - get head of free slots on the page (as new slot)
      - put next, pointer to by head to page free list head (even if its 0)
      - optional: decrement free slot count of this page
      - return slot

  Deallocation steps
    start:
      - is this slot a native array => goto free-slot (no incremental dealloc necessary)
      - is this slot a cell-array => goto first incremental-free-cell-array
      - error (unknown slot type)
    first incremental-free-cell-array: (executed if rc drops to 0, and not yet part of the PSLOT_FREE_LIST)
      - if first cell is no reference => goto incremental-free-cell-array
      - free first cell by decrement refcount for found reference and do gc if dropping to 0
      - put 0 into first cell
      - return
    incremental-free-cell-array:
      - loop in reverse order over the array until a non atomic value (a reference) is found or the array was completely scanned
      - if no reference was found => goto free-slot
    reference-found:
      - decrement refcount for found reference and do gc if dropping to 0
      - mark the location for the next incremental-free
      - if first cell is != 0: return (already part of the PSLOT_FREE_LIST)
      - add this slot to the list of PSLOT_FREE_LIST of this profile (last 2 bytes of the slot are definitely free for this)
      - return
    free-slot:
      - put (old) head of free list on this page into payload byte
      - write this slot as head of the free list on this page
      - optional: increment free slot count of this page
      - if this page is the head of the PPAGE_FREE_LIST => return
      - if next page ptr != 0 (this page is part of the PFREE_LIST) => return
      - put old head (PPAGE_FREE_LIST) into next page of this page
      - put this page as new head of PPAGE_FREE_LIST

  derived page profile data
    | profile idx | slot size | PPAGE_FREE_LIST_HEAD | PSLOT_FREELIST_HEAD | INITIALIZED_PAGE_FREE_HEAD |
    |-------------|-----------|----------------------|---------------------|----------------------------|
    | 0           | 6         | 0x00                 | 0x00 0x00           | 0x00                       |
    | 1           | 8         | 0x00                 | 0x00 0x00           | 0x00                       |
    | 2           | 12        | 0x00                 | 0x00 0x00           | 0x00                       |
    | 3           | 24        | 0x00                 | 0x00 0x00           | 0x00                       |
    | 4           | 36        | 0x00                 | 0x00 0x00           | 0x00                       |
    | 5           | 50        | 0x00                 | 0x00 0x00           | 0x00                       |

  size requested => profile
    1-4  => 0            0000 0001 ... 0000 0100
    5-6  => 1            0000 0101 ... 0000 0110
    7-10 => 2            0000 0111 ... 0000 1010
    11-22 => 3           0000 1011 ... 0001 0110
    23-34 => 4           0001 0111 ... 0010 0010
    35-48 => 5           0010 0011 ... 0011 0000
    49- fail!            0011 0001 ... 1111 1111

    CPX #$04
    BPL SIZE1_4         ;; small slots fast
    CPX #$06
    BPL SIZE5_6
    .
    :                   ;; code size: 4*6 + 4 * 6 (for LDX BNE) = 48 bytes

    DECISION for profile in clocks
    profile 00 (size 6) in 5 clocks + 2 (LDX)
    profile 01 (size 8) in 9 clocks + 5 (LDX + BNE)
            02            13        + 5
            03            17        + 5
            04            21        + 5
            05            24 (using BMI for fialure)
            fail          25

  table based decision
  ;;    naive (slightly worse on profile 0, slightly better on profile 1, better on profile 2..5)
      CPX #$30 ;; 48
      BMI BOCK_TOO_LARGE_ERROR
      LDA profile_table,X       (7..8 cycles), bytes used 48 (code 7 bytes) = 55


  ;;  compressed table (slightly better on profiles 4+, worse on profiles 0-2)
      1-4 LSR  0..2        add carry (ADC #$00)  1..2      LSR 0..1
      5-6 LSR  2..3                              3        LSR 1
      7-10 LSR 3..5                              4..5      LSR 2
      11-22 LSR 5..11                            6..11     LSR 3..5
      23-34 LSR 11-17                           12..17    LSR 6..8
      35-48 LSR 17-24                           18..24    LSR 9..12

      CMP #$30 ;; 48
      BMI BOCK_TOO_LARGE_ERROR
      LSR
      ADC #$00
      TAY
      LDX profile_table,Y       (15 cycles), bytes used 23 (code 11 bytes) = 34

  VARIANCE ----- code = 16 bytes + table 11 bytes = 27 bytes total
      CMP #$05
      BMI SLOT5+
      LDX #0      ;; profile 0 = 6 cycles
      ;; do allocation with profile x

  SLOTS5+:
      LSR
      ADC #$00
      LSR
      TAY
      LDX profile_table,Y
      BNE do_allocation_w_profile_x ;; other profiles 20 cycles

  ;; further compressed (worse than cpx-bmi pairs!)
      CPX #$30 ;; 48
      BMI BOCK_TOO_LARGE_ERROR
      CPX #$05 ;;
      BPL COMPR_TABLE
      ;; profile is 0         8 cycles for profile 0

    COMPR_TABLE:
      TXA
      LSR
      ADC #$00
      LSR
      TAX
      DEX
      LDA profile_table,X       (24..25 cycles), bytes used 11 (code 18 bytes) = 29


  ;; binary search (with small cells found first)

      CPX #07
      BPL SLOT7+
      CPX #05
      BPL SLOT5_6
      ;; profile 0         8 cycles
   SLOTS5_6:
      ;; profile 1         9 cycles
   SLOTS7+:
      CPX #23
      BPL SLOTS23+
      CPX #11
      BPL SLOTS11_22
      ;; profile 2         13 cycles
  SLOTS11_22:
      ;; profile 3         14 cycles
  SLOTS23+:
      CPX #35
      BPL SLOTS35+
      ;; profile 4         13 cycles
  SLOTS35+:
      CPX #48
      BMI BLOCK_TOO_LARGE_ERROR ;; 19 cycles
      ;; profile 5         18 cycles

 |#


(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
         (only-in "./vm-memory-map.rkt"
                  TAGGED_NIL
                  ZP_RP
                  ZP_RA
                  ZP_RB
                  ZP_TEMP
                  ZP_RT
                  ZP_PAGE_FREE_LIST
                  ZP_PAGE_FREE_SLOTS_LIST
                  ZP_PROFILE_PAGE_FREE_LIST
                  ZP_INC_COLLECTIBLE_LIST
                  VM_MEMORY_MANAGEMENT_CONSTANTS)
         (only-in "./vm-register-functions.rkt"
                  CP_RA_TO_RZ))

(module+ test
  (require (only-in racket/list
                    range
                    flatten
                    make-list)
           "../../6510-test-utils.rkt"
           (only-in "../../6510-test-utils.rkt" skip)
           (only-in "../../ast/6510-relocator.rkt" code-len)
           (only-in "../../tools/6510-interpreter.rkt"
                    peek
                    memory-list
                    cpu-state-clock-cycles)
           (only-in "../vm-inspector-utils.rkt"
                    vm-page->strings)
           "./vm-memory-manager-test-utils.rkt"
           (only-in "./vm-memory-map.rkt"
                    ZP_TEMP
                    VM_MEMORY_MANAGEMENT_CONSTANTS)
           (only-in "./vm-pages.rkt"
                    VM_ALLOCATE_NEW_PAGE
                    VM_DEALLOCATE_PAGE
                    VM_INIT_PAGE_MEMORY_MANAGER)
           (only-in "./vm-register-functions.rkt"
                    vm-register-functions-code
                    CP_RA_TO_RT
                    CP_RA_TO_RB
                    CP_RT_TO_RZ
                    SWAP_RA_RB
                    SWAP_ZP_WORD))

  (define PAGE_AVAIL_0 #xcf)      ;; high byte of first page available for allocation
  (define PAGE_AVAIL_0_W #xcf00)  ;; word (absolute address) of first page available
  (define PAGE_AVAIL_1 #xce)      ;; high byte of second page available for allocation
  (define PAGE_AVAIL_1_W #xce00) ;; word (absolute address) of second page available

  (define test-runtime
    (append
     VM_ALLOCATE_NEW_PAGE
     VM_DEALLOCATE_PAGE
     VM_INIT_PAGE_MEMORY_MANAGER
     VM_MEMORY_MANAGEMENT_CONSTANTS

     vm-register-functions-code
     (list (label INIT_CELLSTACK_PAGE_X) (RTS)))))

;; input:  RZ+1 = page to be used
;;         x    = profile (0..5)
;; output: a    = 02
;;         x    = page
;;         => ax = ptr to first free slot
(define-vm-function INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX
   [list
           (LDY !$00)
           (STY ZP_RZ)

           ;; page meta data part 1/2
           (TXA)
           (ORA !$20)
           (STA (ZP_RZ),y)      ;; @00: page-type = 0010 xxxx (x = profile)
           (TYA)
           (INY)
           (STA (ZP_RZ),y)      ;; @01: number of used slots: 00

           (LDY !$02)
           (CLC)
    (label loop_initialize_rcs__)
           ;; (LDA !$00)
           ;; (STA (ZP_RZ),y)      ;; @02: RC of slot 0 = 0
           (TYA)
           (ADC profile_size_table,x)
           (BCS finished_init__)
           ;; (INY)
           (STA (ZP_RZ),y)      ;; @02+1: next free slot
           (TAY)
           (BCC loop_initialize_rcs__)

    (label finished_init__)
           (LDY profile_last_slot_offset_table,x) ;; fixup reference of next free slot of last slot on the page
           (LDA !$00)
           (STA (ZP_RZ),y)      ;; last slot has no next free

           ;; page meta data part 2/2
           (LDY !$ff)
           (STA (ZP_RZ),y)      ;; @ff: previous page: 00
           (DEY)
           (LDA !$02)
           (STA (ZP_RZ),y)      ;; @fe: first free slot starts at 02

           (LDX ZP_RZ+1)
           (RTS)

    (label profile_size_table)
           (byte $06  ;; slot size:  6
                 $08  ;; slot size:  8
                 $0c  ;; slot size: 12
                 $18  ;; slot size: 24
                 $24  ;; slot size: 36
                 $32) ;; slot size: 50

    ;; location for each profile of the last cells ptr to next cell on this page (which should be 0 upon init)
    (label profile_last_slot_offset_table)
           (byte $f8 ;; slot size:  6, slot: f8..fd
                 $f2 ;; slot size:  8, slot: f2..f9
                 $f2 ;; slot size: 12, slot: f2..fd
                 $da ;; slot size: 24, slot: da..f1
                 $da ;; slot size: 36, slot: da..fd
                 $ca ;; slot size: 50, slot: ca..fd
                 )])

(module+ test #| INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX |#
  (define test-alloc-m1-00-state-after-n
    (compact-run-code-in-test-
     ;; #:debug #t
     #:runtime-code (append test-runtime INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"
     ;; (LDX !$20)
     ;; (JSR VM_INIT_PAGE_MEMORY_MANAGER)

     ;; fill page with $ff
     (LDA !$FF)
     (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_04_CODE)
     (DEX)
     (STA $cf00,x)
     (BNE LOOP__TEST_ALLOC_M1_04_CODE)

     ;; now allocate the page
     (JSR VM_ALLOCATE_NEW_PAGE)
     (STX ZP_RZ+1)
     (LDX !$00) ;; do it explicitly
     (JSR INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX)))

  (check-equal? (memory-list test-alloc-m1-00-state-after-n (+ PAGE_AVAIL_0_W #x00) (+ PAGE_AVAIL_0_W #x01))
                (list #x20 #x00)
                "page type $10, number of used slots = $00")
  (check-equal? (memory-list test-alloc-m1-00-state-after-n (+ PAGE_AVAIL_0_W #x02))
                (list #x08)
                "slot0: next free slot at offset $08")
  (check-equal? (memory-list test-alloc-m1-00-state-after-n (+ PAGE_AVAIL_0_W #x08))
                (list #x0e)
                "slot1: next free slot at offset $0e")
  (check-equal? (memory-list test-alloc-m1-00-state-after-n (+ PAGE_AVAIL_0_W #xf8))
                (list #x00)
                "slotx: next free slot at offset $00 = no next")
  (check-equal? (vm-page->strings test-alloc-m1-00-state-after-n PAGE_AVAIL_0)
                '("page-type:      m1 page p0"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $02"))

  (define test-alloc-m1-01-state-after-n
    (compact-run-code-in-test-
     ;; #:debug #t
     #:runtime-code (append test-runtime INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"
     ;; (LDX !$20)
     ;; (JSR VM_INIT_PAGE_MEMORY_MANAGER)

     ;; fill page with $ff
     (LDA !$FF)
     (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_04_CODE)
     (DEX)
     (STA $cc00,x)
     (BNE LOOP__TEST_ALLOC_M1_04_CODE)

     ;; now allocate the page
     (JSR VM_ALLOCATE_NEW_PAGE)
     (STX ZP_RZ+1)
     (LDX !$01) ;; do it explicitly
     (JSR INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX)))

  (check-equal? (memory-list test-alloc-m1-01-state-after-n (+ PAGE_AVAIL_0_W #x00) (+ PAGE_AVAIL_0_W #x01))
                (list #x21 #x00)
                "page type $10, number of used slots = $00")
  (check-equal? (memory-list test-alloc-m1-01-state-after-n (+ PAGE_AVAIL_0_W #x02))
                (list #x0a)
                "slot0: next free slot at offset $08")
  (check-equal? (memory-list test-alloc-m1-01-state-after-n (+ PAGE_AVAIL_0_W #x0a))
                (list #x12)
                "slot1: next free slot at offset $12")
  (check-equal? (memory-list test-alloc-m1-01-state-after-n (+ PAGE_AVAIL_0_W #xf2))
                (list #x00)
                "slotx: next free slot at offset $00 = no next")
  (check-equal? (vm-page->strings test-alloc-m1-01-state-after-n PAGE_AVAIL_0)
                '("page-type:      m1 page p1"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $02"))

  (define test-alloc-m1-02-state-after-n
    (compact-run-code-in-test-
     ;; #:debug #t
     #:runtime-code (append test-runtime INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"
     ;; (LDX !$20)
     ;; (JSR VM_INIT_PAGE_MEMORY_MANAGER)

     ;; fill page with $ff
     (LDA !$FF)
     (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_04_CODE)
     (DEX)
     (STA $cc00,x)
     (BNE LOOP__TEST_ALLOC_M1_04_CODE)

     ;; now allocate the page
     (JSR VM_ALLOCATE_NEW_PAGE)
     (STX ZP_RZ+1)
     (LDX !$02) ;; do it explicitly
     (JSR INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX)))

  (check-equal? (memory-list test-alloc-m1-02-state-after-n (+ PAGE_AVAIL_0_W #x00) (+ PAGE_AVAIL_0_W #x01))
                (list #x22 #x00)
                "page type $10, number of used slots = $00")
  (check-equal? (memory-list test-alloc-m1-02-state-after-n (+ PAGE_AVAIL_0_W #x02))
                (list #x0e)
                "slot0: next free slot at offset $08")
  (check-equal? (memory-list test-alloc-m1-02-state-after-n (+ PAGE_AVAIL_0_W #x0e))
                (list #x1a)
                "slot1: next free slot at offset $0e")
  (check-equal? (memory-list test-alloc-m1-02-state-after-n (+ PAGE_AVAIL_0_W #xf0))
                (list #x00)
                "slotx: next free slot at offset $00 = no next")
  (check-equal? (vm-page->strings test-alloc-m1-02-state-after-n PAGE_AVAIL_0)
                '("page-type:      m1 page p2"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $02")))

(define-vm-function OPTIMISED_ALLOC_M1_P0_SLOT_TO_RA
   (list
           (LDA ZP_PAGE_FREE_SLOTS_LIST+0)
           (BEQ no_page_w_free_slots__)

           (STA ZP_RA+1)        ;; store page to use
           (STA inc_usage__+2)

           (LDY !$00)
           (STY ZP_RA)          ;; offset w/i RA = 0
           (INY)

           (LDA (ZP_RA),y)
           (BEQ check_wether_empty_page_should_be_user__)

           (LDY !$fe)
           (LDA (ZP_RA),y)      ;; a next free slot
           (BEQ no_free_slot_on_page__)

    (label inc_usage__)
           (INC $cf01)

           (TAX)                ;; x = slot to allocate
           (TAY)
           ;; (INY)                ;; could be saved, if @0 = offset to next slot (maybe put @1 = #of references?)
           (LDA (ZP_RA),y)      ;; a = new next free slot
           (LDY !$fe)
           (STA (ZP_RA),y)      ;; page next free slot is updated

           (STX ZP_RA)          ;; store just allocated offset to zp_ra
           (RTS)

    ;; ignore these cases
    (label no_page_w_free_slots__)
    (label no_free_slot_on_page__)
    (label check_wether_empty_page_should_be_user__)
           (BRK)))

(module+ test #| optimized alloc test |#
  (define (test-alloc-m1-slot-p0-optimized #:times (times 1))
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code (append test-runtime ALLOC_M1_SLOT_TO_RA INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX OPTIMISED_ALLOC_M1_P0_SLOT_TO_RA)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"

            (ast-opcode-cmd '() `(162 ,times)) ;; (LDX !$20)
            (DEX)
            (STX $FFFF)
            (BEQ optimized_no_loop__)
     (label optimized_alloc_m1_slot_test_loop)
            (LDA !$04)
            (JSR ALLOC_M1_SLOT_TO_RA)
            ;; store profile for check
            (STX ZP_TEMP)
            (DEC $FFFF)
            (BNE optimized_alloc_m1_slot_test_loop)
     (label optimized_no_loop__)
            (JSR $0100) ;; reset cpu cycle count
            (JSR OPTIMISED_ALLOC_M1_P0_SLOT_TO_RA)))

  (define test-alloc-m1-slot-p0-optimized-2 (test-alloc-m1-slot-p0-optimized #:times 2))

  (inform-check-equal?
   (cpu-state-clock-cycles test-alloc-m1-slot-p0-optimized-2 )
   67
   "optimized allocate a slot of profile 0 on an existing page of the right profile takes n cycles")

  (check-equal?
   (memory-list test-alloc-m1-slot-p0-optimized-2 PAGE_AVAIL_0_W (+ 1 PAGE_AVAIL_0_W))
   (list #x20 #x02)
   "page type $20 = m1 page profile 0, 02 slots allocated")

  (check-equal?
   (memory-list test-alloc-m1-slot-p0-optimized-2 (+ PAGE_AVAIL_0_W #xfe) (+ PAGE_AVAIL_0_W #xff))
   (list #x0e #x00)
   "next free slot is at @0e, next page of same profile is 0"))

(define-vm-function ALLOC_M1_SLOT_TO_RT
  (list
          ;; maybe keep RA if not empty!
          (JSR ALLOC_M1_SLOT_TO_RA)
          (JMP CP_RA_TO_RT)))

(define-vm-function ALLOC_M1_P0_SLOT_TO_RT
  (list
          ;; maybe keep RA if not empty!
          (LDA ZP_RA)
          (PHA)
          (LDA ZP_RA+1)
          (PHA)

          (JSR ALLOC_M1_P0_SLOT_TO_RA)
          (JSR CP_RA_TO_RT)

          (PLA)
          (STA ZP_RA+1)
          (PLA)
          (STA ZP_RA)
          (RTS)))

;; allocate an m1 slot
;;
;; reuse pages marked as available (ZP_PAGE_FREE_SLOTS_LIST,x)
;; else reuse pages marked as free of the same profile (ZP_PROFILE_PAGE_FREE_LIST,x)
;; else allocate an unused page (ZP_PAGE_FREE_LIST)
;; else use a free page of a different profile (ZP_PROFILE_PAGE_FREE_LIST,?)
;;
;; input:   A = size of payload wanted
;; output:  RA = ptr to M1 SLOT (points to RC field)
;;          x = profile used
;;
;; variant: ALLOC_M1_P0_SLOT_TO_RA
;; input:   none
;; output:  RA = ptr to M1 SLOT of profile 0 (max 4 byte payload)
;;
;; variant: ALLOC_M1_PX_SLOT_TO_RA
;; input:   x = profile (0..5)
;; output:  RA = ptr to M1 SLOT of profile x
;;
;; variant ALLOC_M1_P0_SLOT_PAGE_A_TO_RA
;; input:   a = page (must be a m1 page of profile 0!)
;; output   RA = ptr to M1 SLOT of profile 0 (could be on another page)
(define ALLOC_M1_P0_SLOT_PAGE_A_TO_RA '())
(define ALLOC_M1_PX_SLOT_TO_RA '())
(define ALLOC_M1_P0_SLOT_TO_RA '())
(define-vm-function-wol ALLOC_M1_SLOT_TO_RA
   (list
    (label ALLOC_M1_P0_SLOT_PAGE_A_TO_RA) ;; a must hold a page of m1 profile 0!
           (LDY !$00)
           (STY ZP_RA)
           (STY ZP_TEMP)
           (BNE use_page_a_w_profile_temp__)

    (label find_profile_for_slots_size5_plus__)
           (LDX !$00)
           (STX ZP_RA)     ;; make sure setting the page on ra (zp_ra+1) will point to the the first byte of the page

           ;; now map the size to an index in the profile_table__
           ;; (works for all but profile 0! => 5 <= A <= 48)
           (LSR)        ;; if uneven, 1 is in carry
           (ADC !$00)   ;; ... and is added in that case!
           (LSR)        ;;
           (TAY)
           (LDX profile_table__-1,y) ;; since all profiles except 0 are loaded
           (BNE ALLOC_M1_PX_SLOT_TO_RA) ;; always jump!

    (label ALLOC_M1_SLOT_TO_RA)
           (CMP !$05)
           (BPL find_profile_for_slots_size5_plus__)  ;; >= 5, branch

    (label ALLOC_M1_P0_SLOT_TO_RA) ;; e.g. used for fast cell-pairs allocation
           (LDX !$00)      ;; profile 0
           (STX ZP_RA)     ;; clear RA offset

    (label ALLOC_M1_PX_SLOT_TO_RA)
           (STX ZP_TEMP)        ;; store profile in temp!!

    (label do_allocation_w_profile_temp__)
           ;; 1. check for page with free slots of this profile (use when found)
           (LDA ZP_PAGE_FREE_SLOTS_LIST,x)
           (BEQ no_page_with_free_slots__)

    (label use_page_a_w_profile_temp__)
           (STA ZP_RA+1)
           (STA inc_alloc_count__+2) ;; set page for later inc command

    (label use_page_ra_w_profile_temp__)

           ;; don't use completely free pages (which could be reclaimed), if others with free slots exist
           (LDY !$01)
           (LDA (ZP_RA),y)
           (BEQ consider_not_to_use_empty_page__) ;; since it is completely free

    (label continue_page_ra_inc_set__)
           (LDY !$fe)
           (LDA (ZP_RA),y) ;; get free slot
           (BEQ no_free_slot_on_page__)
           ;; fall through if there is a free slot
           ;; a = free slot
    (label use_page_ra_slot_a_inc_set__)
           (TAY)
           (TAX)                ;; remember free slot in x
           (LDA (ZP_RA),y)      ;; a = next free slot
           (LDY !$fe)
           (STA (ZP_RA),y)      ;; store next free slot @fe on this page
           (STX ZP_RA)          ;; store free slot offset into ra

           (LDY !$00)
           (LDA !$01)
           (STA (ZP_RA),y)      ;; write 1 into refcount!

           ;; optional inc # used, if this one drops to 0 (on free), the page us completely unused!
    (label inc_alloc_count__)   ;; page (cf) is overwritten elsewhere (see ST_ inc_alloc_count__+2)
           (INC $cf01)          ;; execute inc on @01 (count of used slots on page)

           (RTS)

    (label consider_not_to_use_empty_page__)
           ;; consider to not use this page since it is completely free
           (LDY !$ff)
           (LDA (ZP_RA),y) ;; get next page of this profile
           (BEQ continue_page_ra_inc_set__) ;; continue, if there is no next page of this profile (and use this completely free page)

           ;; remove this page (which is completely free) from list for free slots
           (STA ZP_PAGE_FREE_SLOTS_LIST,x) ;; a = next page

           ;; put zp_ra+1 page on head of zp_profile_page_free_list
           (LDA ZP_PROFILE_PAGE_FREE_LIST,x)
           ;; (LDY $ff) ;; y is still $ff
           (STA (ZP_RA),y)                   ;; store old head into @ff of this page
           (LDA ZP_RA+1)                     ;; a = completely free page
           (STA ZP_PROFILE_PAGE_FREE_LIST,x) ;; store new head

           ;; now retry allocation with zp_page_fre_slots_lists,x
           (BNE do_allocation_w_profile_temp__) ;; always branch

    (label no_free_slot_on_page__)
           ;; remove this page from the ZP_PAGE_FREE_SLOTS_LIST,x
           (LDX ZP_TEMP)
           (LDY !$ff)
           (LDA (ZP_RA),y)        ;; page @ff = ptr to next page of this profile
           (STA ZP_PAGE_FREE_SLOTS_LIST,x)
           (BNE use_page_a_w_profile_temp__) ;; loop until free slots available?
           ;; fall through if 0 (was last in list)

    (label no_page_with_free_slots__)
           ;; 2. check for free page of this profile (reuse if found)
           (LDA ZP_PROFILE_PAGE_FREE_LIST,x)
           (BEQ no_free_page_of_this_type__)
           (STA ZP_RA+1)

           ;; dequeue this page from free pages
           (LDY !$ff)
           (LDA (ZP_RA),y)
           (STA ZP_PROFILE_PAGE_FREE_LIST,x)

           ;; queue this page into page with free slots
           (LDA ZP_PAGE_FREE_SLOTS_LIST,x)
           (STA (ZP_RA),y)
           (LDA ZP_RA+1)
           (STA ZP_PAGE_FREE_SLOTS_LIST,x)
           (STA inc_alloc_count__+2) ;; set page for later inc command

           ;; now allocate on this page (without check for complete emptiness!)
           (BNE continue_page_ra_inc_set__) ;; always jump (LDA ZP_RA+1 != 0)

    (label no_free_page_of_this_type__)
           ;; 3. check for unallocated page
           (LDA ZP_PAGE_FREE_LIST)
           (BEQ no_free_page_at_all__)
           (JSR VM_ALLOCATE_NEW_PAGE)
           (STX ZP_RZ+1)      ;; x = allocated page (from previous call)

    (label intialize_page_rz_profile_x__)
           ;; now initialize this page to the new type
           (LDX ZP_TEMP)      ;; x = profile
           (JSR INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX)
           (STX ZP_RA+1)
           (STX inc_alloc_count__+2)
           ;; now set this page as page with free slots for this profile
           (LDY ZP_TEMP) ;; y = profile
           (STX ZP_PAGE_FREE_SLOTS_LIST,y)  ;; this should (only) overwrite 0 in ZP_PAGE_FREE_SLOTS,y
           (BPL use_page_ra_slot_a_inc_set__) ;; always jump (LDY ZP_TEMP) >= 0

    (label no_free_page_at_all__)
           ;; 4. try to reuse a free page of a different profile!
           (LDX profile_table__+11) ;; max

    (label loop_for_any_free_page__)
           (LDA ZP_PROFILE_PAGE_FREE_LIST,x)
           (BEQ continue_loop_for_any_free_page__ )
           (STA ZP_RA+1)
           (STA ZP_RZ+1) ;; for initialize!
           ;; dequeue this page from free pages
           (LDY !$ff)
           (LDA (ZP_RA),y)
           (STA ZP_PROFILE_PAGE_FREE_LIST,x)
           (JMP intialize_page_rz_profile_x__)
           ;; (CLC)
           ;; (BCC intialize_page_rz_profile_x__)

    (label continue_loop_for_any_free_page__)
           (DEX)
           (BPL loop_for_any_free_page__)
           ;; error: out of memory!
           (BRK)

    (label profile_table__)
           (byte $01 ;; 05..06 LSR ADC -> 03..03 LSR -> 01
                 $02 ;; 07..10 LSR ADC -> 04..05 LSR -> 02
                 $03 ;; 11..22 LSR ADC -> 06..11 LSR -> 03..05
                 $03
                 $03
                 $04 ;; 23..34 LSR ADC -> 12..17 LSR -> 06..08
                 $04
                 $04
                 $05 ;; 35..48 LSR ADC -> 18..24 LSR -> 09..12
                 $05
                 $05
                 $05)))

(module+ test #| vm_alloc_bucket_slot, allocate one slot of size $0b |#

  (inform-check-equal?
   (code-len ALLOC_M1_SLOT_TO_RA)
   216
   "size of code for allocation of m1 slots is n bytes")

  (define (test-alloc-m1-slot-p0-to-ra-n #:times (times 1))
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code (append test-runtime ALLOC_M1_SLOT_TO_RA INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"

            (ast-opcode-cmd '() `(162 ,times)) ;; (LDX <time>)
            (DEX)
            (STX $FFFF)
            (BEQ no_loop_test_alloc_m1_slot_p0_to_ra_n)
     (label alloc_m1_slot_test_loop)
            (JSR ALLOC_M1_P0_SLOT_TO_RA)
            ;; store profile for check
            (STX ZP_TEMP)
            (DEC $FFFF)
            (BNE alloc_m1_slot_test_loop)
     (label no_loop_test_alloc_m1_slot_p0_to_ra_n)
            (JSR $0100) ;; reset cpu cycle count
            (JSR ALLOC_M1_P0_SLOT_TO_RA)))

  (inform-check-equal?
   (cpu-state-clock-cycles (test-alloc-m1-slot-p0-to-ra-n #:times 2))
   81 ;; optimized uses 67 cycles
   "allocate a slot of profile 0 on an existing page of the right profile takes n cycles")

  (define (test-alloc-m1-slot-to-ra-n n #:times (times 1))
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code (append test-runtime ALLOC_M1_SLOT_TO_RA INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"

            (ast-opcode-cmd '() `(162 ,times)) ;; (LDX !$20)
            (STX $FFFF)
     (label alloc_m1_slot_test_loop)
            (ast-opcode-cmd '() `(169 ,n)) ;; lda n (slot size)
            (JSR $0100) ;; reset cpu cycle count
            (JSR ALLOC_M1_SLOT_TO_RA)
            ;; store profile for check
            (STX ZP_TEMP)
            (DEC $FFFF)
            (BNE alloc_m1_slot_test_loop)))

  (inform-check-equal?
   (cpu-state-clock-cycles (test-alloc-m1-slot-to-ra-n 35 #:times 2))
   112
   "allocate a slot on an existing page of the right profile takes n cycles")

  (inform-check-equal?
   (cpu-state-clock-cycles (test-alloc-m1-slot-to-ra-n 4 #:times 2))
   96
   "allocate a slot of profile 0 on an existing page of the right profile takes n cycles")

  (check-equal?
   (flatten
    (map (lambda (n) (memory-list (test-alloc-m1-slot-to-ra-n n) ZP_RA (+ 1 ZP_RA)))
         (range 1 49)))
   (flatten (make-list 48 `(#x02 ,PAGE_AVAIL_0)))
   "allocates all slots (regardless of profile) at cf02")

  (inform-check-equal?
   (cpu-state-clock-cycles (test-alloc-m1-slot-to-ra-n 35 #:times 1))
   319
   "allocate completely new page, initialize it for that profile and allocate a slot on it takes n cycles")

  (define test-alloc-m1-slot-to-ra-n-35-t-5 (test-alloc-m1-slot-to-ra-n 35 #:times 5))

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-35-t-5 (+ 5 ZP_PAGE_FREE_SLOTS_LIST))
   (list PAGE_AVAIL_0)
   "profile 5: after allocating 5 slots of size 35, $cf is listed as page with free slots of profile 5 (even though all slots were allocated)")

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-35-t-5 (+ 1 PAGE_AVAIL_0_W))
   (list 5)
   "profile 5: after allocating 5 slots of size 35, # of slots allocated on $cf is 5")

  (define test-alloc-m1-slot-to-ra-n-35-t-6 (test-alloc-m1-slot-to-ra-n 35 #:times 6))

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-35-t-6 (+ 5 ZP_PAGE_FREE_SLOTS_LIST))
   (list PAGE_AVAIL_1)
   "profile 5: after allocating 6 slots of size 35, $ce is listed as page with free slots of profile 5 since cf has all slots allocated")

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-35-t-6 (+ 1 PAGE_AVAIL_1_W))
   (list 1)
   "profile 5: after allocating 6 slots of size 35, $ce holds one slot (cf is full)")

  (check-equal?
   (memory-list (test-alloc-m1-slot-to-ra-n 35 #:times 11) (+ 5 ZP_PAGE_FREE_SLOTS_LIST))
   (list #xcd)
   "profile 5: after allocating 11 slots of size 35, $cd is listed as page with free slots of profile 5 since cf and ce has all slots allocated")

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-35-t-6 (+ PAGE_AVAIL_1_W #xfe) (+ PAGE_AVAIL_1_W #xff))
   (list #x34 #x00)
   "profile 5: after allocating 6 slots of size 35, pages next free slot is at 52 ($34), and the next page with free slots of ce is 00")

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-35-t-5 ZP_PAGE_FREE_LIST)
   (list PAGE_AVAIL_1)
   "profile 5: after allocating 5 slots of size 35, $cf is no longer part of the free list, but $ce wasn't allocated yet")

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-35-t-5 (+ PAGE_AVAIL_0_W #xfe) (+ PAGE_AVAIL_0_W #xff))
   (list #x00 #x00)
   "profile 5: after allocating 5 slots of size 35, no free slots are left on page + ptr to next page (of this type is 0)")

  (check-equal?
   (flatten
    (map (lambda (n) (memory-list (test-alloc-m1-slot-to-ra-n n) (+ PAGE_AVAIL_0_W #xfe) (+ PAGE_AVAIL_0_W #xff)))
         (range 35 49)))
   (flatten (make-list 14 '(#x34 #x00)))
   "profile 5: next slot is at #x34 and next page (of same type) is 00")

  (check-equal?
   (flatten
    (map (lambda (n) (memory-list (test-alloc-m1-slot-to-ra-n n) PAGE_AVAIL_0_W))
         (list 35 48)))
   (list #x25 #x25)
   "profile 5: allocated page type is #x20 + 5")

  (check-equal?
   (flatten
    (map (lambda (n) (memory-list (test-alloc-m1-slot-to-ra-n n) (+ PAGE_AVAIL_0_W #xfe) (+ PAGE_AVAIL_0_W #xff)))
         (range 23 35)))
   (flatten (make-list 12 '(#x26 #x00)))
   "profile 4: next slot is at #x26 and next page (of same type) is 00")

  (check-equal?
   (flatten
    (map (lambda (n) (memory-list (test-alloc-m1-slot-to-ra-n n) PAGE_AVAIL_0_W))
         (list 23 34)))
   (list #x24 #x24)
   "profile 4: allocated page type is #x20 + 4")

  (check-equal?
   (memory-list (test-alloc-m1-slot-to-ra-n 23 #:times 8) (+ PAGE_AVAIL_1_W #xfe) (+ PAGE_AVAIL_1_W #xff))
   (list #x26 #x00)
   "profile 4: after allocating 8 slots of size 23, pages next free slot is at 38 ($26), and the next page with free slots of ce is 00")

  (define test-alloc-m1-slot-to-ra-n-23-t-7 (test-alloc-m1-slot-to-ra-n 23 #:times 7))

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-23-t-7 ZP_PAGE_FREE_LIST)
   (list PAGE_AVAIL_1)
   "profile 4: after allocating 7 slots of size 23, $cf is no longer part of the free list, but $ce wasn't allocated yet")

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-23-t-7 (+ PAGE_AVAIL_0_W #xfe) (+ PAGE_AVAIL_0_W #xff))
   (list #x00 #x00)
   "profile 4: after allocating 7 slots of size 23, no free slots are left on page + ptr to next page (of this type is 0)")

  (check-equal?
   (flatten
    (map (lambda (n)(memory-list (test-alloc-m1-slot-to-ra-n n) (+ PAGE_AVAIL_0_W #xfe) (+ PAGE_AVAIL_0_W #xff)))
         (range 11 23)))
   (flatten (make-list 12 '(#x1a #x00)))
   "profile 3: next slot is at #x1a and next page (of same type) is 00")

  (check-equal?
   (flatten
    (map (lambda (n) (memory-list (test-alloc-m1-slot-to-ra-n n) PAGE_AVAIL_0_W))
         (list 11 22)))
   (list #x23 #x23)
   "profile 3: allocated page type is #x20 + 3")

  (check-equal?
   (flatten
    (map (lambda (n)(memory-list (test-alloc-m1-slot-to-ra-n n) (+ PAGE_AVAIL_0_W #xfe) (+ PAGE_AVAIL_0_W #xff)))
         (range 7 11)))
   (flatten (make-list 4 '(#x0e #x00)))
   "profile 2: next slot is at #x0e and next page (of same type) is 00")

  (check-equal?
   (flatten
    (map (lambda (n) (memory-list (test-alloc-m1-slot-to-ra-n n) PAGE_AVAIL_0_W))
         (list 7 10)))
   (list #x22 #x22)
   "profile 2: allocated page type is #x20 + 2")

  (check-equal?
   (flatten
    (map (lambda (n)(memory-list (test-alloc-m1-slot-to-ra-n n) (+ PAGE_AVAIL_0_W #xfe) (+ PAGE_AVAIL_0_W #xff)))
         (range 5 7)))
   (flatten (make-list 2 '(#x0a #x00)))
   "profile 1: next slot is at #x0a and next page (of same type) is 00")

  (check-equal?
   (flatten
    (map (lambda (n) (memory-list (test-alloc-m1-slot-to-ra-n n) PAGE_AVAIL_0_W))
         (list 5 6)))
   (list #x21 #x21)
   "profile 1: allocated page type is #x20 + 1")

  (check-equal?
   (flatten
    (map (lambda (n)(memory-list (test-alloc-m1-slot-to-ra-n n) (+ PAGE_AVAIL_0_W #xfe) (+ PAGE_AVAIL_0_W #xff)))
         (range 1 5)))
   (flatten (make-list 4 '(#x08 #x00)))
   "profile 0: next slot is at #x08 and next page (of same type) is 00")

  (check-equal?
   (flatten
    (map (lambda (n) (memory-list (test-alloc-m1-slot-to-ra-n n) PAGE_AVAIL_0_W))
         (list 1 4)))
   (list #x20 #x20)
   "profile 0: allocated page type is #x20 + 0")

  (check-equal?
   (memory-list (test-alloc-m1-slot-to-ra-n 4 #:times 43) (+ PAGE_AVAIL_1_W #xfe) (+ PAGE_AVAIL_1_W #xff))
   (list #x08 #x00)
   "profile 0: after allocating 43 slots of size 4, pages next free slot is at 08 ($08), and the next page with free slots of ce is 00")

  (define test-alloc-m1-slot-to-ra-n-4-t-42 (test-alloc-m1-slot-to-ra-n 4 #:times 42))

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-4-t-42 ZP_PAGE_FREE_LIST )
   (list PAGE_AVAIL_1)
   "profile 0: after allocating 42 slots of size 4, $cf is no longer part of the free list, but $ce wasn't allocated yet")

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-4-t-42 (+ PAGE_AVAIL_0_W #xfe) (+ PAGE_AVAIL_0_W #xff))
   (list #x00 #x00)
   "profile 0: after allocating 42 slots of size 4, no free slots are left on page + ptr to next page (of this type is 0)")

  (define (test-alloc-m1-slot-to-ra-n-with-free-profile-page n #:time (times 1))
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code (append test-runtime ALLOC_M1_SLOT_TO_RA INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"

     ;; make sure to have an initialized page of profile 5 in ZP_PROFILE_PAGE_FREE_LIST+5
     (JSR VM_ALLOCATE_NEW_PAGE)
     (STX ZP_RZ+1)
     (LDX !$05) ;; profile 5
     (JSR INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX)
     (STX ZP_PROFILE_PAGE_FREE_LIST+5)

     (ast-opcode-cmd '() `(162 ,times)) ;; (LDX !$20)
     (STX $FFFF)
     (label alloc_m1_slot_test_loop)
     (ast-opcode-cmd '() `(169 ,n)) ;; lda n (slot size)
     (JSR $0100) ;; reset cpu cycle count
     (JSR ALLOC_M1_SLOT_TO_RA)
     ;; store profile for check
     (STX ZP_TEMP)
     (DEC $FFFF)
     (BNE alloc_m1_slot_test_loop)))


  (define test-alloc-m1-slot-to-ra-n-with-free-profile-page-35 (test-alloc-m1-slot-to-ra-n-with-free-profile-page 35))

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-with-free-profile-page-35 (+ PAGE_AVAIL_0_W #xfe) (+ PAGE_AVAIL_0_W #xff))
   (list #x34 #x00)
   (format "~s $~x ~s ~s ~s ~s"
           "after registering page" PAGE_AVAIL_0 "as free page for profile 5,"
           "then allocating a slot for profile 5"
           "(no page of this profile with free slots exists),"
           "=> will allocate slot from the page that is listed as free page of this profile"))

  (inform-check-equal?
   (cpu-state-clock-cycles test-alloc-m1-slot-to-ra-n-with-free-profile-page-35)
   141
   "reuse a free page of the same profile takes expected cycles")

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-with-free-profile-page-35 (+ ZP_PROFILE_PAGE_FREE_LIST 5))
   (list #x00)
   "page of profile 5 is no longer listed as being free page in ZP_PROFILE_PAGE_FREE_LIST")

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-with-free-profile-page-35 (+ ZP_PAGE_FREE_SLOTS_LIST 5))
   (list #xcf)
   "page of profile 5 is now listed as page with free slots of this profile")

  (define (test-alloc-m1-slot-to-ra-n-with-just-other-free-profile-page n #:time (times 1))
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code (append test-runtime ALLOC_M1_SLOT_TO_RA INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"

     ;; make sure to have an initialized page of profile 5 in ZP_PROFILE_PAGE_FREE_LIST+5
     (JSR VM_ALLOCATE_NEW_PAGE)
     (STX ZP_RZ+1)
     (LDX !$00) ;; profile 0
     (JSR INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX)
     (STX ZP_PROFILE_PAGE_FREE_LIST)

     ;; make sure to not have any other free page left
     (LDA !$00)
     (STA ZP_PAGE_FREE_LIST)

     (ast-opcode-cmd '() `(162 ,times)) ;; (LDX !$20)
     (STX $FFFF)
     (label alloc_m1_slot_test_loop)
     (ast-opcode-cmd '() `(169 ,n)) ;; lda n (slot size)
     (JSR $0100) ;; reset cpu cycles
     (JSR ALLOC_M1_SLOT_TO_RA)
     ;; store profile for check
     (STX ZP_TEMP)
     (DEC $FFFF)
     (BNE alloc_m1_slot_test_loop)))

  (check-equal?
   (memory-list (test-alloc-m1-slot-to-ra-n-with-just-other-free-profile-page 35) (+ PAGE_AVAIL_0_W #xfe) (+ PAGE_AVAIL_0_W #xff))
   (list #x34 #x00)
   (format "~a $~x ~a ~a ~a ~a ~a"
           "after registering page" PAGE_AVAIL_0 "as free page for profile 0!,"
           "and making sure that no other free page is available,"
           "then allocating a slot for profile 5"
           "(no page of this profile with free slots exists),"
           "=> will allocate slot from the page that is listed as free page of the different profile, reinitializing that page"))

  (define test-alloc-m1-slot-to-ra-n-with-just-other-free-profile-page-35 (test-alloc-m1-slot-to-ra-n-with-just-other-free-profile-page 35))

  (inform-check-equal?
   (cpu-state-clock-cycles test-alloc-m1-slot-to-ra-n-with-just-other-free-profile-page-35)
   383
   "cost to use a free page initilized to another profile, if no free pages exist")

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-with-just-other-free-profile-page-35 PAGE_AVAIL_0_W)
   (list #x25)
   "page was reinitialized to be of profile type 5 ($20 + 5)")

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-with-just-other-free-profile-page-35 ZP_PROFILE_PAGE_FREE_LIST)
   (list #x00)
   "page is now no longer available for profile 0")

  (check-equal?
   (memory-list test-alloc-m1-slot-to-ra-n-with-just-other-free-profile-page-35 (+ ZP_PAGE_FREE_SLOTS_LIST 5))
   (list #xcf)
   "page is now part of the list of pages with free slots of profile 5"))

;; free the m1 slot pointed to by RZ
;;
;; do not GC any data. add this slot to free list. decrement slots used count
(define-vm-function FREE_M1_SLOT_FROM_RZ
   (list
           (LDX ZP_RZ)          ;; offset to slot within page
           (STX ZP_TEMP)        ;; zp_temp = offset

           (LDY !$00)

           ;; optional (set reference count to 0) <- should be done be gc!
           ;; (TYA)
           ;; (STA (ZP_RZ),y)

           (STY ZP_RZ)          ;; set ra offset to point to @0 on page

           ;; - add slot to free list of page
           (LDY !$fe)
           (LDA (ZP_RZ),y)      ;; a = slot offset of previous free slot
           (BNE continue_a_eq_prev__)

           ;; page was full, add this page to the list of pages with free slots
           (TAY) ;; set y = 0
           (LDA (ZP_RZ),y)
           (AND !$0f) ;; extract profile
           (TAX)
           (LDA ZP_PAGE_FREE_SLOTS_LIST,x) ;; get current head of list of pages with free slots
           (CMP ZP_RZ+1)
           (BEQ continue__) ;; already listed as free
           (LDY !$ff)
           (STA (ZP_RZ),y) ;; set previous page
           (LDA ZP_RZ+1)
           (STA ZP_PAGE_FREE_SLOTS_LIST,x)

    (label continue__)
           (LDA !$00) ;; since page was full, previous slot offset is 0
    (label continue_a_eq_prev__)
           (LDY ZP_TEMP)        ;; y = slot offset
           (STA (ZP_RZ),y)
           (TYA)                ;; a = slot offset
           (LDY !$fe)
           (STA (ZP_RZ),y)      ;; free slot of page now points to this slot
           (LDA ZP_RZ+1)
           (STA decrement_count__+2)
    (label decrement_count__)
           (DEC $cf01)          ;; if dropping to 0, this page is completely free, which will be handled, once allocation is done on this page
           ;; optional: (BEQ page_empty_now__) ;; move this page to the list of free pages
           (RTS)
           ;; - decrement slot usage
    ))

(module+ test #| free_m1_slot_from_rz_n |#
  (define (free_m1_slot_from_rz_n-test times)
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code (append test-runtime ALLOC_M1_SLOT_TO_RA INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX FREE_M1_SLOT_FROM_RZ)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"
            (ast-opcode-cmd '() `(162 ,times)) ;; (LDX <time>)
            (STX $FFFF)
     (label alloc_m1_slot_test_loop)
            (JSR ALLOC_M1_P0_SLOT_TO_RA)
            ;; store profile for check
            (STX ZP_TEMP)
            (DEC $FFFF)
            (BNE alloc_m1_slot_test_loop)

            (LDA ZP_RA)
            (STA ZP_RZ)
            (LDA ZP_RA+1)
            (STA ZP_RZ+1)
            (JSR $0100) ;; reset cpu cycle count
            (JSR FREE_M1_SLOT_FROM_RZ)))

  (define free_m1_slot_from_rz_n-test-2 (free_m1_slot_from_rz_n-test 2))

  (inform-check-equal?
   (cpu-state-clock-cycles free_m1_slot_from_rz_n-test-2)
   59
   "free takes n cycles")

  (check-equal?
   (memory-list free_m1_slot_from_rz_n-test-2 (+ PAGE_AVAIL_0_W #xfe) (+ PAGE_AVAIL_0_W #xff))
   (list #x08 00)
   "slot 08 was freed => next free slot is 08, next page is 00")

  (check-equal?
   (memory-list free_m1_slot_from_rz_n-test-2 (+ PAGE_AVAIL_0_W #x00) (+ PAGE_AVAIL_0_W #x01))
   (list #x20 01)
   "page type is $20, 1 slot is allocated")

  (check-equal?
   (memory-list free_m1_slot_from_rz_n-test-2 ZP_PAGE_FREE_SLOTS_LIST)
   (list #xcf))

  (define free_m1_slot_from_rz_n-test-43 (free_m1_slot_from_rz_n-test 43))

  (check-equal?
   (memory-list free_m1_slot_from_rz_n-test-43 (+ PAGE_AVAIL_1_W #xfe) (+ PAGE_AVAIL_1_W #xff))
   (list #x02 00)
   "slot 02 was freed => next free slot is 02, next page is 00")

  (check-equal?
   (memory-list free_m1_slot_from_rz_n-test-43 (+ PAGE_AVAIL_1_W #x00) (+ PAGE_AVAIL_1_W #x01))
   (list #x20 00)
   "page type is $20, 0 slots are allocated"))

;; idea: to make it extremely fast, either inline 10 bytes (cycles 3 + 3 + 4 + 7 = 17)
;;       or use two additional bytes around RA, 238=INC abs and 96=RTS,
;;          allowing to directly JSR the INC command (cycles 6 + 6 = 12), not counting the jsr itself
;;       note: dec could be done on a different register, that is the surrounded by DEC abs and RTS
;; execution cycles = 3 + 3 + 4 + 7 + 6 = 23 (+jsr of caller)
(define-vm-function-wol INC_REFCNT_M1_SLOT_RT
   (list
    ;;        (LDY !$00)
    ;;        (LDA (ZP_RA),y)
    ;;        (CLC)
    ;;        (ADC !$01)
    ;;        (STA (ZP_RA),y)
    ;;        (RTS)

    (label INC_REFCNT_M1_SLOT_RT__IF_PTR)
           (LDA ZP_RT)
           (BEQ done__) ;; nil? then done
           (LSR)
           (BCS done__) ;; atomic? then done

    (label INC_REFCNT_M1_SLOT_RT)
           (LDA ZP_RT+1)
           (LDX ZP_RT)
           (STA inc_abs__+2)
    (label inc_abs__)
           (INC $cf00,x)
    (label done__)
           (RTS)))

(module+ test #| inc_refcnt_m1_slot_ra_n |#
  (define (inc_refcnt_m1_slot_rt_n-test times)
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code (append test-runtime ALLOC_M1_SLOT_TO_RA INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX FREE_M1_SLOT_FROM_RZ INC_REFCNT_M1_SLOT_RT)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"
            (ast-opcode-cmd '() `(162 ,times)) ;; (LDX <time>)
            (STX $FFFF)
            (JSR ALLOC_M1_P0_SLOT_TO_RA)
            (DEC $FFFF)
            (BEQ no_loop__inc_refcnt_m1_slot_ra_n_test)
     (label alloc_m1_slot_test_loop)
            (JSR CP_RA_TO_RT)
            (JSR INC_REFCNT_M1_SLOT_RT)
            (DEC $FFFF)
            (BNE alloc_m1_slot_test_loop)
     (label no_loop__inc_refcnt_m1_slot_ra_n_test)
            (JSR CP_RA_TO_RT)
            (JSR $0100) ;; reset cpu cycle count
            (JSR INC_REFCNT_M1_SLOT_RT)))

  (check-equal?
   (memory-list (inc_refcnt_m1_slot_rt_n-test 1) (+ PAGE_AVAIL_0_W #x02))
   (list #x02)
   "incrementing refcount once puts 2 (allocation starts with 1)")

  (check-equal?
   (memory-list (inc_refcnt_m1_slot_rt_n-test 5) (+ PAGE_AVAIL_0_W #x02))
   (list #x06)
   "incrementing refcount fice times puts 6 (allocation starts with 1)")

  (check-equal?
   (cpu-state-clock-cycles (inc_refcnt_m1_slot_rt_n-test 1))
   23
   "running inc on refcount executes n cycles"))

;; DEC_REFCNT_M1_SLOT_RZ
;;   decrement the refcount of a slot in m1
;;   if refcount drops to 0
;;      case 1: it is NOT a cell-array: free this slot
;;      case 2: it is a cell-array: collect first cell and register array to inc collectibles
;;                                  or (if it is no cell-ptr) continnue with incremental gc on the rest of that cell-array
;; input:  ZP_RZ = ptr to slot
;; output: -
;;
;; INC_GC_M1_SLOT_RZ_CELL_ARRAY
;;   do an incremental cell collection of cell-array in ZP_RZ
;;   incremental collection collects all cells except the first cell of the array
;;
;; input: ZP_RZ = ptr to slot
;; output: -
;;
;; GC_M1_SLOT_RZ
;;   garbage collect slot pointer to by ZP_RZ. refcount must have dropped to 0!
;; input: ZP_RZ = ptr to slot
;; output: -
(define-vm-function DEC_REFCNT_M1_SLOT_RA
  (list
            (JSR CP_RA_TO_RZ)
            (JMP DEC_REFCNT_M1_SLOT_RZ)))

(define-vm-function DEC_REFCNT_M1_SLOT_RB
  (list
            (JSR CP_RB_TO_RZ)
            (JMP DEC_REFCNT_M1_SLOT_RZ)))

(define-vm-function DEC_REFCNT_M1_SLOT_RC
  (list
            (JSR CP_RC_TO_RZ)
            (JMP DEC_REFCNT_M1_SLOT_RZ)))

(define INC_GC_M1_SLOT_RZ_CELL_ARRAY '())
(define DEC_REFCNT_M1_SLOT_RT__IF_PTR '())
(define DEC_REFCNT_M1_SLOT_RZ '())
(define GC_M1_SLOT_RZ '())
(define-vm-function-wol DEC_REFCNT_M1_SLOT_RZ__IF_PTR
   (list
    (label DEC_REFCNT_M1_SLOT_RT__IF_PTR)
           (JSR CP_RT_TO_RZ)

    (label DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
           (LDA ZP_RZ)
           (BEQ done__) ;; nil? then done
           (LSR)
           (BCS done__) ;; atomic? then done

    (label DEC_REFCNT_M1_SLOT_RZ)
           (LDA ZP_RZ+1)
           (LDX ZP_RZ)
           (STA dec_abs__+2)
    (label dec_abs__)
           (DEC $cf00,x)
           (BEQ GC_M1_SLOT_RZ)
    (label done__)
           (RTS)

    (label GC_M1_SLOT_RZ) ;; slot has not been gc'd neither full nor incremental!
           (LDY !$01)
           (LDA (ZP_RZ),y) ;; get array (m1 slot) type
           (AND !$C0)
           (BEQ FIRST_INC_GC_M1_SLOT_RZ_CELL_ARRAY)    ;; cell-array (00xx xxxx) ?  yes => do incremental gc
           (JMP FREE_M1_SLOT_FROM_RZ)

    (label first_slot_is_a_ptr__)
           ;; no optimized free after last slot
           (STA EXEC_OPTIMIZED_LAST_SLOT_FREE__)

           ;; remember current cell-ptr in array
           (LDA (ZP_RZ),y) ;; low byte
           (TAX)
           (INY)
           (LDA (ZP_RZ),y) ;; high byte
           (PHA)
           (DEY)

           (BNE cons_to_collectible_list__) ;; always jump

    (label FIRST_INC_GC_M1_SLOT_RZ_CELL_ARRAY) ;; called if this is the first (inc) gc of this cell-array!
           ;; collect first slot (if it is a ptr) and add it to the list of incremental collectable arrays
           (STY EXEC_OPTIMIZED_LAST_SLOT_FREE__)
           (INY) ;; y = 2

           (LDA (ZP_RZ),y)      ;; lowbyte of first slot
           (BEQ first_is_nil_ptr__)
           (AND !$01)           ;; tagged as pointer?
           (BEQ first_slot_is_a_ptr__ )

    (label first_is_nil_ptr__)
           ;; (DEY)

    (label INC_GC_M1_SLOT_RZ_CELL_ARRAY) ;; incremental gc on cell-array pointed to by zp_rz
           (LDY !$01)
           ;; ZP_RZ points to a cell array
           ;; 1st scan cells backword, discard atomic cells immediately,
           ;; DEC refcount first cell-ptr (if encountered) and store meta data for next incremental run and return.
           (LDA (ZP_RZ),y)
           (AND !$3f)             ;; a = len
           (CMP !$02)             ;; first cell is always handled first, before entering this loop!
           (BMI no_cells_left_for_scanning__)

    (label cell_reverse_scan__)
           (ASL A)
           (TAY)
    (label tight_loop__)
           (LDA (ZP_RZ),y) ;; get lowbyte
           (BEQ loop_nil_ptr__)
           (AND !$01)      ;; check tagging
           (BEQ found_cell_ptr__) ;; found cell ptr, make sure to store (y-2)>>1 into array len, store ptr to next incremental-collectable!!

    (label loop_nil_ptr__)
           (DEY)
           (DEY)
           (CPY !$03) ;; first slot is already checked
           (BPL tight_loop__)

    (label no_cells_left_for_scanning__)
           ;; fully reclaimed

           ;; is this the head of the inc collectible list?
           ;; check page first? or check offset on page firs? which is faster?
           (LDA ZP_RZ+1)
           (CMP ZP_INC_COLLECTIBLE_LIST+1)
           (BNE not_head_of_collectible_list__)
           (LDA ZP_RZ)
           (CMP ZP_INC_COLLECTIBLE_LIST)
           (BNE not_head_of_collectible_list__)

           ;; dequeue
           (LDY !$02)
           (LDA (ZP_RZ),y)
           (STA ZP_INC_COLLECTIBLE_LIST)
           (INY)
           (LDA (ZP_RZ),y)
           (STA ZP_INC_COLLECTIBLE_LIST+1)

    (label not_head_of_collectible_list__)
           (JMP FREE_M1_SLOT_FROM_RZ)

    (label found_cell_ptr__)
           ;; remember current cell-ptr in array
           (LDA (ZP_RZ),y)
           (TAX)
           (INY)
           (LDA (ZP_RZ),y)
           (PHA)

           ;; store new length (remaining cells to inspect)
           (TYA)
           (SEC)
           (SBC !$03)

           ;; === { optimize for cell-arrays with only one cell-ptr
           (CMP !$02)
           (BNE cells_left_to_inspect__)

           ;; if this was the first call to inc_gc'd, then just free and dec ref count, no dequeue necessary
           (LDA EXEC_OPTIMIZED_LAST_SLOT_FREE__)
           (BEQ cells_left_to_inspect__)

           ;; no more cells to inspect, all done for this cell

           (TXA)
           (PHA) ;; now stack holds lb (head) :: hb :: ... of cell to be dec_refcnt'd

           ;; free ZP_RZ
           ;; (JSR FREE_M1_SLOT_FROM_RZ) ;; does not run any DEC_REFCNT => no recursion possible, uses ZP_TEMP
           (JSR no_cells_left_for_scanning__) ;; includes possible dequeue

           ;; now dec refcnt the last (remembered) cell
           (PLA)
           (STA ZP_RZ)
           (PLA)
           (STA ZP_RZ+1)
           (JMP DEC_REFCNT_M1_SLOT_RZ) ;; tail call
           ;; === }

    (label cells_left_to_inspect__)
           (LSR)
           (LDY !$01)
           (STA (ZP_RZ),y) ;; store new length (less then before)


           ;; is the first cell a ptr (then it is already is in the list of collectibles), else it must be listed
           (LDY !$02)
           (LDA (ZP_RZ),y)
           (AND !$01)
           (BEQ already_on_the_collectible_list__)

    (label cons_to_collectible_list__)
           ;; store old head
           (LDA ZP_INC_COLLECTIBLE_LIST)
           (STA (ZP_RZ),y)
           (INY)
           (LDA ZP_INC_COLLECTIBLE_LIST+1)
           (STA (ZP_RZ),y)

           ;; store zp_rz as new head
           (LDA ZP_RZ)
           (STA ZP_INC_COLLECTIBLE_LIST)
           (LDA ZP_RZ+1)
           (STA ZP_INC_COLLECTIBLE_LIST+1)

    (label already_on_the_collectible_list__)
           ;; store remembered cell-ptr into rz
    (label dec_refcnt_remembered_cell_ptr__)
           (PLA)
           (STA ZP_RZ+1)
           (TXA)
           (STA ZP_RZ)
           ;; dec refcount of cell pointed to
           (JMP DEC_REFCNT_M1_SLOT_RZ) ;; tail-call

    (label EXEC_OPTIMIZED_LAST_SLOT_FREE__)
           (byte 0)))

(module+ test #| dec_refcnt_m1_slot_rz_n |#
  (define dec_refcnt_m1_slot_rz_n-test-non-cell-array-slot
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code (append test-runtime
                            ALLOC_M1_SLOT_TO_RA
                            INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX
                            FREE_M1_SLOT_FROM_RZ
                            INC_REFCNT_M1_SLOT_RT
                            DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"
                (LDA !20)
                (JSR ALLOC_M1_SLOT_TO_RA)
                (LDY !$01)
                (LDA !$40) ;; 01xx xxxx is not a cell array!
                (STA (ZP_RA),y) ;; set type
                (LDA ZP_RA)
                (STA ZP_RZ)
                (LDA ZP_RA+1)
                (STA ZP_RZ+1)

                (JSR $0100)
                (JSR DEC_REFCNT_M1_SLOT_RZ__IF_PTR)))

  (inform-check-equal?
   (code-len DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
   204
   "code len of dec refcnt and gc are n bytes")

  (inform-check-equal?
   (cpu-state-clock-cycles dec_refcnt_m1_slot_rz_n-test-non-cell-array-slot)
   102
   "it takes n clock cycles to dec refcnt and free an allocated slot immediately")

  (check-equal?
   (list (memory-list dec_refcnt_m1_slot_rz_n-test-non-cell-array-slot (+ 2 PAGE_AVAIL_0_W))
         (memory-list dec_refcnt_m1_slot_rz_n-test-non-cell-array-slot ZP_INC_COLLECTIBLE_LIST)
         (memory-list dec_refcnt_m1_slot_rz_n-test-non-cell-array-slot (+ #xfe PAGE_AVAIL_0_W) (+ #xff PAGE_AVAIL_0_W)))
   (list (list #x1a)                               ;; next free slot pointed to by @02 (first slot) will be $1a (again)
         (list 0)                                  ;; head of inc collectible is (still) 0
         (list #x02 #x00))                         ;; next free slot on this page is @02 (again)
   "dec refcnt of a ptr to a non cell-array will free it immediately"))

(module+ test #| collect cell array with only n nils |#
  (define (dec_refcnt_m1_slot_rz_n-test-cell-array-slot-wo-cell-ptrs n-nils)
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code (append test-runtime
                            ALLOC_M1_SLOT_TO_RA
                            INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX
                            FREE_M1_SLOT_FROM_RZ
                            INC_REFCNT_M1_SLOT_RT
                            DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"
                (LDA !20)
                (JSR ALLOC_M1_SLOT_TO_RA)
                (LDY !$01)
                ;; (LDA !$0a) ;; 00xx xxxx is a cell array! with xx xxxx = 0a = 10 cells
                (ast-opcode-cmd '() `(169 ,n-nils))
                (STA (ZP_RA),y) ;; set type

                (ASL A)
                (TAY)
                (INY)
                (LDA !$00)      ;; nil
         (label init_loop_nils)
                (STA (ZP_RA),y)
                (DEY)
                (CPY !$01)
                (BNE init_loop_nils)

                (LDA ZP_RA)
                (STA ZP_RZ)
                (LDA ZP_RA+1)
                (STA ZP_RZ+1)

                (JSR $0100)
                (JSR DEC_REFCNT_M1_SLOT_RZ__IF_PTR)))

  (inform-check-equal?
   (cpu-state-clock-cycles (dec_refcnt_m1_slot_rz_n-test-cell-array-slot-wo-cell-ptrs 1))
    140               ;; cucles
    "freeing a cell-array with 1 nil takes n cycles")

  (inform-check-equal?
   (cpu-state-clock-cycles (dec_refcnt_m1_slot_rz_n-test-cell-array-slot-wo-cell-ptrs 10))
    295               ;; cucles
    "freeing a cell-array with 10 nils takes n cycles")

  (check-equal?
   (list
         (memory-list (dec_refcnt_m1_slot_rz_n-test-cell-array-slot-wo-cell-ptrs 1) (+ 2 PAGE_AVAIL_0_W))
         (memory-list (dec_refcnt_m1_slot_rz_n-test-cell-array-slot-wo-cell-ptrs 1) ZP_INC_COLLECTIBLE_LIST)
         (memory-list (dec_refcnt_m1_slot_rz_n-test-cell-array-slot-wo-cell-ptrs 1) (+ #xfe PAGE_AVAIL_0_W) (+ #xff PAGE_AVAIL_0_W)))
   (list (list #x1a)       ;; next free slot points to 1a
         (list 0)          ;; no collectible was registered (still empty)
         (list #x02 #x00))  ;; first free slot on page is $02 (again), next page is $00
   "dec refcnt of a ptr to a cell-array with just atomic cells (or nil-s) will free it immediately"))

(module+ test #| collect call-array with one cell-ptr |#
  (define dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-one-cell-ptrs
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code (append test-runtime
                            ALLOC_M1_SLOT_TO_RA
                            INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX
                            FREE_M1_SLOT_FROM_RZ
                            INC_REFCNT_M1_SLOT_RT
                            DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"

                (LDA !20)
                (JSR ALLOC_M1_SLOT_TO_RA)
                (LDA ZP_RA)
                (STA ZP_RZ)
                (LDA ZP_RA+1)
                (STA ZP_RZ+1)

                (LDA !20)
                (JSR ALLOC_M1_SLOT_TO_RA)

                (LDY !$01)
                (LDA !$02) ;; 00xx xxxx is a cell array! with xx xxxx = 2 cells
                (STA (ZP_RA),y) ;; set type

                ;; set cell-array to hold only nil ptrs
                (ASL A)
                (TAY)
                (INY)
                (LDA !$00)      ;; nil
         (label init_loop_nils)
                (STA (ZP_RA),y)
                (DEY)
                (CPY !$01)
                (BNE init_loop_nils)

                ;; set second cell in cell-array to point to the previously allocated (@02)
                (LDY !$04)
                (LDA ZP_RZ)
                (STA (ZP_RA),y)
                (INY)
                (LDA ZP_RZ+1)
                (STA (ZP_RA),y)

                (LDA ZP_RA)
                (STA ZP_RZ)
                (LDA ZP_RA+1)
                (STA ZP_RZ+1)

                (JSR $0100)
                (JSR DEC_REFCNT_M1_SLOT_RZ__IF_PTR)))

  (check-equal?
   (list
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-one-cell-ptrs (+ #x1a PAGE_AVAIL_0_W))
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-one-cell-ptrs ZP_INC_COLLECTIBLE_LIST)
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-one-cell-ptrs (+ #xfe PAGE_AVAIL_0_W) (+ #xff PAGE_AVAIL_0_W)))
   (list (list #x32)       ;; next free slot points to $32
         (list 0)          ;; no collectible was registered (still empty)
         (list #x02 #x00))  ;; first free slot on page is $1a (again), next page is $00
   "dec refcnt of a ptr to a cell-array with just one ptr in second position, will free immediately"))

(module+ test #| collect cell-array slot with first cell being a cell-ptr |#
  (define dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-first-cell-ptr
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code (append test-runtime
                            ALLOC_M1_SLOT_TO_RA
                            INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX
                            FREE_M1_SLOT_FROM_RZ
                            INC_REFCNT_M1_SLOT_RT
                            DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"

                (LDA !20)
                (JSR ALLOC_M1_SLOT_TO_RA)
                (LDA ZP_RA)
                (STA ZP_RZ)
                (LDA ZP_RA+1)
                (STA ZP_RZ+1)

                (LDA !20)
                (JSR ALLOC_M1_SLOT_TO_RA)

                (LDY !$01)
                (LDA !$01) ;; 00xx xxxx is a cell array! with xx xxxx = 1 cell
                (STA (ZP_RA),y) ;; set type

                ;; set first cell in cell-array to point to the previously allocated (@02)
                (LDY !$02)
                (LDA ZP_RZ)
                (STA (ZP_RA),y)
                (INY)
                (LDA ZP_RZ+1)
                (STA (ZP_RA),y)

                (LDA ZP_RA)
                (STA ZP_RZ)
                (LDA ZP_RA+1)
                (STA ZP_RZ+1)

                (JSR $0100)
                (JSR DEC_REFCNT_M1_SLOT_RZ__IF_PTR)))

  (check-equal?
   (list
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-first-cell-ptr (+ #x1a PAGE_AVAIL_0_W) (+ #x1d PAGE_AVAIL_0_W))
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-first-cell-ptr ZP_INC_COLLECTIBLE_LIST (+ 1 ZP_INC_COLLECTIBLE_LIST))
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-first-cell-ptr (+ #xfe PAGE_AVAIL_0_W) (+ #xff PAGE_AVAIL_0_W))
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-first-cell-ptr (+ #x02 PAGE_AVAIL_0_W)))
   (list (list #x00 #x01 #x00 #x00)   ;; refcount=0, to scan = 1, collectible next = 00 00 (since tail)
         (list #x1a #xcf)   ;; this cell-array is the head of the collectibles
         (list #x02 #x00)   ;; first free slot on page is $02 freed, $1a stiil in use, next page is $00
         (list #x32))       ;; slot 02 points to next free $32
   "dec refcnt of a ptr to a cell-array with just one ptr in first position, will not free but enqueue"))

(module+ test #| first cell ptr incremental collect |#
  (define dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-first-cell-ptr-inc-collect
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code (append test-runtime
                            ALLOC_M1_SLOT_TO_RA
                            INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX
                            FREE_M1_SLOT_FROM_RZ
                            INC_REFCNT_M1_SLOT_RT
                            DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"

                (LDA !20)
                (JSR ALLOC_M1_SLOT_TO_RA)
                (LDA ZP_RA)
                (STA ZP_RZ)
                (LDA ZP_RA+1)
                (STA ZP_RZ+1)

                (LDA !20)
                (JSR ALLOC_M1_SLOT_TO_RA)

                (LDY !$01)
                (LDA !$01) ;; 00xx xxxx is a cell array! with xx xxxx = 1 cell
                (STA (ZP_RA),y) ;; set type

                ;; set first cell in cell-array to point to the previously allocated (@02)
                (LDY !$02)
                (LDA ZP_RZ)
                (STA (ZP_RA),y)
                (INY)
                (LDA ZP_RZ+1)
                (STA (ZP_RA),y)

                (LDA ZP_RA)
                (STA ZP_RZ)
                (LDA ZP_RA+1)
                (STA ZP_RZ+1)

                (JSR DEC_REFCNT_M1_SLOT_RZ__IF_PTR)

                (LDA ZP_INC_COLLECTIBLE_LIST)
                (STA ZP_RZ)
                (LDA ZP_INC_COLLECTIBLE_LIST+1)
                (STA ZP_RZ+1)

                (JSR $0100)
                (JSR INC_GC_M1_SLOT_RZ_CELL_ARRAY)))

  (inform-check-equal?
   (cpu-state-clock-cycles dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-first-cell-ptr-inc-collect)
   112
   "freeing an array incrementally for which no further cells need to be freed")

  (check-equal?
   (list
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-first-cell-ptr-inc-collect (+ #x1a PAGE_AVAIL_0_W) )
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-first-cell-ptr-inc-collect ZP_INC_COLLECTIBLE_LIST (+ 1 ZP_INC_COLLECTIBLE_LIST))
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-first-cell-ptr-inc-collect (+ #xfe PAGE_AVAIL_0_W) (+ #xff PAGE_AVAIL_0_W))
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-first-cell-ptr-inc-collect (+ #x02 PAGE_AVAIL_0_W)))
   (list (list #x02)   ;; next free slot is $02
         (list #x00 #x00)   ;; collectibles is empty again
         (list #x1a #x00)   ;; first free slot on page is $1a, next page is $00
         (list #x32))       ;; slot 02 points to next free $32
   "dec refcnt of a ptr to a cell-array with just one ptr in first position, will not free but enqueue, then inc gc will collect it"))

(module+ test #| collect cell-array with several cell ptrs doing incremental collections |#
  (define dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-cell-ptrs-inc-collect
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code (append test-runtime
                            ALLOC_M1_SLOT_TO_RA
                            INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX
                            FREE_M1_SLOT_FROM_RZ
                            INC_REFCNT_M1_SLOT_RT
                            DEC_REFCNT_M1_SLOT_RZ__IF_PTR)
     #:init-label "VM_INIT_PAGE_MEMORY_MANAGER_N20"

                (LDA !20)
                (JSR ALLOC_M1_SLOT_TO_RA)

                (LDA ZP_RA)
                (STA ZP_RZ)
                (LDA ZP_RA+1)
                (STA ZP_RZ+1)

                (LDY !$01)
                (LDA !$03) ;; 00xx xxxx is a cell array! with xx xxxx = 3 cell
                (STA (ZP_RZ),y) ;; set type

                ;; set first cell in cell-array to point to the previously allocated (@02)
                (LDA !20)
                (JSR ALLOC_M1_SLOT_TO_RA)

                (LDY !$02)
                (LDA ZP_RA)
                (STA (ZP_RZ),y)
                (INY)
                (LDA ZP_RA+1)
                (STA (ZP_RZ),y)

                ;; set second cell to simple byte cell
                (LDY !$04)
                (LDA !$ff)
                (STA (ZP_RZ),y)
                (LDA !$01)
                (INY)
                (STA (ZP_RZ),y)

                ;; set third cell to allocated cell ptr
                (LDA !20)
                (JSR ALLOC_M1_SLOT_TO_RA)

                (LDY !$06)
                (LDA ZP_RA)
                (STA (ZP_RZ),y)
                (INY)
                (LDA ZP_RA+1)
                (STA (ZP_RZ),y)

                ;; dec ref count and do first free (should free frist cell then enqueue for incremental)
                (JSR $0100)
                (JSR DEC_REFCNT_M1_SLOT_RZ__IF_PTR)


                ;; first incremental free call (free third cell (being a ptr) and return (keep this ptr enqueued))
                (LDA ZP_INC_COLLECTIBLE_LIST)
                (STA ZP_RZ)
                (LDA ZP_INC_COLLECTIBLE_LIST+1)
                (STA ZP_RZ+1)

                (JSR INC_GC_M1_SLOT_RZ_CELL_ARRAY)

                ;; second incremental free call (should free the whole array now and return)
                (LDA ZP_INC_COLLECTIBLE_LIST)
                (STA ZP_RZ)
                (STA ZP_RA) ;; keep for testing
                (LDA ZP_INC_COLLECTIBLE_LIST+1)
                (STA ZP_RZ+1)
                (STA ZP_RA+1) ;; keep for testing

                (JSR INC_GC_M1_SLOT_RZ_CELL_ARRAY)))

  (check-equal?
   (list
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-cell-ptrs-inc-collect ZP_INC_COLLECTIBLE_LIST (+ 1 ZP_INC_COLLECTIBLE_LIST))
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-cell-ptrs-inc-collect (+ #xfe PAGE_AVAIL_0_W) (+ #xff PAGE_AVAIL_0_W))
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-cell-ptrs-inc-collect (+ #x4a PAGE_AVAIL_0_W))
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-cell-ptrs-inc-collect (+ #x32 PAGE_AVAIL_0_W))
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-cell-ptrs-inc-collect (+ #x1a PAGE_AVAIL_0_W))
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-cell-ptrs-inc-collect (+ #x02 PAGE_AVAIL_0_W))
    (memory-list dec_refcnt_m1_slot_rz_n-test-cell-array-slot-w-cell-ptrs-inc-collect ZP_RA (+ 1 ZP_RA))
    )
   (list
    (list 0 0)          ;; incremental collectible are back to 00 (empty list)
    (list #x02 00)      ;; latest free slot is $02, so this is the next free slot of the page
    (list #x62)         ;; slot $4a (untouched) still points to $62
    (list #x1a)         ;; slot $32 -> $1a since it is freed second (pointing to freed first)
    (list #x4a)         ;; slot $1a -> $4a since it is freed first (pointing to page free)
    (list #x32)         ;; slot $02 -> $32 since it is free last (pointing to freed second)
    (list #x02 #xcf))   ;; slot that is incrementally collected as a last step
   "alloc cell-array with 3 cells, 1st and 3rd a cell-ptr, 2nd a byte, dec refcount, the incfree and another inc free"))

;; get head of current incremental collectible list and do an increment gc on that cell-array
;;
;; input:  ZP_INC_COLLECTIBLE_LIST (word)
;; output:
;;
;; variant: INC_GC_ARRAYS_LB_IN_A
;; input:   A = lowbyte of collectible list
;;          ZP_INC_COLLECTRIBLE_LIST
(define INC_GC_ARRAYS_LB_IN_A '())
(define-vm-function INC_GC_ARRAYS
   (list
           (LDA ZP_INC_COLLECTIBLE_LIST)
           (BEQ nothing_to_collect__) ;; actually an error, should not be necessary to check! this should be checked by the caller before the call
    (label INC_GC_ARRAYS_LB_IN_A)
           (STA ZP_RZ)
           (LDA ZP_INC_COLLECTIBLE_LIST+1)
           (STA ZP_RZ+1)
           (JMP INC_GC_M1_SLOT_RZ_CELL_ARRAY)

    (label nothing_to_collect__)
           (RTS)))

;; keep calling incremental gc on cell-arrays until no more arrays are available for collecting
;;
;; input:  ZP_INC_COLLECTIBLE_LIST
;; output: ZP_INC_COLLECTIBLE_LIST = 0
(define-vm-function-wol GC_ALL_ARRAYS
   (list
    (label GC_ALL)
    (label GC_ALL_ARRAYS)
           (LDA ZP_INC_COLLECTIBLE_LIST)
           (BEQ done__)
           (JSR INC_GC_ARRAYS_LB_IN_A)
           (JMP GC_ALL_ARRAYS) ;; tail call
    (label done__)
           (RTS)))

(module+ test #| gc_all_arrays |#)

;; allocate a slot of min A size, allocating a new page if necessary
;; input:  A = size
;; usage:  A, X, Y, RB, GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; output: RB = available slot of the given size (or a bit more)
;;         Y = actual size
;;         GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; funcs:
;;   VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS
;;   ALLOC_PAGE_TO_X
;;   INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX
;;   ALLOC_M1_SLOT_TO_RA,
;;   SWAP_RA_RB
(define-vm-function ALLOC_M1_SLOT_TO_RB
   (list
           (JSR CP_RA_TO_RB)
           (JSR ALLOC_M1_SLOT_TO_RA)
           ;; alternative swap implementation
           ;; (LDA !ZP_RA)
           ;; (LDX !ZP_RB)
           ;; (JMP SWAP_ZP_WORD)
           (JMP SWAP_RA_RB)))


(define vm-m1-slot-code
  (append ALLOC_M1_SLOT_TO_RA
          ALLOC_M1_SLOT_TO_RB
          ALLOC_M1_SLOT_TO_RT
          ALLOC_M1_P0_SLOT_TO_RT
          INIT_M1Px_PAGE_RZ_PROFILE_X_TO_AX
          FREE_M1_SLOT_FROM_RZ
          INC_REFCNT_M1_SLOT_RT
          DEC_REFCNT_M1_SLOT_RZ__IF_PTR
          DEC_REFCNT_M1_SLOT_RA
          DEC_REFCNT_M1_SLOT_RB
          DEC_REFCNT_M1_SLOT_RC
          INC_GC_ARRAYS
          GC_ALL_ARRAYS))

(module+ test #| code len of module |#
  (inform-check-equal?
   (code-len vm-m1-slot-code)
   650
   "the whole module taks about n bytes"))

#lang racket/base

#|

  functions for m1 pages and slots

|#


(require "../6510.rkt"
         (only-in "../ast/6510-resolver.rkt"
                  add-label-suffix
                  replace-labels)
         (only-in "./vm-memory-map.rkt"
                  TAGGED_NIL
                  ZP_RP
                  ZP_RA
                  ZP_RB
                  ZP_TEMP
                  ZP_RT
                  VM_MEMORY_MANAGEMENT_CONSTANTS))

(provide INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX       ;; initialize m1 page in x of profile y, returning first free slot in A/X
         DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A   ;; remove all full pages at the head of m1 page list a
         PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ         ;; put the page of m1 slot in rz as head to the m1 page list
         ADD_M1_SLOT_RZ_TO_PFL                  ;; add the given m1 slot in rz to the page free list (slot must not contain any data that needs gc)
         ALLOC_M1_SLOT_TO_RA                    ;; allocate a m1 slot into ra (size wanted in a)
         ALLOC_M1_SLOT_TO_RB                    ;; allocate a m1 slot into ra (size wanted in a)
         FREE_M1_SLOT_RA                        ;; free the m1 slot reference in RA (must not contain any data that need gc)
         FREE_M1_SLOT_RZ                        ;; free the m1 slot reference in RZ (must not contain any data that need gc)
         VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS      ;; remove full pages for the list of m1 slot page
         VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS   ;; enqueue the page of m1 slot RA as head of the list of m1 slot pages
         INC_REFCNT_M1_SLOT_RA                  ;; increment reference count of m1 slot pointer in RA
         VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS)  ;; remove full pages from the list of m1 slot pages of type x

(module+ test
  (require ;; (only-in racket/list make-list)
           "../6510-test-utils.rkt"
           (only-in "../tools/6510-interpreter.rkt"
                    peek
                    memory-list
                    cpu-state-clock-cycles)
           (only-in "../util.rkt" format-hex-byte format-hex-word)
           (only-in "./vm-inspector-utils.rkt" vm-page->strings)
           "./vm-memory-manager-test-utils.rkt"
           (only-in "./vm-mm-pages.rkt"
                    ALLOC_PAGE_TO_X
                    VM_PAGE_SLOT_DATA
                    VM_INITIAL_MM_REGS
                    VM_INITIALIZE_MEMORY_MANAGER)
           (only-in "./vm-mm-register-functions.rkt"
                    CP_RT_TO_RA
                    CP_RA_TO_RT
                    CP_RA_TO_RB
                    SWAP_RA_RB
                    SWAP_ZP_WORD))

  (define PAGE_AVAIL_0 #x8d)      ;; high byte of first page available for allocation
  (define PAGE_AVAIL_0_W #x8d00)  ;; word (absolute address) of first page available
  (define PAGE_AVAIL_1 #x8c)      ;; high byte of second page available for allocation
  (define PAGE_AVAIL_1_W #x8c00) ;; word (absolute address) of second page available

  (define test-runtime
    (append
     INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX
     ALLOC_M1_SLOT_TO_RA
     ALLOC_M1_SLOT_TO_RB
     VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS
     FREE_M1_SLOT_RA
     VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS
     INC_REFCNT_M1_SLOT_RA

     ALLOC_PAGE_TO_X
     CP_RA_TO_RT
     CP_RT_TO_RA
     CP_RA_TO_RB
     SWAP_RA_RB
     SWAP_ZP_WORD
     VM_INITIALIZE_MEMORY_MANAGER
     VM_MEMORY_MANAGEMENT_CONSTANTS
     (list (label INIT_CELLSTACK_PAGE_X) (RTS))
     (list (org #xcec0))
     VM_INITIAL_MM_REGS
     (list (org #xcf00))
     VM_PAGE_SLOT_DATA)))

;; ----------------------------------------
;; @DC-FUN: INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX, group: m1_slot
;; page type slot w/ different sizes (refcount @ ptr-1) x cells
;; math: first entry @FIRST_REF_COUNT_OFFSET__INIT_M1Px_PAGE_A + 1, refcount @ -1, next slot += INC_TO_NEXT_SLOT__INIT_M1Px_PAGE_A, slot-size = INC_TO_NEXT_SLOT__INIT_M1Px_PAGE_A -1
;; input : Y = profile offset (0, 2, 4 ...)
;;         X = page
;; uses  : A, X, Y, RZ
;; output: X = page, initialized as m1 page of profile y
;;         A = first free slot
;; funcs:  -
(define INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX
  (add-label-suffix
   "__" "INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX"
  (list
   (label INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX)
          (STY SEL_PROFILE__)      ;; save profile index in local var
          (TYA)                 ;; profile 0..4 -> a
          (STX ZP_RZ+1)

          (LDY !$00)
          (STY ZP_RZ)

          (TAX)
          (ORA !$10)
          (STA (ZP_RZ),y)       ;; set page type in byte 0 to b0001 <profile>

          (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x) ;; current free page
          (INY)
          (STA (ZP_RZ),y)          ;; store previous page

          (LDA ZP_RZ+1)
          (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x) ;; set page with free slots to this allocated page

          (LDA !$00)
          (INY)
          (STA (ZP_RZ),y)          ;; store number of slots used

          (LDY TABLE__FIRST_REF_COUNT_OFFSET__,x) ;; y = refcount field for first slot
          (INY)
          (TYA)
          (PHA)
          (LDX ZP_RZ+1)
          (STA VM_PAGE_SLOT_DATA,x)                    ;; set first free slot, here x = page
          (DEY)
          (LDX SEL_PROFILE__) ;; profile = 0..3
          (LDA !$00)

          ;; loop to initialize refcounts of each slot to 0-
   (label REF_COUNT_LOOP__)
          (STA (ZP_RZ),y) ;; refcount = 0
          (TYA)
          (CLC)
          (ADC TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE,x) ;; calc next refcount field offset
          (BCS END_REF_COUNT_LOOP__)
          (TAY)
          ;; (ADC !$01)
          (LDA !$00)
          (BCC REF_COUNT_LOOP__) ;; still on this page?

   (label END_REF_COUNT_LOOP__)
          ;; loop to write free slot list
          (LDY TABLE__FIRST_REF_COUNT_OFFSET__,x)
          (INY)  ;; first slot  (refcount field offset + 1)
          (TYA)
   (label WRITE_FREE_LIST__)
          (CLC)
          (ADC TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE,x)
          (BCS ALMOST_DONE__) ;; no longer on the same page => almost done
          (STA (ZP_RZ),y) ;; offset of next free cell == y for next write
          (TAY)
          (BCC WRITE_FREE_LIST__) ;; carry must be clear => always jump

   (label ALMOST_DONE__)
          (LDA !$00)
          (STA (ZP_RZ),y) ;; last offset to next free slot is 00 = no next free slot!
          (LDX ZP_RZ+1)   ;; x = page
          (PLA)           ;; A = first free slot
          (RTS)

   (label SEL_PROFILE__)
          (byte $00) ;; local var

   (label TABLE__FIRST_REF_COUNT_OFFSET__)
          (byte $03) ;; first ref count is 03, add 0a to get to next slot, slot-size $09 (09), page contains 25 slots
          (byte $03) ;; first ref count is 03, add 12 to get to next slot, slot size $11 (17), page contains 14 slots
          (byte $0f) ;; first ref count is 0f, add 1e to get to next slot, slot size $1d (29), page contains 8 slots
          (byte $05) ;; first ref count is 05, add 32 to get to next slot, slot-size $31 (49), page contains 5 slots
          (byte $03) ;; first ref count is 03, add 54 to get to next slot, slot-size $53 (83), page contains 3 slots
   (label TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE)
          (byte $0a) ;; add 0a to get to next slot, slot-size $09 (09), page contains 25 slots
          (byte $12) ;; add 12 to get to next slot, slot size $11 (17), page contains 14 slots
          (byte $1e) ;; add 1e to get to next slot, slot size $1d (29), page contains 8 slots
          (byte $32) ;; add 32 to get to next slot, slot-size $31 (49), page contains 5 slots
          (byte $54) ;; add 54 to get to next slot, slot-size $53 (83), page contains 3 slots
          )))

(module+ test #| vm_alloc_m1_page |#
  (define test-alloc-m1-01-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_01_CODE)
            (DEX)
            (ast-opcode-cmd '() (list 157 0 PAGE_AVAIL_0)) ;; (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_M1_01_CODE)

            ;; now allocate the page
            (JSR ALLOC_PAGE_TO_X)
            (LDY !$01) ;; do it explicitly: profile 1
            (JSR INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX)))

  (check-equal? (vm-page->strings test-alloc-m1-01-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p1"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $04"))
  (check-equal? (memory-list test-alloc-m1-01-state-after (+ PAGE_AVAIL_0_W #x03) (+ PAGE_AVAIL_0_W #x04))
                (list #x00 #x16)
                "slot0: refcount 0, next free slot at offset $16")
  (check-equal? (memory-list test-alloc-m1-01-state-after (+ PAGE_AVAIL_0_W #x15) (+ PAGE_AVAIL_0_W #x16))
                (list #x00 #x28)
                "slot1: refcount 0, next free slot at offset $28")
  (check-equal? (memory-list test-alloc-m1-01-state-after (+ PAGE_AVAIL_0_W #x27) (+ PAGE_AVAIL_0_W #x28))
                (list #x00 #x3a)
                "slot2: refcount 0, next free slot at offset $28")
  (check-equal? (memory-list test-alloc-m1-01-state-after (+ PAGE_AVAIL_0_W #xed) (+ PAGE_AVAIL_0_W #xee))
                (list #x00 #x00)
                "slot13: refcount 0, next free slot at offset $00 = no next")

  (define test-alloc-m1-02-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
          ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_02_CODE)
            (DEX)
            (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_M1_02_CODE)

            ;; now allocate the page
            (JSR ALLOC_PAGE_TO_X)
            (LDY !$02) ;; do it explicitly
            (JSR INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX)))

  (check-equal? (vm-page->strings test-alloc-m1-02-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p2"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $10"))
  (check-equal? (memory-list test-alloc-m1-02-state-after (+ PAGE_AVAIL_0_W #x0f) (+ PAGE_AVAIL_0_W #x10))
                (list #x00 #x2e)
                "slot0: refcount 0, next free slot at offset $2c")
  (check-equal? (memory-list test-alloc-m1-02-state-after (+ PAGE_AVAIL_0_W #x2d) (+ PAGE_AVAIL_0_W #x2e))
                (list #x00 #x4c)
                "slot1: refcount 0, next free slot at offset $4a")
  (check-equal? (memory-list test-alloc-m1-02-state-after (+ PAGE_AVAIL_0_W #x4b) (+ PAGE_AVAIL_0_W #x4c))
                (list #x00 #x6a)
                "slot2: refcount 0, next free slot at offset $68")
  (check-equal? (memory-list test-alloc-m1-02-state-after (+ PAGE_AVAIL_0_W #xe1) (+ PAGE_AVAIL_0_W #xe2))
                (list #x00 #x00)
                "slot7: refcount 0, next free slot at offset $00 = no next")

  (define test-alloc-m1-03-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_03_CODE)
            (DEX)
            (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_M1_03_CODE)

            ;; now allocate the page
            (JSR ALLOC_PAGE_TO_X)
            (LDY !$03) ;; do it explicitly
            (JSR INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX)))

  (check-equal? (vm-page->strings test-alloc-m1-03-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p3"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $06"))
  (check-equal? (memory-list test-alloc-m1-03-state-after (+ PAGE_AVAIL_0_W #x05) (+ PAGE_AVAIL_0_W #x06))
                (list #x00 #x38)
                "slot0: refcount 0, next free slot at offset $38")
  (check-equal? (memory-list test-alloc-m1-03-state-after (+ PAGE_AVAIL_0_W #x37) (+ PAGE_AVAIL_0_W #x38))
                (list #x00 #x6a)
                "slot1: refcount 0, next free slot at offset $6a")
  (check-equal? (memory-list test-alloc-m1-03-state-after (+ PAGE_AVAIL_0_W #x69) (+ PAGE_AVAIL_0_W #x6a))
                (list #x00 #x9c)
                "slot2: refcount 0, next free slot at offset $9c")
  (check-equal? (memory-list test-alloc-m1-03-state-after (+ PAGE_AVAIL_0_W #xcd) (+ PAGE_AVAIL_0_W #xce))
                (list #x00 #x00)
                "slot4: refcount 0, next free slot at offset $00 = no next")

  (define test-alloc-m1-04-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_M1_04_CODE)
            (DEX)
            (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_M1_04_CODE)

            ;; now allocate the page
            (JSR ALLOC_PAGE_TO_X)
            (LDY !$04) ;; do it explicitly
            (JSR INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX)))

  (check-equal? (memory-list test-alloc-m1-04-state-after (+ PAGE_AVAIL_0_W #x00) (+ PAGE_AVAIL_0_W #x02))
                (list #x14 #x00 #x00)
                "page type $13, previous page = $00, slot number used = $00")
  (check-equal? (memory-list test-alloc-m1-04-state-after (+ PAGE_AVAIL_0_W #x03) (+ PAGE_AVAIL_0_W #x04))
                (list #x00 #x58)
                "slot0: refcount 0, next free slot at offset $56")
  (check-equal? (memory-list test-alloc-m1-04-state-after (+ PAGE_AVAIL_0_W #x57) (+ PAGE_AVAIL_0_W #x58))
                (list #x00 #xac)
                "slot1: refcount 0, next free slot at offset $aa")
  (check-equal? (memory-list test-alloc-m1-04-state-after (+ PAGE_AVAIL_0_W #xab) (+ PAGE_AVAIL_0_W #xac))
                (list #x00 #x00)
                "slot2: refcount 0, next free slot at offset $00 = no next")
  (check-equal? (vm-page->strings test-alloc-m1-04-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p4"
                  "previous page:  $00"
                  "slots used:     0"
                  "next free slot: $04")))

;; impl complete, test missing
;; @DC-FUN: ADD_M1_SLOT_RZ_TO_PFL, group: m1_slot
;; add the given m1 slot in RZ back to the page free list of slots
;; input:  RZ, page-meta-data
;; usage:  A, X, Y, RZ
;; output: page-meta-data
;; funcs:
;;   DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_RZ
;;   PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ
(define ADD_M1_SLOT_RZ_TO_PFL
  (add-label-suffix
   "__" "__NEW_ADD_M1_SLOT_RZ_TO_PFL"
   (list
    (label ADD_M1_SLOT_RZ_TO_PFL)
           (JSR DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_RZ)

          ;; now free the slot
   (label REGULAR_FREE__)
          (LDX ZP_RZ+1)
          (STX DEC_CMD__+2)          ;; write page for later dec execution
          (LDA VM_PAGE_SLOT_DATA,x)  ;; first free slot offset
          (BNE CONTINUE__)           ;; regular free

          ;; this page was full (since next free slot was 0) => register with the list of pages with free slots
          (JSR PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ)
          (LDX DEC_CMD__+2)          ;; restore x
          (LDA !$00)                 ;; next free slot offset (=0)

   (label CONTINUE__)
          (LDY !$00)
          (STA (ZP_RZ),y)            ;; set (zp_ptr) = previous free
          (LDA ZP_RZ)                ;; low byte of pointer = new free slot
          (STA VM_PAGE_SLOT_DATA,x)  ;; set new first free slot offset

          (DEC ZP_RZ)                ;; now points to ref count
          (TYA)                      ;; y is still 0 => a := 0
          (STA (ZP_RZ),y)            ;; set refcount := 0

   (label DEC_CMD__)                 ;; decrement number of slots used on the page
          (DEC $c002)                ;; $c0 is overwritten, 02 = location of used slots on that page type

          (RTS))))

;; @DC-FUN: PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ, group: m1_slot
;; put this page to the head of free m1 pages of the same profile as RZ is
;; input:  RZ, GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; usage:  A, X, Y, RZ
;; output: GLOBAL_M1_PX_PAGE_FOR_ALLOC
;;         A = first free slot of that page
;; funcs:  -
(define PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ
  (add-label-suffix
   "__" "__NEW_PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ"
  (list
   (label PUT_PAGE_AS_HEAD_OF_M1_PAGE_RZ)
          (LDA ZP_RZ)
          (STA ZP_TEMP) ;; keep for later

          (LDA !$00)    ;; set to zero
          (STA ZP_RZ)

          (LDY !$01)
          (LDA (ZP_RZ),y) ;; get previous
          (BNE CONTINUE_WITH_RESTORE__)     ;; is != 0 => is still part of the list, don't change the list
          ;; is no longer part of the free list of pages, add this page at the head of the page

          (DEY) ;; now 0
          (LDA (ZP_RZ),y) ;; get encoded page type
          (AND !$07)

          (TAX) ;; now x = page type

          (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x)

          (INY) ;; now 1
          (STA (ZP_RZ),y) ;; set previous

          ;; x = page type, a = page
          (LDA ZP_RZ+1)
          (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x)
          (TAX)  ;; x = page

   (label CONTINUE_WITH_RESTORE__)
          (LDA ZP_TEMP)
          (STA ZP_RZ) ;; restore
          (LDA VM_PAGE_SLOT_DATA,x)           ;; first free slot offset

          (RTS))))

;; @DC-FUN: DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A, group: m1_slot
;; drop all full pages from the list of pages with available slots
;; input:  RZ, GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; usage:  A, X, Y
;; output: GLOBAL_M1_PX_PAGE_FOR_ALLOC of this profile holds page with free slots
;; funcs:  -
(define DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A
  (add-label-suffix
   "__" "NEW_DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A"
  (list
   (label DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_RZ)
          (LDA ZP_RZ+1)
   (label DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A)
          (STA READ_ENC_PAGE_TYPE__+2)
   (label READ_ENC_PAGE_TYPE__)
          (LDA $c000)
          (AND !$07)

          (TAX) ;; now x = page profile

   ;; input: x (unchanged)
   (label DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_PROFILE_X)
          (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x)

   (label LOOP__)
          (TAY) ;; y = page now
          (LDA VM_PAGE_SLOT_DATA,y)
          (BNE DONE__)                  ;; if there is a free slot on the given page (y), then we are done (no full page at head)

          ;; remove this page (in y) from list
          (STY LOAD_PREV_PAGE_CMD__+2)
          (STY STORE_PREV_PAGE_CMD__+2)
          (LDY !$00)
   (label LOAD_PREV_PAGE_CMD__)
          (LDA $c001) ;; $c0 is overwritten with page
          (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x) ;; optional optimization: needs only be done once! (is here done in a loop)
   (label STORE_PREV_PAGE_CMD__)
          (STY $c001) ;; $c0 is overwritten
          (BNE LOOP__) ;; if the current page (in a) is 0 (we are at the end of the list), we are done and can return, else loop

   (label DONE__)
          (RTS))))

;; @DC-FUN: ALLOC_M1_SLOT_TO_RB, group: m1_slot
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
(define ALLOC_M1_SLOT_TO_RB
  (add-label-suffix
   "__" "ALLOC_M1_SLOT_TO_RB"
   (list
    (label ALLOC_M1_SLOT_TO_RB)
           (JSR CP_RA_TO_RB)
           (JSR ALLOC_M1_SLOT_TO_RA)
           ;; alternative swap implementation
           ;; (LDA !ZP_RA)
           ;; (LDX !ZP_RB)
           ;; (JMP SWAP_ZP_WORD)
           (JMP SWAP_RA_RB))))

(module+ test #| |#
  (define alloc-m1-slot-to-rb-t0
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDA !$57)
     (STA ZP_RA) ;; fill ra with some value
     (LDA !$10)
     (JSR ALLOC_M1_SLOT_TO_RB)))

  (check-equal? (peek alloc-m1-slot-to-rb-t0 ZP_RA)
               #x57
               "ra holds original value")
  (check-equal? (memory-list alloc-m1-slot-to-rb-t0 ZP_RB (+ 1 ZP_RB))
               (list #x04 PAGE_AVAIL_0)
               "rb holds an allocated slot"))

;; @DC-FUN: ALLOC_M1_SLOT_TO_RA, group: m1_slot
;; allocate a slot of min A size, allocating a new page if necessary
;; input:  A = size
;; usage:  A, X, Y, RA, GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; output: RA = available slot of the given size (or a bit more)
;;         Y = actual size
;;         GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; funcs:
;;   VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS
;;   ALLOC_PAGE_TO_X
;;   INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX
(define ALLOC_M1_SLOT_TO_RA
  (add-label-suffix
   "__" "ALLOC_M1_SLOT_TO_RA"
  (list
   (label ALLOC_M1_SLOT_TO_RA)
          (LDX !$00)
          (CMP TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE+0)
          (BPL J9PLUS__)

   (label TYPE_X_STORE__)
          (STX PAGE_TYPE_IDX__)
          (JSR VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS) ;; x stays unchanged!

   (label ALLOC_M1_SLOT_TYPE_X_TO_RA)
          (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x) ;;
          (BEQ PAGE__)     ;; if the current free page is $00 (there is no page marked as having free slots) => allocate new page

          ;; ensure zp_ra points into the page
          (STA ZP_RA+1)
          (STA INC_CMD__+2)
          (TAX)
          (LDY VM_PAGE_SLOT_DATA,x)           ;; first free slot offset
          (BEQ PAGE__)    ;; if =0 allocate new page (no more free slots on this page)
          ;; ensure zp_ptr2 points to the slot!

   (label CONTINUE__)
          (STY ZP_RA)

          ;; now get the next free slot (from linked list in this page)
          (LDY !$00)
          (LDA (ZP_RA),y) ;; content of free slot points to the next free one (or 00)
          (STA VM_PAGE_SLOT_DATA,x)           ;; set next free slot for this page (x is still page)

          ;; ensure y holds the actual available slot size
          (LDX PAGE_TYPE_IDX__)
          (LDY TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE,x)
          (DEY)

   (label INC_CMD__)
          (INC $c002) ;; $c0 is overwritten with current page (increases the number of slots actually used)

          (RTS)

   (label FIND_NEXT_FREE_PAGE__)     ;; current page is full, search first non full (or end of list)
          ;; A = page, X = page, Y = 0
          (STA NEXT_PAGE_CMD__+2)

   (label NEXT_PAGE_CMD__)
          (LDA $C001) ;; $c0 is overwritten
          (BEQ PAGE__) ;; next page ptr = $00 => end reached, no more pages
          ;; check whether this page is full
          (TAX)
          (LDY VM_PAGE_SLOT_DATA,x)
          (BEQ FIND_NEXT_FREE_PAGE__) ;; next free slot for page is 00 => page is full, try to find next
          ;; page is not full => this is the new head
          (LDX PAGE_TYPE_IDX__)
          (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x)
          (STA ZP_RA+1)
          (CLC)
          (BCC CONTINUE__)

   (label PAGE__)               ;; allocate a complete new page for page type x or find a page in the list that has free slots
          (JSR ALLOC_PAGE_TO_X)
          (LDY PAGE_TYPE_IDX__)
          (JSR INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX)
          (LDX PAGE_TYPE_IDX__)
          (CLC)
          (BCC ALLOC_M1_SLOT_TYPE_X_TO_RA)

   (label J9PLUS__)
          (CMP TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE+1)
          (BPL J17PLUS__)
          (LDX !$01)
          (BNE TYPE_X_STORE__)

   (label J17PLUS__)
          (CMP TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE+2)
          (BPL J29PLUS__)
          (LDX !$02)
          (BNE TYPE_X_STORE__)

   (label J29PLUS__)
          (CMP TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE+3)
          (BPL J49PLUS__)
          (LDX !$03)
          (BNE TYPE_X_STORE__)

   (label J49PLUS__)
          (CMP TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE+4)
          (BPL J83PLUS__)
          (LDX !$04)
          (BNE TYPE_X_STORE__)

   (label J83PLUS__)
          ;; error, no slot this large can be allocated
          (BRK)

   (label PAGE_TYPE_IDX__)
          (byte $00) ;; local variable holding the selected page typ (0 = slots up to 17 bytes, 2 up to 29 bytes ...)
          )))

(module+ test #| vm_alloc_bucket_slot, allocate one slot of size $0b |#
  (define test-alloc-bucket-slot-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEX)
            (ast-opcode-cmd '() (list 157 0 PAGE_AVAIL_0)) ;; (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

            ;; now allocate the page
            (LDA !$0b) ;; want slot of size 11
            (JSR ALLOC_M1_SLOT_TO_RA)

            (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC+1) ;; type 1
            (STA ZP_TEMP)))

  (check-equal? (memory-list test-alloc-bucket-slot-state-after (+ PAGE_AVAIL_0_W #x03) (+ PAGE_AVAIL_0_W #x04))
                (list #x00 #x16)
                "slot0: refcount 0, next free slot at offset $16")
  (check-equal? (memory-list test-alloc-bucket-slot-state-after ZP_RA (add1 ZP_RA))
                (list #x04 PAGE_AVAIL_0)
                "allocated slot is at xx04")
  (check-equal? (memory-list test-alloc-bucket-slot-state-after ZP_TEMP ZP_TEMP)
                (list PAGE_AVAIL_0)
                "free page for slot type 0 is $cc")
  (check-equal? (vm-page->strings test-alloc-bucket-slot-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p1"
                  "previous page:  $00"
                  "slots used:     1"
                  "next free slot: $16")))

(module+ test #| vm_alloc_bucket_slot 2 times slot size $0b and $09 |#
  (define test-alloc-bucket-slot-2x-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
          ;; fill page with $ff
     (LDA !$FF)
     (LDX !$00)
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
     (DEX)
     (STA $cc00,x)
     (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

     ;; now allocate the page
     (LDA !$0b) ;; want slot of size 11
     (JSR ALLOC_M1_SLOT_TO_RA)
     (LDA !$0a) ;; want slot of size 10, should be on the same page
     (JSR ALLOC_M1_SLOT_TO_RA)

     (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC+1) ;; type 1
     (STA ZP_TEMP)))

  (check-equal? (memory-list test-alloc-bucket-slot-2x-state-after (+ PAGE_AVAIL_0_W #x15) (+ PAGE_AVAIL_0_W #x16))
                (list #x00 #x28)
                "slot1: refcount 0, next free slot at offset $28")
  (check-equal? (memory-list test-alloc-bucket-slot-2x-state-after ZP_RA (add1 ZP_RA))
                (list #x16 PAGE_AVAIL_0)
                "allocated slot is at xx16")
  (check-equal? (memory-list test-alloc-bucket-slot-2x-state-after ZP_TEMP ZP_TEMP)
                (list PAGE_AVAIL_0)
                "free page for slot type 0 is $xx")
  (check-equal? (vm-page->strings test-alloc-bucket-slot-2x-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p1"
                  "previous page:  $00"
                  "slots used:     2"
                  "next free slot: $28")))

(module+ test #| vm_alloc_bucket_slot, alloc 10 x slot size $14 (actual $20)  |#
  (define test-alloc-bucket-slot-xx-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
            ;; fill page with $ff
            (LDA !$FF)
            (LDY !$02) ;; 2 pages
            (LDX !$00) ;; 256 bytes
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEX)
            (ast-opcode-cmd '() (list 157 0 PAGE_AVAIL_1)) ;; (STA $cb00,x)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEY)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

            ;; loop over ...
            (LDA !$0a)
            (STA LOOP_NUM__TEST_ALLOC_BUCKET_SLOT_XX)

     (label LOOP__TEST_ALLOC_BUCKET_SLOT_XX)
            (LDA !$14) ;; want slot of size 20
            (JSR ALLOC_M1_SLOT_TO_RA) ;; ... slot allocation
            (DEC LOOP_NUM__TEST_ALLOC_BUCKET_SLOT_XX)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_XX)

            (JMP TAIL__TEST_ALLOC_BUCKET_SLOT_XX)

     (label LOOP_NUM__TEST_ALLOC_BUCKET_SLOT_XX)
            (byte $20)


     (label TAIL__TEST_ALLOC_BUCKET_SLOT_XX)
            (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC+2) ;; type 2
            (STA ZP_TEMP)))

  (check-equal? (memory-list test-alloc-bucket-slot-xx-state-after ZP_RA (add1 ZP_RA))
                (list #x2e PAGE_AVAIL_1)
                "allocated slot is at cb2e (slot1 on page 2)")
  (check-equal? (memory-list test-alloc-bucket-slot-xx-state-after (+ PAGE_AVAIL_1_W #x4b) (+ PAGE_AVAIL_1_W #x4c))
                (list #x00 #x6a)
                "first free slot page2: refcount 0, next free slot at offset $6a")
  (check-equal? (memory-list test-alloc-bucket-slot-xx-state-after ZP_TEMP ZP_TEMP)
                (list PAGE_AVAIL_1)
                "free page for slot type 1 is $cb")
  (check-equal? (vm-page->strings test-alloc-bucket-slot-xx-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p2"
                  "previous page:  $00"
                  "slots used:     8"
                  "next free slot: $00"))
  (check-equal? (vm-page->strings test-alloc-bucket-slot-xx-state-after PAGE_AVAIL_1)
                '("page-type:      m1 page p2"
                  "previous page:  $00"
                  "slots used:     2"
                  "next free slot: $4c")))

  ;; free-page for slot type 0 = cc


(define VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS #t)

;; @DC-FUN: VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS, group: m1_slot
;; remove full pages in the free list of pages of the same type as are currently in ZP_RA
;; input: RA
;; usage: A, X, Y, RA, GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; output: GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; funcs: -
(define VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS
  (add-label-suffix
   "__" "VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS"
  (list
   (label VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS)
          (LDA ZP_RA+1)
          (STA READ_ENC_PAGE_TYPE__+2)
   (label READ_ENC_PAGE_TYPE__)
          (LDA $c000)
          (AND !$07)

          (TAX) ;; now x = page profile

   ;; input: x (unchanged)
   (label VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS)
          (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x)

   (label LOOP_REMOVE_FULL_PAGES__)
          (TAY) ;; y = page now
          (LDA VM_PAGE_SLOT_DATA,y)
          (BNE DONE__)                  ;; if there is a free slot on the given page (y), then we are done (no full page at head)

          ;; remove this page (in y) from list
          (STY LOAD_PREV_PAGE_CMD__+2)
          (STY STORE_PREV_PAGE_CMD__+2)
          (LDY !$00)
   (label LOAD_PREV_PAGE_CMD__)
          (LDA $c001) ;; $c0 is overwritten with page
          (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x) ;; optional optimization: needs only be done once! (is here done in a loop)
   (label STORE_PREV_PAGE_CMD__)
          (STY $c001) ;; $c0 is overwritten
          (BNE LOOP_REMOVE_FULL_PAGES__) ;; if the current page (in a) is 0 (we are at the end of the list), we are done and can return, else loop

   (label DONE__)
          (RTS))))

;; @DC-FUN: VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS, group: m1_slot
;; put this page as head of the page free list for slots of type as in ZP_RA
;; input:  RA
;; usage:  A, X, Y, RA, TEMP
;; output: GLOBAL_M1_PX_PAGE_FOR_ALLOC
;; funcs: -
(define VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS
  (add-label-suffix
   "__" "__VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS"
  (list
   (label VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS)
          (LDA ZP_RA)
          (STA ZP_TEMP) ;; keep for later

          (LDA !$00)    ;; set to zero
          (STA ZP_RA)

          (LDY !$01)
          (LDA (ZP_RA),y) ;; get previous
          (BNE CONTINUE_WITH_RESTORE__)     ;; is != 0 => is still part of the list, don't change the list
          ;; is no longer part of the free list of pages, add this page at the head of the page

          (DEY) ;; now 0
          (LDA (ZP_RA),y) ;; get encoded page type
          (AND !$07)

          (TAX) ;; now x = page type

          (LDA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x)

          (INY) ;; now 1
          (STA (ZP_RA),y) ;; set previous

          ;; x = page type, a = page
          (LDA ZP_RA+1)
          (STA GLOBAL_M1_PX_PAGE_FOR_ALLOC,x)
          (TAX)  ;; x = page

   (label CONTINUE_WITH_RESTORE__)
          (LDA ZP_TEMP)
          (STA ZP_RA) ;; restore
          (LDA VM_PAGE_SLOT_DATA,x)           ;; first free slot offset

          (RTS))))

;; @DC-FUN: FREE_M1_SLOT_RA, group: gc
;; free the m1 slot pointed to by ra, marking that slot free on the m1-page
;; no check of the slot content is done! in case of cell-arrays: the elements of the array are not checked
;; input:  RA
;; usage: A, X, Y, RA
;; output: RA is invalid
;; funcs:
;;   VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS
;;   VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS
;; currently once allocated pages are not garbage collected. this is bad and needs to be changed
;; (e.g. keep count of used slots)? used slots = 0 => free page
;; INFO: NO GC! (this must be done, freeing specific types (e.g. an array) <- knows the number of slots etc.
;;       REF COUNT IS SET TO ZERO (of this slot)
(define FREE_M1_SLOT_RA
  (add-label-suffix
   "__" "__FREE_M1_SLOT_RA"
  (list
   (label FREE_M1_SLOT_RA)
          ;; make sure to remove fulls from free page list first !!
          (JSR VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS)

          ;; now free the slot
   (label REGULAR_FREE__)
          (LDX ZP_RA+1)
          (STX DEC_CMD__+2)    ;; write page for later dec execution
          (LDA VM_PAGE_SLOT_DATA,x)           ;; first free slot offset
          (BNE CONTINUE__)     ;; regular free

          ;; this page was full (since next free slot was 0) => register with the list of pages with free slots
          (JSR VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS)
          (LDX DEC_CMD__+2)    ;; restore x
          (LDA !$00)                              ;; next free slot offset (=0)

   (label CONTINUE__)
          (LDY !$00)
          (STA (ZP_RA),y)                       ;; set (zp_ptr) = previous free
          (LDA ZP_RA)                           ;; low byte of pointer = new free slot
          (STA VM_PAGE_SLOT_DATA,x)           ;; set new first free slot offset

          (DEC ZP_RA)                           ;; now points to ref count
          (TYA)                                   ;; y is still 0 => a := 0
          (STA (ZP_RA),y)                       ;; set refcount := 0

   (label DEC_CMD__)       ;; decrement number of slots used on the page
          (DEC $c002)                             ;; $c0 is overwritten

          (RTS))))

(module+ test #| vm_free_bucket_slot  allocate two slots, free first slot |#
  (define test-free-bucket-slot-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     ;; fill page with $ff
            (LDA !$FF)
            (LDX !$00)
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEX)
            (ast-opcode-cmd '() (list 157 0 PAGE_AVAIL_0)) ;; (STA $cc00,x)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

            ;; now allocate the page
            (LDA !$0b) ;; want slot of size 11
            (JSR ALLOC_M1_SLOT_TO_RA)
            (JSR CP_RA_TO_RT)

            (LDA !$0a) ;; want slot of size 10, should be on the same page
            (JSR ALLOC_M1_SLOT_TO_RA)

            (JSR CP_RT_TO_RA)
            (JSR FREE_M1_SLOT_RA)))

  (check-equal? (memory-list test-free-bucket-slot-state-after (+ PAGE_AVAIL_0_W #x03)(+ PAGE_AVAIL_0_W #x04))
                (list #x00 #x28)
                "slot0 (now free): refcount 0, next free slot at offset $28")
  (check-equal? (vm-page->strings test-free-bucket-slot-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p1"
                  "previous page:  $00"
                  "slots used:     1"
                  "next free slot: $04")))

(module+ test #| vm_free_bucket_slot  allocate 16 slots, free first slot |#
  (define test-free-bucket-a20-slot-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     ;; fill page with $ff
            (LDY !$03) ;; fill two pages
            (LDA !$FF) ;; with $ff
            (LDX !$00) ;; each 256 bytes long
     (label LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEX)
            (ast-opcode-cmd '() (list 157 0 PAGE_AVAIL_1)) ;; (STA $cb00,x) ;; starting at $cb00
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)
            (DEY)
            (BNE LOOP__TEST_ALLOC_BUCKET_SLOT_CODE)

            ;; now allocate the page
            (LDA !$17)
            (STA LOOP_VAR__TEST_FREE_BUCKET_A20_SLOT_CODE)
     (label LOOP__TEST_FREE_BUCKET_A20_SLOT_CODE)
            (LDA !$14) ;; want slot of size 14 (max size $1e)
            (JSR ALLOC_M1_SLOT_TO_RA)
            (DEC LOOP_VAR__TEST_FREE_BUCKET_A20_SLOT_CODE)
            (BPL LOOP__TEST_FREE_BUCKET_A20_SLOT_CODE)
            (JMP CONT__TEST_FREE_BUCKET_A20_SLOT_CODE )
     (label LOOP_VAR__TEST_FREE_BUCKET_A20_SLOT_CODE)
            (byte $00)

     (label CONT__TEST_FREE_BUCKET_A20_SLOT_CODE)
            ;; select first pointer
            (ast-opcode-cmd '() (list 169 PAGE_AVAIL_1)) ;; (LDA !$cb)
            (STA ZP_RA+1)
            (LDA !$10)
            (STA ZP_RA)
            (JSR FREE_M1_SLOT_RA)

            (ast-opcode-cmd '() (list 169 PAGE_AVAIL_0)) ;; (LDA !$cc)
            (STA ZP_RA+1)
            (LDA !$10)
            (STA ZP_RA)
            (JSR FREE_M1_SLOT_RA)))

  (check-equal? (memory-list test-free-bucket-a20-slot-state-after #xcec7 #xcecb)
                (list #x00 #x00 PAGE_AVAIL_0 #x00 #x00)
                "first free page of profiles 0, 1, 2, 3, 4 is $cc for page profile 1")
  (check-equal? (vm-page->strings test-free-bucket-a20-slot-state-after (sub1 PAGE_AVAIL_1))
                '("page-type:      m1 page p2"
                  "previous page:  $00" ;; is removed, since full
                  "slots used:     8"
                  "next free slot: $00"))
  (check-equal? (vm-page->strings test-free-bucket-a20-slot-state-after PAGE_AVAIL_1)
                '("page-type:      m1 page p2"
                  "previous page:  $00" ;; is the last in list
                  "slots used:     7"
                  "next free slot: $10"))
  (check-equal? (vm-page->strings test-free-bucket-a20-slot-state-after PAGE_AVAIL_0)
                (list "page-type:      m1 page p2"
                      (format "previous page:  $~a" (format-hex-byte PAGE_AVAIL_1)) ;; next free
                  "slots used:     7"
                  "next free slot: $10")))


;; @DC-FUN: INC_REFCNT_M1_SLOT_RA, group: gc
;; increment refcount of m1 slot in RA
;; IDEA for optimization: keep m1 in RA, putting +1 offset on all accesses -> DEC/INC could be saved
;; input:  RA (pointing to some m1 slot)
;; usage:  A, Y, RA
;; output: M1_SLOT Refcount++
;; funcs: -
(define INC_REFCNT_M1_SLOT_RA
  (list
   (label INC_REFCNT_M1_SLOT_RA)
          (DEC ZP_RA)           ;; m1, now pointing to reference count field, no page mod needed, since lowbyte always > 0
          (LDY !$00)
          (LDA (ZP_RA),y)
          (CLC)
          (ADC !$01)            ;; add 1 (there is no increment command for indirect addresses)
          (STA (ZP_RA),y)
          (INC ZP_RA)           ;; restore pointer
          (RTS) ;; 14 bytes


   ;; ;; slightly longer (1 byte longer), self modifying code, 5 cycles faster
   ;; (label ALT_INC_REFCNT_M1_SLOT_RA)
   ;;        (LDA ZP_RA+1)
   ;;        (LDY ZP_RA)
   ;;        (DEY)
   ;;        (STA INC_ABS__+2)
   ;;        (STY INC_ABS__+1)
   ;; (label INC_ABS__)
   ;;        (INC $c000)
   ;;        (RTS)
          ))

(module+ test #| vm_inc_ref_bucket_slot |#
  (define test-inc-ref-bucket-slot-1-state-after
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDA !$f0)
     (STA $f003)
     (STA ZP_RA+1)
     (LDA !$04)
     (STA ZP_RA)

     (JSR INC_REFCNT_M1_SLOT_RA)))

  (check-equal? (memory-list test-inc-ref-bucket-slot-1-state-after #xf003 #xf003)
                (list #xf1))
  (check-equal? (memory-list test-inc-ref-bucket-slot-1-state-after ZP_RA (add1 ZP_RA))
                (list #x04 #xf0))
  (inform-check-equal? (cpu-state-clock-cycles test-inc-ref-bucket-slot-1-state-after)
                       50))


;; impl complete, test missing
;; @DC-FUN: FREE_M1_SLOT_RZ, group: gc
;; free the m1 slot referenced by RZ
;; input: RZ
;; usage: A, X, Y, RZ
;; output:
;; funcs:
;;   GC_INCR_ARRAY_SLOT_RZ >>
;;   ADD_M1_SLOT_RZ_TO_PFL >>
(define FREE_M1_SLOT_RZ
  (add-label-suffix
   "__" "__NEW_FREE_M1_SLOT_RZ"
   (list
    (label FREE_M1_SLOT_RZ)
           (LDY !$00)
           (LDA (ZP_RZ),y)
           (CMP !TAG_BYTE_CELL_ARRAY)
           (BEQ FREE_CELLARR__)
           (CMP !TAG_BYTE_NATIVE_ARRAY)
           (BEQ FREE_NATIVEARR__)

    (label UNKNOWN__)
          ;; unknown object type (or atomic value that cannot be ref counted and MUST NOT END UP in ZP_RZ)
           (RTS)

    (label FREE_CELLARR__)
           (JMP GC_INCR_ARRAY_SLOT_RZ)

    (label FREE_NATIVEARR__)
           (JMP ADD_M1_SLOT_RZ_TO_PFL) ;; just add this slot to the free list of the respective page (and do some housekeeping)
)))

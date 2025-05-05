#lang racket/base
;; [[pdfview:~/Downloads/Small memory software patterns for limited memory systems.2001.pdf::261++0.00][Small memory software patterns for limited memory systems.2001.pdf: Page 261]]

#|

implementation of basic memory primitives for the native 6510 assembler implementation of mil.

primitives are e.g. allocation of cell-pair(s)
evalation stack operations
call frame primitives etc.

|#


;; naming: atomic cell
;;         cell                      :: 16 bit value (finest granular memory managed block)
;;         atomic cell               :: a cell that has no followup value and is complete in itself (currently int-cell, byte-cell, cell-ptr, cell-pair-ptr)
;;         cell-ptr                  :: an atomic cell, lowest bit of low byte is set, points to a cell (of any type)
;;                                     lowbyte: #bxxxx xxx0
;;                                     highbyte = page
;;         cell-pair                 :: a pair of two cells, cell0 and cell1 (in memory: 00 lowbyte-cell0, 01 highbyte-cell0, 02 lowbyte-cell1, 03 highbyte-cell1),
;;                                     each cell can be of any atomic cell
;;         cell-pair-ptr             :: an atomic cell, second lowest bit is set, lowest bit is unset, points to a cell-pair
;;                                     lowbyte: #bxxxx xx01 
;;                                     highbyte = page
;;         int-cell (bool)          :: an atomic cell having 13 bit as payload
;;                                     lowbyte mask: #b0xxx xx11, xxxxx = high bits of int
;;                                     highbyte = lowbyte of int
;;         byte-cell (char)         :: an atomic cell having one byte as payload
;;                                     lowbyte mask: #b1111 1111
;;                                     highbyte = payload
;;         complex cell              :: a cell that functions as header for followup values (follows directly in memory)
;;                                     complex cells cannot be pushed on the stack, they can only be pointed to by cell-ptr!
;;         (cell-structure-header    :: a complex cell that defines a structure)
;;         cell-array-header         :: a complex cell that defines an array, defining the number of cells in the second byte
;;                                     a structure is an array of cells
;;                                     lowbyte mask: #b1000 0011
;;                                     highbyte: #of cells in this array
;;                                     n*2 bytes with cells <- each cell needs to be gc'ed separately
;;         cell-native-array-header  :: a complex cell that defines an array of bytes
;;                                     a string is an native array of bytes
;;                                     lowbyte mask: #b1000 0111
;;                                     highbyte: #of bytes in this array
;;                                     n bytes with byte payloads <- no gc of this necessary (holds plain values, no pointers, no cells)
;;         (cell-float-header        :: a complex cell that defines a float)
;;         page                      :: 256 byte memory managed unit, holding slots
;;         slot                      :: a fixed size portion of memory on a page (sizes are 2=cell, 4=cell=pair, 8 ...), only one size per page is allowed
;;         ref count                 :: byte counting how many pointers to this value exist, there can be pointer to pointers
;;         cell-stack aka eval-stack :: stack of cells. ZP_​CELL_​STACK_​TOS holds the index (on current page), ZP_​CELL_​STACK_​LB_​PTR, ZP_​CELL_​STACK_​HB_​PTR holds the pointer to the low/high byte
;;                                      [RT]         RT is the top of the stack (even though not on the stack memory wise)
;;                                      [cell n lb] [cell n hb]
;;                                             ...
;;                                      [cell 1 lb] [cell 1 hb]
;;                                      [cell 0 lb] [cell 0 hb]
;;                                      ZP_​CELL_​STACK_​TOS points to the current element below RT (cell n)
;;         m1 page px       :: page for slots with ref count at -1 position, with profile x (0..3) <- defines size and payload start offset
;;         call-frame page  :: page for call-frames (stack organized, no ref counting etc.)
;;         cell-pairs page  :: page for cell-pairs, (lowbyte) lsr x 2 to get ref count position
;;         cell page        :: page for cells, (lowbyte) lsr x 1 to get ref count position (last cell unusable)
;;         [s8 page          :: page for slots of size <=8, (lowbyte) lsr x 3 to get ref count position] optional
;;         fid->loc page    :: page that maps a function id to a location of first byte code
;;         code page        :: page holding byte code (and function meta data, module meta data?)
;;         constants page   :: page holding constants (not ref counted)
;;         page block       :: a number of consecutive pages allocated/freed as a block, allowing for larger memory objects (having less wasted bytes (e.g. for call-frames)?)

(require (only-in racket/format ~a))

(require "../6510.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader))
(require (only-in "../tools/6510-interpreter.rkt" peek-word-at-address cpu-state-clock-cycles))
(require (only-in "../ast/6510-calc-opcode-facades.rkt" LDA-immediate))
(require (only-in "../util.rkt" bytes->int low-byte high-byte format-hex-byte format-hex-word))
(require (only-in racket/list flatten take empty? drop make-list))
(require (only-in "../ast/6510-relocator.rkt" command-len))
(require (only-in "../ast/6510-resolver.rkt"
                  add-label-suffix
                  replace-labels))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in "./vm-memory-manager-test-utils.rkt"
                    run-code-in-test-on-code
                    remove-labels-for
                    ))
  (require (only-in uuid uuid-string))
  (require (only-in racket/string string-replace))
  (require (only-in racket/list range))

  (define TEST_COUNTERS #xa003)
  (define (calls-to-mock state (mock-idx 0))
    (car (memory-list state (+ TEST_COUNTERS mock-idx) (+ TEST_COUNTERS mock-idx))))
  (define (wrap-code-for-test bc complete-code (mocked-code-list (list)))
    (append (list
      (org #xa000)
             (JMP TEST_START__)
      (label TEST_COUNTERS)
             (byte 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
             (byte 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0)
      (label TEST_START__)
             (JSR VM_INITIALIZE_MEMORY_MANAGER)
             (JSR $0100)) ;; reset clock cycles
             bc
             (list (BRK))
             (remove-labels-for complete-code (filter (lambda (ast-cmd) (ast-label-def-cmd? ast-cmd)) mocked-code-list))))

  (define (run-code-in-test bc (debug #f) #:mock (mocked-code-list (list)))
    (run-code-in-test-on-code (wrap-code-for-test bc vm-memory-manager mocked-code-list) debug))

  ;; add unique random suffix to labels ending on "__"
  ;; mock away labales in the mock list
  ;; add subroutine that counts calls to mocks incresing TEST_COUNTERS+<idx of mock>
  ;; where idx is the list position number of the mock passed
  (define (list-with-label-suffix #:mock (mocked-code-list (list)) . list-elements )
    (add-label-suffix
     "__" (string-replace (uuid-string) "-" "_")
     (append
      (list (JMP TEST_ENTRY))
      (flatten (map (lambda (mocked-label idx)
                      (list mocked-label
                            ;; INC TEST_COUNTERS+<idx-mock>
                            (ast-unresolved-opcode-cmd
                             '()
                             (list (car (ast-opcode-cmd-bytes (INC $a000))))
                             (ast-resolve-word-scmd (format "TEST_COUNTERS+~a"  idx)))
                            (RTS)
                            ))
                    mocked-code-list
                    (range (length mocked-code-list))))
      (list (label TEST_ENTRY))
      list-elements)))

  ;; run the given code using mocks, calls being counted, and label suffixes for the test-code
  (define (compact-run-code-in-test #:debug (debug #f) #:mock (mocked-labels (list)) . cmds)
    (run-code-in-test
     (apply list-with-label-suffix
            cmds
            #:mock mocked-labels)
     debug
     #:mock mocked-labels)))

(module+ test #| after mem init |#
  (define PAGE_AVAIL_0 #x9a)
  (define PAGE_AVAIL_0_W #x9a00)
  (define PAGE_AVAIL_1 #x99)
  (define PAGE_AVAIL_1_W #x9900))

(require (only-in "../tools/6510-interpreter.rkt"
                  6510-load
                  6510-load-multiple
                  initialize-cpu
                  run-interpreter
                  run-interpreter-on
                  memory-list
                  cpu-state-accumulator
                  peek))

(provide vm-memory-manager
         ast-const-get

         vm-cell-at-nil?
         vm-stack->strings
         vm-cell-at->string
         vm-cell->string
         vm-page->strings
         vm-regt->string
         vm-rega->string
         vm-deref-cell-pair-w->string
         vm-deref-cell-pair->string
         cleanup-string
         cleanup-strings

         POP_CELL_EVLSTK_TO_RT
         VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE
         VM_FREE_CELL_PAIR_PAGE
         VM_LIST_OF_FREE_CELLS
         ALLOC_CELLARR_TO_RA
         GC_CELLPAIR_FREE_LIST

         VM_MEMORY_MANAGEMENT_CONSTANTS
         VM_INITIALIZE_MEMORY_MANAGER
          ;; ---------------------------------------- alloc/free pages
          FREE_PAGE_A                                       ;; free a page (the type specific stuff, of any, must have finished)
          ALLOC_PAGE_TO_X                                   ;; allocate new page (not initialized)

          INIT_CELL_PAGE_X_TO_AX                                  ;; initialize page A for ref counted cells
          INIT_CELLPAIR_PAGE_X_TO_AX                              ;; initialize page A for ref counted cell-pairs
          ;; INIT_CELLSTACK_PAGE_A                             ;; initialize page A to previous cell stack page (X)
          INIT_CELLSTACK_PAGE_X
          ;; ---------------------------------------- alloc/free cells, pairs, slots
          ALLOC_CELL_TO_RT

          ALLOC_CELLPAIR_AX_TO_RT               ;; allocate a cell-pair a from page x (if page has no free cell-pairs, a new page is allocated and is used to get a free cell-pair!)

          ALLOC_CELLPAIR_TO_RT                       ;; allocate a cell-pair from the current page (or from a new page if full)
          FREE_CELLPAIR_RT                        ;; free this cell-pair (adding it to the free tree)
          ;; FREE_CELLPAIR_RA                        ;; free this cell-pair (adding it to the free tree)

          ;; FREE_CELL_RT                             ;; free this cell pointed to by RT (adding it to the free list)
          ;; FREE_CELL_RA                             ;; free this cell pointed to by RT (adding it to the free list)

          GC_CELLPAIR_FREE_LIST                     ;; reclaim all cell-pairs in the queue of free cells

          ;; VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2                   ;; allocate an array of bytes (native) (also useful for strings)
          ALLOC_CELLARR_TO_RA                          ;; allocate an array of cells (also useful for structures)

          ;; VM_ALLOC_M1_SLOT_TO_ZP_PTR2                        ;; allocate a slot of min A size, allocating a new page if necessary
          ;; VM_FREE_M1_SLOT_IN_ZP_PTR2                         ;; free a slot (adding it to the free list)

          ;; VM_ALLOC_MODULE_CODE_SLOT_TO_ZP_PTR                ;; allocate a slot for module code
          ;; VM_FREE_MODULE
          ;; VM_RELOCATE_MODULE_X_TO_                           ;; relocate module identified by page x to ??

          ;; ---------------------------------------- refcount
          DEC_REFCNT_RT                                ;; generic decrement of refcount (dispatches depending on type)
          INC_REFCNT_RT                                ;; generic increment of refcount (dispatches depending on type)

          DEC_REFCNT_CELLPAIR_RT                 ;; decrement refcount, calling vm_free_cell_pair_in_zp_ptr if dropping to 0
          DEC_REFCNT_CELL_RT                      ;; decrement refcount, calling vm_free_cell_in_zp_ptr if dropping to 0

          INC_REFCNT_CELLPAIR_RT                 ;; increment refcount of cell-pair
          INC_REFCNT_CELL_RT                      ;; increment refcount of the cell, rt is pointing to

          DEC_REFCNT_RA                                ;; generic decrement of refcount (dispatches depending on type)
          DEC_REFCNT_CELLPAIR_RA                 ;; decrement refcount, calling vm_free_cell_pair_in_zp_ptr if dropping to 0
          DEC_REFCNT_CELL_RA                      ;; decrement refcount, calling vm_free_cell_in_zp_ptr if dropping to 0
          ;; ---------------------------------------- call frame

          ;; ---------------------------------------- misc

          ;; VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS                ;; remove full pages in the free list of pages of the same type as are currently in ZP_PTR2
          ;; VM_ENQUEUE_PAGE_AS_HEAD_FOR_PTR2_SLOTS             ;; put this page as head of the page free list for slots of type as in ZP_PTR2

          ;; VM_GC_ARRAY_SLOT_PTR                               ;; execute garbage collection on a cell array (decr-ref all array elements and collect if 0)

          FREE_RT                                 ;; free pointer (is cell-ptr, cell-pair-ptr, m1-slot-ptr, slot8-ptr)

          VM_ADD_CELL_PAIR_IN_RT_TO_ON_PAGE_FREE_LIST       ;; add the given cell-pair (in zp_rt) to the free list of cell-pairs on its page

          ;; ---------------------------------------- CELL_STACK / RT / RA
          POP_CELL_EVLSTK_TO_RT                                ;; pop cell-stack into RT (discarding RT)

          PUSH_TO_EVLSTK                               ;; push value into RT, pushing RT onto the call frame cell stack if not empty
          ;; vm_cell_stack_push_rt_if_nonempty
          PUSH_RT_TO_EVLSTK                         ;; push RT onto call frame cell stack

          ;; WRITE_INTm1_TO_RA                             ;; write cell-int -1 into RA
          ;; WRITE_INTm1_TO_RT                             
          ;; WRITE_INTm1_TO_Rx                             ;; x=0 -> RT, x=2 -> RA

          ;; WRITE_INT1_TO_RA                              ;; write cell-int +1 into RA
          ;; WRITE_INT1_TO_RT
          ;; WRITE_INT1_TO_Rx                              ;; x=0 -> RT, x=2 -> RA

          ;; WRITE_INT0_TO_RA                              ;; write cell-int 0 into RA
          ;; WRITE_INT0_TO_RT
          ;; WRITE_INT0_TO_Rx                              ;; x=0 -> RT, x=2 -> RA

          ;; WRITE_INT_A_TO_RA                             ;; write cell-int (only byte sized) A into RA
          ;; WRITE_INT_A_TO_RT
          ;; WRITE_INT_A_TO_Rx                             ;; x=0 -> RT, x=2 -> RA

          ;; WRITE_INT_AY_TO_RA                            ;; int in A(lowbyte)/Y(highbyte) into RA
          ;; WRITE_INT_AY_TO_RT
          WRITE_INT_AY_TO_Rx                              ;; int in A(lowbyte)/Y(highbyte), x=0 -> RT, x=2 -> RA

          ;; WRITE_NIL_TO_RA
          ;; WRITE_NIL_TO_RT
          WRITE_NIL_TO_Rx

          WRITE_CELLPAIR_RT_CELL1_TO_RT
          WRITE_CELLPAIR_RT_CELL0_TO_RT
          WRITE_CELLPAIR_RT_CELLy_TO_RT                            ;; write CELLy (y=0 cell0, y=2 cell1) pointed to by RT into RT
          ;; WRITE_CELLPAIR_RA_CELL1_TO_RT
          ;; WRITE_CELLPAIR_RA_CELL0_TO_RT
          WRITE_CELLPAIR_RA_CELLy_TO_RA                            ;; write CELLy (y=0 cell0, y=2 cell1) pointed to by RA into RA

          WRITE_RA_TO_CELLy_CELLPAIR_RT                            ;; write RA cell into CELLy (y=0 cell0, y=2 cell1) pointer to by RT

          WRITE_CELLPAIR_RT_CELL1_TO_RA       
          WRITE_CELLPAIR_RT_CELL0_TO_RA
          WRITE_CELLPAIR_RT_CELLy_TO_RA                            ;; write CELLy (y=0 cell0, y=2 cell1) pointed to by RT into RA

          WRITE_RT_TO_CELLy_CELLPAIR_RA                            ;; write RT cell into CELLy (y=0 cell0, y=2 cell1) pointer to by RA

          CP_RT_TO_RA                                     ;; copy RT -> RA
          CP_RA_TO_RT                                     ;; copy RA -> RT

          POP_CELL_EVLSTK_TO_CELLy_RT                           ;; POP the cell-stack top into CELLy (y=0 cell0, y=2 cell1) pointed to by RT, reducing the stack size by 1, keeping rt as tos

         ZP_RT
         ZP_VM_PC
         ZP_VM_FUNC_PTR
         ZP_LOCALS_HB_PTR
         ZP_LOCALS_LB_PTR
         ZP_LOCALS_TOP_MARK
         ZP_CELL_STACK_TOS
         ZP_CELL_STACK_LB_PTR
         ZP_CELL_STACK_HB_PTR
         ZP_CALL_FRAME
         ZP_CALL_FRAME_TOP_MARK)

;; constants that are used by the assembler code
(define VM_MEMORY_MANAGEMENT_CONSTANTS
  (list
   ;; highest bit 0 and the lowest 2 bits are reserved for int, cell-ptr and cell-pair-ptr
   ;; => 32 values still available
   (byte-const TAG_BYTE_BYTE_CELL         $ff)
   (byte-const TAG_BYTE_CELL_ARRAY        $83) ;; LSR -> 41, LSR -> 20
   (byte-const TAG_BYTE_CELL_ARRAY_LSR2   $30)
   (byte-const TAG_BYTE_NATIVE_ARRAY      $87) ;; LSR -> 43, LSR -> 21
   (byte-const TAG_BYTE_NATIVE_ARRAY_LSR2 $21)

   (byte-const NEXT_FREE_PAGE_PAGE       $cf)   ;; cf00..cfff is a byte array, mapping each page idx to the next free page idx, 00 = no next free page for the given page
   ;; (word-const VM_FIRST_FREE_SLOT_ON_PAGE     $cf00) ;; location: table of first free slot for each page

   (word-const TAGGED_INT_0              $0003)
   (word-const TAGGED_BYTE0              $00ff)
   (word-const TAGGED_NIL                $0001) ;; tag indicates cell-pair-ptr

   ;; d9..da free to use

   (byte-const ZP_CELL_STACK_TOS         $db) ;; byte (fe = empty stack, 0 = first element, 2 = second element, 4 = third element ...)

   ;; ZP_TEMP may be used as pointer (in combination with ZP_TEMP2)
   (byte-const ZP_TEMP                   $dc) ;; may not be used after sub calls (just within a routine without jsr)
   (byte-const ZP_TEMP2                  $dd) ;; may not be used after sub calls (just within a routine without jsr)
   (byte-const ZP_TEMP3                  $d9)
   ;; (byte-const ZP_TEMP4                $da)

   ;; the following twelve bytes need to be continuous, since they are saved into the call frame!
   (byte-const ZP_VM_PC                  $de) ;; de..df program counter (ptr to currently executing byte code)
   (byte-const ZP_VM_FUNC_PTR            $e0) ;; e0..e1 pointer to the currently running function
   (byte-const ZP_LOCALS_LB_PTR          $e2) ;; e2..e3 pointer to low byte of first local in call-frame
   (byte-const ZP_LOCALS_HB_PTR          $e4) ;; e4..e5 pointer to high byte of first local in call-frame
   (byte-const ZP_CELL_STACK_LB_PTR      $e6) ;; e6..e7 (pointer to low byte of the eval stack of the currently running function (+ZP_CELL_STACK_TOS => pointer to tos of the call-frame, in register mode, actual TOS is ZP_RT!)
   (byte-const ZP_CELL_STACK_HB_PTR      $e8) ;; e8..e9 (pointer to high byte of the eval stack of the currently running function (+ZP_CELL_STACK_TOS => pointer to tos of the call-frame, in register mode, actual TOS is ZP_RT!)
   (byte-const ZP_CALL_FRAME_TOP_MARK    $ea) ;; ea byte pointing to current top of call-frame (is swapped in/out of call-frame page $02)
   (byte-const ZP_LOCALS_TOP_MARK        $eb) ;; eb byte pointing to the byte past the last local on the locals stack
   (byte-const ZP_CALL_FRAME             $f1) ;; f1..f2

   ;; register C
   (byte-const ZP_RC                     $f3) ;; f3..f4
   (byte-const ZP_PART_GCD_CELL_ARRAYS   $f5) ;; f5..f6   this is the cell-arrays global partially collected list (gpcl)

   ;; implementation using registers
   ;; register T = top of stack, used as main register for operations, could be a pointer to a cell or an atomic cell. if it is a pointer to a cell, the low byte here is without tag bits => (zp_rt) points to the cell
   (byte-const ZP_RT                     $fb) ;; fb = low byte, fc = high byte,
   ;; register A
   (byte-const ZP_RA                     $fd) ;; fd = low byte, fe = high byte,
   ;; currently no need to register B (maybe someday)
   ))

;; extract constant definition in assembler into racket constant
(define (ast-const-get ast-commands key)
  (when (empty? ast-commands)
    (raise-user-error (format "key ~a not found in list of ast commands" key)))
  (define ast-command (car ast-commands))
  (cond
    [(and (ast-const-byte-cmd? ast-command)
        (string=? key (ast-const-byte-cmd-label ast-command)))
     (ast-const-byte-cmd-byte ast-command)]
    [(and (ast-const-word-cmd? ast-command)
        (string=? key (ast-const-word-cmd-label ast-command)))
     (ast-const-word-cmd-word ast-command)]
    [else (ast-const-get (cdr ast-commands) key)]))

(define ZP_RT                   (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RT"))
(define ZP_RA                   (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RA"))
(define ZP_RC                   (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_RC"))
(define ZP_PART_GCD_CELL_ARRAYS (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PART_GCD_CELL_ARRAYS"))
(define ZP_CALL_FRAME           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CALL_FRAME"))
(define ZP_CALL_FRAME_TOP_MARK  (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CALL_FRAME_TOP_MARK"))
(define ZP_CELL_STACK_LB_PTR    (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL_STACK_LB_PTR"))
(define ZP_CELL_STACK_HB_PTR    (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL_STACK_HB_PTR"))
(define ZP_CELL_STACK_TOS       (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL_STACK_TOS"))
(define ZP_TEMP                 (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_TEMP"))
(define ZP_TEMP2                (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_TEMP2"))
(define ZP_TEMP3                (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_TEMP3"))
(define ZP_VM_PC                (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_VM_PC"))
(define ZP_VM_FUNC_PTR          (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_VM_FUNC_PTR"))
(define ZP_LOCALS_LB_PTR        (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_LOCALS_LB_PTR"))
(define ZP_LOCALS_HB_PTR        (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_LOCALS_HB_PTR"))
(define ZP_LOCALS_TOP_MARK      (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_LOCALS_TOP_MARK"))
(define TAG_BYTE_BYTE_CELL      (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_BYTE_CELL"))
(define TAG_BYTE_CELL_ARRAY     (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_CELL_ARRAY"))
(define TAG_BYTE_NATIVE_ARRAY   (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAG_BYTE_NATIVE_ARRAY"))
(define TAGGED_NIL              (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "TAGGED_NIL"))

;; write out the cells that are marked as reallocatable
(define (vm-cell-pair-free-tree->string state)
  (define cell-pair-root (peek-word-at-address state #xcec5))
  (cond
    [(= 0 (bitwise-and #xff00 cell-pair-root)) "root is initial"]
    [else
     (format "pair $~a -> [ ~a . ~a ]"
             (format-hex-word cell-pair-root)
             (vm-cell-w->string (peek-word-at-address state cell-pair-root))
             (vm-cell-w->string (peek-word-at-address state (+ 2 cell-pair-root))))]))

;; write a status string of a memory page
(define (vm-page->strings state page)
  (define page-type-enc (peek state (bytes->int 0 page)))
  (define next-free-slot (peek state (bytes->int page #xcf)))
  (define page-type
    (cond
      [(= #x10 (bitwise-and #xf8 page-type-enc))
       (format "m1 page p~a" (bitwise-and #x07 page-type-enc))]
      [(= #x80 (bitwise-and #x80 page-type-enc))
       "cell page"]
      [(= #x40 (bitwise-and #xc0 page-type-enc))
       "cell-pair page"]
      [(= #x20 (bitwise-and #xe0 page-type-enc))
       "s8 page"]
      [(= #x18 page-type-enc)
       "call-frame page"]
      [else (raise-user-error "unknown page type")]))
  (define previous-page
    (cond
      [(not (= 0 (bitwise-and #xc0 page-type-enc)))
       (peek state (bytes->int #xff page))]
      [else (peek state (bytes->int 1 page))]))
  (define slots-used
    (cond
      [(= #x10 (bitwise-and #xf0 page-type-enc))
       (peek state (bytes->int 2 page))]
      [(= #x80 (bitwise-and #x80 page-type-enc))
       (bitwise-and #x7f page-type-enc)]
      [(= #x40 (bitwise-and #xc0 page-type-enc))
       (bitwise-and #x3f page-type-enc)]
      [(= #x20 (bitwise-and #xe0 page-type-enc))
       (bitwise-and #x1f page-type-enc)]
      [else 0]
      ))
  (cond [(= #x18 page-type-enc)
         (list (format "page-type:      ~a" page-type)
               (format "previous page:  $~a" (format-hex-byte previous-page)))]
        [else
         (list (format "page-type:      ~a" page-type)
               (format "previous page:  $~a" (format-hex-byte previous-page))
               (format "slots used:     ~a" slots-used)
               (format "next free slot: $~a" (format-hex-byte next-free-slot)))]))

(define (cleanup-string str)
  (regexp-replace*
   #px"(pair-ptr(\\[[-0-9]*\\])? (\\$[0-9A-Fa-f]*)?|int \\$0{0,3})"
   str
   ""))

(define (cleanup-strings strings)
  (map cleanup-string strings))

;; produce strings describing the current cell-stack status
(define (vm-stack->strings state (max-count 10) (follow #f))
  (define stack-tos-idx (peek state ZP_CELL_STACK_TOS))
  (define stack-lb-page-start (peek-word-at-address state ZP_CELL_STACK_LB_PTR))
  (define stack-hb-page-start (peek-word-at-address state ZP_CELL_STACK_HB_PTR))
  (cond
    [(and (regt-empty? state) (= stack-tos-idx #x01) (= 0 (peek state (add1 stack-lb-page-start)))) (list "stack is empty")]
    [else
     (define values-count (min (- stack-tos-idx 1) max-count))
     (define low-bytes (memory-list state (+ stack-lb-page-start (add1 (- stack-tos-idx values-count))) (+ stack-lb-page-start stack-tos-idx)))
     (define high-bytes (memory-list state (+ stack-hb-page-start (add1 (- stack-tos-idx values-count))) (+ stack-hb-page-start stack-tos-idx)))
     (define stack-item-no (+ values-count (if (regt-empty? state) 0 1)))
     (define stack-strings (reverse (map (lambda (pair) (vm-cell->string (car pair) (cdr pair) state follow)) (map cons low-bytes high-bytes))))
     (cons (format "stack holds ~a ~a" stack-item-no (if (= 1 stack-item-no) "item" "items"))
           (if (regt-empty? state)
               (list "stack is empty")
               (cons (format "~a  (rt)" (vm-regt->string state follow)) stack-strings)))]))

;; make a list of adjacent pairs
(define (pairing list (paired-list '()))
  (if (< (length list) 2)
      (reverse paired-list)
      (pairing (drop list 2) (cons `(,(car list) . ,(cadr list)) paired-list))))

(module+ test #| pairing |#
  (check-equal? (pairing '())
                '())
  (check-equal? (pairing '(1 2 3 4 5 6))
                '((1 . 2) (3 . 4) (5 . 6))))

;; write the car, cdr cell of the cell-pair at word in memory
(define (vm-deref-cell-pair-w->string state word (follow #f) (visited (list)))
  (define derefed-word-car (peek-word-at-address state word))
  (define derefed-word-cdr (peek-word-at-address state (+ 2 word)))
  (format "(~a . ~a)"
          (vm-cell-w->string derefed-word-car state follow visited)
          (vm-cell-w->string derefed-word-cdr state follow visited)))

(define (vm-deref-cell-w->string state word)
  (define derefed-word (peek-word-at-address state word))
  (format "~a" (vm-cell-w->string derefed-word)))

;; write the car, cdr cell of the cell-pair at low/high in memory
(define (vm-deref-cell-pair->string state low high (follow #f) (visited (list)))
  (vm-deref-cell-pair-w->string state (bytes->int low high) follow visited))

(define (vm-deref-cell->string state low high)
  (vm-deref-cell-w->string state (bytes->int low high)))

;; write decoded cell described by word
(define (vm-cell-w->string word (state '()) (follow #f) (visited (list)))
  (vm-cell->string (low-byte word) (high-byte word) state follow visited))

(define (refcount-of-cell-pair state low high)
  (define rc-offset (arithmetic-shift low -2))
  (peek state (bytes->int rc-offset high)))

(define (refcount-of-cell state low high)  
  (define page-type (peek state (bytes->int 0 high)))
  (define rc-offset
    (cond [(= #x80 (bitwise-and page-type #x80)) ;; cell-ptr-page
           (arithmetic-shift low -1)]
          [(= #x00 (bitwise-and page-type #xec))
           (sub1 low)]
          [else (raise-user-error (format "unknown page type ~a" page-type))]))
  (peek state (bytes->int rc-offset high)))

;; write decoded cell described by low high
;; the low 2 bits are used for pointer tagging
(define (vm-cell->string low high (state '()) (follow #f) (visited (list)))
  (cond
    [(memq (bytes->int low high) visited)
     (format "RECURSION->$~a~a" (format-hex-byte high) (format-hex-byte low))]
    [(= 0 low) "empty"]
    [(= 0 (bitwise-and #x01 low)) (format "ptr[~a] $~a~a"
                                          (if (empty? state) "-" (refcount-of-cell state low high))
                                          (format-hex-byte high)
                                          (format-hex-byte (bitwise-and #xfe low)))]
    [(and (= 1 (bitwise-and #x03 low)) (= high 0)) "pair-ptr NIL"]
    [(= 1 (bitwise-and #x03 low))
     (string-append (format "pair-ptr[~a] $~a~a"
                            (if (empty? state) "-" (refcount-of-cell-pair state low high))
                            (format-hex-byte high)
                            (format-hex-byte (bitwise-and #xfd low)))
                    (if follow
                        (vm-deref-cell-pair->string state low high #t (cons (bytes->int low high) visited))
                        ""))]
    [(= 3 (bitwise-and #x83 low)) (format "int $~a~a"
                                          (format-hex-byte (arithmetic-shift low -2))
                                          (format-hex-byte high))]
    [(= TAG_BYTE_BYTE_CELL (bitwise-and #xff low)) (format "byte $~a" (format-hex-byte high))]
    [(= TAG_BYTE_CELL_ARRAY (bitwise-and #xff low))
     (define array-str (format "cell-array len=$~a" (format-hex-byte high)))
     (if follow
         (format "~a [...]" array-str)
         array-str)]
    ;; TODO: a structure has a special value + follow bytes
    ;; (= ? (bitwise-and #xfc low)) e.g. #x04 = structure, high byte = number of fields
    ;; the following number of fields * cells cannot be structure cells, but only atomic or pointer cells
    [else "?"]))


(define (regt-empty? state)
  (= 0 (peek state ZP_RT)))

(define (vm-cell-at-nil? state loc)
  (= TAGGED_NIL (peek-word-at-address state loc)))

(define (vm-cell-at->string state loc (rev-endian #f) (follow #f))
  (vm-cell-w->string (peek-word-at-address state loc rev-endian) state follow))

;; write string of current RT
(define (vm-regt->string state (follow #f))
  (vm-cell->string
   (peek state ZP_RT)
   (peek state (add1 ZP_RT))
   state
   follow))

;; get the actual refcount of a cell-pair-ptr
(define (vm-refcount-cell-pair-ptr state cell-pair-ptr)
  (define lowb  (low-byte cell-pair-ptr))
  (define highb (high-byte cell-pair-ptr))
  (define refc  (arithmetic-shift lowb -2))
  (peek state (bytes->int refc highb) ))

;; get the actual refcount of a cell-pair-ptr
(define (vm-refcount-cell-ptr state cell-ptr)
  (define lowb  (low-byte cell-ptr))
  (define highb (high-byte cell-ptr))
  (define refc  (arithmetic-shift lowb -1))
  (peek state (bytes->int refc highb) ))

;; write string of current RA
(define (vm-rega->string state)
  (vm-cell->string
   (peek state ZP_RA)
   (peek state (add1 ZP_RA))
   state))

(module+ test #| vm-cell->strings |#
  (check-equal? (vm-cell->string #xc4 #xc0)
                "ptr[-] $c0c4")
  (check-equal? (vm-cell->string #xc1 #xc0)
                "pair-ptr[-] $c0c1")
  (check-equal? (vm-cell->string #x7b #x15)
                "int $1e15")
  (check-equal? (vm-cell->string TAG_BYTE_BYTE_CELL #x15)
                "byte $15"))

(define (vm-cells->strings byte-list (result (list)))
  (if (empty? byte-list)
      (reverse result)
      (vm-cells->strings
       (cddr byte-list)
       (cons (vm-cell->string (car byte-list)
                             (cadr byte-list))
             result))))

(module+ test #| vm-cells->strings |#
  (check-equal? (vm-cells->strings '(#x01 #x00 #x03 #x01))
                '("pair-ptr NIL" "int $0001")))

(module+ test #| vm-stack->strings |#
  (define test-vm_stack_to_string-a-code
    (list (JSR PUSH_NIL_TO_EVLSTK)
          (JSR PUSH_NIL_TO_EVLSTK)
          (LDA !$01)
          (LDX !$03)
          (JSR PUSH_INT_TO_EVLSTK)
          (JSR PUSH_NIL_TO_EVLSTK)))
  (define test-vm_stack_to_string-a-state-after
    (run-code-in-test test-vm_stack_to_string-a-code))

  (check-equal? (vm-stack->strings test-vm_stack_to_string-a-state-after)
                '("stack holds 4 items"
                  "pair-ptr NIL  (rt)"
                  "int $0301"
                  "pair-ptr NIL"
                  "pair-ptr NIL")))

;; input:  x  (00 = RT, 02 = RA)
;; output: Rx
;; NO PUSH IS DONE, Rx IS SIMPLY OVERWRITTEN!
(define WRITE_NIL_TO_Rx
  (list
   (label WRITE_NIL_TO_RA)
          (LDX !$02) ;; index 2 => RA
          (BNE WRITE_NIL_TO_Rx)
   (label WRITE_NIL_TO_RT)
          (LDX !$00) ;; index 0 => RT
   (label WRITE_NIL_TO_Rx)
          (LDA !<TAGGED_NIL)
          (STA ZP_RT,x)
          (LDA !>TAGGED_NIL)
          (STA ZP_RT+1,x)
          (RTS)))

;; input: A = lowbyte of int (0..255), written into high byte of cell register RT
;;        Y = highbyte (0.31), written into lowbyte and tagged lowbyte of cell register
;;        X = (0 = RT, 2 = RA)
;; output: Rx = cell-int
(define WRITE_INT_AY_TO_Rx
  (list
   (label WRITE_INTm1_TO_RA)
          (LDX !$02) ;; index 2 => RA
          (BNE WRITE_INTm1_TO_Rx)
   (label WRITE_INTm1_TO_RT)
          (LDX !$00) ;; index 0 => RT
   (label WRITE_INTm1_TO_Rx)
          (LDA !$ff) ;; int lowbyte = ff
          (LDY !$7f) ;; #b[0]111 11[11] = $1f for int high byte
          (BNE VM_WRITE_AY_TO_Rx)


   (label WRITE_INT1_TO_RA)
          (LDX !$02) ;; index 2 => RA
          (BNE WRITE_INT1_TO_Rx)
   (label WRITE_INT1_TO_RT)
          (LDX !$00) ;; index 0 => RT
   (label WRITE_INT1_TO_Rx)
          (LDA !$01)
          (BNE WRITE_INT_A_TO_Rx)

   (label WRITE_INT0_TO_RA)
          (LDX !$02) ;; index 2 => RA
          (BNE WRITE_INT0_TO_Rx)
   (label WRITE_INT0_TO_RT)
          (LDX !$00) ;; index 0 => RT
   (label WRITE_INT0_TO_Rx)
          (LDA !$00)
          (BEQ WRITE_INT_A_TO_Rx)

   (label WRITE_INT_A_TO_RA)
          (LDX !$02) ;; index 2 => RA
          (BNE WRITE_INT_A_TO_Rx)
   (label WRITE_INT_A_TO_RT)
          (LDX !$00) ;; index 0 => RT
   (label WRITE_INT_A_TO_Rx)
          (LDY !$03) ;; #b[0]000 00[11] = high byte of int  0
   (label VM_WRITE_AY_TO_Rx)
          (STY ZP_RT,x)
          (STA ZP_RT+1,x)
          (RTS)

   (label WRITE_INT_AY_TO_RA)
          (LDX !$02) ;; index 2 => RA
          (BNE WRITE_INT_AY_TO_Rx)
   (label WRITE_INT_AY_TO_RT)
          (LDX !$00) ;; index 0 => RT
   (label WRITE_INT_AY_TO_Rx)
          (STA ZP_RT+1,x)
          (TYA)      ;; #b???x xxxx
          (SEC)
          (ROL)      ;; #b??xx xxx1
          (SEC)
          (ROL)      ;; #b?xxx xx11
          (AND !$7f) ;; #xb0xxx xx11 (mask out top bit!)
          (STA ZP_RT,x) ;; encoded tagged byte of int goes into first memory cell, even though it is the high-byte part of int
          (RTS)))

(module+ test #| vm_write_int_ay_to_rx |#
  (define vm-write-int-ay-to-rx-code
    (list
     (LDA !$01)
     (LDY !$02)
     (LDX !$00)
     (JSR WRITE_INT_AY_TO_Rx)))

  (define vm-write-int-ay-to-rx-state
    (run-code-in-test vm-write-int-ay-to-rx-code))

  (check-equal? (vm-regt->string vm-write-int-ay-to-rx-state)
                "int $0201")

  (define vm-write-int-ay-to-rx2-code
    (list
     (LDA !$01)
     (LDY !$02)
     (LDX !$02)
     (JSR WRITE_INT_AY_TO_Rx)))

  (define vm-write-int-ay-to-rx2-state
    (run-code-in-test vm-write-int-ay-to-rx2-code))

  (check-equal? (vm-rega->string vm-write-int-ay-to-rx2-state)
                "int $0201")

  (define vm-write-int-ay-to-rt-code
    (list
     (LDA !$01)
     (LDY !$02)
     (JSR WRITE_INT_AY_TO_RT)))

  (define vm-write-int-ay-to-rt-state
    (run-code-in-test vm-write-int-ay-to-rt-code))

  (check-equal? (vm-regt->string vm-write-int-ay-to-rt-state)
                "int $0201")

  (define vm-write-int-ay-to-ra-code
    (list
     (LDA !$01)
     (LDY !$02)
     (JSR WRITE_INT_AY_TO_RA)))

  (define vm-write-int-ay-to-ra-state
    (run-code-in-test vm-write-int-ay-to-ra-code))

  (check-equal? (vm-rega->string vm-write-int-ay-to-ra-state)
                "int $0201"))

;; input:  RT
;;         RA must be cell-pair-ptr
;; output: cell1 of cell-pair pointed to be RA is set to RT
(define WRITE_RT_TO_CELLy_CELLPAIR_RA
  (list
   (label WRITE_RT_TO_CELL1_CELLPAIR_RA)
          (LDY !$02) ;; offset 2 for cell1
          (BNE WRITE_RT_TO_CELLy_CELLPAIR_RA)

   (label WRITE_RT_TO_CELL0_CELLPAIR_RA)
          (LDY !$00) ;; offset 0 for cell0

   ;; ----------------------------------------
   (label WRITE_RT_TO_CELLy_CELLPAIR_RA)
          (LDA ZP_RT)
          (STA (ZP_RA),y)
          (INY)
          (LDA ZP_RT+1)
          (STA (ZP_RA),y)
          (RTS)))

(module+ test #| vm-write-rt-to-celly-ra |#
  (define vm_write_rt_to_celly_ra_code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR CP_RT_TO_RA)
     (LDA !$01)
     (LDY !$10)
     (JSR WRITE_INT_AY_TO_RT)
     (LDY !$00)
     (JSR WRITE_RT_TO_CELLy_CELLPAIR_RA)
     (LDA !$10)
     (LDY !$01)
     (JSR WRITE_INT_AY_TO_RT)
     (LDY !$02)
     (JSR WRITE_RT_TO_CELLy_CELLPAIR_RA)))

  (define vm_write_rt_to_celly_ra_state
    (run-code-in-test vm_write_rt_to_celly_ra_code))

  (check-equal? (vm-regt->string vm_write_rt_to_celly_ra_state)
                "int $0110")

  (check-equal? (vm-rega->string vm_write_rt_to_celly_ra_state)
                (format "pair-ptr[0] $~a05" (format-hex-byte PAGE_AVAIL_0)))

  (check-equal? (vm-deref-cell-pair-w->string vm_write_rt_to_celly_ra_state (+ PAGE_AVAIL_0_W #x05))
                "(int $1001 . int $0110)"))

;; input:  RT
;;         RA must be cell-pair-ptr
;; output: cell1 of cell-pair pointed to be RA is set to RT
(define WRITE_RA_TO_CELLy_CELLPAIR_RT
  (list
   (label WRITE_RA_TO_CELL1_CELLPAIR_RT)
          (LDY !$02)
          (BNE WRITE_RA_TO_CELLy_CELLPAIR_RT)

   (label WRITE_RA_TO_CELL0_CELLPAIR_RT)
          (LDY !$00)

   ;; ----------------------------------------
   (label WRITE_RA_TO_CELLy_CELLPAIR_RT)
          (LDA ZP_RA)
          (STA (ZP_RT),y)
          (INY)
          (LDA ZP_RA+1)
          (STA (ZP_RT),y)
          (RTS)))

(module+ test #| vm-write-ra-to-celly-rt |#
  (define vm_write_ra_to_celly_rt_code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     (LDA !$01)
     (LDY !$10)
     (JSR WRITE_INT_AY_TO_RA)
     (LDY !$00)
     (JSR WRITE_RA_TO_CELLy_CELLPAIR_RT)
     (LDA !$10)
     (LDY !$01)
     (JSR WRITE_INT_AY_TO_RA)
     (LDY !$02)
     (JSR WRITE_RA_TO_CELLy_CELLPAIR_RT)))

  (define vm_write_ra_to_celly_rt_state
    (run-code-in-test vm_write_ra_to_celly_rt_code))

  (check-equal? (vm-rega->string vm_write_ra_to_celly_rt_state)
                "int $0110")

  (check-equal? (vm-regt->string vm_write_ra_to_celly_rt_state)
                (format "pair-ptr[0] $~a05" (format-hex-byte PAGE_AVAIL_0)))

  (check-equal? (vm-deref-cell-pair-w->string vm_write_ra_to_celly_rt_state (+ PAGE_AVAIL_0_W #x05))
                "(int $1001 . int $0110)"))

;; input:  cell-stack (TOS)
;;         RT (must be a cell-pair ptr
;;         y = (0 = cell0, 2 = cell1)
;; output: cell-stack (one value less)
;;         cell0 of RA is set
(define POP_CELL_EVLSTK_TO_CELLy_RT
  (add-label-suffix
   "__" "__POP_CELL_EVLSTK_TO_CELLy_RT"
  (list
   (label POP_CELL_EVLSTK_TO_CELL1_RT)
          (LDY !$03)
          (BNE Y_ON_HIGHBYTE__)

   (label POP_CELL_EVLSTK_TO_CELL0_RT)
          (LDY !$00)

   ;; ----------------------------------------
   (label POP_CELL_EVLSTK_TO_CELLy_RT)
          (INY)
   (label Y_ON_HIGHBYTE__)
          (STY ZP_TEMP)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_LB_PTR),y)
          (TAX)
          (LDA (ZP_CELL_STACK_HB_PTR),y)
          (LDY ZP_TEMP)
          (STA (ZP_RT),y)
          (DEY)
          (TXA)
          (STA (ZP_RT),y)
          (DEC ZP_CELL_STACK_TOS)
          (RTS))))

(module+ test #| vm-pop-fstos-to-celly-rt |#
  (define vm-pop-fstos-to-celly-rt-code
    (list
     (JSR PUSH_INT_1_TO_EVLSTK)
     (JSR PUSH_INT_m1_TO_EVLSTK)
     (JSR PUSH_INT_1_TO_EVLSTK)
     (JSR ALLOC_CELLPAIR_TO_RT)
     (LDY !$00)
     (JSR POP_CELL_EVLSTK_TO_CELLy_RT)
     (LDY !$02)
     (JSR POP_CELL_EVLSTK_TO_CELLy_RT)
     ))

  (define vm-pop-fstos-to-celly-rt-state
    (run-code-in-test vm-pop-fstos-to-celly-rt-code))

  (check-equal? (vm-stack->strings vm-pop-fstos-to-celly-rt-state)
                (list "stack holds 1 item"
                      (format  "pair-ptr[0] $~a05  (rt)" (format-hex-byte PAGE_AVAIL_0))))
  (check-equal? (vm-deref-cell-pair-w->string vm-pop-fstos-to-celly-rt-state (+ PAGE_AVAIL_0_W #x05))
                "(int $1fff . int $0001)"))

;; input:  RA
;; output: RT (copy of RA)
(define CP_RA_TO_RT
  (list
   (label CP_RA_TO_RT)
   (label CP_RA_TO_RT__VALUE) ;;just value, no tagged byte
          (LDA ZP_RA)
          (STA ZP_RT)
          (LDA ZP_RA+1)
          (STA ZP_RT+1)
          (RTS)))

;; input:  RA
;; output: RC (copy of RA)
(define CP_RA_TO_RC
  (list
   (label CP_RA_TO_RC)
          (LDA ZP_RA)
          (STA ZP_RC)
          (LDA ZP_RA+1)
          (STA ZP_RC+1)
          (RTS)))

;; input:  RT
;; output: RC (copy of RT)
(define CP_RT_TO_RC
  (list
   (label CP_RT_TO_RC)
          (LDA ZP_RT)
          (STA ZP_RC)
          (LDA ZP_RT+1)
          (STA ZP_RC+1)
          (RTS)))

(module+ test #| vm-cp-rt-to-ra |#
  (define vm-cp-ra-to-rt-code
    (list
     (JSR WRITE_INT1_TO_RA)
     (JSR CP_RA_TO_RT)))

  (define vm-cp-ra-to-rt-state
    (run-code-in-test vm-cp-ra-to-rt-code))

  (check-equal? (vm-rega->string vm-cp-ra-to-rt-state)
                "int $0001")
  (check-equal? (vm-regt->string vm-cp-ra-to-rt-state)
                "int $0001"))

;; input:  RT
;; output: RA (copy of RT)
(define CP_RT_TO_RA
  (list
   (label CP_RT_TO_RA)
   (label CP_RT_TO_RA__VALUE) ;;just value, no tagged byte
          (LDA ZP_RT)
          (STA ZP_RA)
          (LDA ZP_RT+1)
          (STA ZP_RA+1)
          (RTS)))

(module+ test #| vm-cp-rt-to-ra |#
  (define vm-cp-rt-to-ra-code
    (list
     (JSR WRITE_INT1_TO_RT)
     (JSR CP_RT_TO_RA)))

  (define vm-cp-rt-to-ra-state
    (run-code-in-test vm-cp-rt-to-ra-code))

  (check-equal? (vm-rega->string vm-cp-rt-to-ra-state)
                "int $0001")
  (check-equal? (vm-regt->string vm-cp-rt-to-ra-state)
                "int $0001"))

;; input:  Y - 0 (cell0), 2 (cell1)
;;         RT (must be cell-pair ptr)
;; output: RT
(define WRITE_CELLPAIR_RT_CELL1_TO_RT #t)
(define WRITE_CELLPAIR_RT_CELL0_TO_RT #t)
(define WRITE_CELLPAIR_RT_CELLy_TO_RT
  (list
   (label WRITE_CELLPAIR_RT_CELL1_TO_RT)
          (LDY !$02)
          (BNE WRITE_CELLPAIR_RT_CELLy_TO_RT)

   (label WRITE_CELLPAIR_RT_CELL0_TO_RT)
          (LDY !$00)

   ;; ----------------------------------------
   (label WRITE_CELLPAIR_RT_CELLy_TO_RT)
          (LDA (ZP_RT),y)
          (TAX)
          (INY)
          (LDA (ZP_RT),y)
          (STA ZP_RT+1)
          (STX ZP_RT)
          (RTS)))

(module+ test #| vm-write-rt-celly-to-rt |#
  (define vm-write-rt-celly-to-rt-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     (LDA !$01)
     (LDY !$10)
     (JSR WRITE_INT_AY_TO_RA)
     (LDY !$00)
     (JSR WRITE_RA_TO_CELLy_CELLPAIR_RT)
     (LDA !$10)
     (LDY !$01)
     (JSR WRITE_INT_AY_TO_RA)
     (LDY !$02)
     (JSR WRITE_RA_TO_CELLy_CELLPAIR_RT)
     (JSR WRITE_CELLPAIR_RT_CELL0_TO_RT)))

  (define vm-write-rt-celly-to-rt-state
    (run-code-in-test vm-write-rt-celly-to-rt-code))

  (check-equal? (vm-regt->string vm-write-rt-celly-to-rt-state)
                "int $1001")

  (check-equal? (vm-deref-cell-pair-w->string vm-write-rt-celly-to-rt-state (+ PAGE_AVAIL_0_W #x05))
                "(int $1001 . int $0110)"))

;; input:  Y - 0 (cell0), 2 (cell1)
;;         RT (must be cell-pair ptr)
;; output: RA
(define WRITE_CELLPAIR_RT_CELL1_TO_RA #t)
(define WRITE_CELLPAIR_RT_CELL0_TO_RA #t)
(define WRITE_CELLPAIR_RT_CELLy_TO_RA
  (list
   (label WRITE_CELLPAIR_RT_CELL1_TO_RA)
          (LDY !$02)
          (BNE WRITE_CELLPAIR_RT_CELLy_TO_RA)

   (label WRITE_CELLPAIR_RT_CELL0_TO_RA)
          (LDY !$00)

   ;; ----------------------------------------
   (label WRITE_CELLPAIR_RT_CELLy_TO_RA)
          (LDA (ZP_RT),y)
          (STA ZP_RA)
          (INY)
          (LDA (ZP_RT),y)
          (STA ZP_RA+1)
          (RTS)))

(define WRITE_CELLPAIR_RA_CELLy_TO_RA
  (list
   (label WRITE_CELLPAIR_RA_CELL1_TO_RA)
          (LDY !$02)
          (BNE WRITE_CELLPAIR_RA_CELLy_TO_RA)

   (label WRITE_CELLPAIR_RA_CELL0_TO_RA)
          (LDY !$00)

   ;; ----------------------------------------
   (label WRITE_CELLPAIR_RA_CELLy_TO_RA)
          (LDA (ZP_RA),y)
          (TAX)
          (INY)
          (LDA (ZP_RA),y)
          (STA ZP_RA+1)
          (TXA)
          (STA ZP_RA)
          (RTS)))

(module+ test #| vm-write-rt-celly-to-ra |#
  (define vm-write-rt-celly-to-ra-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     (LDA !$01)
     (LDY !$10)
     (JSR WRITE_INT_AY_TO_RA)
     (JSR WRITE_RA_TO_CELL0_CELLPAIR_RT)
     (LDA !$10)
     (LDY !$01)
     (JSR WRITE_INT_AY_TO_RA)
     (JSR WRITE_RA_TO_CELL1_CELLPAIR_RT)
     (JSR WRITE_INT0_TO_RA) ;; clear ra
     (JSR WRITE_CELLPAIR_RT_CELL0_TO_RA)))

  (define vm-write-rt-celly-to-ra-state
    (run-code-in-test vm-write-rt-celly-to-ra-code))

  (check-equal? (vm-rega->string vm-write-rt-celly-to-ra-state)
                "int $1001")

  (check-equal? (vm-deref-cell-pair-w->string vm-write-rt-celly-to-ra-state (+ PAGE_AVAIL_0_W #x05))
                "(int $1001 . int $0110)"))

;; input:  call-frame stack, RT
;; output: call-frame stack << RT
;; uses:   A Y
;; CHECK STACK PAGE OVERFLOW
(define PUSH_RT_TO_EVLSTK
  (add-label-suffix
   "__" "__PUSH_RT_TO_EVLSTK"
  (list
   (label PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
          (LDY ZP_RT)
          ;; if RT empty?  = $00 
          (BEQ DONE__)        ;; then no push

   ;; ----------------------------------------
   (label PUSH_RT_TO_EVLSTK)
          (LDY ZP_CELL_STACK_TOS)
          (INY)
          [BNE NO_ERROR__]

   (label ALLOCATE_NEW_STACK_PAGE__)
          (JSR ALLOC_PAGE_TO_X)
          (LDA ZP_CELL_STACK_LB_PTR+1)
          (JSR INIT_CELLSTACK_PAGE_X)
          (STX ZP_CELL_STACK_LB_PTR+1)

          (JSR ALLOC_PAGE_TO_X)
          (LDA ZP_CELL_STACK_HB_PTR+1)
          (JSR INIT_CELLSTACK_PAGE_X)
          (STX ZP_CELL_STACK_HB_PTR+1)

          (LDY !$02)                          ;; new tos starts 

   (label NO_ERROR__)
          (LDA ZP_RT+1)
          (STA (ZP_CELL_STACK_HB_PTR),y)      ;; write high byte! 
          (LDA ZP_RT)
          (STA (ZP_CELL_STACK_LB_PTR),y)      ;; write low byte 
          (STY ZP_CELL_STACK_TOS)             ;; set new tos

   (label DONE__)
          (RTS))))

(module+ test #| vm-cell-stack-just-push-rt |#
  (define vm-cell-stack-just-push-rt-code
    (list     
     (JSR WRITE_INTm1_TO_RT)
     (JSR PUSH_RT_TO_EVLSTK)))

  (define vm-cell-stack-just-push-rt-state
    (run-code-in-test vm-cell-stack-just-push-rt-code))

  (check-equal? (vm-stack->strings vm-cell-stack-just-push-rt-state)
                (list "stack holds 2 items"
                      "int $1fff  (rt)"
                      "int $1fff")))

;; push a cell onto the stack (that is push the RegT, if filled, and write the value into RegT)
;; input: call-frame stack, RT
;;        A = high byte,
;;        X = tagged low
;; output: call-frame stack, RT
(define PUSH_TO_EVLSTK
  (list

   ;; ints are saved high byte first, then low byte !!!!
   ;; X = high byte of int (max 31 = $1f) (stored in low byte (tagged) position)
   ;; A = low byte of int (0..255) (stored in high byte (untagged) position)
   (label PUSH_INT_TO_EVLSTK)         ;; idea: can be optimized since it is known that this is an atomic value
          (TAY)
          (TXA)
          (SEC)
          (ROL)
          (SEC)
          (ROL)
          (AND !$7f)           ;; mask out top bit
          (TAX)
          (TYA)
          (CLC)
          (BCC PUSH_TO_EVLSTK)

   (label PUSH_INT_m1_TO_EVLSTK)
          (LDA !$ff) ;; 1f << 2
          (LDX !$7f)
          (BNE PUSH_TO_EVLSTK)

   (label PUSH_INT_2_TO_EVLSTK)
          (LDA !$02)
          (LDX !$03)
          (BNE PUSH_TO_EVLSTK)

   (label PUSH_INT_1_TO_EVLSTK)
          (LDA !$01)
          (LDX !$03)
          (BNE PUSH_TO_EVLSTK)

   (label PUSH_INT_0_TO_EVLSTK)
          (LDA !$00)
          (LDX !$03)
          (BNE PUSH_TO_EVLSTK)

   ;; push NIL (cell-pair-ptr)           ;; idea: can be optimized since it is known that this is cell-pair-ptr
   (label PUSH_NIL_TO_EVLSTK)
          (LDX !<TAGGED_NIL)
          (LDA !>TAGGED_NIL)

   ;; push a cell
   ;; A = high byte
   ;; X = tagged low byte
   (label PUSH_TO_EVLSTK)
          (PHA)
          (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY) ;; uses A and Y
          (PLA)

   (label VM_WRITE_AX_TO_RT)
          (STX ZP_RT)          
          (STA ZP_RT+1)
          (RTS)))

(module+ test #| vm_cell_stack_push_r (basically on write into rt, since stack is completely empty) |#

  (define vm_cell_stack_push_r_int0_code
    (list
     (JSR PUSH_INT_0_TO_EVLSTK)))

  (define vm_cell_stack_push_r_int0_state
    (run-code-in-test vm_cell_stack_push_r_int0_code))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_int0_state)
                "int $0000")
  (check-equal? (memory-list vm_cell_stack_push_r_int0_state ZP_RT (add1 ZP_RT))
                (list #x03 #x00))

  (define vm_cell_stack_push_r_int1_code
    (list
     (JSR PUSH_INT_1_TO_EVLSTK)))

  (define vm_cell_stack_push_r_int1_state
    (run-code-in-test vm_cell_stack_push_r_int1_code))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_int1_state)
                "int $0001")
  (check-equal? (memory-list vm_cell_stack_push_r_int1_state ZP_RT (add1 ZP_RT))
                (list #x03 #x01))

  (define vm_cell_stack_push_r_intm1_code
    (list
     (JSR PUSH_INT_m1_TO_EVLSTK)))

  (define vm_cell_stack_push_r_intm1_state
    (run-code-in-test vm_cell_stack_push_r_intm1_code))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_intm1_state)
                "int $1fff")
  (check-equal? (memory-list vm_cell_stack_push_r_intm1_state ZP_RT (add1 ZP_RT))
                (list #x7f #xff))

  (define vm_cell_stack_push_r_nil_code
    (list
     (JSR PUSH_NIL_TO_EVLSTK)))

  (define vm_cell_stack_push_r_nil_state
    (run-code-in-test vm_cell_stack_push_r_nil_code))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_nil_state)
                "pair-ptr NIL")
  (check-equal? (memory-list vm_cell_stack_push_r_nil_state ZP_RT (add1 ZP_RT))
                (list #x01 #x00))


  (define vm_cell_stack_push_r_cell_ptr_code
    (list
     (LDX !$03)
     (LDA !$ce)
     (JSR PUSH_TO_EVLSTK)))

  (define vm_cell_stack_push_r_cell_ptr_state
    (run-code-in-test vm_cell_stack_push_r_cell_ptr_code))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_cell_ptr_state)
                "int $00ce")
  (check-equal? (memory-list vm_cell_stack_push_r_cell_ptr_state ZP_RT (add1 ZP_RT))
                (list #x03 #xce))

  (define vm_cell_stack_push_r_cell_pair_ptr_code
    (list
     (LDX !$05)
     (LDA !$ce)
     (JSR PUSH_TO_EVLSTK)))

  (define vm_cell_stack_push_r_cell_pair_ptr_state
    (run-code-in-test vm_cell_stack_push_r_cell_pair_ptr_code))

  (check-equal? (vm-regt->string vm_cell_stack_push_r_cell_pair_ptr_state)
                "pair-ptr[0] $ce05")
  (check-equal? (memory-list vm_cell_stack_push_r_cell_pair_ptr_state ZP_RT (add1 ZP_RT))
                (list #x05 #xce)))

(module+ test #| vm_cell_stack_push_r (push rt, and write rt) |#
  (define vm_cell_stack_push_r_push1_code
    (list
     (JSR PUSH_INT_m1_TO_EVLSTK)
     (JSR PUSH_INT_1_TO_EVLSTK)
     ))

  (define vm_cell_stack_push_r_push1_state
    (run-code-in-test vm_cell_stack_push_r_push1_code))

  (check-equal? (vm-stack->strings vm_cell_stack_push_r_push1_state)
                (list "stack holds 2 items"
                      "int $0001  (rt)"
                      "int $1fff"))

  (check-equal? (memory-list vm_cell_stack_push_r_push1_state ZP_RT (add1 ZP_RT))
                (list #x03 #x01))

  (define vm_cell_stack_push_r_push2_code
    (list
     (JSR PUSH_INT_m1_TO_EVLSTK)
     (JSR PUSH_INT_1_TO_EVLSTK)
     (JSR PUSH_NIL_TO_EVLSTK)
     ))

  (define vm_cell_stack_push_r_push2_state
    [run-code-in-test vm_cell_stack_push_r_push2_code])

  (check-equal? (vm-stack->strings vm_cell_stack_push_r_push2_state)
                (list "stack holds 3 items"
                      "pair-ptr NIL  (rt)"
                      "int $0001"
                      "int $1fff"))

  (check-equal? (memory-list vm_cell_stack_push_r_push2_state ZP_RT (add1 ZP_RT))
                (list #x01 #x00)))

;; pop cell from stack (that is, discard RegT, move tos of call-frame stack into RegT (if available))
;; input: call-frame stack, RT
;; output: call-frame stack reduced by`1, RT <- popped value
;; NO GC CHECKS!
(define POP_CELL_EVLSTK_TO_RT
  (list
   (label POP_CELL_EVLSTK_TO_RT)
          ;; optional: stack marked empty? => error: cannot pop from empty stack!
          ;; (LDY !$00)
          ;; (BEQ ERROR_NO_VALUE_ON_STACK)

          ;; is call-frame stack empty? => mark stack as empty and return | alternatively simply write NIL into RT
          (LDY ZP_CELL_STACK_TOS)
          (CPY !$01) ;; stack empty?
          (BEQ WRITE_00_TO_RT) ;; which effectively clears the RT
          ;; pop value from call-frame stack into RT!
          (LDA (ZP_CELL_STACK_LB_PTR),y) ;; tagged low byte
          (STA ZP_RT)


          ;; (optional) quick check for atomic cells [speeds up popping atomic cells, slows popping cell-ptr, slight slows popping cell-pair-ptr
          ;; (AND !$03)
          ;; (BEQ WRITE_TOS_TO_RT__POP_CELL_EVLSTK_TO_RT)
          ;; (TXA)

          (LDA (ZP_CELL_STACK_HB_PTR),y) ;; high byte
          (STA ZP_RT+1) 
          (DEC ZP_CELL_STACK_TOS)
          (RTS)

   (label WRITE_00_TO_RT)
          ;; mark RT as empty
          (LDA !$00)
          (STA ZP_RT)
          (STA ZP_RT+1)
          (RTS)))

(module+ test #| vm_cell_stack_pop_r (just one value) |#
  (define vm_cell_stack_pop3_r_code
    (list
     (JSR PUSH_INT_1_TO_EVLSTK)
     (JSR PUSH_INT_m1_TO_EVLSTK)
     (JSR PUSH_INT_0_TO_EVLSTK)
     (JSR POP_CELL_EVLSTK_TO_RT)
     ))

  (define vm_cell_stack_pop3_r_state
    (run-code-in-test vm_cell_stack_pop3_r_code))

  (check-equal? (vm-stack->strings vm_cell_stack_pop3_r_state)
                (list "stack holds 2 items"
                      "int $1fff  (rt)"
                      "int $0001"))

  (check-equal? (memory-list vm_cell_stack_pop3_r_state ZP_RT (add1 ZP_RT))
                (list #x7f #xff))

  (define vm_cell_stack_pop2_r_code
    (list
     (JSR PUSH_INT_1_TO_EVLSTK)
     (JSR PUSH_INT_m1_TO_EVLSTK)
     (JSR PUSH_INT_0_TO_EVLSTK)
     (JSR POP_CELL_EVLSTK_TO_RT)
     (JSR POP_CELL_EVLSTK_TO_RT)))

  (define vm_cell_stack_pop2_r_state
    (run-code-in-test vm_cell_stack_pop2_r_code))

  (check-equal? (vm-stack->strings vm_cell_stack_pop2_r_state)
                (list "stack holds 1 item"
                      "int $0001  (rt)"))

  (define vm_cell_stack_pop1_r_code
    (list
     (JSR PUSH_INT_1_TO_EVLSTK)
     (JSR PUSH_INT_m1_TO_EVLSTK)
     (JSR PUSH_INT_0_TO_EVLSTK)
     (JSR POP_CELL_EVLSTK_TO_RT)
     (JSR POP_CELL_EVLSTK_TO_RT)
     (JSR POP_CELL_EVLSTK_TO_RT)))

  (define vm_cell_stack_pop1_r_state
    (run-code-in-test vm_cell_stack_pop1_r_code))

  (check-equal? (vm-stack->strings vm_cell_stack_pop1_r_state)
                (list "stack is empty"))

  (check-equal? (memory-list vm_cell_stack_pop1_r_state ZP_RT (add1 ZP_RT))
                (list #x00 #x00)))

(module+ test #| vm_cell_stack_push_nil_r |#
  (define test-vm_cell_stack_push_nil-a-state-after
    (run-code-in-test
     (list (JSR PUSH_NIL_TO_EVLSTK))))

  (check-equal? (vm-regt->string test-vm_cell_stack_push_nil-a-state-after)
                "pair-ptr NIL")

  (define test-vm_cell_stack_push_nil-b-state-after
    (run-code-in-test
     (list (JSR PUSH_NIL_TO_EVLSTK) ;; 1
           (JSR PUSH_NIL_TO_EVLSTK) ;;
           (JSR PUSH_NIL_TO_EVLSTK) ;; 3
           (JSR PUSH_NIL_TO_EVLSTK) ;;
           (JSR PUSH_NIL_TO_EVLSTK) ;; 5
           (JSR PUSH_NIL_TO_EVLSTK) ;;
           (JSR PUSH_NIL_TO_EVLSTK) ;; 7
           (JSR PUSH_NIL_TO_EVLSTK)))) ;; 8

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_nil-b-state-after)
                '("stack holds 8 items"
                  "pair-ptr NIL  (rt)"
                  "pair-ptr NIL"
                  "pair-ptr NIL"
                  "pair-ptr NIL"
                  "pair-ptr NIL"
                  "pair-ptr NIL"
                  "pair-ptr NIL"
                  "pair-ptr NIL")))

(module+ test #| vm_cell_push_int_r |#
  (define test-vm_cell_stack_push_int-a-state-after
    (run-code-in-test
     (list (JSR PUSH_INT_m1_TO_EVLSTK)
           (LDA !$00) ;; -4096
           (LDX !$10)
           (JSR PUSH_INT_TO_EVLSTK)
           (JSR PUSH_INT_1_TO_EVLSTK)
           (JSR PUSH_INT_0_TO_EVLSTK)
           (LDA !$ff) ;; 4095
           (LDX !$0f)
           (JSR PUSH_INT_TO_EVLSTK))))

  (check-equal? (vm-regt->string test-vm_cell_stack_push_int-a-state-after)
                "int $0fff")
  (check-equal? (vm-stack->strings test-vm_cell_stack_push_int-a-state-after)
                '("stack holds 5 items"
                  "int $0fff  (rt)"
                  "int $0000"
                  "int $0001"
                  "int $1000"
                  "int $1fff")))

;; initial data for the memory management registers
;; put into memory @ #xcec0 - len (currently 3)
(define VM_INITIAL_MM_REGS
  (list
   (label VM_INITIAL_MM_REGS)

   ;; $cec0
   (label VM_FREE_CELL_PAGE) ;; page with free cells
          (byte $00)
   ;; $cec1
   (label VM_FREE_CALL_STACK_PAGE) ;; call stack page with free space
          (byte $00) ;; initial -> first allocation will allocate a new page
   ;; $cec2
   (label VM_FREE_CODE_PAGE) ;; code page with free space
          (byte $00)

   ;; $cec3
   (label VM_FREE_CELL_PAIR_PAGE) ;; page with free cell-pairs
          (byte $00) ;; none

   ;; $cec4
   (label VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; what is the highest page to start searching for a free page
          (byte $cd) ;; safe to start with $cd is index

   ;; $cec5
   (label VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE) ;; list of cell-pairs that are unused but only potentially partially freed (second cell may still hold references to heap objects)
          (word $0000) ;; if high byte is 0, the tree is empty!
          ;; this queue holds only cell-pairs, cell0 is always the pointer to the next in queue of this free cells
          ;; cell1 is left untouched => may still hold live references => to reuse a cell-pair of this queue,
          ;; cell1 must be checked (if ptr, decr ref count and possibly free, else ignore)

   ;; $cec7
   (label VM_FREE_M1_PAGE_P0)
          (byte $00) ;; cell page with free slots for m1 page p0 pages
          (byte $00) ;; cell page with free slots for m1 page p1 pages
          (byte $00) ;; cell page with free slots for m1 page p2 pages
          (byte $00) ;; cell page with free slots for m1 page p3 pages
          (byte $00) ;; cell page with free slots for m1 page p5 pages

   ;; $cecc..cecd
   (label VM_LIST_OF_FREE_CELLS) ;; list of cells that are unused but still allocated (reusable)
          (word $0000)

   ;; cece,,cecf
   (label VM_P0_QUEUE_ROOT_OF_ARRAYS_TO_FREE)
          (word $0000)
   ;; ced0,,ced1
   (label VM_P1_QUEUE_ROOT_OF_ARRAYS_TO_FREE)
          (word $0000)
   ;; ced2,,ced3
   (label VM_P2_QUEUE_ROOT_OF_ARRAYS_TO_FREE)
          (word $0000)
   ;; ced4,,ced5
   (label VM_P3_QUEUE_ROOT_OF_ARRAYS_TO_FREE)
          (word $0000)
   ;; ced6,,ced7
   (label VM_P4_QUEUE_ROOT_OF_ARRAYS_TO_FREE)
          (word $0000)

   ;; $ced8..$ceff (unused) (40 bytes)
   ))

(define VM_LIST_OF_FREE_CELLS               #xcecc)
(define VM_FREE_CELL_PAIR_PAGE              #xcec3)
(define VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE #xcec5)

(define VM_PAGE_SLOT_DATA
  (list
   (label VM_PAGE_SLOT_DATA)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem 0000-07ff is unavailable (zero page, stack ... screen)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem 0800-0fff is unavailable (start of basic ram)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem 1000-17ff is unavailable
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem 1800-1fff is unavailable
          (byte $01 $01 $ff $ff  $ff $ff $ff $ff)     ;; mem 2000-27ff 2000-21ff is unavailable, 2200-27ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 2800-2fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 3000-37ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 3800-3fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 4000-47ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 4800-4fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 5000-57ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 5800-5fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 6000-67ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 6800-6fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 7000-77ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 7800-7fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 8000-87ff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 8800-8fff is free
          (byte $ff $ff $ff $ff  $ff $ff $ff $ff)     ;; mem 9000-97ff is free
          (byte $ff $ff $ff $ff  $ff $01 $01 $01)     ;; mem 9800-9cff is free, 9d00..9dff = first free code page, 9e00..9eff stack page, 9f00..9fff cell page
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem A000-A7ff is unavailable (C64 BASIC)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem A800-Afff is unavailable (C64 BASIC)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem B000-B7ff is unavailable (C64 BASIC)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem B800-Bfff is unavailable (C64 BASIC)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem c000-c7ff is unavailable (vm code)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem c800-cdff is unavailable (vm code, ce00-ceff = other memory management registers + bitmap, cf00-cfff =  used by next free page mapping          
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem D000-D7ff is unavailable (C64 I/O)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem D800-Dfff is unavailable (C64 I/O)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem E000-E7ff is unavailable (C64 KERNAL)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem E800-Efff is unavailable (C64 KERNAL)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)     ;; mem F000-F7ff is unavailable (C64 KERNAL)
          (byte $01 $01 $01 $01  $01 $01 $01 $01)))   ;; mem F800-Ffff is unavailable (C64 KERNAL)

;; initialize memory management (paging)
;; - setup 'next free page' information, basically initializing the whole page with zeros
;; - setup cell stack (to empty)
;;
;; destroys: A Y
(define VM_INITIALIZE_MEMORY_MANAGER
  (flatten
   (list
    (label VM_INITIALIZE_MEMORY_MANAGER)

           ;; initialize NEXT_FREE_PAGE_PAGE (256 byte)
           (LDA !$ff)
           (TAY)
    (label VM_INITIALIZE_MEMORY_MANAGER__LOOP)
           ;; highbyte of this address should be using the constant NEXT_FREE_PAGE_PAGE
           ;; (STA $cf00,y) ;; encoded directly in the next couple of bytes
           ;; (take (ast-opcode-cmd-bytes (STA $cf00,y)) 2)
           (byte 153 0) (byte-ref NEXT_FREE_PAGE_PAGE) ;; fill CF00..CFFF with $FF!
           (INY)
           (BNE VM_INITIALIZE_MEMORY_MANAGER__LOOP)

           ;; alloc cell stack
           (JSR ALLOC_PAGE_TO_X)
           (LDA !$00)
           (STA ZP_CELL_STACK_LB_PTR)
           (JSR INIT_CELLSTACK_PAGE_X)
           (STX ZP_CELL_STACK_LB_PTR+1)

           (JSR ALLOC_PAGE_TO_X)
           (LDA !$00)
           (STA ZP_CELL_STACK_HB_PTR)
           (JSR INIT_CELLSTACK_PAGE_X)
           (STX ZP_CELL_STACK_HB_PTR+1)

           (LDA !$00)
           (STA VM_FREE_M1_PAGE_P0)
           (STA VM_FREE_M1_PAGE_P0+1)
           (STA VM_FREE_M1_PAGE_P0+2)
           (STA VM_FREE_M1_PAGE_P0+3)
           (STA VM_FREE_M1_PAGE_P0+4)

           ;; make sure the list of partially gc'd cell arrays is empty!
           (STA ZP_PART_GCD_CELL_ARRAYS)
           (STA ZP_PART_GCD_CELL_ARRAYS+1)

           (LDA !$01)
           (STA ZP_CELL_STACK_TOS)

           (LDX !$00)
           (STX ZP_RT) ;; set registers (RT, RA, RC) to hold no value!
           (STX ZP_RT+1)
           (STX ZP_RA)
           (STX ZP_RA+1)
           (STX ZP_RC)
           (STX ZP_RC+1)
           (RTS))))

  ;; page type cell page (slot size 2b) (refcount @ ptr >> 1) 84 cells (85th slot is used for previous page pointer)
  ;; offset content
  ;; 00     #b1zzz zzzz page type + number of used slots
  ;; 01     ref-count for cell at 02 (cell 0)
  ;; 02..03 cell 0  (@2 = 8 = next free cell)
  ;; 04     ref-count for cell at 08 (cell 1)
  ;; ...
  ;; 07     ref-count for cell at 08 (cell 4)
  ;; 08..09 cell 1 (@08 = 0a = next free cell)
  ;; ...
  ;; 0e..0f cell 4 (@0e = 20 = next free cell
  ;; 10    ref-count for cell at 20 (cell 5)
  ;; ...
  ;; 1f    ref-count for cell at 20 (cell 20)
  ;; 20..21 cell 5 (@20 = 22 = next free cell)
  ;; ...
  ;; 3e..3f cell 20 (@3e = 80 = next free cell)
  ;; 40..7e ref-count for cell at 80..fc (cell 21..83)
  ;; 7f    unused
  ;; 80..fd cell 21..83
  ;; fe    unused
  ;; ff    previous page of this type
  ;;
  ;; input:  X = allocated uninitialized page
  ;; output: X = initialized page (of type cell page)
  ;;         A = first free slot
  ;;         vm_free_cell_page is new head of the list
  ;;         the page is initialized with each cell pointing to the next free cell on this page (0 marks the end)
  ;; uses: ZP_TEMP, ZP_TEMP2
  (define INIT_CELL_PAGE_X_TO_AX
    (add-label-suffix
     "__" "__INIT_CELL_PAGE_X_TO_AX"
    (list
     (label INIT_CELL_PAGE_X_TO_AX)
            ;; page is in A
            (STX ZP_TEMP+1)
            (LDA !$02)
            (STA VM_PAGE_SLOT_DATA,x) ;; set slot @02 as the first free slot

            (LDA !$03)
            (STA BLOCK_LOOP_COUNT__) ;; how many blocks do we have (3)

            (LDA !$00)
            (STA ZP_TEMP)

            (LDY !$01)
            (LDX !$01)
            (STX LOOP_COUNT__)

     ;; option: optimization: maybe clearing the whole page would be faster (and shorter) for setting all refcounts to 0?
     (label LOOP_REF_COUNT__)
            (STA (ZP_TEMP),y) ;; refcount set to 0
            (INY)
            (DEX)
            (BNE LOOP_REF_COUNT__)
            (LDA LOOP_COUNT__)
            (ASL A)
            (ASL A) ;; times 4
            (STA LOOP_COUNT__)
            (TAX)
            (TAY) ;;
            (LDA !$00)
            (DEC BLOCK_LOOP_COUNT__)
            (BPL LOOP_REF_COUNT__)

            ;; initialize the free list of the cells (first byte in a cell = offset to next free cell)
            (LDA !$02)
            (STA BLOCK_LOOP_COUNT__) ;; how many blocks do we have (3, but the first block is written separately)

            ;; block 1
            (LDY !$02)
            (LDA !$08)
            (STA LOOP_COUNT__)
            (STA (ZP_TEMP),y)

            ;; block 2
            (TAY)
            (LDX !$04)
            (LDA !$0a)
            (DEX) ;; one loop less

     ;; blocks and their numbers
     ;; iterations offset next free (
     ;; #01        02     <- 08
     ;; #04        08..0f <- 0a.. last= 20   ->  # = prev offset*2, offset = prev offset*4
     ;; #10        20..3f <- 22.. last= 80
     ;; #40        80..7d <- 82.. last= 00

     (label LOOP_NEXT_FREE__)
            (STA (ZP_TEMP),y)
            (TAY)
            (CLC)
            (ADC !$02)
            (DEX)
            (BNE LOOP_NEXT_FREE__)

            ;; block n+1
            ;; write last entry
            (LDA LOOP_COUNT__)
            (ASL A)
            (TAX)
            (ASL A)
            (STA LOOP_COUNT__)
            (STA (ZP_TEMP),y)
            (TAY)
            (CLC)
            (ADC !$02)
            (DEX)
            (DEC BLOCK_LOOP_COUNT__)
            (BPL LOOP_NEXT_FREE__)

            ;; write last entry
            (LDA !$00)
            (LDY !$fc) ;; fc..fd is the last cell, fe..ff is unusable (since ff holds the previous page)
            (STA (ZP_TEMP),y)

            (LDY !$ff)
            (LDA VM_FREE_CELL_PAGE) ;; store last free cell page in $ff
            (STA (ZP_TEMP),y)

            ;; store page type in byte 0
            (LDY !$00)
            (LDA !$80)
            (STA (ZP_TEMP),y)

            (LDX ZP_TEMP+1) ;; page
            (STX VM_FREE_CELL_PAGE) ;; store allocated page as new free cell page
            (LDA !$02)

            (RTS)

     (label LOOP_COUNT__)
            (byte $00)
     (label BLOCK_LOOP_COUNT__)
            (byte $00))))

(module+ test #| vm_alloc_page__cell |#
  (define test-alloc-page--cell-code
    (list
            ;; fill page with cc
            (LDX !$00)
            (LDA !$77)            
     (label FILL_PAGE__TEST_ALLOC_PAGE__CELL)
            ;; (STA $9700,x)
            (ast-opcode-cmd '() (list 157 0 PAGE_AVAIL_0))
            (DEX)
            (BNE FILL_PAGE__TEST_ALLOC_PAGE__CELL)

            ;; now do allocation and write structure data into the page
            (JSR ALLOC_PAGE_TO_X)
            (JSR INIT_CELL_PAGE_X_TO_AX)))

  (define test-alloc-page--cell-state-after
    (run-code-in-test test-alloc-page--cell-code))

  (check-equal? (memory-list test-alloc-page--cell-state-after PAGE_AVAIL_0_W (+ PAGE_AVAIL_0_W #x0f))
                (list #x80
                      #x00       ;; ref count cell 0 (@2)
                      #x08 #x77  ;; cell0: next free @8
                      #x00       ;; refcount cell1 (@8)
                      #x00       ;; refcount cell2 (@a)
                      #x00       ;; refcount cell3 (@c)
                      #x00       ;; refcount cell4 (@e)
                      #x0a #x77  ;; cell1: next free @10
                      #x0c #x77  ;; cell2: next free @12
                      #x0e #x77  ;; cell3: next free @14
                      #x20 #x77  ;; cell4: next free @32
                      ))
  (check-equal? (memory-list test-alloc-page--cell-state-after (+ PAGE_AVAIL_0_W #x10) (+ PAGE_AVAIL_0_W #x1f))
                (make-list #x10 #x0))
  (check-equal? (memory-list test-alloc-page--cell-state-after (+ PAGE_AVAIL_0_W #x20) (+ PAGE_AVAIL_0_W #x27))
                (list #x22 #x77  ;; cell5: next free @34
                      #x24 #x77  ;; cell6: next free @36
                      #x26 #x77  ;; cell7: next free @38
                      #x28 #x77  ;; cell8: next free @40
                      ))
  (check-equal? (memory-list test-alloc-page--cell-state-after (+ PAGE_AVAIL_0_W #x38) (+ PAGE_AVAIL_0_W #x3f))
                (list #x3a #x77  ;; cell17: next free @58
                      #x3c #x77  ;; cell18: next free @60
                      #x3e #x77  ;; cell19: next free @62
                      #x80 #x77  ;; cell20: next free @128
                      ))
  (check-equal? (memory-list test-alloc-page--cell-state-after (+ PAGE_AVAIL_0_W #x40) (+ PAGE_AVAIL_0_W #x7e))
                (make-list #x3f #x0)
                "refcounts are all zero")
  (check-equal? (memory-list test-alloc-page--cell-state-after (+ PAGE_AVAIL_0_W #x80) (+ PAGE_AVAIL_0_W #x87))
                (list #x82 #x77  ;; cell21: next free @130
                      #x84 #x77  ;; cell22: next free @132
                      #x86 #x77  ;; cell23: next free @134
                      #x88 #x77  ;; cell24: next free @136
                      ))
  (check-equal? (memory-list test-alloc-page--cell-state-after (+ PAGE_AVAIL_0_W #xf8) (+ PAGE_AVAIL_0_W #xff))
                (list #xfa #x77  ;; cell: next free @250
                      #xfc #x77  ;; cell: next free @252
                      #x00 #x77  ;; cell: next free 0
                      #x00       ;; unused
                      #x00       ;; pointer to previous page
                      )))


;; cell stack page(s)
;; offset  content
;; ---------------
;; 00      #b0001 1011
;; 01      previous page (of the stack)
;; 02..ff  payload (either lowbyte or highbyte of the cell)
;;
;; input:  A old stack page
;;         X new stack page
;; output: X new stack page
;; uses:   A, X, Y
;;         ZP_TEMP, ZP_TEMP+1
(define INIT_CELLSTACK_PAGE_X
  (list
   (label INIT_CELLSTACK_PAGE_X)
          (STX ZP_TEMP+1)         ;; write page into hightbyte of ZP_TEMP ptr
          (TAX)                   ;; old page in a -> x
          (LDA !$00)
          (STA ZP_TEMP)           ;; write 0 into lowbyte of ZP_TEMP ptr
          (TAY)                   ;; init y with 0
          (LDA !$1b)              ;; id for page type: cellstack
          (STA (ZP_TEMP),y)       ;; first byte on page: page type: cellstack
          (INY)
          (TXA)                   ;; x (old stack page) -> a
          (STA (ZP_TEMP),y)       ;; second byte on page: previous stack page
          (LDX ZP_TEMP+1)         ;; restore A with new page
          (RTS)))

(module+ test #| alloc cell stack pages |#
  (define alloc-cell-stack-pages-code
    (list
     (JSR ALLOC_PAGE_TO_X)
     (LDA !$05)
     (JSR INIT_CELLSTACK_PAGE_X)
     (STX ZP_RT+1)

     (JSR ALLOC_PAGE_TO_X)
     (LDA !$03)
     (JSR INIT_CELLSTACK_PAGE_X)
     (STX ZP_RT)))

  (define alloc-cell-stack-pages-state
    (run-code-in-test alloc-cell-stack-pages-code))

  (check-equal? (memory-list alloc-cell-stack-pages-state ZP_RT (add1 ZP_RT))
                (list PAGE_AVAIL_1 PAGE_AVAIL_0)
                ".. is new low byte page, .. is new high byte page")
  (check-equal? (memory-list alloc-cell-stack-pages-state PAGE_AVAIL_1_W (add1 PAGE_AVAIL_1_W))
                (list #x1b #x03)
                "new low byte page is initialized with cell-stack page type and 03")
  (check-equal? (memory-list alloc-cell-stack-pages-state PAGE_AVAIL_0_W (add1 PAGE_AVAIL_0_W))
                (list #x1b #x05)
                "new low byte page is initialized with cell-stack page type and 05"))

  ;; cell-pair page layout  (new layout with cell-pair-ptr having bit0 always set and bit1 always unset!)
  ;; offset  content
  ;; ---------------
  ;; 00      #b01xx xxxx page type + number of used slots
  ;; 01      ref-count cell-pair at 05 (cell-pair 0)
  ;; 02      ref-count cell-pair at 09 (cell-pair 1)
  ;; 03..04   unused (2)
  ;; 05..08   cell-pair 0     (#b0000 01[01] >> 2 = #b0000 0001)
  ;; 09..0c   cell-pair 1     (#b0000 10[01] >> 2 = #b0000 0010)
  ;; 0d..0f   unused (3)
  ;; 10      ref-count for cell-pair at 40 (cell-pair 2)
  ;; 11      ref-count for cell-pair at 44 (cell-pair 3)
  ;; ..3e     ref-count for cell-pair at f9 (cell-pair 48)
  ;; 3f..40   unused (2)
  ;; 41..44   cell-pair 2     (#b0100 00[01] >> 2 = #b0001 0000)
  ;; 45..48   cell-pair 3     (#b0100 01[01] >> 2 = #b0001 0001)
  ;; ...
  ;; f9..fc   cell-pair 48    (#b1111 10[01] >> 2 = #b0011 1110)
  ;; fd..fe   unused (2)
  ;; ff      previous page of this type
  ;;
  ;; VM_PAGE_SLOT_DATA + pageidx: holds the index within the page of the first free cell-pair on that page (0 = no free cell-pair on this page)
  ;; the free cell-pair holds in byte 0 of the cell-pair the offset of the next free cell-pair (0 = no other free cell-pair)
  ;;
  ;; allocate a complete new page and initialize it to hold reference counted cell-pairs
  ;; connect all cell-pairs in a free-list
  ;; also set the first free slot of this allocated page (in VM_PAGE_SLOT_DATA + pageidx)
  ;;
  ;; input:  X = page allocated but uninitialized
  ;; output: X = page allocated and initialized for cell-pairs usage
  ;;         A = first free slot ($05)
  ;; usage:  X, Y, ZP_TEMP, ZP_TEMP2
(define INIT_CELLPAIR_PAGE_X_TO_AX
  (add-label-suffix
   "__" "__INIT_CELLPAIR_PAGE_X_TO_AX"
    (list
     (label INIT_CELLPAIR_PAGE_X_TO_AX)
            ;; page is in X

            (STX ZP_TEMP+1)

            (LDY !$00)
            (STY ZP_TEMP)
            (LDA !$40) ;; page type cell-pairs w/ 0 slots allocated
            (STA (ZP_TEMP),y)

            (LDY !$ff)
            (LDA VM_FREE_CELL_PAIR_PAGE)
            (STA (ZP_TEMP),y) ;; previous page of this type is (@$ff = )

            (STX VM_FREE_CELL_PAIR_PAGE)
            
            ;; first write all reference count fields (zero)
            ;; block0 (two ref counts)
            (LDY !$01)
            (LDA !$00) ;; reference count initialized with 0
            (STA (ZP_TEMP),y) ;; @ 01
            (INY)
            (STA (ZP_TEMP),y) ;; @ 02

            (LDY !$10)
            (STY ZP_TEMP)
            (LDY !$2F)

     (label SECOND_RC_BLOCK__)
            (STA (ZP_TEMP),y)
            (DEY)
            (BPL SECOND_RC_BLOCK__)

            (STA ZP_TEMP) ;; clear lowbyte of ptr

            ;; write all cell-pairs to point to next free one
            (LDY !$05)
            (LDA !$09)
            (STA (ZP_TEMP),y) ;; @05 <- 09
            (TAY)
            (LDA !$41)
            (STA (ZP_TEMP),y) ;; @09 <- 41

     (label SECOND_CELL_PAIR_BLOCK__)
            (TAY)
            (CLC)
            (ADC !$04)
            (STA (ZP_TEMP),y)
            (CMP !$F9)
            (BNE SECOND_CELL_PAIR_BLOCK__)

            (TAY)
            (LDA !$00)
            (STA (ZP_TEMP),y) ;; last cell points to 0

            (LDX ZP_TEMP+1) ;; return page initialized in X (high byte)
            (LDA !$05)      ;; return first free slot in A (low byte)
            (STA VM_PAGE_SLOT_DATA,x)

            (RTS))))

(module+ test #| vm_alloc_page_for_cell_pairs |#
  (define vm-alloc-page-for-cell-pairs-code
    (list
     (LDA !$a0)
     (STA VM_FREE_CELL_PAIR_PAGE)
     (JSR ALLOC_PAGE_TO_X)
     (JSR INIT_CELLPAIR_PAGE_X_TO_AX)
     (STX ZP_RT+1) ;; to test read out actual page
     (STA ZP_RT)))

  (define vm-alloc-page-for-cell-pairs-state
    (run-code-in-test vm-alloc-page-for-cell-pairs-code))

  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state ZP_RT (+ 1 ZP_RT))
                (list #x05 PAGE_AVAIL_0)
                "page and slot .. was allocated")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state PAGE_AVAIL_0_W (+ PAGE_AVAIL_0_W #x02))
                (list #b01000000 #x00 #x00)
                "page type is #b01000000 and refcounts cell0 and cell1 are both 0")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #x05) (+ PAGE_AVAIL_0_W #x05))
                (list #x09)
                "cell0 first byte points to next free (09)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #x09) (+ PAGE_AVAIL_0_W #x09))
                (list #x41)
                "cell1 first byte points to next free (41)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #x10) (+ PAGE_AVAIL_0_W #x3e))
                (make-list #x2f #x00)
                "refcounts are all 0 (in block 2)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #x41) (+ PAGE_AVAIL_0_W #x41))
                (list #x45)
                "cell1 first byte points to next free (45)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #x45) (+ PAGE_AVAIL_0_W #x45))
                (list #x49)
                "cell2 first byte points to next free (49)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #xf5) (+ PAGE_AVAIL_0_W #xf5))
                (list #xf9)
                "cell47 first byte points to next free (f9)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #xf9) (+ PAGE_AVAIL_0_W #xf9))
                (list #x00)
                "last cell first byte points to 0 (no next free)")
  (check-equal? (memory-list vm-alloc-page-for-cell-pairs-state (+ PAGE_AVAIL_0_W #xff) (+ PAGE_AVAIL_0_W #xff))
                (list #xa0)
                "last byte on page points to previous free page of cell-pairs"))

;; whether a page is free or used is kept in the 256 bytes starting at VM_PAGE_SLOT_DATA
;; each byte represents one page
;;   00 = allocated (used) but no free slots
;;   01 = system page, not available for memory management
;;   ff = free page (not allocated yet)
;; VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH  (255..0) keeps the max idx to start looking for a page that is free
;; parameter: a = page
;; result: (none)
(define FREE_PAGE_A
  (list
   (label FREE_PAGE_A)
          (CMP VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH)
          (BMI NO_CHANGE__FREE_PAGE_A)
          (STA VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH)

   (label NO_CHANGE__FREE_PAGE_A)
          (TAY)
          (LDA !$ff) ;; free/unallocated page
          (STA VM_PAGE_SLOT_DATA,y)
          (RTS)))

;; does a linear search for the next free page
;; allocate a page (completely uninitialized), just the page, update the memory page status in VM_PAGE_SLOT_DATA
;; parameter: (none)
;; result: X = allocated free page (uninitialized)
;; uses: A, X
(define ALLOC_PAGE_TO_X
  (list
   (label ALLOC_PAGE_TO_X)
          (LDX VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH)

   (label LOOP__ALLOC_PAGE_TO_X)
          (LDA VM_PAGE_SLOT_DATA,x)
          (DEX)
          (BEQ OUT_OF_MEMORY__ALLOC_PAGE_TO_X) ;; cannot be lower then 1!!
          (CMP !$ff)
          (BNE LOOP__ALLOC_PAGE_TO_X)

          ;; found page marked unallocated ($ff)
          (INX) ;; restore original index
          (LDA !$00) ;; mark as initially full but allocated
          (STA VM_PAGE_SLOT_DATA,x)
          (STX VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; set index for next search

          (RTS)

   (label OUT_OF_MEMORY__ALLOC_PAGE_TO_X)
          (BRK)
          ))

(module+ test #| vm-free-page and vm-alloc-page--page-uninit |#
  (define vm-free-page-code
    (list
     (JSR ALLOC_PAGE_TO_X) ;; page is in A ($cc)
     (TXA)
     (PHA)
     (JSR ALLOC_PAGE_TO_X) ;; page is in A ($cb)
     (TXA)
     (JSR WRITE_INT_A_TO_RT)
     (PLA)
     (JSR FREE_PAGE_A )
     (JSR ALLOC_PAGE_TO_X) ;; allocated page should be $cc again
     (TXA)
     (JSR WRITE_INT_A_TO_RA)))

  (define vm-free-page-state
    (run-code-in-test vm-free-page-code))

  (check-equal? (vm-rega->string vm-free-page-state)
                (format "int $00~a" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-regt->string vm-free-page-state)
                (format "int $00~a" (format-hex-byte PAGE_AVAIL_1))))

;; allocate a cell-pair from this page (if page has no free cell-pairs, a new page is allocated and is used to get a free cell-pair!)
;; this will not check the free cell-pair tree!
;; input:  X : page to allocate cell-pair on (a new page is allocated, if this page does not have any free cell-pairs)
;; output: ZP_RT
;; WARNING: ZP_RT IS OVERWRITTEN !! NO PUSH INTO THE CELL-STACK IS DONE!
(define ALLOC_CELLPAIR_AX_TO_RT
  (add-label-suffix
   "__" "__ALLOC_CELLPAIR_AX_TO_RT"
  (list
   (label ALLOC_CELLPAIR_PFL_X_TO_RT)
          (LDA VM_PAGE_SLOT_DATA,x)

   (label ALLOC_CELLPAIR_AX_TO_RT) ;; <-- real entry point of this function
          (STX ZP_RT+1) ;; safe as highbyte of ptr
          (STA ZP_RT)
          (LDY !$00)
          (LDA (ZP_RT),y) ;; next free cell
          (STA VM_PAGE_SLOT_DATA,x)

          ;; increase the slot number used on this page
          (STX INC_CMD__+2) ;; overwrite $c0 (page in following INC command)
   (label INC_CMD__)
          (INC $c000)
          (RTS))))

(module+ test #| vm-alloc-cell-pair-on-page-a-into-rt |#
  (define vm-alloc-cell-pair-on-page-a-into-rt-code
    (list
     (JSR ALLOC_PAGE_TO_X)
     (JSR INIT_CELLPAIR_PAGE_X_TO_AX)
     (JSR ALLOC_CELLPAIR_AX_TO_RT)))

  (define vm-alloc-cell-pair-on-page-a-into-rt-state
    (run-code-in-test vm-alloc-cell-pair-on-page-a-into-rt-code))

  (check-equal? (vm-page->strings vm-alloc-cell-pair-on-page-a-into-rt-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09"))
  (check-equal? (vm-regt->string vm-alloc-cell-pair-on-page-a-into-rt-state)
                (format "pair-ptr[0] $~a05" (format-hex-byte PAGE_AVAIL_0))))

;; find out what kind of cell zp_rt points to,
;; then call the right decrement refcounts function
;; input:  ZP_RT
;; output: the right refcount is decremented
;;         (in case of m1 pages, @ZP_RT-1)
;;         (in case of cell pages @ZP_RT>>1)
;;         (in case of cell-pair pages @ZP_RT>>2)
(define DEC_REFCNT_CELLPAIR_RT #t)
(define DEC_REFCNT_CELL_RT #t)
(define DEC_REFCNT_RT
  (add-label-suffix
   "__" "__DEC_REFCNT_RT"
  (list
   (label DEC_REFCNT_RT)
          (LDA ZP_RT)
          (BEQ UNKNOWN__) ;; empty? -> unknown
          (LSR)
          (BCC DEC_REFCNT_CELL_RT) ;; points to anything!, check page type and then decrement accordingly
          (LSR)
          (BCC LSR_CELL_PAIR__) ;; points to a cell-pair, page type = cell-pair-page
          
          ;; check other types of cells (no other cell types allowed in RT)!
          ;; (LDA ZP_RT)
          ;; (CMP !TAG_BYTE_CELL_ARRAY)
          ;; (BEQ DECR_CELL_ARRAY__)
          ;; (CMP !TAG_BYTE_NATIVE_ARRAY)
          ;; (BEQ DECR_NATIVE_ARRAY__)

   (label UNKNOWN__)
          ;; unknown object type (or atomic value that cannot be ref counted and MUST NOT END UP in ZP_RT)
   (label DONE__)
          (RTS)

   (label DECR_CELL_ARRAY__)
   (label DECR_NATIVE_ARRAY__)
          ;; (JMP DEC_REFCNT_M1_SLOT_RT)
          (BRK)

   ;; input: cell-pair ptr in ZP_RT
   ;; decrement ref count, if 0 deallocate
   (label DEC_REFCNT_CELLPAIR_RT)
          (LDA ZP_RT)
          (LSR)
          (LSR)
   (label LSR_CELL_PAIR__)
          (TAX)
          (LDA ZP_RT+1)
          (BEQ DONE__) ;; empty -> done
          ;; not nil!
          (STA DEC_PAGE_CELLPAIR__+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
   (label DEC_PAGE_CELLPAIR__)
          (DEC $c000,x) ;; c0 is overwritten by page (see above)
          (BNE DONE__)
          ;; copy RT->RA
          ;; (STA ZP_RA+1)
          ;; (LDA ZP_RT)
          ;; (STA ZP_RA)
          ;; (JMP FREE_CELLPAIR_RA)
          ;;
          (JMP FREE_CELLPAIR_RT) ;; free delayed
          
   ;; input: cell ptr in ZP_RT
   ;; decrement ref count, if 0 deallocate
   (label DEC_REFCNT_CELL_RT)
          ;; find out which page type is used (cell-ptr-page, m1-page, slot-page)
          (LDA ZP_RT+1) ;; highbyte (page)
          (BEQ DONE__) ;; page=0 => empty, nothing to be done
          (STA LOAD_PAGE_TYPE__CELL__+2)
   (label LOAD_PAGE_TYPE__CELL__)
          (LDA $c000) ;; c0 is overwritten by page
          (BMI IS_CELL_PAGE__CELL__)
          (AND !$e8)
          (BEQ IS_M1_PAGE__)

          (BRK) ;; unhandled page type

   (label IS_M1_PAGE__)
          (JSR CP_RT_TO_RA)
          (JMP DEC_REFCNT_M1_SLOT_RA)

   (label IS_CELL_PAGE__CELL__)
          (LDA ZP_RT) ;; lowbyte (offset)
          (LSR)
          (TAX)

          (LDA ZP_RT+1) ;; highbyte (page)
          (STA DEC_PAGE__CELL__+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
   (label DEC_PAGE__CELL__)
          (DEC $c000,x) ;; c0 is overwritten by page (see above)
          (BNE DONE__)
          (JMP NEW_FREE_CELL_RT) ;; free delayed
)))


;; find out what kind of cell zp_rt points to,
;; then call the right decrement refcounts function
;; input:  ZP_RT
;; output: the right refcount is decremented
;;         (in case of m1 pages, @ZP_RT-1)
;;         (in case of cell pages @ZP_RT>>1)
;;         (in case of cell-pair pages @ZP_RT>>2)
;;         (in case of 8s pages @ZP_RT>>3)
(define DEC_REFCNT_CELL_RA #t)
(define DEC_REFCNT_CELLPAIR_RA #t)
(define DEC_REFCNT_RA
  (add-label-suffix
   "__" "__DEC_REFCNT_RA"
  (list
   (label DEC_REFCNT_RA)
          (LDA ZP_RA)
          (BEQ UNKNOWN__) ;; low-byte of a pointer may never be 0!
          (LSR)
          (BCC DEC_REFCNT_CELL_RA) ;; lowest bit is 0 -> cell-ptr
          (LSR)
          (BCC LSR_CELLPAIR__) ;; ends on b01 => cell-pair ptr

          ;; check other types of cells
          ;; (LDA ZP_RA)
          ;; (CMP !TAG_BYTE_CELL_ARRAY)
          ;; (BEQ DECR_CELL_ARRAY__)
          ;; (CMP !TAG_BYTE_NATIVE_ARRAY)
          ;; (BEQ DECR_NATIVE_ARRAY__)

   (label UNKNOWN__)
          ;; unknown object type (or atomic value that cannot be ref counted and MUST NOT END UP in ZP_RT)
   (label DONE__)
          (RTS)

   (label DECR_CELL_ARRAY__)
   (label DECR_NATIVE_ARRAY__)
          ;; (JMP DEC_REFCNT_M1_SLOT_RA)
          (BRK)

   ;; input: cell-pair ptr in ZP_RA
   ;; decrement ref count, if 0 deallocate
   (label DEC_REFCNT_CELLPAIR_RA)
          (LDA ZP_RA)
          (LSR)
          (LSR)
   (label LSR_CELLPAIR__)
          (TAX)
          (LDA ZP_RA+1)
          (BEQ DONE__)
          ;; not nil
          (STA DEC_PAGE_CELLPAIR_CNT__+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
   (label DEC_PAGE_CELLPAIR_CNT__)
          (DEC $c000,x) ;; c0 is overwritten by page (see above)
          (BNE DONE__)
          (JMP FREE_CELLPAIR_RA) ;; free delayed

   ;; input: cell ptr in ZP_RA
   ;; decrement ref count, if 0 deallocate
   (label DEC_REFCNT_CELL_RA)
          ;; find out which page type is used (cell-ptr-page, m1-page, slot-page)
          (LDA ZP_RA+1) ;; highbyte (page)
          (BEQ DONE__) ;; page=0 => empty, nothing to be done
          (STA LOAD_CELL_PAGE_TYPE__+2)
   (label LOAD_CELL_PAGE_TYPE__)
          (LDA $c000) ;; c0 is overwritten by page
          (BMI IS_CELL_PAGE__)
          (AND !$e8)
          (BEQ IS_M1_PAGE__)

          (BRK) ;; unhandled page type

   (label IS_M1_PAGE__)
          (JMP DEC_REFCNT_M1_SLOT_RA)
          ;; (LDX ZP_RA)
          ;; (DEX)
          ;; (BNE NOW_DECREMENT_CELL_CNT__) ;; is never 0! for m1 pages

   (label IS_CELL_PAGE__)
          (LDA ZP_RA) ;; lowbyte (offset)
          (LSR)
          (TAX)

   (label NOW_DECREMENT_CELL_CNT__)
          (LDA ZP_RA+1)
          (STA DEC_PAGE_CELL_CNT__+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
   (label DEC_PAGE_CELL_CNT__)
          (DEC $c000,x) ;; c0 is overwritten by page (see above)
          (BNE DONE__)
          (JMP NEW_FREE_CELL_RA) ;; free delayed
)))

;; macro that does pointer detection and jumping to certain labels for the respective pointer type
(define (PTR_DETECTION_MACRO_Rx
         register
         label-unknown
         label-cell
         label-cellpair
         label-cellarr
         label-nativearr
         label-m1)
  (replace-labels
   (hash "REGISTER"   register
         "UNKNOWN"    label-unknown
         "CELL"       label-cell
         "CELLPAIR"   label-cellpair
         "CELLARR"    label-cellarr
         "NATIVEARR"  label-nativearr
         "M1"         label-m1)
   (list
    (LDA REGISTER)                      ;; load zero page register tagged low byte
    (BEQ UNKNOWN)                       ;; tagged low byte 0 => illegal or nil => jump to unknown
    (LSR)                               ;;
    (BCC CELL)                          ;; lowbyte ends on '0' => is a cell pointer
    (LSR)
    (BCC CELLPAIR)                      ;; lowbyte ends on '01' => is a cell-pair pointer

    (CMP !TAG_BYTE_CELL_ARRAY_LSR2)
    (BEQ CELLARR)                       ;; tagged low byte = cell-array tag => is cell-array
    (CMP !TAG_BYTE_NATIVE_ARRAY_LSR2)
    (BEQ NATIVEARR)                     ;; tagged low byte = native-array tag => is native-array

    ;; fall-back
    (LDY  !$00)
    (LDA (REGISTER),y)                  ;; get first byte of page
    (AND !$f8)                          ;; mask out low 3 bits
    (CMP !$10)                          ;;
    (BEQ M1))))                         ;; jump if page tag b00010xxx => m1-slots page

(define (PTR_DETECTION_MACRO_RC
         label-unknown
         label-cell
         label-cellpair
         label-cellarr
         label-nativearr
         label-m1)
  (PTR_DETECTION_MACRO_Rx
   "ZP_RC"
   label-unknown
   label-cell
   label-cellpair
   label-cellarr
   label-nativearr
   label-m1))

(define (PTR_DETECTION_MACRO_RT
         label-unknown
         label-cell
         label-cellpair
         label-cellarr
         label-nativearr
         label-m1)
  (PTR_DETECTION_MACRO_Rx
   "ZP_RT"
   label-unknown
   label-cell
   label-cellpair
   label-cellarr
   label-nativearr
   label-m1))

;; find out what kind of cell zp_rt points to,
;; then call the right decrement refcounts function
;; input:  ZP_RT
;; output: the right refcount is decremented
;;         (in case of m1 pages, @ZP_RT-1)
;;         (in case of cell pages @ZP_RT>>1)
;;         (in case of cell-pair pages @ZP_RT>>2)
(define INC_REFCNT_CELLARR_RT #t)
(define INC_REFCNT_NATIVEARR_RT #t)
(define INC_REFCNT_CELL_RT #t)
(define INC_REFCNT_CELLPAIR_RT #t)
(define INC_REFCNT_RT
  (add-label-suffix
   "__" "__INC_REFCNT_RT"
   (flatten
  (list
   (label INC_REFCNT_RT)
          (PTR_DETECTION_MACRO_RT
           "UNKNOWN__"
           "INC_REFCNT_CELL_RT"
           "LSR__INC_RFCNT_CELLPAIR__"
           "IS_M1_PAGE__"
           "IS_M1_PAGE__"
           "IS_M1_PAGE__")

   (label UNKNOWN__)
          ;; unknown object type (or atomic value that cannot be ref counted and MUST NOT END UP in ZP_RT)
   (label DONE__)
          (RTS)

   (label INC_REFCNT_CELLPAIR_RT)
          (LDA ZP_RT)
          (LSR)
          (LSR)
   (label LSR__INC_RFCNT_CELLPAIR__)
          (TAX)
          (LDA ZP_RT+1)
          (BEQ DONE__)
          ;; not nil
          (STA INC_PAGE_CNT__CELLPAIR__+2) ;; store high byte (page) into inc-command high-byte (thus +2 on the label)
   (label INC_PAGE_CNT__CELLPAIR__)
          (INC $c000,x) ;; c0 is overwritten by page (see above)
          (RTS)

   (label INC_REFCNT_CELL_RT)
          ;; find out which page type is used (cell-ptr-page, m1-page, slot-page)
          (LDA ZP_RT+1) ;; highbyte (page)
          (BEQ DONE__) ;; page=0 => empty, nothing to be done
          (STA LOAD_PAGE_TYPE__CELL__+2)
   (label LOAD_PAGE_TYPE__CELL__)
          (LDA $c000) ;; c0 is overwritten by page
          (BMI IS_CELL_PAGE__)
          (AND !$ec)
          (BEQ IS_M1_PAGE__)

          (BRK) ;; unhandled page type

   (label INC_REFCNT_CELLARR_RT)
   (label INC_REFCNT_NATIVEARR_RT)
   (label IS_M1_PAGE__)
          (LDX ZP_RT)
          (DEX)
          (BNE NOW_INCREMENT_REFCNT__CELL__) ;; is never 0! for m1 pages

   (label IS_CELL_PAGE__)
          (LDA ZP_RT) ;; lowbyte (offset)
          (LSR)
          (TAX)

   (label NOW_INCREMENT_REFCNT__CELL__)
          (LDA ZP_RT+1)
          (STA INC_PAGE_REFCNT_CELL__+2) ;; store high byte (page) into inc-command high-byte (thus +2 on the label)
   (label INC_PAGE_REFCNT_CELL__)
          (INC $c000,x) ;; c0 is overwritten by page (see above)
          (RTS)))))

(module+ test #| vm-refcount-decr-rt |#
  (define vm-refcount-decr-rt-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR INC_REFCNT_RT)
     (JSR INC_REFCNT_RT)))

  (define vm-refcount-decr-rt-state
    (run-code-in-test vm-refcount-decr-rt-code))

  (check-equal? (vm-refcount-cell-pair-ptr vm-refcount-decr-rt-state (+ PAGE_AVAIL_0_W #x05))
                2)

  (define vm-refcount-decr-rt-code2
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR INC_REFCNT_RT)
     (JSR INC_REFCNT_RT)
     (JSR DEC_REFCNT_RT)))

  (define vm-refcount-decr-rt-state2
    (run-code-in-test vm-refcount-decr-rt-code2))

  (check-equal? (vm-refcount-cell-pair-ptr vm-refcount-decr-rt-state2 (+ PAGE_AVAIL_0_W #x05))
                1))

(module+ test #| vm-refcount-mmcr-rt--cell-pair-ptr |#
  (define vm-refcount-mmcr-rt--cell-pair-ptr-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR INC_REFCNT_CELLPAIR_RT)
     (JSR INC_REFCNT_CELLPAIR_RT)))

  (define vm-refcount-mmcr-rt--cell-pair-ptr-state
    (run-code-in-test vm-refcount-mmcr-rt--cell-pair-ptr-code))

  (check-equal? (vm-regt->string vm-refcount-mmcr-rt--cell-pair-ptr-state)
                (format "pair-ptr[2] $~a05" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-refcount-cell-pair-ptr vm-refcount-mmcr-rt--cell-pair-ptr-state (+ PAGE_AVAIL_0_W #x05))
                2)

  (define vm-refcount-mmcr-rt--cell-pair-ptr-code2
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR INC_REFCNT_CELLPAIR_RT)
     (JSR INC_REFCNT_CELLPAIR_RT)
     (JSR DEC_REFCNT_CELLPAIR_RT)))

  (define vm-refcount-mmcr-rt--cell-pair-ptr-state2
    (run-code-in-test vm-refcount-mmcr-rt--cell-pair-ptr-code2))

  (check-equal? (vm-regt->string vm-refcount-mmcr-rt--cell-pair-ptr-state2)
                (format "pair-ptr[1] $~a05" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-refcount-cell-pair-ptr vm-refcount-mmcr-rt--cell-pair-ptr-state2 (+ PAGE_AVAIL_0_W #x05))
                1))

(module+ test #| vm-refcount-mmcr-rt--cell-pair-ptr |#
  (define vm-refcount-mmcr-rt--cell-ptr-code
    (list
     (JSR ALLOC_CELL_TO_RT)
     (JSR INC_REFCNT_CELL_RT)
     (JSR INC_REFCNT_CELL_RT)))

  (define vm-refcount-mmcr-rt--cell-ptr-state
    (run-code-in-test vm-refcount-mmcr-rt--cell-ptr-code))

  (check-equal? (vm-regt->string vm-refcount-mmcr-rt--cell-ptr-state)
                (format "ptr[2] $~a02" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-refcount-cell-ptr vm-refcount-mmcr-rt--cell-ptr-state (+ PAGE_AVAIL_0_W #x02))
                2)

  (define vm-refcount-mmcr-rt--cell-ptr-code2
    (list
     (JSR ALLOC_CELL_TO_RT)
     (JSR INC_REFCNT_CELL_RT)
     (JSR INC_REFCNT_CELL_RT)
     (JSR DEC_REFCNT_CELL_RT)))

  (define vm-refcount-mmcr-rt--cell-ptr-state2
    (run-code-in-test vm-refcount-mmcr-rt--cell-ptr-code2))

  (check-equal? (vm-regt->string vm-refcount-mmcr-rt--cell-ptr-state2)
                (format "ptr[1] $~a02" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-refcount-cell-ptr vm-refcount-mmcr-rt--cell-ptr-state2 (+ PAGE_AVAIL_0_W #x02))
                1))

;; free nonatomic (is cell-ptr, cell-pair-ptr, cell-array-ptr, native-array-ptr)
;; parameter: zp_rt
(define FREE_RT
  (add-label-suffix
   "__" "FREE_RT"
  (list
   (label FREE_RT)
          (LDA ZP_RT)
          (TAY)
          (LSR)
          (BCC FREE_CELL__)
          (LSR)
          (BCC FREE_CELLPAIR__)
          ;; check other types of cells
          (CPY !TAG_BYTE_CELL_ARRAY)
          (BEQ FREE_CELL_ARRAY__)
          (CPY !TAG_BYTE_NATIVE_ARRAY)
          (BEQ FREE_NATIVE_ARRAY__)

          ;; unknown pointer type in zp_rt
          (BRK)

   (label FREE_CELL__)
          (JMP NEW_FREE_CELL_RT)

   (label FREE_CELLPAIR__)
          (JMP FREE_CELLPAIR_RT)

   (label FREE_CELL_ARRAY__)
   (label FREE_NATIVE_ARRAY__)
          ;; VM_FREE_M1_SLOT_IN_RT
          (BRK))))

(module+ test #| vm-free-ptr-in-rt |#
  (define vm-free-ptr-in-rt-code
    (list
            (LDA !$00)
            (STA $ff)
            (JMP TEST_START__FREE_RT)

     (label NEW_FREE_CELL_RT)
            (INC $ff)
            (RTS)

     (label TEST_START__FREE_RT )
            (JSR ALLOC_CELL_TO_RT)
            (JSR FREE_RT)))

  (define vm-free-ptr-in-rt-state
    (run-code-in-test vm-free-ptr-in-rt-code #:mock (list (label NEW_FREE_CELL_RT))))

  (check-equal? (memory-list vm-free-ptr-in-rt-state #xff #xff)
                (list #x01)
                "dispatches call to free-cell-ptr routine")

  (define vm-free-ptr-in-rt-2-code
    (list
            (LDA !$00)
            (STA $ff)
            (JMP TEST_START__FREE_RT)

     (label FREE_CELLPAIR_RT)
            (INC $ff)
            (RTS)

     (label TEST_START__FREE_RT )
            (JSR ALLOC_CELLPAIR_TO_RT)
            (JSR FREE_RT)))

  (define vm-free-ptr-in-rt-2-state
    (run-code-in-test vm-free-ptr-in-rt-2-code #:mock (list (label FREE_CELLPAIR_RT))))

  (check-equal? (memory-list vm-free-ptr-in-rt-2-state #xff #xff)
                (list #x01)
                "dispatches call to free-cell-pair-ptr routine"))

;; get the page and unused cellpair for allocation
;;
;; get the complete ptr, do not allocate this cellpair yet
;; allocate a new page if necessary
;; do not use any cellpair free list
;; input:  VM_FREE_CELL__PAIR_PAGE
;;         VM_PAGE_SLOT_DATA+PAGE
;; output: A = lowbyte
;;         X = highbyte (page)
;;         Y = ?
(define GET_FRESH_CELLPAIR_TO_AX
  (list
   (label GET_FRESH_CELLPAIR_TO_AX)
          (LDX VM_FREE_CELL_PAIR_PAGE)
          (BEQ NEW_PAGE__GET_FRESH_CELLPAIR_TO_AX)
          (LDA VM_PAGE_SLOT_DATA,x)
          (BNE DONE__GET_FRESH_CELLPAIR_TO_AX) ;; allocate new page first

   (label NEW_PAGE__GET_FRESH_CELLPAIR_TO_AX)
          (JSR ALLOC_PAGE_TO_X)
          (JMP INIT_CELLPAIR_PAGE_X_TO_AX)

   (label DONE__GET_FRESH_CELLPAIR_TO_AX)
          (RTS)))

(module+ test #| get-page-for-alloc-cellpair-to-ax |#
  (define get-page-for-alloc-cellpair-to-ax-state
    (run-code-in-test
     (list
      (JSR ALLOC_PAGE_TO_X)
      (STX VM_FREE_CELL_PAIR_PAGE)
      (LDA !$09)                           ;; make first free slot on page to be 08
      (STA VM_PAGE_SLOT_DATA,x)

      (JSR GET_FRESH_CELLPAIR_TO_AX)  ;; no new allocate should take place => stay on page_0

      (STA ZP_RT)
      (STX ZP_RT+1))))

  (check-equal? (memory-list get-page-for-alloc-cellpair-to-ax-state ZP_RT (add1 ZP_RT))
                (list #x09 PAGE_AVAIL_0)
                "cell 09 is allocated on page_0"))

(module+ test #| get-page-for-alloc-cellpair-to-ax |#
  (define get-page-for-alloc-cellpair-to-ax-2-state
    (run-code-in-test
     (list
      (JSR ALLOC_PAGE_TO_X)
      (STX VM_FREE_CELL_PAIR_PAGE)
      (LDA !$00)                          ;; mark page to have no free cells
      (STA VM_PAGE_SLOT_DATA,x)

      (JSR GET_FRESH_CELLPAIR_TO_AX) ;; should allocate a new page

      (STA ZP_RT)
      (STX ZP_RT+1))))

  (check-equal? (memory-list get-page-for-alloc-cellpair-to-ax-2-state ZP_RT (add1 ZP_RT))
                (list #x05 PAGE_AVAIL_1)
                "since first page (page_0) is marked as full, first slot (05) of page_1 is allocated"))
;; get the page and unused cell for allocation
;;
;; get the complete ptr, do not allocate this cell yet
;; allocate a new page if necessary
;; do not use any cell free list
;; input:  VM_FREE_CELL_PAGE
;;         VM_PAGE_SLOT_DATA+PAGE
;; output: A = lowbyte
;;         X = highbyte (page)
;;         Y = ?
(define GET_FRESH_CELL_TO_AX
  (add-label-suffix
   "__" "__GET_FRESH_CELL_TO_AX"
  (list
   (label GET_FRESH_CELL_TO_AX)
          (LDX VM_FREE_CELL_PAGE)
          (BEQ NEW_PAGE__)
          (LDA VM_PAGE_SLOT_DATA,x)
          (BNE DONE__) ;; allocate new page first

   (label NEW_PAGE__)
          (JSR ALLOC_PAGE_TO_X)
          (JMP INIT_CELL_PAGE_X_TO_AX)

   (label DONE__)
          (RTS))))

(module+ test #| get-page-for-alloc-cell-to-ax |#
  (define get-page-for-alloc-cell-to-ax-state
    (run-code-in-test
     (list
      (JSR ALLOC_PAGE_TO_X)
      (STX VM_FREE_CELL_PAGE)
      (LDA !$08)                           ;; make first free slot on page to be 08
      (STA VM_PAGE_SLOT_DATA,x)

      (JSR GET_FRESH_CELL_TO_AX)  ;; no new allocate should take place => stay on page_0

      (STA ZP_RT)
      (STX ZP_RT+1))))

  (check-equal? (memory-list get-page-for-alloc-cell-to-ax-state ZP_RT (add1 ZP_RT))
                (list #x08 PAGE_AVAIL_0)
                "cell 08 is allocated on page_0"))

(module+ test #| get-page-for-alloc-cell-to-ax |#
  (define get-page-for-alloc-cell-to-ax-2-state
    (run-code-in-test
     (list
      (JSR ALLOC_PAGE_TO_X)
      (STX VM_FREE_CELL_PAGE)
      (LDA !$00)                          ;; mark page to have no free cells
      (STA VM_PAGE_SLOT_DATA,x)

      (JSR GET_FRESH_CELL_TO_AX) ;; should allocate a new page

      (STA ZP_RT)
      (STX ZP_RT+1))))

  (check-equal? (memory-list get-page-for-alloc-cell-to-ax-2-state ZP_RT (add1 ZP_RT))
                (list #x02 PAGE_AVAIL_1)
                "since first page (page_0) is marked as full, first slot (02) of page_1 is allocated"))

;; allocate the cell at A on page X
;;
;; update next free cell in vm_page_slot_data
;; update number of allocated cells on page X
;; input:  A = lowbyte
;;         X = highbyte (page)
;;         # cells allocated on PAGE
;; output: A = next free cell
;;         X = PAGE
;;         Y = 0
;;         VM_PAGE_SLOT_DATA + PAGE = next free cell
;;         # cells allocated on PAGE ++
(define ALLOC_CELL_AX_TO_RT
  (add-label-suffix
   "__" "__ALLOC_CELL_AX_TO_RT"
  (list
   (label ALLOC_CELL_PFL_X_TO_RT)
          (LDA VM_PAGE_SLOT_DATA,x)               ;; get first free slot on page
          ;; (BEQ ERROR)
   ;;     ----------------------------
   (label ALLOC_CELL_AX_TO_RT)
          (STX ZP_RT+1)                           ;; safe as highbyte of ptr
          (STA ZP_RT)                             ;; safe as lowbyte of ptr

          (LDY !$00)
          (LDA (ZP_RT),y)                         ;; next free cell
          (STA VM_PAGE_SLOT_DATA,x)

          ;; increase the slot number on this page
          (STX INC_CMD__+2) ;; overwrite $c0
   (label INC_CMD__)
          (INC $c000)
          (RTS))))

(module+ test #| alloc-cell-a-on-page-x-to-rt |#
  (define test-alloc-cell-a-on-page-x-to-rt-state
    (run-code-in-test
     (list
      (JSR ALLOC_PAGE_TO_X)
      (JSR INIT_CELL_PAGE_X_TO_AX)
      (JSR ALLOC_CELL_AX_TO_RT))))

  (check-equal? (memory-list test-alloc-cell-a-on-page-x-to-rt-state ZP_RT (add1 ZP_RT))
                (list #x02 PAGE_AVAIL_0))

  (check-equal? (vm-page->strings test-alloc-cell-a-on-page-x-to-rt-state PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page has 1 slot in use"))

(module+ test
  (define test-alloc-cell-a-on-page-x-to-rt-twice-state
    (run-code-in-test
     (list
      (JSR ALLOC_PAGE_TO_X)
      (JSR INIT_CELL_PAGE_X_TO_AX)
      (JSR ALLOC_CELL_AX_TO_RT)
      (LDA !$08)
      (LDX ZP_RT+1)
      (JSR ALLOC_CELL_AX_TO_RT))))

  (check-equal? (memory-list test-alloc-cell-a-on-page-x-to-rt-twice-state ZP_RT (add1 ZP_RT))
                (list #x08 PAGE_AVAIL_0))

  (check-equal? (vm-page->strings test-alloc-cell-a-on-page-x-to-rt-twice-state PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "page has 2 slot in use"))

;; allocate (or reuse from free-list) cell into rt
;;
;; input:  VM_LIST_OF_FREE_CELLS
;;         VM_FREE_CELL_PAGE
;;         VM_PAGE_SLOT_DATA
;;         # cells allocated on PAGE
;; output: ZP_RT: ptr to heap allocated cell (cell itself is not initialized!)
;;         VM_LIST_OF_FREE_CELLS
;;         A, X, Y: ?
(define ALLOC_CELL_TO_RT
  (add-label-suffix
   "__" "__ALLOC_CELL_TO_RT"
  (list
   (label ALLOC_CELL_TO_RT)
          (LDA VM_LIST_OF_FREE_CELLS+1)
          (BNE ALLOC_CELL_GFL_PAGE_A_TO_RT)
          (JSR GET_FRESH_CELL_TO_AX)
          (JMP ALLOC_CELL_AX_TO_RT)

   (label ALLOC_CELL_GFL_TO_RT)
          (LDA VM_LIST_OF_FREE_CELLS+1)
          ;; (BEQ ERROR_ALLOC_CELL_GFL_TO_RT)
   (label ALLOC_CELL_GFL_PAGE_A_TO_RT)
          (STA ZP_RT+1)
          (LDA VM_LIST_OF_FREE_CELLS)
          (STA ZP_RT)

          ;; read output this old cell and store its content as new head of the free list
          (LDY !$00)
          (LDA (ZP_RT),y)
          (STA VM_LIST_OF_FREE_CELLS)
          (INY)
          (LDA (ZP_RT),y)
          (STA VM_LIST_OF_FREE_CELLS+1)
          (RTS))))

(module+ test #| vm_alloc_cell_ptr_to_rt (once on a new page) |#
  (define test-alloc-cell-to-rt-code
    (list
     (JSR ALLOC_CELL_TO_RT)))

  (define test-alloc-cell-to-rt-state-after
    (run-code-in-test test-alloc-cell-to-rt-code))

  (check-equal? (memory-list test-alloc-cell-to-rt-state-after VM_LIST_OF_FREE_CELLS VM_LIST_OF_FREE_CELLS)
                (list #x00)
                "list of free cells is empty")

  (check-equal? (memory-list test-alloc-cell-to-rt-state-after ZP_RT (add1 ZP_RT))
                (list #x02 PAGE_AVAIL_0))

  (check-equal? (vm-page->strings test-alloc-cell-to-rt-state-after PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page has 1 slot in use"))

(module+ test #| vm_alloc_cell_ptr_to_rt (twice on a new page) |#
  (define test-alloc-cell-to-rt-twice-code
    (list
     (JSR ALLOC_CELL_TO_RT)
     (JSR ALLOC_CELL_TO_RT)))

  (define test-alloc-cell-to-rt-twice-state-after
    (run-code-in-test test-alloc-cell-to-rt-twice-code))

  (check-equal? (memory-list test-alloc-cell-to-rt-twice-state-after VM_LIST_OF_FREE_CELLS VM_LIST_OF_FREE_CELLS)
                (list #x00)
                "list of free cells is empty")

  (check-equal? (memory-list test-alloc-cell-to-rt-twice-state-after ZP_RT (add1 ZP_RT))
                (list #x08 PAGE_AVAIL_0))

  (check-equal? (vm-page->strings test-alloc-cell-to-rt-twice-state-after PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "page has 2 slots in use"))

(module+ test #| vm_alloc_cell_to_zp_ptr (twice, then free first on a new page) |#
  (define test-alloc-cell-to-rt-twicenfree-code
    (list
     (JSR ALLOC_CELL_TO_RT)
     (JSR CP_RT_TO_RA)

     (JSR ALLOC_CELL_TO_RT)
     (JSR NEW_FREE_CELL_RA)))

  (define test-alloc-cell-to-rt-twicenfree-state-after
    (run-code-in-test test-alloc-cell-to-rt-twicenfree-code))

  (check-equal? (memory-list test-alloc-cell-to-rt-twicenfree-state-after VM_LIST_OF_FREE_CELLS (add1 VM_LIST_OF_FREE_CELLS))
                (list #x02 PAGE_AVAIL_0)
                "free cell list has xx02 now as head of the list")

  (check-equal? (vm-page->strings test-alloc-cell-to-rt-twicenfree-state-after PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "page has still 2 slots in use (even though $cc02 was freed)")

  (check-equal? (memory-list test-alloc-cell-to-rt-twicenfree-state-after (+ PAGE_AVAIL_0_W #x02) (+ PAGE_AVAIL_0_W #x03))
                (list #x00 #x00)
                "since xx02 is now part of the free cell list, it points to the next free cell which is $0000 (none)"))

(module+ test #| vm_alloc_cell_to_zp_ptr (twice, then free first on a new page, then allocate again) |#
  (define test-alloc-cell-to-rt-twicenfreenalloc-code
    (list
     (JSR ALLOC_CELL_TO_RT)
     (JSR CP_RT_TO_RA)

     (JSR ALLOC_CELL_TO_RT)
     (JSR NEW_FREE_CELL_RA)

     (JSR ALLOC_CELL_TO_RT)))

  (define test-alloc-cell-to-rt-twicenfreenalloc-state-after
    (run-code-in-test test-alloc-cell-to-rt-twicenfreenalloc-code))

  (check-equal? (vm-page->strings test-alloc-cell-to-rt-twicenfreenalloc-state-after PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "still (only) two slots are used on the page, one from the free list was reused")

  (check-equal? (memory-list test-alloc-cell-to-rt-twicenfreenalloc-state-after VM_LIST_OF_FREE_CELLS VM_LIST_OF_FREE_CELLS)
                (list #x00) ;; lowbyte is zero => it is initial (high byte is not heeded in that case)
                "free cell list is initial again"))

;; actively free all enqueued cell pairs of the free-list!
;; can be useful to find out whether a whole page is not used at all. free cells are still marked as used on a page.
(define GC_CELLPAIR_FREE_LIST
  (add-label-suffix
   "__" "__GC_CELLPAIR_FREE_LIST"
  (list
   (label GC_CELLPAIR_FREE_LIST)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1) ;; get highbyte (page) from ptr to cell-pair
          (BNE CONTINUE__)   ;; if = 0, queue is empty, i'm done
          (RTS)

   (label CONTINUE__)
          ;; put ptr to cell-pair into RA, now RA->cell0,cell1 with cell0 = pointer to the next in queue, cell1 could still be something that needs gc
          (STA ZP_RA+1)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (STA ZP_RA)

          ;; set new tree root for free tree to original cell0
          (LDY !$00)
          (LDA (ZP_RA),y)
          (BEQ CELL0_IS_NO_PTR__) ;; is zero => completely empty
          (AND !$03)
          (CMP !$03)
          (BEQ CELL0_IS_NO_PTR__) ;; is no ptr
          (INY)
          (LDA (ZP_RA),y)
          (BEQ CELL0_IS_NO_PTR__) ;; is nil
          (DEY)

          ;; cell0 is a cell-pair-ptr => make new root of free queue
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (LDA (ZP_RA),y)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (BNE CHECK_CELL1__) ;; since must be !=0, it cannot be on page 0 always branch!

   (label CELL0_IS_NO_PTR__)
          ;; queue is now empty, this was the last cell-pair
          ;; clear queue
          (LDA !$00)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1) ;; just reset highbyte (checked at start of this function)
          ;; (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)

   (label CHECK_CELL1__)
          ;; now check cell1 on remaining ptrs
          (LDY !$02)
          (LDA (ZP_RA),y) ;; get low byte
          (BEQ CELL1_IS_NO_PTR__) ;; = 0 means totally empty => no ptr
          (AND !$03)       ;; mask out all but low 2 bits
          (CMP !$03)
          (BEQ CELL1_IS_NO_PTR__) ;; no need to do further deallocation
          (INY)
          (LDA (ZP_RA),y)
          (BEQ CELL1_IS_NO_PTR__) ;; is nil

          ;; write cell1 into zp_ptr and decrement          
          (LDA ZP_RA)
          (STA RA_COPY__)
          (LDA ZP_RA+1)
          (STA RA_COPY__+1)
          (JSR WRITE_CELLPAIR_RA_CELL1_TO_RA)
          (JSR DEC_REFCNT_RA) ;; this may change the queue again, which is alright, since RA was removed from queue
          (LDA RA_COPY__)
          (STA ZP_RA)
          (LDA RA_COPY__+1)
          (STA ZP_RA+1)

  (label CELL1_IS_NO_PTR__)
          ;; now add ra to its page as free cell-pair on that page
          (LDX ZP_RA+1)                 ;; A = page -> x
          (LDA $cf00,x)         ;; current first free cell offset
          (LDY !$00)
          (STA (ZP_RA),y)       ;; write into lowbyte of cell pointed to by RA
          ;; (INY)
          ;; (TXA)
          ;; (STA (ZP_RA),y)       ;; write page into highbyte of cell pointed to by RA
          (LDA ZP_RA)             ;; get offset into page of cell RA points to
          (STA $cf00,x)           ;; new first free cell now points to RA
          (LDA ZP_RA+1)
          (STA DEC_COMMAND__+2)
   (label DEC_COMMAND__)
          (DEC $c000)         ;; decrement number of used slots on cell-pair page (c0 is overwritten with page in zp_ra+1
          (JMP GC_CELLPAIR_FREE_LIST) ;; do this until queue is empty

   (label RA_COPY__)
          (word 0))))

;; input:  none
;; output: zp_rt = free cell-pair
;;
;; try to reuse root of free tree: use root but make sure to deallocate cell1 of the root (since this might still point to some data)
;; if no free tree available, find page with free cells (VM_FREE_CELL_PAIR_PAGE)
;; if no free cell page is available, allocate a new page and used the first free slot there
;; NOTE: the cell-pair is not initialized (cell0 and/or cell1 may contain old data that needs to be overwritten!)
(define ALLOC_CELLPAIR_TO_RT
  (add-label-suffix
   "__" "__ALLOC_CELLPAIR_TO_RT"
  (list
   (label ERROR__ALLOC_CELLPAIR_GFL_TO_RT)
          (BRK)

   ;; ----------------------------------------
   (label ALLOC_CELLPAIR_TO_RT)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1) ;; get highbyte (page) from ptr to cell-pair
          (BNE REUSE_CELL_PAIR__)   ;; if != 0, cell-pair can be reused
          ;; no cell-pair to reuse available => need to allocate a new one

          ;; get a cell-pair on the given page (or allocate a new page)
          (JSR GET_FRESH_CELLPAIR_TO_AX)
          (JMP ALLOC_CELLPAIR_AX_TO_RT)

   (label ALLOC_CELLPAIR_GFL_TO_RT)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (BEQ ERROR__ALLOC_CELLPAIR_GFL_TO_RT) ;; no free cell in global free list

   (label REUSE_CELL_PAIR__)
          ;; put root of free tree into zp_rt (and copy in TEMP_PTR of this function)
          (STA ZP_RT+1)
          (STA TEMP_PTR__+1)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (STA ZP_RT)
          (STA TEMP_PTR__)

          ;; set new tree root for free tree to original cell0
          (LDY !$00)
          (LDA (ZP_RT),y)
          (BEQ CELL0_IS_NO_PTR__) ;; is zero => completely empty
          (AND !$03)
          (CMP !$03)
          (BEQ CELL0_IS_NO_PTR__) ;; is no ptr
          (INY)
          (LDA (ZP_RT),y)
          (BEQ CELL0_IS_NO_PTR__) ;; it is nil which is handled same as no pointer
          (DEY)

          ;; ;; cell0 is a cell-ptr or cell-pair-ptr
          ;; (LSR)
          ;; (BCC CELL0_IS_CELL_PTR__) ;; <- this cannot happen! cell0 is freed before entering it into the tree

          ;; cell0 is a cell-pair-ptr => make new root of free queue 
          ;; (LDA (ZP_RT),y)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (LDA (ZP_RT),y)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (BNE CHECK_CELL1__) ;; since must be !=0, it cannot be on page 0 always branch!

   ;; (label CELL0_IS_CELL_PTR__)
   ;;        ;; cell0 is a cell-ptr => decrement cell0
   ;;        (JSR WRITE_CELLPAIR_RT_CELL0_TO_RT)
   ;;        (JSR DEC_REFCNT_RT)
   ;;        ;; restore original cell-pair-ptr
   ;;        (LDA TEMP_PTR__+1)
   ;;        (STA ZP_RT+1)
   ;;        (LDA TEMP_PTR__)
   ;;        (STA ZP_RT)
   ;;        ;; continue as if cell0 was no ptr, since cell-ptr was handled already

   (label CELL0_IS_NO_PTR__)
          (LDA !$00)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          ;; (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)

   (label CHECK_CELL1__)
          ;; check whether cell1 is non-ptr or ptr
          (LDY !$02)
          (LDA (ZP_RT),y) ;; get low byte
          (BEQ CELL1_IS_NO_PTR__) ;; = 0 means totally empty => no ptr
          (AND !$03)       ;; mask out all but low 2 bits
          (CMP !$03)
          (BEQ CELL1_IS_NO_PTR__) ;; no need to do further deallocation
          (INY)
          (LDA (ZP_RT),y) ;; get high byte (page)
          (BEQ CELL1_IS_NO_PTR__) ;; this is nil, no need for deallocation

          ;; write cell1 into zp_ptr and decrement
          (JSR WRITE_CELLPAIR_RT_CELL1_TO_RT)
          (JSR DEC_REFCNT_RT)
          ;; continue as if cell1 is atomic, since it was already handled

          ;; restore zp_ptr to the cell-pair to be reused
          (LDA TEMP_PTR__+1)
          (STA ZP_RT+1)
          (LDA TEMP_PTR__)
          (STA ZP_RT)

   (label CELL1_IS_NO_PTR__)
          ;; cleanup cell-pair to ensure it is empty
          (LDA !$00)
          (LDY !$03)
   (label LOOP_CLEAN_RT__)
          (STA (ZP_RT),y)
          (DEY)
          (BPL LOOP_CLEAN_RT__)

          (RTS)

   (label TEMP_PTR__)
          (word $0000))))

(module+ test #| vm-allocate-cell-pair-ptr-to-rt |#
  ;; test case 1: allocate new page, allocate first cell-pair on new page, no existing (reusable) pair available
  (define vm-allocate-cell-pair-ptr-to-rt-1-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)))

  (define vm-allocate-cell-pair-ptr-to-rt-1-state
    (run-code-in-test vm-allocate-cell-pair-ptr-to-rt-1-code))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-1-state)
                (format "pair-ptr[0] $~a05" (format-hex-byte PAGE_AVAIL_0)))

  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-1-state VM_FREE_CELL_PAIR_PAGE VM_FREE_CELL_PAIR_PAGE)
                (list PAGE_AVAIL_0))

  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-1-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09"))

  ;; test case 2: allocate another cell-pair on existing page with already allocated slots
  (define vm-allocate-cell-pair-ptr-to-rt-2-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR ALLOC_CELLPAIR_TO_RT)))

  (define vm-allocate-cell-pair-ptr-to-rt-2-state
    (run-code-in-test vm-allocate-cell-pair-ptr-to-rt-2-code))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-2-state)
                (format "pair-ptr[0] $~a09" (format-hex-byte PAGE_AVAIL_0)))

  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-2-state VM_FREE_CELL_PAIR_PAGE VM_FREE_CELL_PAIR_PAGE)
                (list PAGE_AVAIL_0))

  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-2-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41"))

  ;; test case 3: allocate last cell-pair on existing page
  (define vm-allocate-cell-pair-ptr-to-rt-3-code
    (list
            (LDX !49)
     (label TEST_LOOP__ALLOC_CELLPAIR_TO_RT_3_CODE)
            (TXA)
            (PHA)
            (JSR ALLOC_CELLPAIR_TO_RT)
            (PLA)
            (TAX)
            (DEX)
            (BNE TEST_LOOP__ALLOC_CELLPAIR_TO_RT_3_CODE)))

  (define vm-allocate-cell-pair-ptr-to-rt-3-state
    (run-code-in-test vm-allocate-cell-pair-ptr-to-rt-3-code))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-3-state)
                (format "pair-ptr[0] $~af9" (format-hex-byte PAGE_AVAIL_0)))

  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-3-state VM_FREE_CELL_PAIR_PAGE VM_FREE_CELL_PAIR_PAGE)
                (list PAGE_AVAIL_0))

  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-3-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     49"
                      "next free slot: $00"))

  ;; test case 3a: allocate one past last cell-pair on existing page
  (define vm-allocate-cell-pair-ptr-to-rt-3a-code
    (list
            (LDX !50)
     (label TEST_LOOP__ALLOC_CELLPAIR_TO_RT_3A_CODE)
            (TXA)
            (PHA)
            (JSR ALLOC_CELLPAIR_TO_RT)
            (PLA)
            (TAX)
            (DEX)
            (BNE TEST_LOOP__ALLOC_CELLPAIR_TO_RT_3A_CODE)))

  (define vm-allocate-cell-pair-ptr-to-rt-3a-state
    (run-code-in-test vm-allocate-cell-pair-ptr-to-rt-3a-code))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-3a-state)
                (format "pair-ptr[0] $~a05" (format-hex-byte PAGE_AVAIL_1)))

  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-3a-state VM_FREE_CELL_PAIR_PAGE VM_FREE_CELL_PAIR_PAGE)
                (list PAGE_AVAIL_1))

  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-3a-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     49"
                      "next free slot: $00"))
  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-3a-state PAGE_AVAIL_1)
                (list "page-type:      cell-pair page"
                      (format "previous page:  $~a" (format-hex-byte PAGE_AVAIL_0))
                      "slots used:     1"
                      "next free slot: $09"))

  ;; test case 4: allocate first of pairs stored in the free tree, which is filled with just non ptr atomic cells => no gc
  (define vm-allocate-cell-pair-ptr-to-rt-4-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     ;; copy allocated cell-pair ptr to the tree
     (LDA ZP_RT)
     (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
     (LDA ZP_RT+1)
     (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)

     ;; fill cell0 with zeros, cell1 within int1
     (LDA !$00)
     (STA ZP_RA)
     (STA ZP_RA+1)
     (JSR WRITE_RA_TO_CELL0_CELLPAIR_RT)
     (JSR WRITE_INT1_TO_RA)
     (JSR WRITE_RA_TO_CELL1_CELLPAIR_RT)

     (JSR ALLOC_CELLPAIR_TO_RT)))

  (define vm-allocate-cell-pair-ptr-to-rt-4-state
    (run-code-in-test vm-allocate-cell-pair-ptr-to-rt-4-code))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-4-state)
                (format "pair-ptr[0] $~a05" (format-hex-byte PAGE_AVAIL_0))
                "cell pair at cc05 is reused (was head of tree)")
  (check-equal? (vm-cell-pair-free-tree->string vm-allocate-cell-pair-ptr-to-rt-4-state)
                "root is initial")

  ;; test case 4: allocate first of pairs stored in the free tree, which has its slot1 filled with ptr that needs to be gc'ed
  (define vm-allocate-cell-pair-ptr-to-rt-5-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR WRITE_INT1_TO_RA)
     (JSR WRITE_RA_TO_CELL0_CELLPAIR_RT)
     (JSR WRITE_RA_TO_CELL1_CELLPAIR_RT)                      ;; cc05: 03 01 03 01
     (JSR INC_REFCNT_CELLPAIR_RT)           ;; cc01 = 1
     (JSR CP_RT_TO_RA)                               ;; RA = cc05

     (JSR ALLOC_CELLPAIR_TO_RT)                 ;; RT = cc09

     ;; copy allocated cell-pair ptr to the tree
     (LDA ZP_RT)
     (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
     (LDA ZP_RT+1)
     (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)

     ;; fill cell0 with zeros, cell1 cell-pair-ptr (with refcount = 1)
     (JSR WRITE_RA_TO_CELL1_CELLPAIR_RT)                     ;; cc09: 05 cc 00 00

     (LDA !$00)
     (STA ZP_RA)
     (STA ZP_RA+1)
     (JSR WRITE_RA_TO_CELL0_CELLPAIR_RT)

     (JSR ALLOC_CELLPAIR_TO_RT)))

  (define vm-allocate-cell-pair-ptr-to-rt-5-state
    (run-code-in-test vm-allocate-cell-pair-ptr-to-rt-5-code))

  (check-equal? (vm-regt->string vm-allocate-cell-pair-ptr-to-rt-5-state)
                (format "pair-ptr[0] $~a09" (format-hex-byte PAGE_AVAIL_0))
                "cellp-pair at cc09 is reused (was at head of tree)")
  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-5-state (+ PAGE_AVAIL_0_W #x01) (+ PAGE_AVAIL_0_W #x01))
                (list #x00)
                "refcount of cc05 (is at cc01) is zero again!")
  (check-equal? (vm-cell-pair-free-tree->string vm-allocate-cell-pair-ptr-to-rt-5-state)
                (format "pair $~a05 -> [ pair-ptr NIL . int $0001 ]" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (memory-list vm-allocate-cell-pair-ptr-to-rt-5-state (+ PAGE_AVAIL_0_W #x06) (+ PAGE_AVAIL_0_W #x08))
                (list #x00 #x03 #x01)
                "pair at cc06 holds 00 in cell0-page (no further elements in queue) and int 1 in cell1")
  (check-equal? (vm-page->strings vm-allocate-cell-pair-ptr-to-rt-5-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41")
                "still both are used on the page, one was allocated (reused) from tree, and the other is now head of the tree"))

;; impl complete, test missing
(define NEW_DEC_REFCNT_CELL_RC #t)
(define NEW_DEC_REFCNT_CELL_RT #t)
(define NEW_DEC_REFCNT_CELL_RA #t)
(define NEW_DEC_REFCNT_CELLPAIR_RT #t)
(define NEW_DEC_REFCNT_CELLPAIR_RA #t)
(define NEW_DEC_REFCNT_CELLPAIR_RC #t)
(define FREE_M1_SLOT_RCm1 #t)
(define NEW_DEC_REFCNT_M1_SLOT_RC #t)
(define NEW_DEC_REFCNT_CELLARR_RC #t)
(define NEW_DEC_REFCNT_NATIVEARR_RC #t)
(define NEW_DEC_REFCNT_RC
  (add-label-suffix
   "__" "__NEW_DEC_REFCNT_RC"
   (flatten
   (list
   (label NEW_DEC_REFCNT_RC)    ;; RC -> [cell] || [cellA][cellB] || [cell-natarr-header][byte0][byte1] ...[byten] || [cell-arr-header][cell0][cell1]...[celln]
          (PTR_DETECTION_MACRO_RC
           "UNKNOWN__"
           "CELL_ALREADY_LSRED__"
           "CELLPAIR_ALREADY_LSRED__"
           "NEW_DEC_REFCNT_CELLARR_RC"
           "NEW_DEC_REFCNT_NATIVEARR_RC"
           "NEW_DEC_REFCNT_M1_SLOT_RC")

   (label UNKNOWN__)
          ;; unknown object type (or atomic value that cannot be ref counted and MUST NOT END UP in ZP_RC)
   (label DONE__)
          (RTS)

   (label NEW_DEC_REFCNT_M1_SLOT_RC)
   (label NEW_DEC_REFCNT_CELLARR_RC)
   (label NEW_DEC_REFCNT_NATIVEARR_RC)
          ;; TODO decrement count (both are on m1 pages, but native arrays can be freed without looking at its contents!)
          (DEC ZP_RC) ;; no page boundary will be crossed, since lowbyte always > 0
          (LDY !$00)
          (LDA (ZP_RC),y)
          (SEC)
          (SBC !$01)
          (STA (ZP_RC),y)
          (BEQ FREE_M1_SLOT_RCm1) ;; m1 since ZP_RC was decremented and currently points to the refcnt
          (INC ZP_RC)               ;; restore original rc
          (RTS)

   (label FREE_M1_SLOT_RCm1)
          ;; if native array, simply add this m1slot to the free list of this m1 page profile
          ;; if cell array, follow the algorithm
          (INC ZP_RC)
          (JMP NEW_FREE_M1_SLOT_RC)


   (label NEW_DEC_REFCNT_CELLPAIR_RA)
          (JSR CP_RA_TO_RC)
          (CLC)
          (BCC NEW_DEC_REFCNT_CELLPAIR_RC)

   (label NEW_DEC_REFCNT_CELLPAIR_RT)
          (JSR CP_RT_TO_RC)

   ;; input: cell-pair ptr in ZP_RA
   ;; decrement ref count, if 0 deallocate
   (label NEW_DEC_REFCNT_CELLPAIR_RC)
          (LDA ZP_RC)
          (LSR)
          (LSR)
   (label CELLPAIR_ALREADY_LSRED__)
          (TAX)
          ;; now decrement cell count
          (LDA ZP_RC+1)
          (BEQ DONE__) ;; nil -> done
          (STA DEC_PAGE_CELLPAIR_CNT__+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
   (label DEC_PAGE_CELLPAIR_CNT__)
          (DEC $c000,x) ;; c0 is overwritten by page (see above)
          (BNE DONE__)
          (JMP NEW_FREE_CELLPAIR_RC) ;; free (since refcnt dropped to 0)


   (label NEW_DEC_REFCNT_CELL_RA)
          (JSR CP_RA_TO_RC)
          (CLC)
          (BCC NEW_DEC_REFCNT_CELL_RC)

   (label NEW_DEC_REFCNT_CELL_RT)
          (JSR CP_RT_TO_RC)

   ;; input: cell ptr in ZP_RA
   ;; decrement ref count, if 0 deallocate
   (label NEW_DEC_REFCNT_CELL_RC)  ;; RC -> [cell] | [cell-array] | [native-array]
          (LDA ZP_RC) ;; lowbyte (offset)
          (LSR)
   (label CELL_ALREADY_LSRED__)
          ;; check what cell kind the target is: cell, cell-ptr, cell-pair-ptr, native-array, cell-array
          (LDY ZP_RC+1)
          (BEQ DONE__) ;; nil -> done
          (STY LDA_PAGE_TYPE__+2)
          (TAX)
   (label LDA_PAGE_TYPE__)
          (LDA $c000)
          (ASL A)
          (BCS NEW_DEC_REFCNT_CELL_RC_TO_CELL__)
          ;; can't really be cellpair type page (was checked before)
          ;; (LSL)
          ;; (BCS NEW_DEC_REFCNT_CELL_RC_TO_CELLPAIR)
          ;; else must be a m1 slot
   (label NEW_DEC_REFCNT_CELL_RC_TO_M1_SLOT__)
          (LDX ZP_RC)
          (DEX)
          (STY DEC_PAGE_M1_SLOT_CNT__+2)
   (label DEC_PAGE_M1_SLOT_CNT__)
          (DEC $C000,x)
          (BNE DONE__)
          (JMP NEW_FREE_M1_SLOT_RC)

   (label NEW_DEC_REFCNT_CELL_RC_TO_CELL__)
          (STY DEC_PAGE_CELL_CNT__+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
   (label DEC_PAGE_CELL_CNT__)
          (DEC $c000,x)               ;; c0 is overwritten by page (see above)
          (BNE DONE__)
          (JMP NEW_FREE_CELL_RC)      ;; free (since refcnt dropped to 0)
          ))))

(module+ test #| NEW_DEC_REFCNT_RC |#
  (define dec-refcnt-rc--dec-ref--cell
    (compact-run-code-in-test
     ;; #:debug #t
     #:mock (list (label NEW_FREE_CELL_RC))

     (JSR ALLOC_CELL_TO_RT)    ;; new cell in RT (with refcount = 0)
     (JSR INC_REFCNT_CELL_RT) ;; now should be 1
     (JSR INC_REFCNT_CELL_RT) ;; now should be 2
     (JSR CP_RT_TO_RC)

     ;; unit under test
     (JSR NEW_DEC_REFCNT_RC)))

  (check-equal? (calls-to-mock dec-refcnt-rc--dec-ref--cell)
                #x00
                "no free cell has taken place!")
  (check-equal? (peek dec-refcnt-rc--dec-ref--cell (+ PAGE_AVAIL_0_W 01))
                #x01
                "remaining refcount on cell0 is 1")

  (define dec-refcnt-rc--dec-ref-to-0--cell
    (compact-run-code-in-test
     #:mock (list (label NEW_FREE_CELL_RC))

     (JSR ALLOC_CELL_TO_RT)    ;; new cell in RT (with refcount = 0)
     (JSR INC_REFCNT_CELL_RT) ;; now should be 1
     (JSR CP_RT_TO_RC)

     ;; unit under test
     (JSR NEW_DEC_REFCNT_RC)))

  (check-equal? (calls-to-mock dec-refcnt-rc--dec-ref-to-0--cell)
                #x01
                "free cell has taken place!")
  (check-equal? (peek dec-refcnt-rc--dec-ref-to-0--cell (+ PAGE_AVAIL_0_W 01))
                #x00
                "remaining refcount on cell0 is 0")


  (define dec-refcnt-rc--dec-ref-to-0--cellpair
    (compact-run-code-in-test
     #:mock (list (label NEW_FREE_CELLPAIR_RC))

     (JSR ALLOC_CELLPAIR_TO_RT)     ;; new cellpair in RT (with refcount = 0)
     (JSR INC_REFCNT_CELLPAIR_RT)   ;; now should be 1
     (JSR CP_RT_TO_RC)

     ;; unit under test
     (JSR NEW_DEC_REFCNT_RC)))

  (check-equal? (calls-to-mock dec-refcnt-rc--dec-ref-to-0--cellpair)
                #x01
                "free cellpair has taken place!")
  (check-equal? (peek dec-refcnt-rc--dec-ref-to-0--cellpair (+ PAGE_AVAIL_0_W 01))
                #x00
                "remaining refcount on cellpair0 is 0")

  (define dec-refcnt-rc--dec-ref--cellpair
    (compact-run-code-in-test
     #:mock (list (label NEW_FREE_CELLPAIR_RC))

     (JSR ALLOC_CELLPAIR_TO_RT)     ;; new cellpair in RT (with refcount = 0)
     (JSR INC_REFCNT_CELLPAIR_RT)   ;; now should be 1
     (JSR INC_REFCNT_CELLPAIR_RT)   ;; now should be 2
     (JSR CP_RT_TO_RC)

     ;; unit under test
     (JSR NEW_DEC_REFCNT_RC)))

  (check-equal? (calls-to-mock dec-refcnt-rc--dec-ref--cellpair)
                #x00
                "no free cellpair has taken place!")
  (check-equal? (peek dec-refcnt-rc--dec-ref--cellpair (+ PAGE_AVAIL_0_W 01))
                #x01
                "remaining refcount on cellpair0 is 1")

  (define dec-refcnt-rc--dec-ref-to-0--m1_slot
    (compact-run-code-in-test
     #:mock (list (label NEW_FREE_M1_SLOT_RC))

     (LDA !$10)
     (JSR ALLOC_M1_SLOT_TO_RA)     ;; new m1_slot in RA (with refcount = 0)
     (JSR INC_REFCNT_M1_SLOT_RA)   ;; now should be 1
     (JSR CP_RA_TO_RC)

     ;; unit under test
     (JSR NEW_DEC_REFCNT_RC)))

  (check-equal? (calls-to-mock dec-refcnt-rc--dec-ref-to-0--m1_slot)
                #x01
                "free m1_slot has taken place!")
  (check-equal? (peek dec-refcnt-rc--dec-ref-to-0--m1_slot (+ PAGE_AVAIL_0_W 03))
                #x00
                "remaining refcount on m1_slot0 is 0")

  (define dec-refcnt-rc--dec-ref--m1_slot
    (compact-run-code-in-test
     #:mock (list (label NEW_FREE_M1_SLOT_RC))

     (LDA !$10)
     (JSR ALLOC_M1_SLOT_TO_RA)     ;; new m1_slot in RT (with refcount = 0)
     (JSR INC_REFCNT_M1_SLOT_RA)   ;; now should be 1
     (JSR INC_REFCNT_M1_SLOT_RA)   ;; now should be 2
     (JSR CP_RA_TO_RC)

     ;; unit under test
     (JSR NEW_DEC_REFCNT_RC)))

  (check-equal? (calls-to-mock dec-refcnt-rc--dec-ref--m1_slot)
                #x00
                "no free m1_slot has taken place!")
  (check-equal? (peek dec-refcnt-rc--dec-ref--m1_slot (+ PAGE_AVAIL_0_W 03))
                #x01
                "remaining refcount on m1_slot 0 is 1"))

;; impl complete, test missing
(define NEW_FREE_M1_SLOT_RC
  (add-label-suffix
   "__" "__NEW_FREE_M1_SLOT_RC"
   (list
    (label NEW_FREE_M1_SLOT_RC)
           (LDY !$00)
           (LDA (ZP_RC),y)
           (CMP !TAG_BYTE_CELL_ARRAY)
           (BEQ NEW_FREE_CELLARR__)
           (CMP !TAG_BYTE_NATIVE_ARRAY)
           (BEQ NEW_FREE_NATIVEARR__)

    (label UNKNOWN__)
          ;; unknown object type (or atomic value that cannot be ref counted and MUST NOT END UP in ZP_RC)
           (RTS)

    (label NEW_FREE_CELLARR__)
           (JMP NEW_FREE_CELLARR_RC)
    (label NEW_FREE_NATIVEARR__)
           (JMP NEW_ADD_M1_SLOT_RC_TO_PFL) ;; just add this slot to the free list of the respective page (and do some housekeeping)
)))

;; impl complete, test missing
(define NEW_FREE_CELLPAIR_RT #t)
(define NEW_FREE_CELLPAIR_RA #t)
(define NEW_FREE_CELLPAIR_RC
  (add-label-suffix
   "__" "__NEW_FREE_CELLPAIR_RC"
   (list
    (label NEW_FREE_CELLPAIR_RA)
           (JSR CP_RA_TO_RC)
           ;; (CLC)
           ;; (BCC NEW_FREE_CELLPAIR_RC)
           (JMP NEW_FREE_CELLPAIR_RC)

    (label NEW_FREE_CELLPAIR_RT)
           (JSR CP_RT_TO_RC)

    (label NEW_FREE_CELLPAIR_RC)
           (LDY !$00)
           (STY ZP_TEMP+1) ;; indicator of a ptr to free is set to 0 => currently no additional ptr marked for free

           ;; check cell0
           (LDA (ZP_RC),y) ;; LOWBYTE OF FIRST cell0
           (BEQ CELL_0_FREE_TO_USE__) ;; empty
           (STA ZP_TEMP)
           (AND !$03)
           (CMP !$03)
           (BEQ CELL_0_FREE_TO_USE__)
           ;; make sure to call free on cell0 (could be any type of cell)
           ;; remember ZP_PTR

           ;; store cell0 into ZP_TEMP (for later tail call of free)
           (INY)
           (LDA (ZP_RC),y)
           (STA ZP_TEMP+1) ;; cell0 was stored in zp_temp -> cell0 is now free to be used as link for free cell pairs

   ( label CELL_0_FREE_TO_USE__)
           ;; cell0 is no ptr and can thus be discarded (directly)

           ;; enqueue rt as new root of free cellpairs

           ;; simply add this cell-pair as head to free tree
           ;; set cell0 to point to old root
           (LDY !$01)
           (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
           (STA (ZP_RC),y)
           (DEY)
           (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
           (STA (ZP_RC),y)

           ;; set new root to point to cell-pair
           (LDA ZP_RC+1)
           (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
           (LDA ZP_RC)
           (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)

           ;; write original cell0 -> zp_rc
           (LDA ZP_TEMP+1)
           (BEQ DONE__)
           ;; otherwise zp_temp was used to store a pointer that needs to be decremented
           (STA ZP_RC+1)
           (LDA ZP_TEMP)
           (STA ZP_RC)

           (JMP NEW_DEC_REFCNT_RC) ;; chain call

    (label DONE__)
           (RTS))))

;; impl missing, test missing
(define NEW_FREE_CELLARR_RC
  (add-label-suffix
   "__" "__NEW_FREE_CELLARR_RC"
   (list
    (label NEW_FREE_CELLARR_RC)
           (RTS))))

;; impl complete, test missing
(define NEW_ADD_M1_SLOT_RC_TO_PFL
  (add-label-suffix
   "__" "__NEW_ADD_M1_SLOT_RC_TO_PFL"
   (list
    (label NEW_ADD_M1_SLOT_RC_TO_PFL)
           (JSR NEW_DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_RC)

          ;; now free the slot
   (label REGULAR_FREE__)
          (LDX ZP_RC+1)
          (STX DEC_CMD__+2)          ;; write page for later dec execution
          (LDA VM_PAGE_SLOT_DATA,x)  ;; first free slot offset
          (BNE CONTINUE__)           ;; regular free

          ;; this page was full (since next free slot was 0) => register with the list of pages with free slots
          (JSR NEW_PUT_PAGE_AS_HEAD_OF_M1_PAGE_RC)
          (LDX DEC_CMD__+2)          ;; restore x
          (LDA !$00)                 ;; next free slot offset (=0)

   (label CONTINUE__)
          (LDY !$00)
          (STA (ZP_RC),y)            ;; set (zp_ptr) = previous free
          (LDA ZP_RC)                ;; low byte of pointer = new free slot
          (STA VM_PAGE_SLOT_DATA,x)  ;; set new first free slot offset

          (DEC ZP_RC)                ;; now points to ref count
          (TYA)                      ;; y is still 0 => a := 0
          (STA (ZP_RC),y)            ;; set refcount := 0

   (label DEC_CMD__)                 ;; decrement number of slots used on the page
          (DEC $c002)                ;; $c0 is overwritten, 02 = location of used slots on that page type

          (RTS))))

(define NEW_PUT_PAGE_AS_HEAD_OF_M1_PAGE_RC
  (add-label-suffix
   "__" "__NEW_PUT_PAGE_AS_HEAD_OF_M1_PAGE_RC"
  (list
   (label NEW_PUT_PAGE_AS_HEAD_OF_M1_PAGE_RC)
          (LDA ZP_RC)
          (STA ZP_TEMP) ;; keep for later

          (LDA !$00)    ;; set to zero
          (STA ZP_RC)

          (LDY !$01)
          (LDA (ZP_RC),y) ;; get previous
          (BNE CONTINUE_WITH_RESTORE__)     ;; is != 0 => is still part of the list, don't change the list
          ;; is no longer part of the free list of pages, add this page at the head of the page

          (DEY) ;; now 0
          (LDA (ZP_RC),y) ;; get encoded page type
          (AND !$07)

          (TAX) ;; now x = page type

          (LDA VM_FREE_M1_PAGE_P0,x)

          (INY) ;; now 1
          (STA (ZP_RC),y) ;; set previous

          ;; x = page type, a = page
          (LDA ZP_RC+1)
          (STA VM_FREE_M1_PAGE_P0,x)
          (TAX)  ;; x = page

   (label CONTINUE_WITH_RESTORE__)
          (LDA ZP_TEMP)
          (STA ZP_RC) ;; restore
          (LDA VM_PAGE_SLOT_DATA,x)           ;; first free slot offset

          (RTS))))

(define NEW_DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A
  (add-label-suffix
   "__" "NEW_DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A"
  (list
   (label NEW_DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_RC)
          (LDA ZP_RC+1)
   (label NEW_DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A)
          (STA READ_ENC_PAGE_TYPE__+2)
   (label READ_ENC_PAGE_TYPE__)
          (LDA $c000)
          (AND !$07)

          (TAX) ;; now x = page profile

   ;; input: x (unchanged)
   (label NEW_DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_PROFILE_X)
          (LDA VM_FREE_M1_PAGE_P0,x)

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
          (STA VM_FREE_M1_PAGE_P0,x) ;; optional optimization: needs only be done once! (is here done in a loop)
   (label STORE_PREV_PAGE_CMD__)
          (STY $c001) ;; $c0 is overwritten
          (BNE LOOP__) ;; if the current page (in a) is 0 (we are at the end of the list), we are done and can return, else loop

   (label DONE__)
          (RTS))))

(define NEW_GC_ALL
  (add-label-suffix
   "__" "__NEW_GC_ALL"
   (list
    (label NEW_GC_ALL)
          (JSR NEW_GC_CELL_ARRAYS)    ;; until no more cell arrays are available
          (JSR GC_CELLPAIR_FREE_LIST) ;; until no more cell pairs are available
          (JMP NEW_GC_CELLS)          ;; until no moew cells are available
          )))

(define NEW_GC_CELLS
  (add-label-suffix
   "__" "NEW_GC_CELLS"
   (list
    (label NEW_GC_CELLS)
           (LDX VM_LIST_OF_FREE_CELLS+1)
           (BNE CONTINUE__)
           (RTS)
    (label CONTINUE__)
           (STX ZP_RC+1)
           (LDA VM_LIST_OF_FREE_CELLS)
           (STA ZP_RC)

           ;; remove this cell from the list of free cells
           ;; put next of rc into vm_list_of_free
           (LDY !$00)
           (LDA (ZP_RC),y)
           (STA VM_LIST_OF_FREE_CELLS)
           (INY)
           (LDA (ZP_RC),y)
           (STA VM_LIST_OF_FREE_CELLS+1)


           ;; return RC to its page free list
           ;; store in cell, pointed to by rc the previous head, now next
           (LDA VM_PAGE_SLOT_DATA,x) ;; old head of page free list
           (LDY !$00)                ;; optimization: remove this command if previous sequence makes sure y = 0
           (STA (ZP_RC),y)
           ;; (INY)                  ;; only the lowbyte needs to be set, since hb is known
           ;; (TXA)
           ;; (STA (ZP_RC),y)

           ;; mark RC as new first free slot on page (head)
           (LDA ZP_RC)
           (STA VM_PAGE_SLOT_DATA,x)


           ;; decrement number of used slots on this cell-page
           (STX DEC_CMD__+2)
           (STX DEC_CMD__+5)
    (label DEC_CMD__)
           (DEC $c000) ;; c0 is overwritten by page, cell-page holds # of slots in first byte
           (LDA $c000) ;; c0 is overwritten
           (AND !$3f)
           (BNE NEW_GC_CELLS) ;; loop

           ;; no more cells allocated on that page
           ;; return page to (completely) free pages? <- not implemented yet
           (BEQ NEW_GC_CELLS) ;; loop
           )))

;; do incremental collections until all cell arrays (and their slots) were garbage collected
(define NEW_GC_CELL_ARRAYS
  (add-label-suffix
   "__" "__NEW_GC_CELL_ARRAYS"
   (list
    (label NEW_GC_CELL_ARRAYS)
          (LDA ZP_PART_GCD_CELL_ARRAYS+1)
          (BNE GC_CONT__) ;; only if high byte (page) != 0, there seems to be a cell array to be worked on

   (label RETURN__)
          (RTS)

   (label GC_CONT__)
          (STA ZP_RC+1)
          (LDA ZP_PART_GCD_CELL_ARRAYS)
          (STA ZP_RC)
          (JSR NEW_GC_INCR_ARRAY_SLOT_RC)
          (CLC)
          (BCC NEW_GC_CELL_ARRAYS)))) ;; call until ZP_PART_GCD_CELL_ARRAYS is empty

;; may destroy RC (on dec refcnt of a cell in the array)
;; will free this cell-array, if no refcnts need to be dec (anymore)
;; will add this cell array to ZP_PART_GCD_CELL_ARRAYS if not completely gc'd
(define NEW_GC_INCR_ARRAY_SLOT_RC
  (add-label-suffix
   "__" "__NEW_GC_INCR_ARRAY_SLOT_RC"
  (list

   (label GC_INCR_CELLARR_GFL)
          (LDA ZP_PART_GCD_CELL_ARRAYS+1)
          (BNE GC_CONT__) ;; only if high byte (page) != 0, there seems to be a cell array to be worked on

   (label RETURN__)
          (RTS)

   (label GC_CONT__)
          (STA ZP_RC+1)
          (LDA ZP_PART_GCD_CELL_ARRAYS)
          (STA ZP_RC)

   (label NEW_GC_INCR_ARRAY_SLOT_RC) ;; cellarr layout: 00 = type, 01 = #of cells, 02/03 = cell0, 04/05 = cell1 ...
          ;; loop over slots and decrement their slots
          (LDY !$01)
          (LDA (ZP_RC),y)  ;; a = number of array elements
          (STA PREV_LAST_ENTRY__)
          (BEQ RETURN__) ;; number = 0 => nothing to do (should never happen)
          (ASL A) ;; e.g. 1 => 2 (low byte of last position)
          (TAY) ;;

   (label LOOP__)
          (LDA (ZP_RC),y) ;; load tagged low byte
          (BEQ NEXT__)

          (TAX)
          (AND !$03)
          (CMP !$03)
          (BEQ NEXT__) ;; cannot be other pointer than cell-ptr or cell-pair-ptr => this not a pointer => move on to next

   (label IS_PTR__)
          (INY)             ;;  RC -> [cell-array-header] [len n+1] [cell0]... [celln]
          (LDA (ZP_RC),y)   ;;  A = hb celln, X = lb celln
          (PHA)

          ;; check if working on the head of partially garbage collected cell arrays (and skip if so)
          (LDA ZP_PART_GCD_CELL_ARRAYS+1)
          (CMP ZP_RC+1)
          (BNE PUT_OLD_HEAD__) ;; != => not the head => enqueue this cell array as head

          (LDA ZP_PART_GCD_CELL_ARRAYS)
          (CMP ZP_RC)
          (BEQ CONT_WITH_DEC_PREP__) ;; == => is head => do not enqueue, but continue

          ;; put old head of ZP_PART_GCD_CELL_ARRAYS into last cell (which will be dec_refcnt ed)
          (LDA ZP_PART_GCD_CELL_ARRAYS+1)
   (label PUT_OLD_HEAD__)
          (STA (ZP_RC),y)   ;;  RC -> [cell-array-header] [len n+1] [cell0]... [lbcelln hb RC]
          (DEY)             ;;
          (LDA ZP_PART_GCD_CELL_ARRAYS)
          (STA (ZP_RC),y)   ;; RC -> [cell-array-header] [len n+1] [cell0]... [old head ZP_PART_GCD_CELL_ARRAYS]

          ;; put this array as new head of ZP_PART_GCD_CELL_ARRAYS (only if it is not already part of the list, right?)
          (LDA ZP_RC+1)
          (STA ZP_PART_GCD_CELL_ARRAYS+1)
          (LDA ZP_RC)
          (STA ZP_PART_GCD_CELL_ARRAYS)

   (label CONT_WITH_DEC_PREP__)
          (PLA)

          ;; store cell to be dec_refcnt ed into rc
          (STA ZP_RC+1)
          (STX ZP_RC)

          ;; calc new # of cells left in this cell array
          (DEY)
          (TYA)
          (LSR)
          (LDY !$01)
          (STA (ZP_RC),y) ;; set new number of (relevant) cells in the array

          (JMP NEW_DEC_REFCNT_RC)
          ;; optional optimization: if no deallocation took place (refcount did NOT drop to 0), keep looping
   (label NEXT__)
          (DEY)
          (DEY)
          (BNE LOOP__)

          ;; now completely done, all cells were inspected

          (TYA) ;; a := 0
          (INY) ;; y := 1
          (STA (ZP_RC),y) ;; set new number of (relevant) cells in the array to 0 (completely gc'd)

   (label DONE__)
          ;; if this cell array was enqueued as head, make sure to dequeue!!
          ;; compare with head, if equal => dequeue (need last entry before running)
          (LDA ZP_RC)
          (CMP ZP_PART_GCD_CELL_ARRAYS)
          (BNE NO_MORE_DEQUEUE__)
          (LDA ZP_RC+1)
          (CMP ZP_PART_GCD_CELL_ARRAYS+1)
          (BNE NO_MORE_DEQUEUE__)

          (LDA PREV_LAST_ENTRY__)
          (ASL A) ;; e.g. 1 => 2 (low byte of last position)
          (TAY) ;;
          (INY)
          (INY) ;; now low byte right behind last position [actually data for next head]
          (LDA (ZP_RC),y)
          (STA ZP_PART_GCD_CELL_ARRAYS)
          (INY)
          (LDA (ZP_RC),y)
          (STA ZP_PART_GCD_CELL_ARRAYS+1)

   (label NO_MORE_DEQUEUE__)
          (JMP NEW_ADD_M1_SLOT_RC_TO_PFL)

   (label PREV_LAST_ENTRY__)
          (byte 0))))

;; impl complete, test missing
(define NEW_FREE_CELL_RT #t)
(define NEW_FREE_CELL_RA #t)
(define NEW_FREE_CELL_RC
  (add-label-suffix
   "__" "__NEW_FREE_CELL_RC"
   (list
    (label NEW_FREE_CELL_RA)
           (JSR CP_RA_TO_RC)
           ;; (CLC)
           ;; (BCC NEW_FREE_CELL_RC)
           (JMP NEW_FREE_CELL_RC)

    (label NEW_FREE_CELL_RT)
           (JSR CP_RT_TO_RC)

    (label NEW_FREE_CELL_RC)
           ;; clear
           (LDY !$00)
           (STY LIST_TO_FREE__+1)

          (LDA (ZP_RC),y)
          (BEQ DONE__) ;; lowbyte = 0 = cell-ptr into zero page (illegal value or nil) => nothing to do
          (AND !$03)
          (CMP !$03)
          (BEQ IS_NEITHER_CELL_NOR_CELLPAIR_PTR__) ;; cell points to something != cell ptr and != cellpair ptr


          ;;  case 1: rc points to a cell pair: RC -> [cell]-> [cellA][cellB]
          ;;  case 2: rc points to a cell:      RC -> [cell]-> [cell]

   (label IS_A_PTR__)
          ;; cell is a pointer => save pointed to for tail call in temp
          ;; enqueue this rt in list to decrement refcount
          (LDA (ZP_RC),y)
          (STA LIST_TO_FREE__)
          (INY)
          (LDA (ZP_RC),y)
          (STA LIST_TO_FREE__+1)               ;; might be overwritten if cell-array is freed!

   (label JUST_FREE_THIS_CELL__)
          ;; COPY previous head of free cells into this cell
          (LDA VM_LIST_OF_FREE_CELLS+1)
          (STA (ZP_RC),y)
          (LDA VM_LIST_OF_FREE_CELLS)
          (DEY)
          (STA (ZP_RC),y)                       ;; RC -> [cell] -> (old) FREE_CELL_LIST

          ;; write this cell as new head into the list
          (LDA ZP_RC)
          (STA VM_LIST_OF_FREE_CELLS)
          (LDA ZP_RC+1)
          (STA VM_LIST_OF_FREE_CELLS+1)         ;; (new) FREE_CELL_LIST -> [cell] -> ...

          (LDA LIST_TO_FREE__+1)
          (BNE PREP_TAILCALL__)                          ;; there wasn't any further pointer => done with free
   (label DONE__)
          (RTS)

   (label PREP_TAILCALL__)
          ;; fill rc for tail calling
          (STA ZP_RC+1)
          (LDA LIST_TO_FREE__)
          (STA ZP_RC)                           ;; RC -> [cellA][cellB]  || [cell] || [cell-arr-header][cell0][cell1]...[celln] || [cell-natarr-header][byte0][byte1] ...[byten]
          (JMP NEW_DEC_REFCNT_RC)               ;; tail call since cell did hold a reference ;; the type of the cell was alread checked so optimization could directly call the right decr function

   (label IS_NEITHER_CELL_NOR_CELLPAIR_PTR__)
          ;; could still be something pointer to (like cellarr or nativearr
          (LDA (ZP_RC),y)
          (CMP !TAG_BYTE_CELL_ARRAY)
          (BEQ IS_A_PTR__)                ;; RC -> [cell] -> [cell-arr-header][cell0][cell1]...[celln]
          (CMP !TAG_BYTE_NATIVE_ARRAY)
          (BEQ IS_A_PTR__)                ;; RC -> [cell] -> [cell-natarr-header][byte0][byte1] ...[byten]

          ;; seems to be a value that does not need to be freed
          (BNE JUST_FREE_THIS_CELL__)


   (label LIST_TO_FREE__)
          (word 0) ;; holds (any) ptr for tail call (if necessary), use highbyte != 0 to detect whether pointer is set
)))

(module+ test #| new_free_cell_rc |#
  (define new-free-cell-ptr-in-rc-tailcall-state
    (compact-run-code-in-test                   ;; VM_LIST_OF_FREE_CELLS = 0000
     (JSR ALLOC_CELL_TO_RT)                     ;; RT -> [cell 0 @ ..02]
     (JSR INC_REFCNT_CELL_RT)
     (JSR CP_RT_TO_RA)                          ;; RA -> [cell 0]
     (JSR ALLOC_CELL_TO_RT)                     ;; RT -> [cell 1 @ ..08]
     (JSR WRITE_RA_TO_CELL0_CELLPAIR_RT)        ;; RT -> [cell 1] -> [cell 0]

     (JSR NEW_FREE_CELL_RT)                     ;; VM_LIST_OF_FREE_CELLS -> [cell 0] -> [cell 1] -> 0000
     ;; #:debug #t
     ))                                         ;; PFL -> [cell 2 @ ..0a]

  (check-equal? (memory-list new-free-cell-ptr-in-rc-tailcall-state VM_LIST_OF_FREE_CELLS (add1 VM_LIST_OF_FREE_CELLS))
                (list #x02 PAGE_AVAIL_0)
                (format "~a02 is new head of the free list" (number->string PAGE_AVAIL_0 16)))
  (check-equal? (memory-list new-free-cell-ptr-in-rc-tailcall-state (+ PAGE_AVAIL_0_W #x02) (+ PAGE_AVAIL_0_W #x03))
                (list #x08 PAGE_AVAIL_0)
                (format "~a02, which was freed, is referencing ~a08 as next in the free list"
                        (number->string PAGE_AVAIL_0 16)
                        (number->string PAGE_AVAIL_0 16)))
  (check-equal? (memory-list new-free-cell-ptr-in-rc-tailcall-state (+ PAGE_AVAIL_0_W #x08) (+ PAGE_AVAIL_0_W #x09))
                (list #x00 #x00)
                (format "~a08, which was freed, is the tail of the free list"
                        (number->string PAGE_AVAIL_0 16)))
  (check-equal? (vm-page->strings new-free-cell-ptr-in-rc-tailcall-state PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "two slots still allocated on page, they are however on the free list to be reused")

  (define new-free-cell-ptr-in-rt-state
    (compact-run-code-in-test
     (JSR ALLOC_CELL_TO_RT)
     (JSR NEW_FREE_CELL_RT)))

  (check-equal? (memory-list new-free-cell-ptr-in-rt-state VM_LIST_OF_FREE_CELLS (add1 VM_LIST_OF_FREE_CELLS))
                (list #x02 PAGE_AVAIL_0)
                "allocated cell is freed by adding it as head to the list of free cells")

  (check-equal? (memory-list new-free-cell-ptr-in-rt-state (+ PAGE_AVAIL_0_W #x02) (+ PAGE_AVAIL_0_W #x03))
                (list #x00 #x00)
                "the cell is set to 00 00, marking the end of the list of free cells")

  (check-equal? (vm-page->strings new-free-cell-ptr-in-rt-state PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page has still 1 slot in use (it was freed, but is no in free list, not completely unallocated)")

  (define new-free-cell-ptr-in-rt-realloc-state
    (compact-run-code-in-test
     (JSR ALLOC_CELL_TO_RT)
     (JSR NEW_FREE_CELL_RT)
     (JSR ALLOC_CELL_TO_RT)))

  (check-equal? (memory-list new-free-cell-ptr-in-rt-realloc-state VM_LIST_OF_FREE_CELLS VM_LIST_OF_FREE_CELLS)
                (list #x00)
                "list of free cells is empty again")

  (check-equal? (memory-list new-free-cell-ptr-in-rt-realloc-state ZP_RT (add1 ZP_RT))
                (list #x02 PAGE_AVAIL_0))

  (check-equal? (vm-page->strings new-free-cell-ptr-in-rt-realloc-state PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page has 1 slot in use")

  (define new-free-cell-ptr-in-rt-2xfree-state
    (compact-run-code-in-test
     (JSR ALLOC_CELL_TO_RT)
     (JSR CP_RT_TO_RA)
     (JSR ALLOC_CELL_TO_RT)
     (JSR NEW_FREE_CELL_RT)        ;; free cc08
     (JSR CP_RA_TO_RT)
     (JSR NEW_FREE_CELL_RT)))      ;; then free cc02

  (check-equal? (memory-list new-free-cell-ptr-in-rt-2xfree-state VM_LIST_OF_FREE_CELLS (add1 VM_LIST_OF_FREE_CELLS))
                (list #x02 PAGE_AVAIL_0)
                "last allocated cell is freed by adding it as head to the list of free cells")

  (check-equal? (memory-list new-free-cell-ptr-in-rt-2xfree-state (+ PAGE_AVAIL_0_W #x02) (+ PAGE_AVAIL_0_W #x03))
                (list #x08 PAGE_AVAIL_0)
                "the cell is set to $cc08, the next element in the free list")

  (check-equal? (memory-list new-free-cell-ptr-in-rt-2xfree-state (+ PAGE_AVAIL_0_W #x08) (+ PAGE_AVAIL_0_W #x08))
                (list #x00)
                "the cell is set to 00, marking the end of the list of free cells")

  (check-equal? (vm-page->strings new-free-cell-ptr-in-rt-2xfree-state PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $0a")
                "page has still 2 slot in use (it was freed, but is no in free list, not completely unallocated)"))

;; input cell ptr is in ZP_RT
;; ---
;; put this cell into the free-list (as head)
;; the cell will then hold the previous head of the free-list
;; allocating cells will first reuse this free-list
;; option: keep count (length) of this list to decide when to really free a cell
;;         and add it to the free list on its respective page!
;;
;; input:  RT
;; output:
;; (define FREE_CELL_RT
;;   (add-label-suffix
;;    "__" "__FREE_CELL_RT"
;;   (list
;;    (label FREE_CELL_RT)
;;           ;; check if previous content was a pointer
;;           (LDY !$00)
;;           (STY LIST_TO_FREE__+1)
;;           ;; cell-ptr      xxxxx0 but in total != 0
;;           ;; cell-pair-ptr xxxx01
;;           ;; => 10 00 01 == pointer, only 11 is no pointer, header cells may have other patterns
;;           (LDA (ZP_RT),y)
;;           (BEQ CELL_IS_NO_PTR__)
;;           (AND !$03)
;;           (CMP !$03)
;;           (BEQ CELL_IS_NO_PTR__)

;;           ;; cell is a pointer => save pointed to for tail call in temp (either cell-ptr or cell-pair-ptr)
;;           ;; enqueue this rt in list to decrement refcount
;;           ;; TODO: given arrays that may hold lists of references, this might make it necessary to keep them as list
;;           (LDA (ZP_RT),y)
;;           (STA LIST_TO_FREE__)
;;           (INY)
;;           (LDA (ZP_RT),y)
;;           (STA LIST_TO_FREE__+1)               ;; might be overwritten if cell-array is freed!
;;           (DEY)
;;           (BNE CONT__)

;;    (label CELL_IS_NO_PTR__)
;;           ;; check cell-type
;;           (LDA (ZP_RT),y)
;;           (CMP !TAG_BYTE_CELL_ARRAY)
;;           (BEQ FREE_CELL_ARRAY__)
;;           (CMP !TAG_BYTE_NATIVE_ARRAY)
;;           (BEQ FREE_NATIVE_ARRAY__)

;;           ;; seems to be a value that does not need to be freed
;;           (BNE CONT__)

;;    (label FREE_CELL_ARRAY__)
;;    (label FREE_NATIVE_ARRAY__)
;;           ;; this might end in some recursion depth, if arrays keep pointing to arrays and they are garbage collected
;;           (JMP DEC_REFCNT_M1_SLOT_RA) ;; cell and native arrays are allocated on m1 pages
;;           ;; this cell was a header field of a m1slot -> no further processing

;;   (label CONT__)
;;           ;; COPY previous head of free cells into this cell
;;           (LDA VM_LIST_OF_FREE_CELLS)
;;           (STA (ZP_RT),y)
;;           (LDA VM_LIST_OF_FREE_CELLS+1)
;;           (INY)
;;           (STA (ZP_RT),y)

;;           ;; write this cell as new head into the list
;;           (LDA ZP_RT)
;;           (STA VM_LIST_OF_FREE_CELLS)
;;           (LDA ZP_RT+1)
;;           (STA VM_LIST_OF_FREE_CELLS+1)

;;           (LDA LIST_TO_FREE__+1)
;;           (BEQ DONE__) ;; there wasn't any further pointer => done with free
;;           ;; fill rt for tail calling
;;           (STA ZP_RT+1)
;;           (LDA LIST_TO_FREE__)
;;           (STA ZP_RT)
;;           (JMP DEC_REFCNT_RT) ;; tail call if cell did hold a reference

;;    (label DONE__)
;;           (RTS)

;;    (label LIST_TO_FREE__) ;; TODO not yet a list, but needs to become one!
;;           (word 0))))

;; (define FREE_CELL_RA
;;   (add-label-suffix
;;    "__" "__FREE_CELL_RA"
;;   (list
;;    (label FREE_CELL_RA)
;;           ;; check if previous content was a pointer
;;           (LDY !$00)
;;           (STY LIST_TO_FREE__+1)

;;           (LDA (ZP_RA),y)
;;           (BEQ CELL_IS_NO_PTR__)
;;           (AND !$03)
;;           (CMP !$03)
;;           (BEQ CELL_IS_NO_PTR__)

;;           ;; cell is a pointer => save for tail call in temp (either cell-ptr or cell-pair-ptr)
;;           ;; enqueue this rt in list to decrement refcount
;;           ;; TODO: given arrays that may hold lists of references, this might make it necessary to keep them as list
;;           (LDA (ZP_RA),y)
;;           (STA LIST_TO_FREE__)
;;           (INY)
;;           (LDA (ZP_RA),y)
;;           (STA LIST_TO_FREE__+1)               ;; might be overwritten if cell-array is freed!
;;           (DEY)
;;           (BNE CONT__)

;;    (label CELL_IS_NO_PTR__)
;;           ;; check cell-type
;;           (LDA (ZP_RA),y)
;;           (CMP !TAG_BYTE_CELL_ARRAY)
;;           (BEQ FREE_CELL_ARRAY__)
;;           (CMP !TAG_BYTE_NATIVE_ARRAY)
;;           (BEQ FREE_NATIVE_ARRAY__)

;;           ;; seems to be a value that does not need to be freed
;;           (BNE CONT__)

;;    (label FREE_CELL_ARRAY__)
;;    (label FREE_NATIVE_ARRAY__)
;;           ;; this might end in some recursion depth, if arrays keep pointing to arrays and they are garbage collected
;;           (JMP DEC_REFCNT_M1_SLOT_RA) ;; cell and native arrays are allocated on m1 pages
;;           ;; this cell was a header field of a m1slot -> no further processing

;;   (label CONT__)
;;           ;; COPY previous head of free cells into this cell
;;           (LDA VM_LIST_OF_FREE_CELLS)
;;           (STA (ZP_RA),y)
;;           (LDA VM_LIST_OF_FREE_CELLS+1)
;;           (INY)
;;           (STA (ZP_RA),y)

;;           ;; write this cell as new head into the list
;;           (LDA ZP_RA)
;;           (STA VM_LIST_OF_FREE_CELLS)
;;           (LDA ZP_RA+1)
;;           (STA VM_LIST_OF_FREE_CELLS+1)

;;           (LDA LIST_TO_FREE__+1)
;;           (BEQ DONE__) ;; there wasn't any further pointer => done with free
;;           ;; fill rt for tail calling
;;           (STA ZP_RA+1)
;;           (LDA LIST_TO_FREE__)
;;           (STA ZP_RA)
;;           (JMP DEC_REFCNT_RA) ;; tail call if cell did hold a reference

;;    (label DONE__)
;;           (RTS)

;;    (label LIST_TO_FREE__) ;; TODO not yet a list, but needs to become one!
;;           (word 0))))

;; (module+ test #| vm-free-cell-ptr-in-rt |#
;;   (define vm-free-cell-ptr-in-rt-tailcall-code
;;     (list
;;      (JSR ALLOC_CELL_TO_RT)
;;      (JSR INC_REFCNT_CELL_RT)
;;      (JSR CP_RT_TO_RA)
;;      (JSR ALLOC_CELL_TO_RT)
;;      (JSR WRITE_RA_TO_CELL0_CELLPAIR_RT)
;;      (JSR FREE_CELL_RT)))

;;   (define vm-free-cell-ptr-in-rt-tailcall-state
;;     (run-code-in-test vm-free-cell-ptr-in-rt-tailcall-code))

;;   (check-equal? (memory-list vm-free-cell-ptr-in-rt-tailcall-state VM_LIST_OF_FREE_CELLS (add1 VM_LIST_OF_FREE_CELLS))
;;                 (list #x02 PAGE_AVAIL_0)
;;                 "cc02 is new head of the free list")
;;   (check-equal? (memory-list vm-free-cell-ptr-in-rt-tailcall-state (+ PAGE_AVAIL_0_W #x02) (+ PAGE_AVAIL_0_W #x03))
;;                 (list #x08 PAGE_AVAIL_0)
;;                 "cc02, which was freed, is referencing cc08 as next in the free list")
;;   (check-equal? (memory-list vm-free-cell-ptr-in-rt-tailcall-state (+ PAGE_AVAIL_0_W #x08) (+ PAGE_AVAIL_0_W #x09))
;;                 (list #x00 #x00)
;;                 "cc08, which was freed, is the tail of the free list")
;;   (check-equal? (vm-page->strings vm-free-cell-ptr-in-rt-tailcall-state PAGE_AVAIL_0)
;;                 (list "page-type:      cell page"
;;                       "previous page:  $00"
;;                       "slots used:     2"
;;                       "next free slot: $0a")
;;                 "two slots still allocated on page, they are however on the free list to be reused")

;;   (define vm-free-cell-ptr-in-rt-code
;;     (list
;;      (JSR ALLOC_CELL_TO_RT)
;;      (JSR FREE_CELL_RT)))

;;   (define vm-free-cell-ptr-in-rt-state
;;     (run-code-in-test vm-free-cell-ptr-in-rt-code))

;;   (check-equal? (memory-list vm-free-cell-ptr-in-rt-state VM_LIST_OF_FREE_CELLS (add1 VM_LIST_OF_FREE_CELLS))
;;                 (list #x02 PAGE_AVAIL_0)
;;                 "allocated cell is freed by adding it as head to the list of free cells")

;;   (check-equal? (memory-list vm-free-cell-ptr-in-rt-state (+ PAGE_AVAIL_0_W #x02) (+ PAGE_AVAIL_0_W #x03))
;;                 (list #x00 #x00)
;;                 "the cell is set to 00 00, marking the end of the list of free cells")

;;   (check-equal? (vm-page->strings vm-free-cell-ptr-in-rt-state PAGE_AVAIL_0)
;;                 (list "page-type:      cell page"
;;                       "previous page:  $00"
;;                       "slots used:     1"
;;                       "next free slot: $08")
;;                 "page has still 1 slot in use (it was freed, but is no in free list, not completely unallocated)")

;;   (define vm-free-cell-ptr-in-rt-realloc-code
;;     (list
;;      (JSR ALLOC_CELL_TO_RT)
;;      (JSR FREE_CELL_RT)
;;      (JSR ALLOC_CELL_TO_RT)))

;;   (define vm-free-cell-ptr-in-rt-realloc-state
;;     (run-code-in-test vm-free-cell-ptr-in-rt-realloc-code))

;;   (check-equal? (memory-list vm-free-cell-ptr-in-rt-realloc-state VM_LIST_OF_FREE_CELLS VM_LIST_OF_FREE_CELLS)
;;                 (list #x00)
;;                 "list of free cells is empty again")

;;   (check-equal? (memory-list vm-free-cell-ptr-in-rt-realloc-state ZP_RT (add1 ZP_RT))
;;                 (list #x02 PAGE_AVAIL_0))

;;   (check-equal? (vm-page->strings vm-free-cell-ptr-in-rt-realloc-state PAGE_AVAIL_0)
;;                 (list "page-type:      cell page"
;;                       "previous page:  $00"
;;                       "slots used:     1"
;;                       "next free slot: $08")
;;                 "page has 1 slot in use")

;;   (define vm-free-cell-ptr-in-rt-2xfree-code
;;     (list
;;      (JSR ALLOC_CELL_TO_RT)
;;      (JSR CP_RT_TO_RA)
;;      (JSR ALLOC_CELL_TO_RT)
;;      (JSR FREE_CELL_RT)       ;; free cc08
;;      (JSR CP_RA_TO_RT)
;;      (JSR FREE_CELL_RT)))     ;; then free cc02

;;   (define vm-free-cell-ptr-in-rt-2xfree-state
;;     (run-code-in-test vm-free-cell-ptr-in-rt-2xfree-code))

;;   (check-equal? (memory-list vm-free-cell-ptr-in-rt-2xfree-state VM_LIST_OF_FREE_CELLS (add1 VM_LIST_OF_FREE_CELLS))
;;                 (list #x02 PAGE_AVAIL_0)
;;                 "last allocated cell is freed by adding it as head to the list of free cells")

;;   (check-equal? (memory-list vm-free-cell-ptr-in-rt-2xfree-state (+ PAGE_AVAIL_0_W #x02) (+ PAGE_AVAIL_0_W #x03))
;;                 (list #x08 PAGE_AVAIL_0)
;;                 "the cell is set to $cc08, the next element in the free list")

;;   (check-equal? (memory-list vm-free-cell-ptr-in-rt-2xfree-state (+ PAGE_AVAIL_0_W #x08) (+ PAGE_AVAIL_0_W #x08))
;;                 (list #x00)
;;                 "the cell is set to 00, marking the end of the list of free cells")

;;   (check-equal? (vm-page->strings vm-free-cell-ptr-in-rt-2xfree-state PAGE_AVAIL_0)
;;                 (list "page-type:      cell page"
;;                       "previous page:  $00"
;;                       "slots used:     2"
;;                       "next free slot: $0a")
;;                 "page has still 2 slot in use (it was freed, but is no in free list, not completely unallocated)"))

;; input:  cell-pair ptr is in ZP_RT
;; uses: ZP_TEMP, ZP_TEMP+1
;; -----
;; put the cell-pair itself as new root to the free-tree
;; put the old free-tree into cell1
;; tail call free on old cell1 in this cell-pair (if not atomic, if atomic no tail call)
;; result: this cell-pair is the new root of the free-tree for cell-pairs with:
;;              cell0 = old free tree root, cell1 = non-freed (yet) original cell
(define FREE_CELLPAIR_RT
  (add-label-suffix
   "__" "__FREE_CELLPAIR_RT"
  (list
   (label FREE_CELLPAIR_RT)

          (LDY !$00)
          (STY ZP_TEMP+1) ;; indicator of a ptr to free is set to 0 => currently no additional ptr marked for free

          ;; check cell0
          (LDA (ZP_RT),y) ;; LOWBYTE OF FIRST cell0
          (BEQ CELL_0_NO_PTR__) ;; empty
          (STA ZP_TEMP)
          (AND !$03)
          (CMP !$03)
          (BEQ CELL_0_NO_PTR__)
          ;; make sure to call free on cell0 (could be any type of cell)
          ;; remember ZP_PTR
          
          ;; store cell0 into ZP_TEMP (for later tail call of free)
          (INY)
          (LDA (ZP_RT),y)
          (STA ZP_TEMP+1)

   (label CELL_0_NO_PTR__)
          ;; cell0 is no ptr and can thus be discarded (directly)

          ;; enqueue rt as new root of free cellpairs

          ;; simply add this cell-pair as head to free tree
          ;; set cell0 to point to old root
          (LDY !$01)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (STA (ZP_RT),y)
          (DEY)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (STA (ZP_RT),y)

          ;; set new root to point to cell-pair
          (LDA ZP_RT+1)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (LDA ZP_RT)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)

          ;; write original cell0 -> zp_rt
          (LDA ZP_TEMP+1)
          (BEQ DONE__)
          ;; otherwise zp_temp was used to store a pointer that needs to be decremented
          (STA ZP_RT+1)
          (LDA ZP_TEMP)
          (STA ZP_RT)

          (JMP DEC_REFCNT_RT) ;; chain call

   (label DONE__)
          (RTS))))

;; input:  cell-pair ptr is in ZP_RA
;; uses: ZP_TEMP, ZP_TEMP+1
;; -----
;; put the cell-pair itself as new root to the free-tree
;; put the old free-tree into cell1
;; tail call free on old cell1 in this cell-pair (if not atomic, if atomic no tail call)
;; result: this cell-pair is the new root of the free-tree for cell-pairs with:
;;              cell0 = old free tree root, cell1 = non-freed (yet) original cell
(define FREE_CELLPAIR_RA
  (add-label-suffix
   "__" "FREE_CELL_PAIR_RA"
  (list
   (label FREE_CELLPAIR_RA)

          (LDY !$00)
          (STY ZP_TEMP+1) ;; indicator of a ptr to free is set to 0 => currently no additional ptr marked for free

          ;; check cell0
          (LDA (ZP_RA),y) ;; LOWBYTE OF FIRST cell0
          (BEQ CELL_0_NO_PTR__)
          (STA ZP_TEMP)
          (AND !$03)
          (CMP !$03)
          (BEQ CELL_0_NO_PTR__)
          ;; make sure to call free on cell0 (could be any type of cell)
          ;; remember ZP_PTR

          ;; store cell0 into ZP_TEMP (for later tail call of free)
          (INY)
          (LDA (ZP_RA),y)
          (STA ZP_TEMP+1)

   (label CELL_0_NO_PTR__)
          ;; cell0 is no ptr and can thus be discarded (directly)

          ;; simply add this cell-pair as head to free tree
          ;; set cell0 to point to old root
          (LDY !$01)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (STA (ZP_RA),y)
          (DEY)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (STA (ZP_RA),y)

          ;; set new root to point to cell-pair
          (LDA ZP_RA+1)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (LDA ZP_RA)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)

          ;; write original cell0 -> zp_ra
          (LDA ZP_TEMP+1)
          (BEQ DONE__)
          ;; otherwise zp_temp was used to store a pointer that needs to be decremented
          (STA ZP_RA+1)
          (LDA ZP_TEMP)
          (STA ZP_RA)

          (JMP DEC_REFCNT_RA) ;; chain call

   (label DONE__)
          (RTS))))

(module+ test #| vm-free-cell-pair-ptr-in-rt |#
  (define vm-free-cell-pair-ptr-in-rt-code
    (list
     (LDA !$00)
     (STA ZP_RT)
     (STA ZP_RT+1)
     (JSR FREE_CELLPAIR_RT)))

  (define vm-free-cell-pair-ptr-in-rt-state
    (run-code-in-test vm-free-cell-pair-ptr-in-rt-code))

  (check-equal? (vm-regt->string vm-free-cell-pair-ptr-in-rt-state)
                "empty")

  (define vm-free-cell-pair-ptr-in-rt-2-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR FREE_CELLPAIR_RT)))

  (define vm-free-cell-pair-ptr-in-rt-2-state
    (run-code-in-test vm-free-cell-pair-ptr-in-rt-2-code))

  (check-equal? (vm-page->strings vm-free-cell-pair-ptr-in-rt-2-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09")
                "page has still 1 slot in use (it was freed, but is now in free list, not completely unallocated)")
  (check-equal? (vm-cell-pair-free-tree->string vm-free-cell-pair-ptr-in-rt-2-state)
                (format "pair $~a05 -> [ empty . empty ]" (format-hex-byte PAGE_AVAIL_0)))

  (define vm-free-cell-pair-ptr-in-rt-3-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR WRITE_INT1_TO_RA)
     (JSR WRITE_RA_TO_CELL1_CELLPAIR_RT)
     (JSR WRITE_INT0_TO_RA)
     (JSR WRITE_RA_TO_CELL0_CELLPAIR_RT)
     (JSR INC_REFCNT_RT)
     (JSR CP_RT_TO_RA)

     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR WRITE_RA_TO_CELL0_CELLPAIR_RT)
     (JSR WRITE_NIL_TO_RA)
     (JSR WRITE_RA_TO_CELL1_CELLPAIR_RT)

     ;; rt = ( -+ . NIL )                cell-pair @ cc09
     ;;         |
     ;;         +--> ( int0 . int1 )     cell-pair @ cc05

     (JSR FREE_CELLPAIR_RT)))

  (define vm-free-cell-pair-ptr-in-rt-3-state
    (run-code-in-test vm-free-cell-pair-ptr-in-rt-3-code))

  (check-equal? (vm-page->strings vm-free-cell-pair-ptr-in-rt-3-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41")
                "page has still 2 slot in use (it was freed, but is now in free list, not completely unallocated)")
  (check-equal? (vm-cell-pair-free-tree->string vm-free-cell-pair-ptr-in-rt-3-state)
                (format "pair $~a05 -> [ pair-ptr[-] $~a09 . int $0001 ]"
                        (format-hex-byte PAGE_AVAIL_0)
                        (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-deref-cell-pair-w->string vm-free-cell-pair-ptr-in-rt-3-state (+ PAGE_AVAIL_0_W #x09))
                "(empty . pair-ptr NIL)")

  (define vm-free-cell-pair-ptr-in-rt-4-code
    (list
     (JSR ALLOC_CELL_TO_RT)
     (JSR INC_REFCNT_RT)
     (JSR WRITE_INTm1_TO_RA)
     (JSR WRITE_RA_TO_CELL0_CELLPAIR_RT)
     (JSR CP_RT_TO_RA)

     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR WRITE_RA_TO_CELL0_CELLPAIR_RT)
     (JSR WRITE_INT1_TO_RA)
     (JSR WRITE_RA_TO_CELL1_CELLPAIR_RT)

     ;; rt = ( -+ . int1 )               cell-pair @ cb05
     ;;         |
     ;;         +--> ( int-1 )           cell      @ cc02

     (JSR FREE_CELLPAIR_RT)))

  (define vm-free-cell-pair-ptr-in-rt-4-state
    (run-code-in-test vm-free-cell-pair-ptr-in-rt-4-code))

  (check-equal? (vm-page->strings vm-free-cell-pair-ptr-in-rt-4-state PAGE_AVAIL_0)
                (list "page-type:      cell page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $08")
                "page has still 1 slot in use (it was freed, but is now in free list, not completely unallocated)")
  (check-equal? (vm-page->strings vm-free-cell-pair-ptr-in-rt-4-state PAGE_AVAIL_1)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09")
                "page has still 1 slot in use (it was freed, but is now in free list, not completely unallocated)")
  (check-equal? (vm-cell-pair-free-tree->string vm-free-cell-pair-ptr-in-rt-4-state)
                (format "pair $~a05 -> [ empty . int $0001 ]" (format-hex-byte PAGE_AVAIL_1)))
  (check-equal? (vm-deref-cell-w->string vm-free-cell-pair-ptr-in-rt-4-state VM_LIST_OF_FREE_CELLS)
                (format "ptr[-] $~a02" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-deref-cell-w->string vm-free-cell-pair-ptr-in-rt-4-state (+ PAGE_AVAIL_0 #x02))
                "empty"))

;; add the given cell-pair (in zp_ptr) to the free list of cell-pairs on its page
;; input:  zp_ptr = pointer to cell-pair that is added to the free list on its page
;; output: reduces the number of used slots in byte 0
;;         next free slot of this page is the given cell-pair
(define VM_ADD_CELL_PAIR_IN_RT_TO_ON_PAGE_FREE_LIST
  (add-label-suffix
   "__" "VM_ADD_CELL_PAIR_IN_RT_TO_ON_PAGE_FREE_LIST"
  (list
   (label VM_ADD_CELL_PAIR_IN_RT_TO_ON_PAGE_FREE_LIST)
          (LDX ZP_RT+1)
          (STX DEC_CMD__+2) ;; set page for dec command
          (LDA VM_PAGE_SLOT_DATA,x) ;; old first free on page
          (LDY !$00)
          (STA (ZP_RT),y) ;; set old free to next free on this very cell
          (LDA ZP_RT) ;; load idx within page
          (STA VM_PAGE_SLOT_DATA,x) ;; set this cell as new first free cell on page

          ;; clear refcount, too (should have been done already)
          (LSR)
          (LSR)
          (TAY);; y now pointer to refcount byte (of cellpair)
          (LDA !$00)
          (STA ZP_RT) ;; modify pointer such that zp_ptr points to beginning of page
          (STA (ZP_RT),y) ;; clear refcount byte, too

   (label DEC_CMD__)
          (DEC $c000) ;; $c0 is overwritten by actual page
          (RTS))))

(module+ test #| vm-add-cell-pair-in-rt-to-on-page-free-list |#
  (define vm-add-cell-pair-in-rt-to-on-page-free-list-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)
     (JSR FREE_CELLPAIR_RT)
     (JSR VM_ADD_CELL_PAIR_IN_RT_TO_ON_PAGE_FREE_LIST)))

  (define vm-add-cell-pair-in-rt-to-on-page-free-list-state
    (run-code-in-test vm-add-cell-pair-in-rt-to-on-page-free-list-code))

  (check-equal? (vm-page->strings vm-add-cell-pair-in-rt-to-on-page-free-list-state PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     0"
                      "next free slot: $05")
                "again all slots are available on that page"))

(module+ test #| use case: allocate, free, reallocate small list of cell-pairs |#
  (define use-case-2-a-code
    (list
     (JSR ALLOC_CELLPAIR_TO_RT)                     ;; rt = freshly allocated cell (cc05)
     (JSR INC_REFCNT_CELLPAIR_RT)               ;; ref(rt) ++ (=1)
     ;; set cdr to nil     
     (JSR WRITE_NIL_TO_RA)
     (JSR WRITE_RA_TO_CELL1_CELLPAIR_RT)                          ;; (cdr rt) := nil
     ;; set car to int 0
     (JSR WRITE_INT1_TO_RA)
     (JSR WRITE_RA_TO_CELL0_CELLPAIR_RT)                          ;; (car rt) := int0

     (JSR CP_RT_TO_RA)                                   ;; ra := rt

     (JSR ALLOC_CELLPAIR_TO_RT)                     ;; rt = freshly allocated cell (cc09)
     (JSR INC_REFCNT_CELLPAIR_RT)               ;; ref(rt) ++ (=1)

     ;; set cdr 
     (JSR WRITE_RA_TO_CELL1_CELLPAIR_RT)                          ;; (cdr rt) := ra
     ;; set car to int0
     (JSR WRITE_INT0_TO_RA)
     (JSR WRITE_RA_TO_CELL0_CELLPAIR_RT)                          ;; (car rt) := int0

     ;; now:
     ;;   rt[cc09|1] (int0 . ->[cc05|1](int0 . nil))
     ;; notation:
     ;;   [<mem-location>|<ref-count>]
     ;;   (<car-cell> . <cdr-cell>)
     ;;   intX, nil :: atomic value cells
     ;;   -> :: cell-ptr
     ))

  (define use-case-2-a-state-after
    (run-code-in-test use-case-2-a-code))

  (check-equal? (vm-deref-cell-pair-w->string use-case-2-a-state-after (+ PAGE_AVAIL_0_W #x09))
                (format "(int $0000 . pair-ptr[1] $~a05)" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-deref-cell-pair-w->string use-case-2-a-state-after (+ PAGE_AVAIL_0_W #x05))
                "(int $0001 . pair-ptr NIL)")
  (check-equal? (vm-regt->string use-case-2-a-state-after)
                (format "pair-ptr[1] $~a09" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-page->strings use-case-2-a-state-after PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41"))

  (define use-case-2-b-code
    (append use-case-2-a-code ;; zp_ptr[cc08|1] (int0 . ->[cc04|1](int0 . nil))
            (list
             (JSR DEC_REFCNT_CELLPAIR_RT)
             ;; now:
             ;;   free_tree -> [cc08|0] (int0 . ->[cc04|1] (int0 . nil))
             )))

  (define use-case-2-b-state-after
    (run-code-in-test use-case-2-b-code))

  (check-equal? (vm-cell-pair-free-tree->string use-case-2-b-state-after)
                (format "pair $~a09 -> [ empty . pair-ptr[-] $~a05 ]"
                        (format-hex-byte PAGE_AVAIL_0)
                        (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-page->strings use-case-2-b-state-after PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41"))

  (define use-case-2-c-code
    (append use-case-2-b-code ;; free_tree -> [cd08|0] (int0 . ->[cd04|1] (int0 . nil))
            (list (LDA !$FF) ;; marker for debug, remove when done
                  (JSR ALLOC_CELLPAIR_TO_RT)
                  (JSR INC_REFCNT_CELLPAIR_RT)
                  ;; now:
                  ;;   zp_rt = [cd08|1] not initialized
                  ;;   free_tree -> [cd04|0] (int0 . nil)
                  )))

  (define use-case-2-c-state-after
    (run-code-in-test use-case-2-c-code))

  (check-equal? (vm-regt->string use-case-2-c-state-after)
                (format "pair-ptr[1] $~a09"
                        (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-cell-pair-free-tree->string use-case-2-c-state-after)
                (format "pair $~a05 -> [ pair-ptr NIL . pair-ptr NIL ]" (format-hex-byte PAGE_AVAIL_0)))
  (check-equal? (vm-page->strings use-case-2-c-state-after PAGE_AVAIL_0)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     2"
                      "next free slot: $41")))

;; ----------------------------------------
;; page type slot w/ different sizes (refcount @ ptr-1) x cells
;; math: first entry @FIRST_REF_COUNT_OFFSET__INIT_M1Px_PAGE_A + 1, refcount @ -1, next slot += INC_TO_NEXT_SLOT__INIT_M1Px_PAGE_A, slot-size = INC_TO_NEXT_SLOT__INIT_M1Px_PAGE_A -1
;; input : Y = profile offset (0, 2, 4 ...)
;;         X = page
;; uses  : ZP_RA
;; output: X = page
;;         A = first fre slot
(define INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX
  (add-label-suffix
   "__" "INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX"
  (list
   (label INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX)
          (STY SEL_PROFILE__)      ;; save profile index in local var
          (TYA)                 ;; profile 0..4 -> a
          (STX ZP_RA+1)

          (LDY !$00)
          (STY ZP_RA)

          (TAX)
          (ORA !$10)
          (STA (ZP_RA),y)       ;; set page type in byte 0 to b0001 <profile>

          (LDA VM_FREE_M1_PAGE_P0,x) ;; current free page
          (INY)
          (STA (ZP_RA),y)          ;; store previous page

          (LDA ZP_RA+1)
          (STA VM_FREE_M1_PAGE_P0,x) ;; set page with free slots to this allocated page

          (LDA !$00)
          (INY)
          (STA (ZP_RA),y)          ;; store number of slots used

          (LDY TABLE__FIRST_REF_COUNT_OFFSET__,x) ;; y = refcount field for first slot
          (INY)
          (TYA)
          (PHA)
          (LDX ZP_RA+1)
          (STA VM_PAGE_SLOT_DATA,x)                    ;; set first free slot, here x = page
          (DEY)
          (LDX SEL_PROFILE__) ;; profile = 0..3
          (LDA !$00)

          ;; loop to initialize refcounts of each slot to 0-
   (label REF_COUNT_LOOP__)
          (STA (ZP_RA),y) ;; refcount = 0
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
          (STA (ZP_RA),y) ;; offset of next free cell == y for next write
          (TAY)
          (BCC WRITE_FREE_LIST__) ;; carry must be clear => always jump

   (label ALMOST_DONE__)
          (LDA !$00)
          (STA (ZP_RA),y) ;; last offset to next free slot is 00 = no next free slot!
          (LDX ZP_RA+1)   ;; x = page
          (PLA)           ;; A = first free slot
          (RTS)

   (label SEL_PROFILE__)
          (byte $00) ;; local var

   (label TABLE__FIRST_REF_COUNT_OFFSET__)
          (byte $03) ;; first ref count is 03, add 0a to get to next slot, slot-size $09 (09), contains 25 slots
          (byte $03) ;; first ref count is 03, add 12 to get to next slot, slot size $11 (17), contains 14 slots
          (byte $0f) ;; first ref count is 0f, add 1e to get to next slot, slot size $1d (29), contains 8 slots
          (byte $05) ;; first ref count is 05, add 32 to get to next slot, slot-size $31 (49), contains 5 slots
          (byte $03) ;; first ref count is 03, add 54 to get to next slot, slot-size $53 (83), contains 3 slots
   (label TABLE__INC_TO_NEXT_SLOT_M1Px_PAGE)
          (byte $0a) ;; add 0a to get to next slot, slot-size $09 (09), contains 25 slots
          (byte $12) ;; add 12 to get to next slot, slot size $11 (17), contains 14 slots
          (byte $1e) ;; add 1e to get to next slot, slot size $1d (29), contains 8 slots
          (byte $32) ;; add 32 to get to next slot, slot-size $31 (49), contains 5 slots
          (byte $54) ;; add 54 to get to next slot, slot-size $53 (83), contains 3 slots
          )))

(module+ test #| vm_alloc_m1_page |#
  (define test-alloc-m1-01-code
    (list
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

  (define test-alloc-m1-01-state-after
    (run-code-in-test test-alloc-m1-01-code))

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

  (define test-alloc-m1-02-code
    (list
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

  (define test-alloc-m1-02-state-after
    (run-code-in-test test-alloc-m1-02-code))

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

  (define test-alloc-m1-03-code
    (list
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

  (define test-alloc-m1-03-state-after
    (run-code-in-test test-alloc-m1-03-code))

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

  (define test-alloc-m1-04-code
    (list
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

  (define test-alloc-m1-04-state-after
    (run-code-in-test test-alloc-m1-04-code))

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

;; allocate a slot of min A size, allocating a new page if necessary
;; input:  A = size
;; output: ZP_RA = available slot of the given size (or a bit more)
;;         Y = actual size
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
          (JSR VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS)

   (label ALLOC_M1_SLOT_TYPE_X_TO_RA)
          (LDA VM_FREE_M1_PAGE_P0,x) ;;
          (BEQ NEW_PAGE__)     ;; if the current free page is $00 (there is no page marked as having free slots) => allocate new page

          ;; ensure zp_ra points into the page
          (STA ZP_RA+1)
          (STA INC_CMD__+2)
          (TAX)
          (LDY VM_PAGE_SLOT_DATA,x)           ;; first free slot offset
          (BEQ NEW_PAGE__)    ;; if =0 allocate new page (no more free slots on this page)
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
          (BEQ NEW_PAGE__) ;; next page ptr = $00 => end reached, no more pages
          ;; check whether this page is full
          (TAX)
          (LDY VM_PAGE_SLOT_DATA,x)
          (BEQ FIND_NEXT_FREE_PAGE__) ;; next free slot for page is 00 => page is full, try to find next
          ;; page is not full => this is the new head
          (LDX PAGE_TYPE_IDX__)
          (STA VM_FREE_M1_PAGE_P0,x)
          (STA ZP_RA+1)
          (CLC)
          (BCC CONTINUE__)

   (label NEW_PAGE__)               ;; allocate a complete new page for page type x or find a page in the list that has free slots
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
  (define test-alloc-bucket-slot-code
    (list
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

            (LDA VM_FREE_M1_PAGE_P0+1) ;; type 1
            (STA ZP_TEMP)))

  (define test-alloc-bucket-slot-state-after
    (run-code-in-test test-alloc-bucket-slot-code))

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
  (define test-alloc-bucket-slot-2x-code
    (list
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

     (LDA VM_FREE_M1_PAGE_P0+1) ;; type 1
     (STA ZP_TEMP)))

  (define test-alloc-bucket-slot-2x-state-after
    (run-code-in-test test-alloc-bucket-slot-2x-code))

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
  (define test-alloc-bucket-slot-xx-code
    (list
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
            (LDA VM_FREE_M1_PAGE_P0+2) ;; type 2
            (STA ZP_TEMP)))

  (define test-alloc-bucket-slot-xx-state-after
    (run-code-in-test test-alloc-bucket-slot-xx-code))

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


;; inc ref count bucket slot
;; dec ref count bucket slot

;; remove full pages in the free list of pages of the same type as are currently in ZP_RA
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
          (LDA VM_FREE_M1_PAGE_P0,x)

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
          (STA VM_FREE_M1_PAGE_P0,x) ;; optional optimization: needs only be done once! (is here done in a loop)
   (label STORE_PREV_PAGE_CMD__)
          (STY $c001) ;; $c0 is overwritten
          (BNE LOOP_REMOVE_FULL_PAGES__) ;; if the current page (in a) is 0 (we are at the end of the list), we are done and can return, else loop

   (label DONE__)
          (RTS))))

;; put this page as head of the page free list for slots of type as in ZP_RA
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

          (LDA VM_FREE_M1_PAGE_P0,x)

          (INY) ;; now 1
          (STA (ZP_RA),y) ;; set previous

          ;; x = page type, a = page
          (LDA ZP_RA+1)
          (STA VM_FREE_M1_PAGE_P0,x)
          (TAX)  ;; x = page

   (label CONTINUE_WITH_RESTORE__)
          (LDA ZP_TEMP)
          (STA ZP_RA) ;; restore
          (LDA VM_PAGE_SLOT_DATA,x)           ;; first free slot offset

          (RTS))))

;; free the m1 slot pointed to by ra, marking that slot free on the m1-page
;; input:  ZP_RA
;; output: ZP_RA is invalid
;; currently once allocated pages are not garbage collected. this is bad and needs to be changed
;; (e.g. keep count of used slots)? used slots = 0 => free page
;; INFO: NO GC! (this must be done, freeing specific types (e.g. an array) <- knows the number of slots etc.
;;       REF COUNT IS SET TO ZERO
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
  (define test-free-bucket-slot-code
    (list
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

  (define test-free-bucket-slot-state-after
    (run-code-in-test test-free-bucket-slot-code))

  (check-equal? (memory-list test-free-bucket-slot-state-after (+ PAGE_AVAIL_0_W #x03)(+ PAGE_AVAIL_0_W #x04))
                (list #x00 #x28)
                "slot0 (now free): refcount 0, next free slot at offset $28")
  (check-equal? (vm-page->strings test-free-bucket-slot-state-after PAGE_AVAIL_0)
                '("page-type:      m1 page p1"
                  "previous page:  $00"
                  "slots used:     1"
                  "next free slot: $04")))

(module+ test #| vm_free_bucket_slot  allocate 16 slots, free first slot |#
  (define test-free-bucket-a20-slot-code
    (list
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
            (JSR FREE_M1_SLOT_RA)
            ))

  (define test-free-bucket-a20-slot-state-after
    (run-code-in-test test-free-bucket-a20-slot-code))

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
          (RTS)))

(module+ test #| vm_inc_ref_bucket_slot |#
  (define test-inc-ref-bucket-slot-1-code
    (list
     (LDA !$f0)
     (STA $f003)
     (STA ZP_RA+1)
     (LDA !$04)
     (STA ZP_RA)

     (JSR INC_REFCNT_M1_SLOT_RA)))

  (define test-inc-ref-bucket-slot-1-state-after
    (run-code-in-test test-inc-ref-bucket-slot-1-code))

  (check-equal? (memory-list test-inc-ref-bucket-slot-1-state-after #xf003 #xf003)
                (list #xf1))
  (check-equal? (memory-list test-inc-ref-bucket-slot-1-state-after ZP_RA (add1 ZP_RA))
                (list #x04 #xf0)))

;; TODO: check whether this should operate on RA or rather on RT
;; input: ZP_RA  pointer to bucket slot (which can be anything, but most likely a cell-array or a native-array)
;; use: may use ZP_RT during GC? :: TODO: fix that, RT may not be overwritten
(define DEC_REFCNT_M1_SLOT_RA
  (add-label-suffix
   "__" "__DEC_REFCNT_M1_SLOT_RA"
   (list
    (label DEC_REFCNT_M1_SLOT_RA)
           (DEC ZP_RA)
           (LDY !$00)
           (LDA (ZP_RA),y)
           (SEC)
           (SBC !$01)            ;;  pointers are organized such that there is no page boundary crossed (=> no adjustment of highbyte necessary)
           (STA (ZP_RA),y)
           (BNE NO_GC__)

           ;; DO GC THIS SLOT and then FREE!!
           ;; what kind of object is this (read header cell)
           ;; then dispatch an header cell type
           (INC ZP_RA) ;; now pointing at the first (lowbyte) of the cell header
           (LDA (ZP_RA),y) ;; y still 0
           (CMP !TAG_BYTE_CELL_ARRAY)       ;;
           (BNE NEXT0__)

           ;; TODO: register array as free  (lazy)
           ;; its a regular array slot, (gc each slot, beware recursion!!!!)
           ;; save rt
           ;; TODO: clean this ugly code up!
           (LDA ZP_RT)
           (STA SAVE_RT__)
           (LDA ZP_RT+1)
           (STA SAVE_RT__+1)

           (JSR CP_RA_TO_RT) ;; illegal use of rt here!
           (JSR GC_ARRAY_SLOT_RT) ;; illegal use of rt here!, uses RA, too!!
           (JSR CP_RT_TO_RA) ;; illegal use of rt here!

           (LDA SAVE_RT__)
           (STA ZP_RT)
           (LDA SAVE_RT__+1)
           (STA ZP_RT+1)

           (JMP FREE_M1_SLOT_RA)

    (label NEXT0__)
           (CMP !TAG_BYTE_NATIVE_ARRAY)
           (BNE NEXT1__)

           ;; it's a native array slot (no gc necessary)
           (JMP FREE_M1_SLOT_RA)

    (label NEXT1__)
           (BRK) ;; error, unknown complex slot type

    (label NO_GC__)
           (INC ZP_RA)
           (RTS)

    (label SAVE_RT__)
           (word $0000))))

(module+ test #| vm_dec_ref_bucket_slot (no gc) |#
  (define test-dec-ref-bucket-slot-1-code
    (list
     (LDA !$f0)
     (STA $f003) ;; $f004 - 1 = location for ref counting (now set to $f0)
     (STA ZP_RA+1)
     (LDA !$04)
     (STA ZP_RA) ;; RA is set to $f004

     (JSR DEC_REFCNT_M1_SLOT_RA)))

  (define test-dec-ref-bucket-slot-1-state-after
    (run-code-in-test test-dec-ref-bucket-slot-1-code))

  (check-equal? (memory-list test-dec-ref-bucket-slot-1-state-after #xf003 #xf003)
                (list #xef)
                "f0 - 1 = ef")
  (check-equal? (memory-list test-dec-ref-bucket-slot-1-state-after ZP_RA (add1 ZP_RA))
                (list #x04 #xf0)
                "points to $f004"))

;; (module+ test #| vm_dec_ref_bucket_slot (gc native array) |#

;;   (define test-dec-ref-bucket-slot-2-code
;;     (list
;;             (LDA !$00)
;;             (STA $a000) ;; counter for how often VM_GC_ARRAY_SLOT_PTR was called
;;             (JMP TEST_DEC_REF_BUCKET_SLOT_2_CODE)

;;      (label VM_GC_ARRAY_SLOT_PTR)
;;             (INC $a000)
;;             (RTS)

;;      (label TEST_DEC_REF_BUCKET_SLOT_2_CODE)
;;             (LDA !$10)
;;             (JSR VM_ALLOC_NATIVE_ARRAY_TO_ZP_PTR2)
;;             (JSR VM_COPY_PTR2_TO_PTR) ;; allocation is in zp_ptr2
;;             (JSR VM_REFCOUNT_INCR_ZP_PTR__M1_SLOT) ;; inc ref uses zp_ptr

;;             (JSR VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT) ;; dec ref uses zp_ptr
;;      ))

;;   (define test-dec-ref-bucket-slot-2-state-after
;;     (run-code-in-test test-dec-ref-bucket-slot-2-code
;;                       #:mock (list (label VM_GC_ARRAY_SLOT_PTR))))

;;   (check-equal? (vm-page->strings test-dec-ref-bucket-slot-2-state-after #xcc)
;;                 (list "page-type:      m1 page p1"
;;                       "previous page:  $00"
;;                       "slots used:     0"
;;                       "next free slot: $10"))
;;   (check-equal? (memory-list test-dec-ref-bucket-slot-2-state-after #xa000 #xa000)
;;                 (list #x00)
;;                 "VM_GC_ARRAY_SLOT_PTR should NOT have been called (native arrays do not need to free any cells."))

;; (module+ test #| vm_dec_ref_bucket_slot (gc cell array) |#
;;   ;; check that all cells ref counts (if present) in the bucket slot (cell-array) are decremented
;;   (define test-dec-ref-bucket-slot-3-code
;;     (list
;;             (LDA !$00)
;;             (STA $a000) ;; counter for how often VM_GC_ARRAY_SLOT_PTR was called
;;             (JMP TEST_DEC_REF_BUCKET_SLOT_3_CODE)
;;      (label VM_GC_ARRAY_SLOT_PTR)
;;             (INC $a000)
;;             (RTS)

;;      (label TEST_DEC_REF_BUCKET_SLOT_3_CODE)
;;             (LDA !$04)
;;             (JSR VM_ALLOC_CELL_ARRAY_TO_ZP_PTR2)
;;             (JSR VM_COPY_PTR2_TO_PTR) ;; allocation is in zp_ptr2
;;             (JSR VM_REFCOUNT_INCR_ZP_PTR__M1_SLOT) ;; inc ref uses zp_ptr

;;             (JSR VM_REFCOUNT_DECR_ZP_PTR__M1_SLOT) ;; dec ref uses zp_ptr
;;             ))

;;   (define test-dec-ref-bucket-slot-3-state-after
;;     (run-code-in-test test-dec-ref-bucket-slot-3-code
;;                       #:mock (list (label VM_GC_ARRAY_SLOT_PTR))))

;;   (check-equal? (vm-page->strings test-dec-ref-bucket-slot-3-state-after #xcc)
;;                 (list "page-type:      m1 page p0"
;;                       "previous page:  $00"
;;                       "slots used:     0"
;;                       "next free slot: $04"))
;;   (check-equal? (memory-list test-dec-ref-bucket-slot-3-state-after #xa000 #xa000)
;;                 (list #x01)
;;                 "GC_ARRAY_SLOT_RT should have been called exactly once"))

;; TODO: check necessity for this function, adjust to rt/ra usage
;; TODO: reactivate when encountered necessary

;; mark array to be collected and process array entries (from the back) until first to actually gc
;; gc array referenced by ra (after ref count dropped to 0)
(define VM_GC_ARRAY_RA
  (add-label-suffix
   "__" "__VM_GC_ARRAY_RA"
  (list
   (label VM_GC_ARRAY_RA)
          (LDY !$01)
          (LDA (ZP_RA),y)  ;; a = number of array elements
          (ASL A)
          (TAY) ;; y holds index to lowbyte
          (INY) ;; now highbyte (no page wrap possible)

   (label LOOP_OVER_ENTRIES__)
          (LDA (ZP_RA),y)
          (BEQ ENTRY_HIGH_EMPTY__)
          (STA ZP_TEMP+1)
          (DEY)
          (LDA (ZP_RA),y)
          (BEQ ENTRY_LOW_EMPTY__)
          (STA ZP_TEMP)
          (AND !$03)
          (CMP !$03)
          (BNE ENTRY_IS_PTR__)

   (label ENTRY_HIGH_EMPTY__)
          (DEY)
   (label ENTRY_LOW_EMPTY__)
          (DEY)
          (CPY !$01)
          (BNE LOOP_OVER_ENTRIES__)

          ;; all entries were collected
          ;; array is completely collected => slot can be returned to m1 page as free!
          (RTS)

   (label ENTRY_IS_PTR__)
          (STY ZP_TEMP+2) ;; currently on low byte of slot that's freed and can be used to store free page
          (DEY)
          (DEY)
          (TYA)
          (LSR)         ;; remaining size of array
          (BNE KEEP_ARRAY_IN_TO_FREE_LIST__)

          ;; count dropped to 0 => array can be put back as completely free into page
          ;; TODO: put array back to free slot in page
          ;; still the original cell ptr needs to be decremented
          (JSR FREE_M1_SLOT_RA)
          (JMP DECR_ORG_CELL_PTR__)

   (label KEEP_ARRAY_IN_TO_FREE_LIST__)
          (LDY !$01)
          (STA (ZP_RA),y) ;; store array size (entry after size = reference to next free array)
          ;; first call => put old free array of same size into slot
          (LDA ZP_RA+1) ;; page this array is stored in
          (STA LOAD_PAGETYPE__+2)
   (label LOAD_PAGETYPE__)

          ;; write old root of this page type into cell where ptr was located
          (LDA $c000) ;; load byte 0 of the page (c0 is overwritten with actual page
          (AND !$07) ;; mask out profile
          (TAX)
          (LDA VM_P0_QUEUE_ROOT_OF_ARRAYS_TO_FREE,x) ;; low byte of last free
          (LDY ZP_TEMP+2)
          (STA (ZP_RA),y)
          (INX)
          (INY)
          (LDA VM_P0_QUEUE_ROOT_OF_ARRAYS_TO_FREE,x) ;; high byte of last free
          (STA (ZP_RA),y)

          ;; now write this array as root into the free list
          (LDA ZP_RA+1)
          (STA VM_P0_QUEUE_ROOT_OF_ARRAYS_TO_FREE,x) ;; high byte
          (DEX)
          (LDA ZP_RA)
          (STA VM_P0_QUEUE_ROOT_OF_ARRAYS_TO_FREE,x) ;; low byte

   (label DECR_ORG_CELL_PTR__)
          ;; now reuse RA to store pointer originally in cell
          (LDA ZP_TEMP)
          (STA ZP_RA)
          (LDA ZP_TEMP+1)
          (STA ZP_RA+1)
          (JMP DEC_REFCNT_RA)
)))

;; execute garbage collection on a cell array (decr-ref all array elements and collect if 0)
;; input:  ZP_RT = pointer to array (slot)
;; used:   ZP_RA   = dereferenced array element (if array element is a ptr)
;;         ZP_RT   = pointer to last element of array
;; ouput: -
(define GC_ARRAY_SLOT_RT
  (add-label-suffix
   "__" "__GC_ARRAY_SLOT_RT"
  (list
   (label GC_ARRAY_SLOT_RT)
          ;; loop over slots and decrement their slots
          (LDY !$01)
          (LDA (ZP_RT),y)  ;; a = number of array elements
          (STA LOOP_COUNT__) ;;

          (LDY !$00)

   (label LOOP__)
          (INY)
          (INY)
          ;; deref zp_ptr into zp_ptr2?
          (LDA (ZP_RT),y) ;; load tagged low byte
          (BEQ NEXT__)
          (STA ZP_RA)
          (AND !$03)
          (CMP !$03)
          (BEQ NEXT__)
          (INY)
          (LDA (ZP_RT),y) ;; load high byte
          (STA ZP_RA+1)
          (STY LOOP_IDX__)
          (JSR DEC_REFCNT_RA)
          (LDY LOOP_IDX__)
    (label NEXT__)
          (DEC LOOP_COUNT__)
          (BNE LOOP__)

          (RTS)

   (label LOOP_COUNT__)
          (byte $00)
   (label LOOP_IDX__)
          (byte $00)
   )))


;; input: RA
;; usage: A, X, Y, RA, RC
(define VM_GC_ARRAY_SLOTS_RA
  (add-label-suffix
   "__" "__VM_GC_ARRAY_SLOTS_RA"
  (list
   (label VM_GC_ARRAY_SLOTS_RA)
          ;; loop over slots and decrement their slots
          (LDY !$01)
          (LDA (ZP_RA),y)  ;; a = number of array elements
          (STA LOOP_COUNT__) ;;

          (LDA ZP_RA)
          (STA ZP_RC)
          (LDA ZP_RA+1)
          (STA ZP_RC+1)

          (LDY !$00)

   (label LOOP__)
          (INY)
          (INY)
          ;; deref zp_ptr into zp_ptr2?
          (LDA (ZP_RC),y) ;; load tagged low byte
          (BEQ NEXT__)
          (STA ZP_RA)
          (AND !$03)
          (CMP !$03)
          (BEQ NEXT__)
          (INY)
          (LDA (ZP_RC),y) ;; load high byte
          (STA ZP_RA+1)
          (STY LOOP_IDX__)

          ;; TODO: possible recursion! save ZP_RC??
          ;; better! do lazy
          (JSR DEC_REFCNT_RA)
          ;; restore RC
          (LDY LOOP_IDX__)
    (label NEXT__)
          (DEC LOOP_COUNT__)
          (BNE LOOP__)

          (RTS)

   (label LOOP_COUNT__)
          (byte $00)
   (label LOOP_IDX__)
          (byte $00)
   )))


(module+ test #| vm_gc_array_slot_ptr |#
  (define test-gc-array-slot-ptr-code
    (list
     (LDA !$04)
     (JSR ALLOC_CELLARR_TO_RA)                       ;; ZP_RA = pointer to the allocated array (with 4 cells)

     (JSR ALLOC_CELLPAIR_TO_RT)                    ;; ZP_RT = allocated cell-pair
     (JSR INC_REFCNT_CELLPAIR_RT)

     ;; wrote a new cell-pair @2
     (LDA !$02)
     (JSR WRITE_RT_TO_ARR_ATa_RA)    ;; tos (cell-pair) -> @2

     (JSR PUSH_INT_m1_TO_EVLSTK)                    ;; int -1 -> stack
     (LDA !$01)
     (JSR WRITE_RT_TO_ARR_ATa_RA)    ;; tos (int -1) -> @1

     (JSR CP_RA_TO_RT)                            ;; overwrite tos (-1) with ptr to array
     (JSR GC_ARRAY_SLOT_RT)                       ;; run gc on slot elements -> cell-pair should be gc'd
     ))

  (define test-gc-array-slot-ptr-state-after
    (run-code-in-test test-gc-array-slot-ptr-code))

  (check-equal? (vm-stack->strings test-gc-array-slot-ptr-state-after)
                (list "stack holds 2 items"
                      (format "ptr[0] $~a04  (rt)" (format-hex-byte PAGE_AVAIL_0))
                      (format "pair-ptr[0] $~a05" (format-hex-byte PAGE_AVAIL_1))))
  (check-equal? (vm-page->strings test-gc-array-slot-ptr-state-after PAGE_AVAIL_1)
                (list "page-type:      cell-pair page"
                      "previous page:  $00"
                      "slots used:     1"
                      "next free slot: $09"))
  (check-equal? (memory-list test-gc-array-slot-ptr-state-after (+ PAGE_AVAIL_1_W #x01) (+ PAGE_AVAIL_1_W #x01))
                (list #x00)
                "refcount for cell-pair at cb04..cb07 is at cb01 = 0 (was freed)")
  (check-equal? (vm-cell-pair-free-tree->string test-gc-array-slot-ptr-state-after)
                (format "pair $~a05 -> [ empty . empty ]" (format-hex-byte PAGE_AVAIL_1))
                "...and added as free tree root (for reuse)"))

;; allocate an array of bytes (native) (also useful for strings)
;; input:  A = number of bytes (1..)
;; output: ZP_RA -> points to an allocated array (not initialized)
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
  (define test-alloc-native-array-code
    (list
     (LDA !$10)
     (JSR ALLOC_NATARR_TO_RA)))

  (define test-alloc-native-array-state-after
    (run-code-in-test test-alloc-native-array-code))

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

;; allocate an array of cells (also useful for structures)
;; input:  A = number of cells (1..)
;; output: ZP_RA -> points to an allocated array
(define ALLOC_CELLARR_TO_RA
  (add-label-suffix
   "__" "__ALLOC_CELLARR_TO_RA"
  (list
   (label ALLOC_CELLARR_TO_RA)
          ;; optional: optimization for arrays with 3 cells => s8 page!
          (PHA)
          (ASL A)       ;; *2
          (CLC)
          (ADC !$02)    ;; add (tag byte, length) to total slot size

          (JSR ALLOC_M1_SLOT_TO_RA)

          (PLA)
          (TAX) ;; save array len in x
          (ASL A)
          (TAY) ;; use number of array elements * 2 as loop counter          

          ;; initialize slots/array with zeros (actually writes one byte more than needed)
          (LDA 0)
          (INY)
   (label LOOP_INIT__)
          (STA (ZP_RA),y)
          (DEY)
          (BNE LOOP_INIT__)

          ;; y = 0 now
          ;; write header cell
          (LDA !TAG_BYTE_CELL_ARRAY)
          (STA (ZP_RA),y) ;; store tag byte
          (INY)
          (TXA)
          (STA (ZP_RA),y) ;; store number of array elements

          (RTS))))

(module+ test #| vm_allocate_cell_array |#
  (define test-alloc-cell-array-code
    (list
     (LDA !$04)
     (JSR ALLOC_CELLARR_TO_RA)))

  (define test-alloc-cell-array-state-after
    (run-code-in-test test-alloc-cell-array-code))

  (check-equal? (vm-page->strings test-alloc-cell-array-state-after PAGE_AVAIL_0)
                (list
                 "page-type:      m1 page p1"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $16"))
  (check-equal? (memory-list test-alloc-cell-array-state-after ZP_RA (add1 ZP_RA))
                (list #x04 PAGE_AVAIL_0))
  (check-equal? (memory-list test-alloc-cell-array-state-after (+ PAGE_AVAIL_0_W #x04)(+ PAGE_AVAIL_0_W #x0d))
                (list TAG_BYTE_CELL_ARRAY #x04
                      #x00 #x00
                      #x00 #x00
                      #x00 #x00
                      #x00 #x00)
                "array is filled with zeros")
)


;; write the tos into array element a (0 indexed), array pointed to by zp_ptr2
;; input:  a = index (0 indexed)
;;         ZP_RA = pointer to array
;;         ZP_RT = cell to store
;; usage: A, X, Y, RT, RA
;; NO CHECKING (NO BOUNDS, NO TYPE ...)
;; DECREMENT ref of pointer if array element was a pointer (cell-ptr or cell-pair-ptr)
(define WRITE_RT_TO_ARR_ATa_RA
  (add-label-suffix
   "__" "__WRITE_RT_TO_ARR_ATa_RA"
  (list
   (label POP_EVLSTK_TO_ARR_ATa_RA)
          (JSR WRITE_RT_TO_ARR_ATa_RA)
          (JMP POP_CELL_EVLSTK_TO_RT)

   (label POP_EVLSTK_TO_ARR_ATa_RA__CHECK_BOUNDS)
          (JSR WRITE_RT_TO_ARR_ATa_RA__CHECK_BOUNDS)
          (JMP POP_CELL_EVLSTK_TO_RT)

   (label WRITE_RT_TO_ARR_ATa_RA__CHECK_BOUNDS)
          (LDY !$01)
          (CMP (ZP_RA),y)
          (BPL BOUNDS_ERR__)
          (CMP !$00)
          (BPL WRITE_RT_TO_ARR_ATa_RA)
   (label BOUNDS_ERR__)
          (BRK)

   (label WRITE_RT_TO_ARR_ATa_RA)
          (ASL A)
          (CLC)
          (ADC !$02) ;; point to first cell (index 0)
          (STA ARRAY_INDEX__) ;; keep for later


          ;; get previous content into rt and decr ref count (if applicable)
          (TAY)
          (LDA (ZP_RA),y) ;; if low byte (tagged)
          (AND !$03)
          (CMP !$03)
          (BEQ NO_GC__)
          (INY)
          (LDA (ZP_RA),y) ;; if high byte is 0, it is nil, no gc there
          (BEQ NO_GC__)
          (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
          (LDY ARRAY_INDEX__)
          (LDA (ZP_RA),y) ;; if high byte is 0, it is nil, no gc there
          (STA ZP_RT)
          (INY)
          (LDA (ZP_RA),y) ;; previous low byte in that slot (load again)
          (STA ZP_RT+1)
          (JSR DEC_REFCNT_RT) ;; decrement array slot
          (JSR POP_CELL_EVLSTK_TO_RT) ;; restore RT

   (label NO_GC__)
          (LDY ARRAY_INDEX__)
          (LDA ZP_RT)
          (STA (ZP_RA),y) ;;
          (INY)
          (LDA ZP_RT+1)
          (STA (ZP_RA),y)

          (RTS)

   (label ARRAY_INDEX__)
          (byte 0))))

(module+ test #| vm_cell_stack_write_tos_to_array_ata_ptr |#
  (define vm_cell_stack_write_tos_to_array_ata_ptr-code
    (list
     (LDA !$04)
     (JSR ALLOC_CELLARR_TO_RA)

     (LDA !$ff)
     (LDX !$01)
     (JSR PUSH_INT_TO_EVLSTK)

     (LDA !$02)
     (JSR WRITE_RT_TO_ARR_ATa_RA)))

  (define vm_cell_stack_write_tos_to_array_ata_ptr-state-after
    (run-code-in-test vm_cell_stack_write_tos_to_array_ata_ptr-code))

  (check-equal? (vm-page->strings vm_cell_stack_write_tos_to_array_ata_ptr-state-after PAGE_AVAIL_0)
                (list
                 "page-type:      m1 page p1"
                 "previous page:  $00"
                 "slots used:     1"
                 "next free slot: $16"))
  (check-equal? (memory-list vm_cell_stack_write_tos_to_array_ata_ptr-state-after ZP_RA (add1 ZP_RA))
                (list #x04 PAGE_AVAIL_0)
                "points to the cell-array in this m1 page")
  (check-equal? (memory-list vm_cell_stack_write_tos_to_array_ata_ptr-state-after (+ PAGE_AVAIL_0_W #x04) (+ PAGE_AVAIL_0_W #x0d))
                (list TAG_BYTE_CELL_ARRAY #x04
                      #x00 #x00
                      #x00 #x00
                      #x07 #xff
                      #x00 #x00)
                (string-append "slot is a cell-array, with 4 elements"
                               "slot 0 = empty"
                               "slot 1 = empty"
                               "slot 2 = int $1fff"
                               "slot 3 = empty")))

(module+ test #| write to array bounds checks |#
  (define to-array-ata-ra-4-state
    (run-code-in-test
     (list
      (LDA !$04)
      (JSR ALLOC_CELLARR_TO_RA)

      (LDA !$ff)
      (LDX !$01)
      (JSR PUSH_INT_TO_EVLSTK)

      (LDA !$04) ;; out of bounds
      (JSR WRITE_RT_TO_ARR_ATa_RA__CHECK_BOUNDS)

      (JSR PUSH_INT_0_TO_EVLSTK))
    ))

  (check-equal? (vm-stack->strings to-array-ata-ra-4-state)
               (list "stack holds 1 item"
                     "int $01ff  (rt)")
               "never got to pushing 0 since access index 4 is out of bounds")

  (define to-array-ata-ra-ff-state
    (run-code-in-test
     (list
      (LDA !$04)
      (JSR ALLOC_CELLARR_TO_RA)

      (LDA !$ff)
      (LDX !$01)
      (JSR PUSH_INT_TO_EVLSTK)

      (LDA !$ff) ;; out of bounds
      (JSR WRITE_RT_TO_ARR_ATa_RA__CHECK_BOUNDS)

      (JSR PUSH_INT_0_TO_EVLSTK))
    ))

  (check-equal? (vm-stack->strings to-array-ata-ra-ff-state)
               (list "stack holds 1 item"
                     "int $01ff  (rt)")
               "never got to pushing 0 since access index ff is out of bounds")

  (define to-array-ata-ra-0-state
    (run-code-in-test
     (list
      (LDA !$04)
      (JSR ALLOC_CELLARR_TO_RA)

      (LDA !$ff)
      (LDX !$01)
      (JSR PUSH_INT_TO_EVLSTK)

      (LDA !$00) ;; in bounds
      (JSR WRITE_RT_TO_ARR_ATa_RA__CHECK_BOUNDS)

      (JSR PUSH_INT_0_TO_EVLSTK))
    ))

  (check-equal? (vm-stack->strings to-array-ata-ra-0-state)
               (list "stack holds 2 items"
                     "int $0000  (rt)"
                     "int $01ff")
               "got to pushing 0 since access index 0 is in bounds")

  (define to-array-ata-ra-3-state
    (run-code-in-test
     (list
      (LDA !$04)
      (JSR ALLOC_CELLARR_TO_RA)

      (LDA !$ff)
      (LDX !$01)
      (JSR PUSH_INT_TO_EVLSTK)

      (LDA !$03) ;; in bounds
      (JSR WRITE_RT_TO_ARR_ATa_RA__CHECK_BOUNDS)

      (JSR PUSH_INT_0_TO_EVLSTK))
    ))

  (check-equal? (vm-stack->strings to-array-ata-ra-3-state)
               (list "stack holds 2 items"
                     "int $0000  (rt)"
                     "int $01ff")
               "got to pushing 0 since access index 0 is in bounds"))

(define PUSH_ARR_ATa_RA_TO_EVLSTK
  (add-label-suffix
   "__" "PUSH_ARR_ATa_RA_TO_EVLSTK"
  (list
   (label PUSH_ARR_ATa_RA_TO_EVLSTK)
          (PHA)
          (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
          (PLA)
          (CLC)
          (BCC WRITE_ARR_ATa_RA_TO_RT)

   (label CHECK_BOUNDS__)
          (PHA)
          (JSR PUSH_RT_TO_EVLSTK_IF_NONEMPTY)
          (PLA)

   (label CHECK_BOUNDS__)
          (LDY !$01)
          (CMP (ZP_RA),y)
          (BPL BOUNDS_ERR__)
          (CMP !$00)
          (BPL WRITE_ARR_ATa_RA_TO_RT)
   (label BOUNDS_ERR__)
          (BRK)  ;; out of bounds error

   (label WRITE_ARR_ATa_RA_TO_RT)
          (ASL A)
          (CLC)
          (ADC !$03)                    ;; get y to point to high byte of cell at index
          (TAY)
          (LDA (ZP_RA),y)               ;; copy high byte
          (STA ZP_RT+1)
          (DEY)
          (LDA (ZP_RA),y)               ;; copy low byte
          (STA ZP_RT)
          (RTS))))

(module+ test #| vm_cell_stack_push_array_ata_ptr |#
  (define test-cell-stack-push-array-ata-ptr-code
    (list
     (LDA !$04)
     (JSR ALLOC_CELLARR_TO_RA)

     (LDA !$02)
     (JSR PUSH_ARR_ATa_RA_TO_EVLSTK) ;; @2 = empty -> stack => stack is still empty

     (LDA !$ff)
     (LDX !$01)
     (JSR PUSH_INT_TO_EVLSTK)            ;; int $1ff -> stack

     (LDA !$02)
     (JSR WRITE_RT_TO_ARR_ATa_RA) ;; tos (int $1ff) -> @2 (overwriting 0 (empty) in array)

     (LDA !$02)
     (JSR PUSH_ARR_ATa_RA_TO_EVLSTK)  ;; @2 (now int $1ff) -> stack
     ))

  (define test-cell-stack-push-array-ata-ptr-state-after
    (run-code-in-test test-cell-stack-push-array-ata-ptr-code))

  (inform-check-equal? (cpu-state-clock-cycles test-cell-stack-push-array-ata-ptr-state-after)
                1289)
  (check-equal? (vm-stack->strings test-cell-stack-push-array-ata-ptr-state-after)
                (list "stack holds 2 items"
                      "int $01ff  (rt)"
                      "int $01ff")))

;; idea: have a list of code pages (adding new page as head if allocated)
;;       TODO: how does relocation work here?
;; idea: function id = ptr to bytecode
;; idea: relocatable function ids = function id = ptr into reloc table (identified by lowest bit = 1)
;;       reloc table is a page with entries:
;;             function id -> [lowbyte] [highbyte] <- actual location of function
;;             moving a function can be done by rewriting function id entry in that table
;;             calling that function needs to do one more indirection
;;             functions currently in the call stack cannot (easily) be relocated!
;; module descriptor
;;   module name string
;;   child modules (list of references to child modules)
;;   modules code pages head (ptr to the head of the list of code pages of this module)

;; code page
;;   00           : page type
;;   01..02       : pointer to module descriptor
;;   03           : len of bytecode 0..127 (blen), highest bit set means exported function <-- start of first function descriptor
;;   04           : #locals
;;   05           : #params
;;   06..06+blen  : bytecode    <- function id points on first byte of byte code? [must start word aligned!]
;;   07+blen      : len of function name str (slen) [only if exported function]
;;   08+blen..    : function name                   [only if exported function]
;;   09+blen+slen :                        <-- start of next function descriptor
;;

;; could a constants pool be put into these pages, too? (relocation would be difficult, though)

;; load from disk: https://c64os.com/post/c64kernalrom#file_load


;; input:  x,y  id
;; output: x,y  ptr to function bytecode
(define VM_LOCATE_FUNCTION_BY_ID
  (list
   (label VM_LOCATE_FUNCTION_BY_ID)
   (RTS)))

;; input:  zp_ptr ptr to function name string
;; output: x,y    ptr to function bytecode
(define VM_LOCATE_FUNCTION_BY_NAME
  (list
   (label VM_LOCATE_FUNCTION_BY_NAME)
          (BRK))) ;; not implemented yet

;; input:  x/y  ptr to the bytecode descriptor
;;         descriptor:
;;           00..01  ptr to corresponding module descriptor
;;           02     byte code len
;;           03     function name string len
;;           04     #locals (max)
;;           05     #params
;;           06..   function byte code
;;           ...    function name string
;; output: x/y  function id
;; make sure to have a page allocated that can hold this function
;;
(define VM_REGISTER_FUNCTION
  (list
   (label VM_REGISTER_FUNCTION)
          ;; check for space on a code page of the given module (else allocate)
          ;; copy data into the code page
          ;; optional: create function id mapping on function id mapping page
          (RTS))) ;; not implemented yet

(define vm-memory-manager
  (append VM_MEMORY_MANAGEMENT_CONSTANTS
          VM_INITIALIZE_MEMORY_MANAGER

          ;; ---------------------------------------- alloc/free pages
          FREE_PAGE_A                                       ;; free a page (the type specific stuff, of any, must have finished)
          ALLOC_PAGE_TO_X                                   ;; allocate new page (not initialized)

          INIT_CELL_PAGE_X_TO_AX                                  ;; initialize page (in a) for cell usage
          INIT_CELLPAIR_PAGE_X_TO_AX                             ;; initialize page (in x, free slot in a) for ref counted cell-pairs
          INIT_CELLSTACK_PAGE_X                              ;; initialize page with previous references to previous cell stack pages

          INIT_M1Px_PAGE_X_PROFILE_Y_TO_AX                         ;; allocate page and initialize for ref counted m1 slots of a specific profile (and thus size)
          ;; VM_ALLOC_PAGE_FOR_S8_SLOTS                         ;; allocate page and initialize to hold ref counted 8 byte slots <- really, maybe s8 slots can be removed alltogether

          ;; VM_ALLOC_PAGE_FOR_MODULE_CODE                      ;; allocate page and initialize to hold immutable byte code (not ref counted)

          ;; ---------------------------------------- alloc/free cells, pairs, slots
          GET_FRESH_CELL_TO_AX
          ALLOC_CELL_AX_TO_RT
          ;; ALLOC_CELL_PFL_X_TO_RT
          ALLOC_CELL_TO_RT

          GET_FRESH_CELLPAIR_TO_AX
          ALLOC_CELLPAIR_AX_TO_RT               ;; allocate a cell-pair from this page (if page has no free cell-pairs, a new page is allocated and is used to get a free cell-pair!)
          ;; ALLOC_CELLPAIR_ON_PAGE_X_TO_RT

          ALLOC_CELLPAIR_TO_RT                       ;; allocate a cell-pair from the current page (or from a new page if full)
          FREE_CELLPAIR_RT                        ;; free this cell-pair (adding it to the free tree)
          FREE_CELLPAIR_RA                        ;; free this cell-pair (adding it to the free tree)

          ;; FREE_CELL_RT                             ;; free this cell pointed to by RT (adding it to the free list)
          ;; FREE_CELL_RA                             ;; free this cell pointed to by RT (adding it to the free list)

          GC_CELLPAIR_FREE_LIST                     ;; reclaim all cell-pairs in the queue of free cells

          ALLOC_NATARR_TO_RA                        ;; allocate an array of bytes (native) (also useful for strings)
          ALLOC_CELLARR_TO_RA                          ;; allocate an array of cells (also useful for structures)

          ALLOC_M1_SLOT_TO_RA                             ;; allocate a slot of min A size, allocating a new page if necessary
          FREE_M1_SLOT_RA                              ;; free a slot (adding it to the free list)
          ;; VM_REMOVE_FULL_PAGE_FOR_TYPE_X_SLOTS
          VM_REMOVE_FULL_PAGES_FOR_RA_SLOTS                  ;; remove full pages in the free list of pages of the same type as are currently in ZP_RA
          VM_ENQUEUE_PAGE_AS_HEAD_FOR_RA_SLOTS               ;; put this page as head of the page free list for slots of type as in ZP_RA

          ;; VM_ALLOC_MODULE_CODE_SLOT_TO_ZP_PTR                ;; allocate a slot for module code
          ;; VM_FREE_MODULE
          ;; VM_RELOCATE_MODULE_X_TO_                           ;; relocate module identified by page x to ??

          ;; ---------------------------------------- refcount
          DEC_REFCNT_RT                                ;; generic decrement of refcount (dispatches depending on type)
          INC_REFCNT_RT                                ;; generic increment of refcount (dispatches depending on type)

          ;; DEC_REFCNT_CELLPAIR_RT                 ;; decrement refcount, calling vm_free_cell_pair_in_zp_ptr if dropping to 0
          ;; DEC_REFCNT_CELL_RT                      ;; decrement refcount, calling vm_free_cell_in_zp_ptr if dropping to 0

          ;; INC_REFCNT_CELLPAIR_RT                 ;; increment refcount of cell-pair
          INC_REFCNT_M1_SLOT_RA                         ;; increment refcount of m1-slot
          ;; INC_REFCNT_CELL_RT                      ;; increment refcount of the cell, rt is pointing to

          DEC_REFCNT_RA                                ;; generic decrement of refcount (dispatches depending on type)
          ;; DEC_REFCNT_CELLPAIR_RA                 ;; decrement refcount, calling vm_free_cell_pair_in_zp_ptr if dropping to 0
          DEC_REFCNT_M1_SLOT_RA                       ;; decrement refcount, calling vm_free_m1_slot_in_zp_ptr if dropping to 0
          ;; DEC_REFCNT_CELL_RA                      ;; decrement refcount, calling vm_free_cell_in_zp_ptr if dropping to 0
          ;; ---------------------------------------- call frame

          ;; ---------------------------------------- misc

          ;; VM_REMOVE_FULL_PAGES_FOR_PTR2_SLOTS                ;; remove full pages in the free list of pages of the same type as are currently in ZP_PTR2
          ;; VM_ENQUEUE_PAGE_AS_HEAD_FOR_PTR2_SLOTS             ;; put this page as head of the page free list for slots of type as in ZP_PTR2

          GC_ARRAY_SLOT_RT                               ;; execute garbage collection on a cell array (decr-ref all array elements and collect if 0)
          
          FREE_RT                                 ;; free pointer (is cell-ptr, cell-pair-ptr, m1-slot-ptr, slot8-ptr)

          VM_ADD_CELL_PAIR_IN_RT_TO_ON_PAGE_FREE_LIST       ;; add the given cell-pair (in zp_rt) to the free list of cell-pairs on its page

          ;; ---------------------------------------- CELL_STACK / RT / RA
          POP_CELL_EVLSTK_TO_RT                                ;; pop cell-stack into RT (discarding RT)

          PUSH_TO_EVLSTK                               ;; push value into RT, pushing RT onto the call frame cell stack if not empty
          ;; vm_cell_stack_push_rt_if_nonempty
          PUSH_RT_TO_EVLSTK                         ;; push RT onto call frame cell stack

          ;; WRITE_ARR_ATa_RA_TO_RT
          PUSH_ARR_ATa_RA_TO_EVLSTK

          ;; POP_EVLSTK_TO_ARR_ATa_RA
          ;; POP_EVLSTK_TO_ARR_ATa_RA__CHECK_BOUNDS
          ;; WRITE_RT_TO_ARR_ATa_RA__CHECK_BOUNDS
          WRITE_RT_TO_ARR_ATa_RA             ;; write RT into array in RA at index A (GC previous slot entry, if applicable)

          ;; WRITE_INTm1_TO_RA                             ;; write cell-int -1 into RA
          ;; WRITE_INTm1_TO_RT                             
          ;; WRITE_INTm1_TO_Rx                             ;; x=0 -> RT, x=2 -> RA

          ;; WRITE_INT1_TO_RA                              ;; write cell-int +1 into RA
          ;; WRITE_INT1_TO_RT
          ;; WRITE_INT1_TO_Rx                              ;; x=0 -> RT, x=2 -> RA

          ;; WRITE_INT0_TO_RA                              ;; write cell-int 0 into RA
          ;; WRITE_INT0_TO_RT
          ;; WRITE_INT0_TO_Rx                              ;; x=0 -> RT, x=2 -> RA

          ;; WRITE_INT_A_TO_RA                             ;; write cell-int (only byte sized) A into RA
          ;; WRITE_INT_A_TO_RT
          ;; WRITE_INT_A_TO_Rx                             ;; x=0 -> RT, x=2 -> RA

          ;; WRITE_INT_AY_TO_RA                            ;; int in A(lowbyte)/Y(highbyte) into RA
          ;; WRITE_INT_AY_TO_RT
          WRITE_INT_AY_TO_Rx                              ;; int in A(lowbyte)/Y(highbyte), x=0 -> RT, x=2 -> RA

          ;; WRITE_NIL_TO_RA
          ;; WRITE_NIL_TO_RT
          WRITE_NIL_TO_Rx

          ;; WRITE_CELLPAIR_RT_CELL1_TO_RT
          ;; WRITE_CELLPAIR_RT_CELL0_TO_RT
          WRITE_CELLPAIR_RT_CELLy_TO_RT                            ;; write CELLy (y=0 cell0, y=2 cell1) pointed to by RT into RT
          ;; WRITE_CELLPAIR_RA_CELL1_TO_RT
          ;; WRITE_CELLPAIR_RA_CELL0_TO_RT
          WRITE_CELLPAIR_RA_CELLy_TO_RA                            ;; write CELLy (y=0 cell0, y=2 cell1) pointed to by RA into RA

          WRITE_RA_TO_CELLy_CELLPAIR_RT                            ;; write RA cell into CELLy (y=0 cell0, y=2 cell1) pointer to by RT

          ;; WRITE_CELLPAIR_RT_CELL1_TO_RA
          ;; WRITE_CELLPAIR_RT_CELL0_TO_RA
          WRITE_CELLPAIR_RT_CELLy_TO_RA                            ;; write CELLy (y=0 cell0, y=2 cell1) pointed to by RT into RA

          WRITE_RT_TO_CELLy_CELLPAIR_RA                            ;; write RT cell into CELLy (y=0 cell0, y=2 cell1) pointer to by RA

          CP_RT_TO_RA                                     ;; copy RT -> RA
          CP_RA_TO_RT                                     ;; copy RA -> RT
          CP_RA_TO_RC                                     ;; copy RA -> RC
          CP_RT_TO_RC                                     ;; copy RT -> RC

          POP_CELL_EVLSTK_TO_CELLy_RT                           ;; POP the cell-stack top into CELLy (y=0 cell0, y=2 cell1) pointed to by RT, reducing the stack size by 1, keeping rt as tos


          NEW_DEC_REFCNT_RC
          NEW_FREE_CELL_RC ;; includes NEW_FREE_CELL_RT and _RA
          NEW_GC_INCR_ARRAY_SLOT_RC
          NEW_GC_CELL_ARRAYS
          NEW_GC_CELLS
          NEW_GC_ALL
          NEW_DROP_FULL_PAGES_AT_HEAD_OF_M1_PAGE_A
          NEW_PUT_PAGE_AS_HEAD_OF_M1_PAGE_RC
          NEW_ADD_M1_SLOT_RC_TO_PFL
          NEW_FREE_CELLARR_RC
          NEW_FREE_CELLPAIR_RC ;; includes NEW_FREE_CELLPAIR_RT and _RA
          NEW_FREE_M1_SLOT_RC


    (list (label END__MEMORY_MANAGER))
          ;; ---------------------------------------- registers and maps
          (list (org #xcec0))
          VM_INITIAL_MM_REGS
          (list (org #xcf00))
          VM_PAGE_SLOT_DATA))

(module+ test #| vm-memory-manager |#
  (inform-check-equal? (foldl + 0 (map command-len (flatten vm-memory-manager)))
                       1961))

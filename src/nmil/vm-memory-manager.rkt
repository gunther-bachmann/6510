#lang racket/base

;; ----------------------------------------
;; page type: call-frame page
;; => allocation/deallocation is always done on tos
;;    no need for a free list (stack structure is coded into the stack pages)
;;    need for max size left
;; memory layout of call frame page (organized in stack)
;;  00 : unused (page type)
;;  01 : previous page (just high byte), 00 for first stack page
;;  02 : first frame payload byte 0
;;  ... : first frame payload byte size-1
;;  free-1 : size of (prev) frame
;;  free : unused
;; ...ff : unused
;;
;; VM_FREE_SLOT_FOR_PAGE + pageidx: holds free-idx (initially 02) <- points to the first free byte (-1 = size of previous)

;; ----------------------------------------
;; page type: cell-pairs page
;; memory layout of a cell-pairs page
;; offset content
;; 00     unused (page type)
;; 01     ref-count for cell-pair at 04 (cell-pair 0)
;; 02     ref-count for cell-pair at 08 (cell-pair 1)
;; 03     ref-count for cell-pair at 0C (cell-pair 2)
;; 04..07  cell-pair 0
;; 08..0b  cell-pair 1
;; 0c..0f  cell-pair 2
;; 10     ref-count for cell-pair at 40 (cell-pair 3)
;; 11     ref-count for cell-pair at 44 (cell-pair 4)
;; ..3F   ref-count for cell-pair at fc (cell-pair 50)
;; 40     cell-pair 3
;; 44     cell-pair 4
;; ..fc   cell-pair 50
;;
;; VM_FREE_SLOT_FOR_PAGE + pageidx: holds the index within the page of the first free cell-pair on that page (0 = no free cell-pair on this page)
;; the free cell-pair holds in byte 0 of the cell-pair the offset of the next free cell-pair (0 = no other free cell-pair)
;;


(require (only-in racket/format ~a))

(require "../6510.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader))
(require (only-in racket/list flatten take empty? drop))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in racket/port open-output-nowhere))
  (require (only-in "../tools/6510-disassembler.rkt" disassemble-bytes))
  (require (only-in "../tools/6510-debugger.rkt" run-debugger-on)))
(require (only-in "../tools/6510-interpreter.rkt" 6510-load 6510-load-multiple initialize-cpu run-interpreter run-interpreter-on memory-list cpu-state-accumulator peek))

(provide vm-memory-manager vm-stack->strings ast-const-get)

;; delete every n-th element from the given list
(define (ndelete lst n)
  (let recur ([i 1]
              [rest lst])
    (cond [(null? rest) '()]
          [(= i n) (recur 1 (cdr rest))]
          [else (cons (car rest) (recur (+ i 1) (cdr rest)))])))

(module+ test #| ndelete |#
  (check-equal? (ndelete (cdr (cdr '(0 1 2
                                       3 4 5
                                       5 6 7)))
                         3)
                '(2
                  3 5
                  5 7)))

;; produce strings describing the current cell-stack status
(define (vm-stack->strings state)
  (define stack-tos-idx (peek state ZP_CELL_TOS))
  (cond
    [(> stack-tos-idx #xf0) (list "stack is empty")]
    [else
     (define stack (memory-list state ZP_CELL0 (+ 1 stack-tos-idx #xda)))
     (define stack-values (cons (car stack) (ndelete (cdr (cdr stack)) 3)))
     (define stack-item-no (/ (+ 2 stack-tos-idx) 3))
     (cons (format "stack holds ~a ~a" stack-item-no (if (= 1 stack-item-no) "item" "items"))
           (reverse (map (lambda (pair) (vm-cell->string (car pair) (cdr pair))) (pairing stack-values))))]))

;; make a list of adjacent pairs
(define (pairing list (paired-list '()))
  (if (< (length list) 2)
      (reverse paired-list)
      (pairing (drop list 2) (cons `(,(car list) . ,(cadr list)) paired-list))))

(module+ test #| pairing |#
  (check-equal? (pairing '(1 2 3 4 5 6))
                '((1 . 2) (3 . 4) (5 . 6))))

;; format a hexadecimal byte
(define (format-hex-byte byte)
  (~a (number->string byte 16) #:width 2 #:align 'right #:pad-string "0"))

(define (vm-cell->string low high)
  (cond
    [(= 1 (bitwise-and #x01 low)) (format "cell-ptr $~a~a"
                                          (format-hex-byte high)
                                          (format-hex-byte (bitwise-and #xfe low)))]
    [(= 2 (bitwise-and #x02 low)) (format "cell-pair-ptr $~a~a"
                                          (format-hex-byte high)
                                          (format-hex-byte (bitwise-and #xfc low)))]
    [(= 0 (bitwise-and #x83 low)) (format "cell-int $~a~a"
                                          (format-hex-byte (arithmetic-shift low -2))
                                          (format-hex-byte high))]
    [(= #xfc (bitwise-and #xfc low)) (format "cell-byte $~a" (format-hex-byte high))]
    [else "?"]))

(module+ test #| vm-cell->string |#
  (check-equal? (vm-cell->string #xc5 #xc0)
                "cell-ptr $c0c4")
  (check-equal? (vm-cell->string #xc2 #xc0)
                "cell-pair-ptr $c0c0")
  (check-equal? (vm-cell->string #x78 #x15)
                "cell-int $1e15")
  (check-equal? (vm-cell->string #xfc #x15)
                "cell-byte $15"))

(module+ test #| vm-stack->string |#
  (define test-vm_stack_to_string-a-code
    (list (org #xc000)
          (JSR VM_INITIALIZE_MEMORY_MANAGER)
          (JSR VM_CELL_STACK_PUSH_NIL)
          (JSR VM_CELL_STACK_PUSH_NIL)
          (LDA !$03)
          (LDY !$01)
          (JSR VM_CELL_STACK_PUSH_INT)))
  (define test-vm_stack_to_string-a
    (append test-vm_stack_to_string-a-code
            (list (BRK))
            vm-memory-manager))

  (define test-vm_stack_to_string-a-state-before (6510-load-multiple (initialize-cpu) (assemble-to-code-list test-vm_stack_to_string-a)))
  (define test-vm_stack_to_string-a-state-after  (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on test-vm_stack_to_string-a-state-before)))

  ;; (run-debugger-on test-vm_stack_to_string-a-state-before)

  (check-equal? (vm-stack->strings test-vm_stack_to_string-a-state-after)
                '("stack holds 3 items"
                  "cell-int $0301"
                  "cell-pair-ptr $0000"
                  "cell-pair-ptr $0000")))

;; test one roundtrip:

;; use case: allocate, free, reallocate small list of cell-pairs, keeping tail because ptr was copied (ref count incremented)
;; - allocate cell-pair
;; - set cell1 to an int
;; - allocate cell-pair
;; - set cell2 to allocated cell-pair
;; - increment ref count of cell2 (thus it must be kept, when head is decremented)
;; - set cell1 to an int
;; - (set cell2 to nil) => (equivalent to '(1 2))
;; - decrement list head (cell-pair) <- should put the head of the list on the free tree, keeping the pointer to the next cell-pair
;; - allocate cell-pair <- should recycle the top of the free tree, decrement ref count of cell2, since ref count cell2 is not dropping to 0, top of tree is set to 0

;; Method index + description
;; DATA
;;  VM_MEMORY_MANAGEMENT_CONSTANTS              :: constants that are used by the assembler code
;;  VM_ALLOC_PAGE_JUMP_TABLE                    :: jump table  page-type->allocation method
;;  VM_INITIAL_MM_REGS                          :: (initial data for) the memory management registers
;;  VM_FREE_PAGE_BITMAP                         :: bitmap indicating free pages (0) or allocated pages (1)
;;
;; CODE (FULL PAGE)
;;  VM_INITIALIZE_MEMORY_MANAGER                :: initialize memory management (paging, cell stack)
;;  VM_ALLOC_PAGE                               :: INCOMPLETE! allocate page (of any kind)
;;  VM_ALLOC_PAGE__LIST_CELL_PAIRS              :: allocate a complete new page and initialize it to hold reference counted cell-pairs
;;  VM_FREE_PAGE                                :: free the given page (may then be allocated again via VM_ALLOC_PAGE*
;;  VM_ALLOC_PAGE__PAGE_UNINIT                  :: allocate page (without initialization for specific type)
;;  VM_ALLOC_PAGE__CALL_STACK                   :: INCOMPLETE! allocate page for call stack usage
;;
;; CODE
;;  VM_ALLOC_CELL_PAIR_ON_PAGE                  :: allocate a cell-pair on given page, auto allocate new page if full
;;  VM_REFCOUNT_DECR                            :: INCOMPLETE: dispatch to type specific decrement of ref count
;;  VM_REFCOUNT_DECR_CELL_PAIR                  :: decrement ref count for cell-pair, mark as free if ref count drops to 0 (calls VM_FREE_CELL_PAIR)
;;  VM_REFCOUNT_INCR_CELL_PAIR                  :: increments ref count for cell-pair
;;  VM_FREE_NON_ATOMIC                          :: INCOMPLETE: free a non atomic cell (e.g. cell-ptr, cell-pair, float, array/struct)
;;  VM_ALLOC_CELL_PAIR                          :: allocate a cell-pair (reuse marked free, allocate new if no reuse possible)
;;  VM_FREE_CELL_PAIR                           :: mark cell-pair as free, tail call free on cell1 (which is used for free tree)
;;  VM_ADD_CELL_PAIR_TO_ON_PAGE_FREE_LIST       :: add the given cell-pair to its free list on its page (cell1 and cell2 must not point to anything), refcount is set to 0

;; STACK Functions
;;   VM_CELL_STACK_PUSH_NIL                     :: NIL->Stack++
;;
;;   VM_CELL_STACK_PUSH_INT                     :: INT->Stack++
;;   VM_CELL_STACK_PUSH_INT_0                   :: INT->Stack++
;;   VM_CELL_STACK_PUSH_INT_1                   :: INT->Stack++
;;   VM_CELL_STACK_PUSH_INT_2                   :: INT->Stack++
;;   VM_CELL_STACK_PUSH_INT_m1                  :: INT->Stack++
;;
;; STACK ZP_PTR Functions
;;   VM_CELL_STACK_PUSH_CELLy_OF_ZP_PTR         :: ZP_PTR (CELLy) -> Stack++
;;   VM_CELL_STACK_PUSH_CELL0_OF_ZP_PTR         :: ZP_PTR (CELL0) -> Stack++
;;   VM_CELL_STACK_PUSH_CELL1_OF_ZP_PTR         :: ZP_PTR (CELL1) -> Stack++
;;
;;   VM_CELL_STACK_PUSH_ZP_PTRy                 :: ZP_PTRy -> Stack++
;;   VM_CELL_STACK_PUSH_ZP_PTR                  :: ZP_PTR  -> Stack++
;;   VM_CELL_STACK_PUSH_ZP_PTR2                 :: ZP_PTR2 -> Stack++

;;   VM_CELL_STACK_WRITE_TOS_TO_CELLy_OF_ZP_PTR :: Stack -> ZP_PTR (CELLy)
;;   VM_CELL_STACK_WRITE_TOS_TO_CELL0_OF_ZP_PTR :: Stack -> ZP_PTR (CELL0)
;;   VM_CELL_STACK_WRITE_TOS_TO_CELL1_OF_ZP_PTR :: Stack -> ZP_PTR (CELL1)
;;
;;   VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS :: ZP_PTR (CELLy) -> Stack
;;   VM_CELL_STACK_WRITE_CELL0_OF_ZP_PTR_TO_TOS :: ZP_PTR (CELL0) -> Stack
;;   VM_CELL_STACK_WRITE_CELL1_OF_ZP_PTR_TO_TOS :: ZP_PTR (CELL1) -> Stack
;;
;;   VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRy         :: Stack -> ZP_PTRy
;;   VM_CELL_STACK_WRITE_TOS_TO_ZP_PTR          :: Stack -> ZP_PTR
;;   VM_CELL_STACK_WRITE_TOS_TO_ZP_PTR2         :: Stack -> ZP_PTR2
;;
;;   VM_CELL_STACK_WRITE_ZP_PTRy_TO_TOS         :: ZP_PTRy -> Stack
;;   VM_CELL_STACK_WRITE_ZP_PTR_TO_TOS          :: ZP_PTR  -> Stack
;;   VM_CELL_STACK_WRITE_ZP_PTR2_TO_TOS         :: ZP_PTR2 -> Stack
;;
;;   VM_CELL_STACK_POP                          :: Stack -> Stack--
;;
;; ZP_PTR Functions
;;   VM_COPY_PTR2_TO_PTR                        :: ZP_PTR2->ZP_PTR
;;   VM_COPY_PTR_TO_PTR2                        :: ZP_PTR->ZP_PTR2

;; constants that are used by the assembler code
(define VM_MEMORY_MANAGEMENT_CONSTANTS
  (list
   ;; page type list-pair-cells
   (byte-const PT_LIST_PAIR_CELLS        $00)   ;; page type: list pair cells
   (byte-const PT_CALL_STACK             $01)   ;; page type: call stack
   (byte-const PT_CODE                   $02)   ;; page type: code

   (word-const VM_FREE_PAGE_BITMAP       $ced0) ;; location: free page bitmap (ced0..ceff)

   (byte-const NEXT_FREE_PAGE_PAGE       $cf)   ;; cf00..cfff is a byte array, mapping each page idx to the next free page idx, 00 = no next free page for the given page
   (word-const VM_FREE_SLOT_FOR_PAGE     $cf00) ;; location: table of first free slot for each page

   (word-const TAGGED_INT_0              $0000)
   (word-const TAGGED_BYTE0              $00fc)
   (word-const TAGGED_NIL                $0002) ;; tag indicates cell-pair-ptr

   ;; zp_ptr holds either a cell-ptr or a cell-pair (ptr) with out the tag bits!
   ;; these two must be adjacent (for some code to work!)
   (byte-const ZP_PTR                    $fb)   ;; fb = low byte (with out tag bits), fc = high byte
   (byte-const ZP_PTR2                   $fd)   ;; fd = low byte (with out tag bits), fe = high byte

   ;; these two must be adjacent (for some code to work)
   (byte-const ZP_PTR_TAGGED             $9e)   ;; 9e = low byte with tag bits
   (byte-const ZP_PTR2_TAGGED            $9f)   ;; 9f = low byte with tag bits

   ;; zero page cell stack: tos always points to the untagged low byte!
   (byte-const ZP_CELL_TOS               $d9)  ;; current offset to tos $fe = stack empty, 1 = cell0, 4 = cell1, 7 = cell2 ...
   (byte-const ZP_CELL0                  $da)  ;; da = low byte with tag
   (byte-const ZP_CELL0_LOW              $db)  ;; db = low byte without tag, dc = high byte
   (byte-const ZP_CELL1                  $dd)  ;; dd = low byte with tag
   (byte-const ZP_CELL1_LOW              $de)  ;; de = low byte without tag, df = high byte
   ;; ... cell2 = e0..e2, 3 = e3..e5, 4 = e6..e8, 5= e9..eb, 6 = ec..ee, 7 = ef..f1

   (byte-const ZP_CELL7                  $ef)
   (byte-const ZP_CELL7_LOW              $f0)))

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

(define ZP_PTR           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PTR"))
(define ZP_PTR_TAGGED    (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PTR_TAGGED"))
(define ZP_PTR2          (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PTR2"))
(define ZP_PTR2_TAGGED   (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PTR2_TAGGED"))
(define ZP_CELL_TOS      (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL_TOS"))
(define ZP_CELL0         (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL0"))

;; naming convention
;; cell-stack         == stack of cells (could be any atomic or ptr cell)
;; zp_ptr             == cell ptr (either cell-ptr or cell-pair-ptr)
;; cell-ptr           == pointer to any type of cell (except to cell-pairs)
;; cell-pair-ptr      == pointer to cell-pair (only)
;; untagged low byte  == low byte without ptr tags
;; tagged low byte    == low byte with tags (ptr or other)
;; page bitmap        == set of bits each indicating whether a page is free/used (total #x20 bytes long)
;; ref count          == byte counting how many pointers to this value exist, there can be pointer to pointers


;; rules
;;   zp_ptr is regarded as volatile!
;;   pointers pushed on the stack increase the ref count
;;   pointers poped from the stack decrease the ref count
;;   copying to zp_ptr does NOT change the ref count
;;   writing a cell-ptr into a cell increases ref count (regardless where this cell-ptr comes from)
;;   erasing cell-ptr from a cell decreases ref count

;; pop cell from stack, decrease ref count if it is a reference
;; input: stack
;; output: stack--
;;         A  ?
;;         X  TOS (on untagged low byte)
;;         Y  unchanged if cell is no ptr
;;            ? if cell is a ptr
;;    zp_ptr  is overwritte if cell is a ptr
;; check stack empty!
(define VM_CELL_STACK_POP
  (list
   (label VM_CELL_STACK_POP)
          (LDX ZP_CELL_TOS)
          (BMI STACK_EMPTY_ERROR__VM_CELL_STACK_POP)

          ;; now check whether deallocation needs to take place (that is the cell being popped is a ptr)
          (DEX)
          (LDA ZP_CELL0,x) ;; tagged low byte
          (AND !$03)
          (BEQ DO_POP__VM_CELL_STACK_POP) ;; is no pointer => jump

          ;; is a pointer?
          (LSR)
          (BCC IS_CELL_PAIR_PTR__VM_CELL_STACK_POP)

          ;; is a cell-ptr
          ;; not implemented yet: should decrement cell-ptr and check whether it can be garbage collected!

   (label STACK_EMPTY_ERROR__VM_CELL_STACK_POP)
          (BRK) ;; not implemented yet

   (label IS_CELL_PAIR_PTR__VM_CELL_STACK_POP)
          ;; move cell-pair-ptr (tos) -> zp_ptr
          (LDA ZP_CELL0_LOW+1,x)  ;; high byte
          (BEQ DO_POP__VM_CELL_STACK_POP) ;; hight byte = 0 => is nil => no need to decr cell pair
          (STA ZP_PTR+1)      ;; to zp_ptr+1
          (LDA ZP_CELL0_LOW,x)    ;; low byte untagged
          (STA ZP_PTR)        ;; to zp_ptr
          ;; no need for copying tagged low byte, since I already know it is a cell-pair-ptr
          (JSR VM_REFCOUNT_DECR_CELL_PAIR) ;; decrement and gc if necessary

   (label VM_CELL_STACK_POP__NO_GC) ;; entry for just popping!
          (LDX ZP_CELL_TOS)   ;; restore tos
          (DEX)               ;; dex (such that in total x-=3)

   (label DO_POP__VM_CELL_STACK_POP)
          (DEX) ;; x was already decremented => dec 2x
          (DEX)
          (STX ZP_CELL_TOS) ;; store new tos
          (RTS)))

(module+ test #| vm_cell_stack_pop |#

  (define (wrap-bytecode-for-test bc)
    (append (list (org #xc000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER))
            bc
            (list (BRK))
            vm-memory-manager))

  (define (run-code-in-test bc (debug #f))
    (define wrapped-code (wrap-bytecode-for-test bc))
    (define state-before
      (6510-load-multiple (initialize-cpu)
                          (assemble-to-code-list wrapped-code)))
    (if debug
        (run-debugger-on state-before)
        (parameterize ([current-output-port (open-output-nowhere)])
          (run-interpreter-on state-before))))

  (define test-vm_cell_stack_pop-a-code (list (org #xc000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_CELL_STACK_PUSH_NIL)
                  (JSR VM_CELL_STACK_POP)
                  (JSR VM_CELL_STACK_POP) ;; stops at brk in this routine
                  (LDA !$00)              ;; is never run
                  (STA ZP_CELL_TOS)))
  (define test-vm_cell_stack_pop-a
    (append test-vm_cell_stack_pop-a-code
            (list (BRK))
            vm-memory-manager))

  (define test-vm_cell_stack_pop-a-state-before (6510-load-multiple (initialize-cpu) (assemble-to-code-list test-vm_cell_stack_pop-a)))
  ;; (define test-vm_cell_stack_pop-a-state-after  (parameterize ([current-output-port (open-output-nowhere)]) (run-interpreter-on test-vm_cell_stack_pop-a-state-before)))

  (define test-vm_cell_stack_pop-a-state-after
    (run-code-in-test
     (list (JSR VM_CELL_STACK_PUSH_NIL)
           (JSR VM_CELL_STACK_POP)
           (JSR VM_CELL_STACK_POP) ;; stops at brk in this routine
           (LDA !$00)              ;; is never run
           (STA ZP_CELL_TOS))))
  ;; (run-debugger-on test-vm_cell_stack_pop-a-state-before)

  (check-equal? (memory-list test-vm_cell_stack_pop-a-state-after ZP_CELL_TOS (+ ZP_CELL_TOS 3))
                '(#xfe
                  #x02 #x00 #x00)
                "tos = fe (empty), nil is (still) in memory but off from stack, second pop runs into brk, no clearing of zp_cell_tos!"))

;; push a cell in A/Y (low/high) onto the cell-stack
;; input: stack
;;        A = low, encoded cell (tagged)
;;        Y = high, cell
;; output: stack++[nil]
;; registers: A  ?
;;            X  TOS (on untagged low byte)
;;            Y  unchanged
;; check stack full!
(define VM_CELL_STACK_PUSH
  (list
   (label VM_CELL_STACK_PUSH_CELL1_OF_ZP_PTR)
          (LDY !$02)
          (BNE VM_CELL_STACK_PUSH_CELLy_OF_ZP_PTR)

   (label VM_CELL_STACK_PUSH_CELL0_OF_ZP_PTR)
          (LDY !$00)

   ;; push celly of cell-pair pointer to by zp_ptr (y=0 car-cell, y=2 cdr-cell) onto cell-stack
   ;; input:  cell-stack
   ;;         y register (0 = car-cell, 2 = cdr-cell)
   ;;         zp_ptr pointing to the cells to read from (psuh to cell-stack)
   ;; output: cell-stack with celly pushed (=> ZP_CELL_TOS+=3)
   ;;         zp_ptr and pointed to cells unchanged
   ;;         a = hight byte of celly
   ;;         x = TOS+1
   ;;         y = y+1
   ;;         flags: result of lda high byte of celly
   ;; check stack full!
   (label VM_CELL_STACK_PUSH_CELLy_OF_ZP_PTR)
          (LDA (ZP_PTR),y)
          (TAX)
          (INY)
          (LDA (ZP_PTR),y)
          (TAY)
          (TXA)
          (CLC)
          (BCC VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_ZP_PTR2)
          (LDY !$01)
          (BNE VM_CELL_STACK_PUSH_ZP_PTRy)
   (label VM_CELL_STACK_PUSH_ZP_PTR)
          (LDY !$00)

   ;; push zp_ptry (y=0 zp_ptr, y=1 zp_ptr1) onto cell-stack
   ;; input:  stack
   ;;         zp_ptry
   ;; output: stack++[zp_ptry] (uses zp_ptr_tagged, too)
   ;;         A  high byte of zp_ptr
   ;;         X  TOS
   ;;         Y  0|2 (orig Y * 2)
   ;; check stack full!
   (label VM_CELL_STACK_PUSH_ZP_PTRy)
          (LDA ZP_PTR_TAGGED,y)
          (TAX)
          ;; Y = Y*2
          (TYA)
          (ASL A)
          (TAY)
          (LDA ZP_PTR+1,y)
          (TAY)
          (TXA)
          (CLC)
          (BCC VM_CELL_STACK_PUSH)

   ;; ints are saved high byte first, then low byte !!!!
   ;; input:  stack
   ;;         A = high byte of int (max 31 = $1f)
   ;;         Y = low byte of int (0..255)
   ;; output: stack++[INT]
   ;;         A  = transformed / tagged low byte of int
   ;;         Y  = low byte of int
   ;;         X  = tos
   ;; check stack full!
   (label VM_CELL_STACK_PUSH_INT)
          (ASL A)
          (ASL A)
          (AND !$7c)           ;; mask out top and two low bits!
          (BPL VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_INT_m1)
          (LDA !$7c) ;; 1f << 2
          (LDY !$ff)
          (BNE VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_INT_2)
          (LDY !$02)
          (LDA !$00)
          (BEQ VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_INT_1)
          (LDY !$01)
          (LDA !$00)
          (BEQ VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_INT_0)
          (LDA !$00)
          (TAY)
          (BEQ VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_NIL)
          (LDA !<TAGGED_NIL)
          (LDY !>TAGGED_NIL)

   ;; ----------------------------------------
   (label VM_CELL_STACK_PUSH)
          (LDX ZP_CELL_TOS)
          (INX)
          (INX)
          ;; inx just two times, to point right past cell to the tagged low byte (is later ++ before saved as tos)

          ;; check that stack pointer does not run out of bound
          (CPX !ZP_CELL7) ;; stack runs from  cell0 .. cell7
          (BCS NO_ERROR__VM_CELL_STACK_PUSH)

          (BRK)

   (label NO_ERROR__VM_CELL_STACK_PUSH)
          (STA ZP_CELL0,x)       ;; write lowbyte
          (AND !$fc)             ;; mask out ptr tag bits
          (STA ZP_CELL0_LOW,x)   ;; write untagged lowbyte
          (STY ZP_CELL0_LOW+1,x) ;; write high byte
          (INX)
          (STX ZP_CELL_TOS)      ;; set new tos
          (RTS)))

(module+ test #| vm_cell_stack_push_nil |#
  (define test-vm_cell_stack_push_nil-a-state-after
    (run-code-in-test
     (list (JSR VM_CELL_STACK_PUSH_NIL))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_nil-a-state-after)
                '("stack holds 1 item"
                  "cell-pair-ptr $0000"))

  (define test-vm_cell_stack_push_nil-b-state-after
    (run-code-in-test
     (list (JSR VM_CELL_STACK_PUSH_NIL) ;; 1
           (JSR VM_CELL_STACK_PUSH_NIL) ;;
           (JSR VM_CELL_STACK_PUSH_NIL) ;; 3
           (JSR VM_CELL_STACK_PUSH_NIL) ;;
           (JSR VM_CELL_STACK_PUSH_NIL) ;; 5
           (JSR VM_CELL_STACK_PUSH_NIL) ;;
           (JSR VM_CELL_STACK_PUSH_NIL) ;; 7
           (JSR VM_CELL_STACK_PUSH_NIL)))) ;; 8

(check-equal? (vm-stack->strings test-vm_cell_stack_push_nil-b-state-after)
                '("stack holds 8 items"
                  "cell-pair-ptr $0000"
                  "cell-pair-ptr $0000"
                  "cell-pair-ptr $0000"
                  "cell-pair-ptr $0000"
                  "cell-pair-ptr $0000"
                  "cell-pair-ptr $0000"
                  "cell-pair-ptr $0000"
                  "cell-pair-ptr $0000")))

(define VM_CELL_STACK_WRITE_INT_TO_TOS
  (list
   (label VM_CELL_STACK_WRITE_INT_1_TO_TOS)
          (LDY !$01)
          (LDA !$00)
          (BEQ VM_CELL_STACK_WRITE_INT_TO_TOS)

   (label VM_CELL_STACK_WRITE_INT_0_TO_TOS)
          (LDA !$00)
          (TAY)

   ;; ----------------------------------------
   (label VM_CELL_STACK_WRITE_INT_TO_TOS)
          (LDX ZP_CELL_TOS)
          (ASL A)
          (ASL A)
          (AND !$7c)             ;; mask out top and two low bits!
          (STA ZP_CELL0-1,x)     ;; write int high byte first
          (STA ZP_CELL0_LOW-1,x) ;; write untagged int high byte (the same)
          (STY ZP_CELL0_LOW,x)  ;; write int low byte
          (RTS)))

(module+ test #| vm_cell_push_int |#
    (define test-vm_cell_stack_push_int-a-state-after
    (run-code-in-test
     (list (JSR VM_CELL_STACK_PUSH_INT_m1)
           (LDA !$10) ;; -4096
           (LDY !$00)
           (JSR VM_CELL_STACK_PUSH_INT)
           (JSR VM_CELL_STACK_PUSH_INT_1)
           (JSR VM_CELL_STACK_PUSH_INT_0)
           (LDA !$0f) ;; 4095
           (LDY !$ff)
           (JSR VM_CELL_STACK_PUSH_INT))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_int-a-state-after)
                '("stack holds 5 items"
                  "cell-int $0fff"
                  "cell-int $0000"
                  "cell-int $0001"
                  "cell-int $1000"
                  "cell-int $1fff")))

(module+ test #| vm_cell_stack_push_celly_of_zp_ptr |#
  (define test-vm_cell_stack_push_celly_to_zp_ptr-a-state-after
    (run-code-in-test
     (list
      (LDA !$02)
      (LDY !$12)
      (JSR VM_CELL_STACK_PUSH_INT)                      ;;  push cell-int $0212

      (JSR VM_ALLOC_CELL_PAIR)
      (JSR VM_REFCOUNT_INCR_CELL_PAIR)
      (LDY !$00)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELLy_OF_ZP_PTR)  ;; write cell-int $0212 -> car of allocated cell-pair

      (JSR VM_CELL_STACK_POP)
      (JSR VM_CELL_STACK_PUSH_NIL)                      ;; push cell-pair-ptr nil

      (LDY !$02)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELLy_OF_ZP_PTR)  ;; write cell-pair-ptr nil -> cdr of allocated cell-pair

      (JSR VM_CELL_STACK_POP)                           ;; empty stack

      (LDY !$00)
      (JSR VM_CELL_STACK_PUSH_CELLy_OF_ZP_PTR)          ;; push car (should be cell-int $0212)
      (LDY !$02)
      (JSR VM_CELL_STACK_PUSH_CELLy_OF_ZP_PTR))))       ;; push cdr (should be cell-pair-ptr nil)

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_celly_to_zp_ptr-a-state-after)
                '("stack holds 2 items"
                  "cell-pair-ptr $0000"
                  "cell-int $0212"))
  (check-equal? (memory-list test-vm_cell_stack_push_celly_to_zp_ptr-a-state-after ZP_PTR (+ 1 ZP_PTR))
                '(#x04 #xcd)
                "zp_ptr => cd04 (first free pair cell)")
  (check-equal? (memory-list test-vm_cell_stack_push_celly_to_zp_ptr-a-state-after #xcd04 #xcd07)
                '(#x08 #x12 #x02 #x00)
                "zp_ptr => (int530 . nil)"))

;; write TOS into celly of cell-pair pointer to by zp_ptr (y=0 car-cell, y=2 cdr-cell)
;; input:  cell-stack
;;         y register (0 = car-cell, 2 = cdr-cell)
;;         zp_ptr pointing to the cells to write
;; output: cell-stack unchanged
;;         zp_ptr celly is set to tos cell
;;         a = ?
;;         x = TOS-1 => to pop execute (DEX) (DEX) (STX ZP_CELL_TOS)
;;         y = y+1
;;         flags: result of lda high byte of tos-cell
;; no stack empty check!
(define VM_CELL_STACK_WRITE_TOS_TO_CELLy_OF_ZP_PTR
  (list
   (label VM_CELL_STACK_WRITE_TOS_TO_CELL1_OF_ZP_PTR)
          (LDY !$02)
          (BNE VM_CELL_STACK_WRITE_TOS_TO_CELLy_OF_ZP_PTR)
   (label VM_CELL_STACK_WRITE_TOS_TO_CELL0_OF_ZP_PTR)
          (LDY !$00)

   ;;------------------------------------------------
   (label VM_CELL_STACK_WRITE_TOS_TO_CELLy_OF_ZP_PTR)
          ;; move just tagged low byte and high byte, since CELLy has no untagged byte!!
          (LDX ZP_CELL_TOS)
          (DEX) ;; move to tagged low byte
          (LDA ZP_CELL0,x)
          (STA (ZP_PTR),y) ;; cell low byte
          (INY)
          (LDA ZP_CELL0_LOW+1,x)
          (STA (ZP_PTR),y) ;; cell high byte
          (RTS)))

(module+ test #| vm_cell_stack_write_tos_to_celly_of_zp_ptr |#
  (define test-vm_cell_stack_write_tos_to_celly_of_zp_ptr-a-state-after
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CELL_PAIR)
      (JSR VM_REFCOUNT_INCR_CELL_PAIR)
      (LDA !$02)
      (LDY !$12)
      (JSR VM_CELL_STACK_PUSH_INT)
      (LDY !$00)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELLy_OF_ZP_PTR)
      (JSR VM_CELL_STACK_POP)
      (JSR VM_CELL_STACK_PUSH_NIL)
      (LDY !$02)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELLy_OF_ZP_PTR)
      (JSR VM_CELL_STACK_POP))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_write_tos_to_celly_of_zp_ptr-a-state-after)
                '("stack is empty"))
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_celly_of_zp_ptr-a-state-after ZP_PTR (+ 1 ZP_PTR))
                '(#x04 #xcd)
                "zp_ptr => cd04 (first free pair cell)")
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_celly_of_zp_ptr-a-state-after #xcd04 #xcd07)
                '(#x08 #x12 #x02 #x00)
                "zp_ptr => (int530 . nil)"))

;; write zp_ptry (y=0 zp_ptr, y=1 zp_ptr1) into top of cell-stack
;; input:  stack
;;         zp_ptry
;; output: stack[zp_ptry] (uses zp_ptr_tagged, too)
;;         A  high byte of zp_ptr
;;         X  TOS
;;         Y  0|2 (orig Y * 2)
;; NO CHECKS WHATSO EVER, NO GC NO REFCOUNT STUFF
(define VM_CELL_STACK_WRITE_ZP_PTRy_TO_TOS
  (list
   (label VM_CELL_STACK_WRITE_ZP_PTR2_TO_TOS)
          (LDY !$01)
          (BNE VM_CELL_STACK_WRITE_ZP_PTRy_TO_TOS)
   (label VM_CELL_STACK_WRITE_ZP_PTR_TO_TOS)
          (LDY !$00)

   ;; ----------------------------------------
   (label VM_CELL_STACK_WRITE_ZP_PTRy_TO_TOS)
          (LDX ZP_CELL_TOS)
          (LDA ZP_PTR_TAGGED,y)
          (STA ZP_CELL0-1,x)    ;; tagged low byte

          ;; Y = Y*2
          (TYA)
          (ASL A)
          (TAY)

          (LDA ZP_PTR,y)
          (STA ZP_CELL0_LOW-1,x)  ;; untagged low byte
          (LDA ZP_PTR+1,y)
          (STA ZP_CELL0_LOW,x)
          (RTS))  ;; high byte of ptr
)

(define VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS
  (list
   (label VM_CELL_STACK_WRITE_CELL1_OF_ZP_PTR_TO_TOS)
          (LDY !$02)
          (BNE VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS)
   (label VM_CELL_STACK_WRITE_CELL0_OF_ZP_PTR_TO_TOS)
          (LDY !$00)

   ;; ----------------------------------------
   (label VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS)
          ;; write tagged, untagged low byte and high byte
          (LDX ZP_CELL_TOS)
          (DEX) ;; move to tagged low byte
          (LDA (ZP_PTR),y) ;; cell low byte
          (STA ZP_CELL0,x) ;; tagged
          (AND !$7c)
          (STA ZP_CELL0_LOW,x) ;; untagged
          (INY)
          (LDA (ZP_PTR),y) ;; cell high byte
          (STA ZP_CELL0_LOW+1,x)
          (RTS)))

(module+ test #| vm_cell_stack_push_zp_ptry |#
  (define test-vm_cell_stack_push_zp_ptry-a-state-after
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CELL_PAIR)
      (JSR VM_REFCOUNT_INCR_CELL_PAIR)
      (LDY !$00)
      (JSR VM_CELL_STACK_PUSH_ZP_PTRy))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_zp_ptry-a-state-after)
                '("stack holds 1 item"
                  "cell-pair-ptr $cd04"))
  (check-equal? (memory-list test-vm_cell_stack_push_zp_ptry-a-state-after ZP_PTR (+ ZP_PTR 1))
                '(#x04 #xcd)
                "zp_ptr => cd04 (first free pair cell)")
  (check-equal? (memory-list test-vm_cell_stack_push_zp_ptry-a-state-after ZP_PTR_TAGGED ZP_PTR_TAGGED)
                '(#x06)
                "tagged lowbyte is 04 | 02 = 06")

  (define test-vm_cell_stack_push_zp_ptry-b-state-after
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CELL_PAIR)
      (JSR VM_REFCOUNT_INCR_CELL_PAIR)
      (JSR VM_COPY_PTR_TO_PTR2)
      (LDA !$00)
      (STA ZP_PTR)
      (STA ZP_PTR+1)
      (LDY !$01)
      (JSR VM_CELL_STACK_PUSH_ZP_PTRy))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_zp_ptry-b-state-after)
                '("stack holds 1 item"
                  "cell-pair-ptr $cd04"))
  (check-equal? (memory-list test-vm_cell_stack_push_zp_ptry-b-state-after ZP_PTR2 (+ 1 ZP_PTR2))
                '(#x04 #xcd)
                "zp_ptr2 => cd04 (first free pair cell)")
  (check-equal? (memory-list test-vm_cell_stack_push_zp_ptry-b-state-after ZP_PTR2_TAGGED ZP_PTR2_TAGGED)
                '(#x06)
                "tagged lowbyte is 04 | 02 = 06"))

;; write tos of cell-stack to zp_ptry (y=0 zp_ptr, y=1 zp_ptr2), (makes only sense if it is a cell-ptr or cell-pair-ptr
;; input:  stack
;; output: stack unchanged
;;         ZP_PTRy
;;         A  high byte of cell-ptr
;;         X  TOS-1
;;         Y  0|2 (orig *2)
(define VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRy
  (list
   (label VM_CELL_STACK_WRITE_TOS_TO_ZP_PTR2)
          (LDY !$01)
          (BNE VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRy)
   (label VM_CELL_STACK_WRITE_TOS_TO_ZP_PTR)
          (LDY !$00)

   ;;------------------------------------------------
   (label VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRy)
          (LDX ZP_CELL_TOS)
          (DEX)
          (LDA ZP_CELL0,x)    ;; tagged low byte
          (STA ZP_PTR_TAGGED,y)
          (TYA)
          (ASL A)
          (TAY)
          (LDA ZP_CELL0+1,x)  ;; untagged low byte
          (STA ZP_PTR,y)
          (LDA ZP_CELL0+2,x)  ;; high byte of ptr
          (STA ZP_PTR+1,y)
          (RTS)))

(module+ test #| vm_cell_stack_write_tos_to_zp_ptry |#
  (define test-vm_cell_stack_write_tos_to_zp_ptry-a-state-after
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CELL_PAIR)
      (JSR VM_REFCOUNT_INCR_CELL_PAIR)
      (LDY !$00)
      (JSR VM_CELL_STACK_PUSH_ZP_PTRy)

      (LDA !$00)
      (STA ZP_PTR)
      (STA ZP_PTR+1)
      (STA ZP_PTR2)
      (STA ZP_PTR2+1)

      (LDY !$00)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRy))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_write_tos_to_zp_ptry-a-state-after)
                '("stack holds 1 item"
                  "cell-pair-ptr $cd04"))
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_zp_ptry-a-state-after ZP_PTR (+ 1 ZP_PTR))
                '(#x04 #xcd)
                "zp_ptr => cd04 (first free pair cell)")
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_zp_ptry-a-state-after ZP_PTR_TAGGED ZP_PTR_TAGGED)
                '(#x06)
                "tagged lowbyte is 04 | 02 = 06")

  (define test-vm_cell_stack_write_tos_to_zp_ptry-b-state-after
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CELL_PAIR)
      (JSR VM_REFCOUNT_INCR_CELL_PAIR)
      (LDY !$00)
      (JSR VM_CELL_STACK_PUSH_ZP_PTRy)

      (LDA !$00)
      (STA ZP_PTR)
      (STA ZP_PTR+1)
      (STA ZP_PTR2)
      (STA ZP_PTR2+1)

      (LDY !$01)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRy))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_write_tos_to_zp_ptry-b-state-after)
                '("stack holds 1 item"
                  "cell-pair-ptr $cd04"))
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_zp_ptry-b-state-after ZP_PTR2 (+ 1 ZP_PTR2))
                '(#x04 #xcd)
                "zp_ptr2 => cd04 (first free pair cell)")
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_zp_ptry-b-state-after ZP_PTR2_TAGGED ZP_PTR2_TAGGED)
                '(#x06)
                "tagged lowbyte is 04 | 02 = 06"))

;; copy zp_ptr2 to zp_ptr (including tag byte)
;; input:  ZP_PTR2
;; output: ZP_PTR
;;         A  ?
;;         X  unchanged
;;         Y  unchanged
(define VM_COPY_PTR2_TO_PTR
  (list
   (label VM_COPY_PTR2_TO_PTR)
          (LDA ZP_PTR2+1)       ;; high byte
          (STA ZP_PTR+1)
          (LDA ZP_PTR2)         ;; low byte (clean of tag)
          (STA ZP_PTR)
          (LDA ZP_PTR2_TAGGED)  ;; tagged low byte
          (STA ZP_PTR_TAGGED)
          (RTS)))

;; copy zp_ptr to zp_ptr2 (including tag byte)
;; input:  ZP_PTR
;; output: ZP_PTR2
;;         A  ?
;;         X  unchanged
;;         Y  unchanged
(define VM_COPY_PTR_TO_PTR2
  (list
   (label VM_COPY_PTR_TO_PTR2)
          (LDA ZP_PTR+1)        ;; high byte
          (STA ZP_PTR2+1)
          (LDA ZP_PTR)          ;; low byte (clean of tag)
          (STA ZP_PTR2)
          (LDA ZP_PTR_TAGGED)   ;; tagged low byte
          (STA ZP_PTR2_TAGGED)
          (RTS)))

;; jump table  page-type->allocation method
(define VM_ALLOC_PAGE_JUMP_TABLE
  (flatten ;; necessary because word ref creates a list of ast-byte-codes ...
   (list
    (label VM_ALLOC_PAGE__LIST_CELLS_PAIRS_JT)
           (word-ref VM_ALLOC_PAGE__LIST_CELL_PAIRS)
    (label VM_ALLOC_PAGE__CALL_STACK_JT)
           (word-ref VM_ALLOC_PAGE__CALL_STACK))))

;; initial data for the memory management registers
;; put into memory @ #xced0 - len (currently 3)
(define VM_INITIAL_MM_REGS
  (list
   (label VM_INITIAL_MM_REGS)

   ;; $cdc0
   (label VM_FREE_CELL_PAGE) ;; cell page with free cells
          (byte $00)
   ;; $cdc1
   (label VM_FREE_CALL_STACK_PAGE) ;; call stack page with free space
          (byte $00) ;; initial -> first allocation will allocate a new page
   ;; $cdc2
   (label VM_FREE_CODE_PAGE) ;; code page with free space
          (byte $00)

   ;; $cdc3
   (label VM_FREE_CELL_PAIR_PAGE) ;; cell page with free cells
          (byte $00) ;; none

   ;; $cdc4
   (label VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; what is the highest page to start searching for a free page
          (byte $1f) ;; safe to start with $1F is index

   ;; $cdc5
   (label VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (word $0000) ;; if high byte is 0, the tree is empty!

   ;; $cdc7
   ))

;; put into memory @ #xced0
(define VM_FREE_PAGE_BITMAP
  (list
   (label VM_FREE_PAGE_BITMAP)
          (byte #b11111111)     ;; mem 0000-07ff is unavailable
          (byte #b11111111)     ;; mem 0800-0fff is unavailable
          (byte #b11111111)     ;; mem 1000-17ff is unavailable
          (byte #b11111111)     ;; mem 1800-1fff is unavailable
          (byte #b00000011)     ;; mem 2000-21ff is unavailable, 2200-27ff is free
          (byte #b00000000)     ;; mem 2800-2fff is free
          (byte #b00000000)     ;; mem 3000-37ff is free
          (byte #b00000000)     ;; mem 3800-3fff is free
          (byte #b00000000)     ;; mem 4000-47ff is free
          (byte #b00000000)     ;; mem 4800-4fff is free
          (byte #b00000000)     ;; mem 5000-57ff is free
          (byte #b00000000)     ;; mem 5800-5fff is free
          (byte #b00000000)     ;; mem 6000-67ff is free
          (byte #b00000000)     ;; mem 6800-6fff is free
          (byte #b00000000)     ;; mem 7000-77ff is free
          (byte #b00000000)     ;; mem 7800-7fff is free
          (byte #b00000000)     ;; mem 8000-87ff is free
          (byte #b00000000)     ;; mem 8800-8fff is free
          (byte #b00000000)     ;; mem 9000-97ff is free
          (byte #b11100000)     ;; mem 9800-9cff is free, 9d00..9dff = first free code page, 9e00..9eff stack page, 9f00..9fff cell page
          (byte #b11111111)     ;; mem A000-A7ff is unavailable
          (byte #b11111111)     ;; mem A800-Afff is unavailable
          (byte #b11111111)     ;; mem B000-B7ff is unavailable
          (byte #b11111111)     ;; mem B800-Bfff is unavailable
          (byte #b00000000)     ;; mem C000-C7ff is free
          (byte #b11000000)     ;; mem C800-Cdff is free, ce00-ceff = other memory management registers + bitmap, cf00-cfff =  used by next free page mapping
          (byte #b11111111)     ;; mem D000-D7ff is unavailable
          (byte #b11111111)     ;; mem D800-Dfff is unavailable
          (byte #b11111111)     ;; mem E000-E7ff is unavailable
          (byte #b11111111)     ;; mem E800-Efff is unavailable
          (byte #b11111111)     ;; mem F000-F7ff is unavailable
          (byte #b11111111)     ;; mem F800-Ffff is unavailable
          ))


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
           (LDA !0)
           (TAY)
    (label VM_INITIALIZE_MEMORY_MANAGER__LOOP)
           ;; highbyte of this address should be using the constant NEXT_FREE_PAGE_PAGE
           ;; (STA $cf00,y) ;; encoded directly in the next couple of bytes
           ;; (car (ast-opcode-cmd-bytes (STA $cf00,y)))
           (byte 153 0) (byte-ref NEXT_FREE_PAGE_PAGE)
           (INY)
           (BNE VM_INITIALIZE_MEMORY_MANAGER__LOOP)

           ;; initialize cell stack
           (LDX !$FE)          ;; negative and 2 x inc = cell0+x -> tagged low byte
           (STX ZP_CELL_TOS)

           (RTS))))

;; INCOMPLETE!
;; parameter:
;;   A = page-type to allocate (see constants)
;;       (00 = fixed slot size list-pair-cells,
;;        01 = variable slot size native arrays for call-stack)
;; result:
;;   A = allocated page
(define VM_ALLOC_PAGE
  (list
   (label VM_ALLOC_PAGE)
          (ASL A)
          (TAY)
          (LDA VM_ALLOC_PAGE__LIST_CELL_PAIRS_JT+1,y) ;; high byte from jump table
          (STA VM_ALLOC_PAGE__JT+2)                   ;; write high byte of jump target
          (LDA VM_ALLOC_PAGE__LIST_CELL_PAIRS_JT,y)   ;; low byte from jump table
          (STA VM_ALLOC_PAGE__JT+1)                   ;; write low byte of jump target
   (label VM_ALLOC_PAGE__JT)
          (JMP $0000)                                 ;; jump to
   ))

;; (JSR VM_ALLOC_PAGE__LIST_PAIR_CELLS)      ;; direct calls (if type is statically known)
;; (JSR VM_ALLOC_PAGE__CALL_STACK)


;; memory layout of a cell-pairs page
;; offset content
;; --------------
;; 00     unused
;; 01     ref-count for cell-pair at 04 (cell-pair 0)
;; 02     ref-count for cell-pair at 08 (cell-pair 1)
;; 03     ref-count for cell-pair at 0C (cell-pair 2)
;; 04..07  cell-pair 0
;; 08..0b  cell-pair 1
;; 0c..0f  cell-pair 2
;; 10     ref-count for cell-pair at 40 (cell-pair 3)
;; 11     ref-count for cell-pair at 44 (cell-pair 4)
;; ..3F   ref-count for cell-pair at fc (cell-pair 50)
;; 40     cell-pair 3
;; 44     cell-pair 4
;; ..fc   cell-pair 50
;;
;; VM_FREE_SLOT_FOR_PAGE + pageidx: holds the index within the page of the first free cell-pair on that page (0 = no free cell-pair on this page)
;; the free cell-pair holds in byte 0 of the cell-pair the offset of the next free cell-pair (0 = no other free cell-pair)
;;
;; allocate a complete new page and initialize it to hold reference counted cell-pairs
;; connect all cell-pairs in a free-list
;; also set the first free slot of this allocated page (in VM_FREE_SLOT_FOR_PAGE + pageidx)
(define VM_ALLOC_PAGE__LIST_CELL_PAIRS
  (list
   (label VM_ALLOC_PAGE__LIST_CELL_PAIRS)
          (JSR VM_ALLOC_PAGE__PAGE_UNINIT)
          ;; now initialize page in A

          (STA ZP_PTR+1)
          (TAY) ;; page used as idx
          (LDA !$04) ;; first free slot (after initialization)
          (STA VM_FREE_SLOT_FOR_PAGE,y)

          ;; first write all reference count fields (zero)
          (LDY !$01)
          (STY ZP_PTR)
          (LDY !$02)
          (LDA !$00) ;; reference count initialized with 0

   (label FIRST_RC_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)
          (STA (ZP_PTR),y)
          (DEY)
          (BPL FIRST_RC_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)

          (LDY !$10)
          (STY ZP_PTR)
          (LDY !$2F)

   (label SECOND_RC_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)
          (STA (ZP_PTR),y)
          (DEY)
          (BPL SECOND_RC_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)

          (STA ZP_PTR) ;; clear lowbyte of ptr
          ;; write all cell-pairs to point to next free one
          (LDA !$04)
   (label FIRST_CELL_PAIR_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)
          (TAY)
          (CLC)
          (ADC !$04)
          (STA (ZP_PTR),y)
          (CMP !$0C)
          (BMI FIRST_CELL_PAIR_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)

          ;; write last pointer in this block to next free cell pair (in next block)
          (TAY)
          (LDA !$40)
          (STA (ZP_PTR),y)

   (label SECOND_CELL_PAIR_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)
          (TAY)
          (CLC)
          (ADC !$04)
          (STA (ZP_PTR),y)
          (CMP !$FC)
          (BNE SECOND_CELL_PAIR_BLOCK__VM_ALLOC_PAGE__LIST_CELL_PAIRS)

          (RTS)))


;; whether a page is free or used is kept in VM_FREE_PAGE_BITMAP
;; each bit represents one page 1 = used, 0 = free
;; VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH keeps the max idx to start looking for a page that is free
;; parameter: a = page
;; result: (none)
(define VM_FREE_PAGE
  (list
   (label VM_FREE_PAGE)
          (PHA)

          (LSR)
          (LSR)
          (LSR)
          (CMP VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; check for max of highest page #
          (BMI VM_FREE_PAGE__CONTINUE)
          (STA VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; store new max

   (label VM_FREE_PAGE__CONTINUE)
          (TAY) ;; index into bitmap
          (PLA)
          (AND !$07)
          (TAX)
          (LDA VM_FREE_PAGE_BITMAP,y)
          (EOR BITS,x) ;; clear bit that must be set because this page was in use
          (STA VM_FREE_PAGE_BITMAP,y)
          (RTS)))

;; allocate a page (completely uninitialized), just the page, update the memory page bitmap (VM_FREE_PAGE_BITMAP)
;; parameter: (none)
;; result: A = allocated free page (uninitialized)
;; uses: A, Y, X, one stack value
(define VM_ALLOC_PAGE__PAGE_UNINIT
  (list
   (label VM_ALLOC_PAGE__PAGE_UNINIT)
          (byte-const TEMP_TYPE #xfa)
          (byte-const PAGE #xfc)

          (word-const OUT_OF_MEMORY_ERROR #xc100)

          ;; use a sensible place to start looking for free page, from top to bottom
          (LDY VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH)


  (label  CHECK_PAGE_BITMAP)
          (LDA VM_FREE_PAGE_BITMAP,y)
          (CMP !$FF) ;; all pages used?
          (BNE FP_FOUND)
          (DEY)
          (BPL CHECK_PAGE_BITMAP)
          (JMP OUT_OF_MEMORY_ERROR) ;; TODO if there are enqueued ref count decs, check those first

   (label FP_FOUND)
          (PHA)
          (TYA) ;; Y = index into bitmap -> A
          (STA VM_HIGHEST_PAGE_IDX_FOR_ALLOC_SEARCH) ;; remember last page allocated (<- since always allocating form top -> bottom) this is the new top
          (ASL A)
          (ASL A)
          (ASL A)
          ;; now A has the topmost 5 bits set to the free page
          (STA PAGE)
          ;; now get the lower three bits (look for first bit not set)
          (LDX !$07)
          (PLA) ;; get bit map byte again

   (label SHIFT_OUT)
          (ASL A)
          (BCC UNSET_BIT_FOUND)
          (DEX)
          (BPL SHIFT_OUT)
          (BRK) ;; should never get here, there must be at least one bit not set

   (label UNSET_BIT_FOUND)
          (TXA)      ;; get the index of the bit into A (can be of value 0..7) => lower 3 bits
          (ORA PAGE) ;; combine with bits already found for page
          (STA PAGE) ;; store the full page idx

          ;; mark this page as used in the bitmap!
          (LDA VM_FREE_PAGE_BITMAP,y) ;; y should still hold the index into the bitmap
          (ORA BITS,x)                ;; x should still hold the index of the bit in the bitmap byte
          ;; make sure to set the bit of allocated page in page bitmap
          (STA VM_FREE_PAGE_BITMAP,y)
          (LDA PAGE) ;; return the new page in A
          (RTS)

   (label BITS)
          (byte #b00000001
                #b00000010
                #b00000100
                #b00001000
                #b00010000
                #b00100000
                #b01000000
                #b10000000)

   (label ALT_UNSET_BIT_FOUND) ;; 4 bytes less, mean 28 cycles more (calc BITS by shifting $01 x-times
          (TXA)
          (ORA PAGE) ;; combine with bits
          (STA PAGE)

          (LDA !$01)
          (label SHIFT_AGAIN)
          (ASL A)
          (DEX)
          (BPL SHIFT_AGAIN)
          (ROR)
          (ORA VM_FREE_PAGE_BITMAP,y)
          (STA VM_FREE_PAGE_BITMAP,y)
          (LDA PAGE)
          (RTS)
          ))



;; next free lowbyte on this page is in A (or 0)
;; resulting ptr is in ZP_PTR
;; first free element is adjusted
;; ---
;; allocate a cell-pair from this page (if page has no free cell-pairs, a new page is allocated and is used to get a free cell-pair!)
;;
(define VM_ALLOC_CELL_PAIR_ON_PAGE
  (list
   (label ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_PAIR_ON_PAGE)
          (JSR VM_ALLOC_PAGE__LIST_CELL_PAIRS)
          (LDA ZP_PTR+1)
          (STA VM_FREE_CELL_PAIR_PAGE)

   ;; ----------------------------------------
   (label VM_ALLOC_CELL_PAIR_ON_PAGE) ;; <-- real entry point of this function
          (STA ZP_PTR+1) ;; safe as highbyte of ptr
          (TAX)
          (LDA VM_FREE_SLOT_FOR_PAGE,x)
          (BEQ ALLOC_NEW_PAGE_PREFIX__VM_ALLOC_CELL_PAIR_ON_PAGE) ;; allocate new page first

   (label CELL_ON_THIS_PAGE__VM_ALLOC_CELL_PAIR_ON_PAGE_)
          (STA ZP_PTR)
          (ORA !$02)
          (STA ZP_PTR_TAGGED)
          (LDY !$00)
          (LDA (ZP_PTR),y) ;; next free cell
          (STA VM_FREE_SLOT_FOR_PAGE,x)
          (RTS)))

;; find out what kind of cell zp_ptr points to.
;;   in case of cell-pair,
(define VM_REFCOUNT_DECR
  (list
   (label VM_REFCOUNT_DECR)
          (LDA ZP_PTR_TAGGED)
          (LSR)
          (BCS DECR_CELL_PTR__VM_REFCOUNT_DECR)
          (LSR)
          (BCS DECR_CELL_PAIR__VM_REFCOUNT_DECR)
          ;; check other types of cells
          (BRK)

   (label DECR_CELL_PAIR__VM_REFCOUNT_DECR)
          (JMP VM_REFCOUNT_DECR_CELL_PAIR)
   (label DECR_CELL_PTR__VM_REFCOUNT_DECR)
          ;; implement VM_REFCOUNT_DECR_CELL_PTR
          (BRK)))

;; input: cell ptr in ZP_PTR
;; decrement ref count, if 0 deallocate
(define VM_REFCOUNT_DECR_CELL_PAIR
  (list
   (label VM_REFCOUNT_DECR_CELL_PAIR)
          (LDA ZP_PTR+1)
          (STA PAGE__VM_REFCOUNT_DECR_CELL_PAIR+2) ;; store high byte (page) into dec-command high-byte (thus +2 on the label)
          (LDA ZP_PTR)
          (LSR)
          (LSR)
          (TAX)
   (label PAGE__VM_REFCOUNT_DECR_CELL_PAIR)
          (DEC $c000,x) ;; c0 is overwritten by page (see above)
          (BNE DONE__VM_REFCOUNT_DECR_CELL_PAIR)
          (JMP VM_FREE_CELL_PAIR) ;; free delayed
   (label DONE__VM_REFCOUNT_DECR_CELL_PAIR)
          (RTS)))

(define VM_REFCOUNT_INCR_CELL_PAIR
  (list
   (label VM_REFCOUNT_INCR_CELL_PAIR)
          (LDA ZP_PTR+1)
          (STA PAGE__VM_REFCOUNT_INCR_CELL_PAIR+2) ;; store high byte (page) into inc-command high-byte (thus +2 on the label)
          (LDA ZP_PTR)
          (LSR)
          (LSR)
          (TAX)
   (label PAGE__VM_REFCOUNT_INCR_CELL_PAIR)
          (INC $c000,x) ;; c0 is overwritten by page (see above)
          (RTS)))

;; TODO: Free nonatomic (is cell-ptr, cell-pair)
;; parameter: zp_ptr
(define VM_FREE_NON_ATOMIC
  (list
   (label VM_FREE_NON_ATOMIC)
          (BRK)))

;; result: zp_ptr = free cell-pair
;; try to reuse root of free tree: use root but make sure to deallocate cell2 of the root (since this might still point to some data)
;; if no free tree available, find page with free cells (VM_FREE_CELL_PAIR_PAGE)
;; if no free cell page is available, allocate a new page and used the first free slot there
;; NOTE: the cell-pair is not initialized (cell1 and/or cell2 may contain old data that needs to be overwritten!)
(define VM_ALLOC_CELL_PAIR
  (list
   (label VM_ALLOC_CELL_PAIR)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (BNE REUSE_CELL_PAIR__VM_ALLOC_CELL_PAIR)
          ;; no cell-pair to free available

          ;; get a cell-pair on the given page (or allocate a new page)
          (LDA VM_FREE_CELL_PAIR_PAGE) ;; TODO
          (JMP VM_ALLOC_CELL_PAIR_ON_PAGE)

   (label REUSE_CELL_PAIR__VM_ALLOC_CELL_PAIR)
          ;; put root of free tree into zp_ptr (and copy in TEMP_PTR of this function)
          (STA ZP_PTR+1)
          (STA TEMP_PTR__VM_ALLOC_CELL_PAIR+1)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (STA ZP_PTR)
          (STA TEMP_PTR__VM_ALLOC_CELL_PAIR)

          ;; set new tree root for free tree to original cell0
          (LDY !$00)
          (LDA (ZP_PTR),y)
          (AND !$03)
          (BEQ CELL0_IS_ATOMIC__VM_ALLOC_CELL_PAIR)

          ;; cell0 is a cell-ptr or cell-pair-ptr
          (LSR)
          (BCS CELL0_IS_CELL_PTR__VM_ALLOC_CELL_PAIR)

          ;; cell0 is a cell-pair-ptr
          (LDA (ZP_PTR),y)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (INY)
          (LDA (ZP_PTR),y)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (BNE CHECK_CELL1__VM_ALLOC_CELL_PAIR) ;; since must be !=0, it cannot be on page 0 always branch!

   (label CELL0_IS_CELL_PTR__VM_ALLOC_CELL_PAIR)
          ;; cell0 is a cell-ptr => decrement cell0
          (JSR WRITE_CELLy_INTO_ZP_PTR_AND_REFCOUNT_DECR__VM_ALLOC_CELL_PAIR)
          (LDA !$00)
          ;; continue as if cell0 was atomic, since cell-ptr was handled already

   (label CELL0_IS_ATOMIC__VM_ALLOC_CELL_PAIR)
          ;; a is zero (otherwise would not have branched here)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)

   (label CHECK_CELL1__VM_ALLOC_CELL_PAIR)
          ;; check whether cell1 is atomic or ptr
          (LDY !$02)
          (LDA (ZP_PTR),y) ;; get low byte
          (AND !$03)       ;; mask out all but low 2 bits
          (BEQ CELL1_IS_ATOMIC__VM_ALLOC_CELL_PAIR) ;; no need to do further deallocation

          ;; write cell1 into zp_ptr and decrement
          (JSR WRITE_CELLy_INTO_ZP_PTR_AND_REFCOUNT_DECR__VM_ALLOC_CELL_PAIR)
          ;; continue as if cell1 is atomic, since it was already handled

   (label CELL1_IS_ATOMIC__VM_ALLOC_CELL_PAIR)
          ;; restore zp_ptr to the cell-pair to be reused
          (LDA TEMP_PTR__VM_ALLOC_CELL_PAIR+1)
          (STA ZP_PTR+1)
          (LDA TEMP_PTR__VM_ALLOC_CELL_PAIR)
          (STA ZP_PTR)

          (RTS)

   ;; subroutine that writes CELLy (00 = cell0, 02 = cell1) of zp_ptr into zp_ptr (overwriting it)
   ;; and does a generic ref count decr, freeing (mark for freeing) handling cell-ptr and cell-pair-ptr
   (label WRITE_CELLy_INTO_ZP_PTR_AND_REFCOUNT_DECR__VM_ALLOC_CELL_PAIR)
          (LDA (ZP_PTR),y)
          (PHA)
          (INY)
          (LDA (ZP_PTR),y)
          (STA ZP_PTR+1)
          (PLA)
          (STA ZP_PTR_TAGGED) ;; tag
          (AND $fc)
          (STA ZP_PTR) ;; cleared from tag, => real pointer
          (JMP VM_REFCOUNT_DECR)

   (label TEMP_PTR__VM_ALLOC_CELL_PAIR)
          (word $0000)))

;; cell ptr is in ZP_PTR
;; -----
;; put the cell-pair itself as new root to the free-tree
;; put the old free-tree into cell1
;; tail call free on old cell1 in this cell-pair (if not atomic, if atomic no tail call)
;; result: this cell-pair is the new root of the free-tree for cell-pairs with:
;;              cell1 = old free tree root, cell2 = non-freed (yet) original cell
(define VM_FREE_CELL_PAIR
  (list
   (label VM_FREE_CELL_PAIR)

          ;; check cell0
          (LDY !$00)
          (LDA (ZP_PTR),y) ;; LOWBYTE OF FIRST cell0
          (AND !$03)
          (BEQ CELL_0_ATOMIC__VM_FREE_CELL_PAIR)
          ;; make sure to call free on cell0 (could be any type of cell)
          ;; remember ZP_PTR

          ;; store cell0 into TEMP_PTR__VM_FREE_CELL_PAIR (for later tail call of free)
          (LDA (ZP_PTR),y)
          (STA TEMP_PTR__VM_FREE_CELL_PAIR)
          (INY)
          (LDA (ZP_PTR),y)
          (STA TEMP_PTR__VM_FREE_CELL_PAIR+1)

   (label CELL_0_ATOMIC__VM_FREE_CELL_PAIR)
          ;; cell0 is atomic and can thus be discarded (directly)

          ;; simply add this cell-pair as head to free tree
          ;; set cell0 to point to old root
          (LDY !$01)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (STA (ZP_PTR),y)
          (DEY)
          (LDA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)
          (STA (ZP_PTR),y)
          ;; set new root to point to cell-pair
          (LDA ZP_PTR+1)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE+1)
          (LDA ZP_PTR)
          (STA VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE)

          ;; write original cell0 -> zp_ptr
          (LDA TEMP_PTR__VM_FREE_CELL_PAIR+1)
          (BEQ DONE__VM_FREE_CELL_PAIR)
          (STA ZP_PTR+1)
          (LDA TEMP_PTR__VM_FREE_CELL_PAIR)
          (STA ZP_PTR)

          (LDA !$00)
          (STA TEMP_PTR__VM_FREE_CELL_PAIR+1) ;; mark temp_ptr as clear

          (JMP VM_FREE_NON_ATOMIC) ;; chain call

   (label DONE__VM_FREE_CELL_PAIR)
          (RTS)

   (label TEMP_PTR__VM_FREE_CELL_PAIR)
          (word $0000)))

;; zp_ptr = pointer to cell-pair that is added to the free list on its page
(define VM_ADD_CELL_PAIR_TO_ON_PAGE_FREE_LIST
  (list
   (label VM_ADD_CELL_PAIR_TO_ON_PAGE_FREE_LIST)
          (LDX ZP_PTR+1)
          (LDA VM_FREE_SLOT_FOR_PAGE,x) ;; old first free on page
          (LDY !$00)
          (STA (ZP_PTR),y) ;; set old free to next free on this very cell
          (LDA ZP_PTR) ;; load idx within page
          (STA VM_FREE_SLOT_FOR_PAGE,x) ;; set this cell as new first free cell on page

          ;; clear refcount, too (should have been done already)
          (LSR)
          (LSR)
          (TAY);; y now pointer to refcount byte
          (LDA !$00)
          (STA ZP_PTR) ;; modify pointer such that zp_ptr points to beginning of page
          (STA (ZP_PTR),y) ;; clear refcount byte, too
          (RTS)))


(module+ test #| use case 1 allocate, free, reallocate single cell-pair |#
  (define use-case-1-code
    (list
      (JSR VM_ALLOC_CELL_PAIR)
      (JSR VM_REFCOUNT_INCR_CELL_PAIR)
      ;; set cell2 to int 0
      (JSR VM_CELL_STACK_PUSH_INT_0)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELL1_OF_ZP_PTR)
      ;; set cell1 to int 0
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELL0_OF_ZP_PTR)))

  (define use-case-1-a-state-after
    (run-code-in-test use-case-1-code))

  (check-equal? (memory-list use-case-1-a-state-after ZP_PTR (+ 1 ZP_PTR))
                '(#x04 #xcd)
                "zp_ptr -> $cd04 = first free cell-pair on page $cd after initialization")
  (check-equal? (memory-list use-case-1-a-state-after #xcd01 #xcd01)
                '(#x01)
                "refcount for first cell-pair allocated = 1")
  (check-equal? (memory-list use-case-1-a-state-after #xcd04 #xcd07)
                '(#x00 #x00 #x00 #x00)
                "cell1=int0 cell2=int0")
  (check-equal? (memory-list use-case-1-a-state-after #xcfcd #xcfcd)
                '(#x08)
                "next free cell-pair on page $cd is at $08")

  (define use-case-1-b-code
    (append
     use-case-1-code ;; cell was allocated and set to hold int 0 in car and cdr
     (list
      ;; refcount will drop to zero
      (JSR VM_REFCOUNT_DECR_CELL_PAIR))))

  (define use-case-1-b-state-after
    (run-code-in-test use-case-1-b-code))

  (check-equal? (memory-list use-case-1-b-state-after #xcec5 #xcec6) ;;
                '(#x04 #xcd )
                "root of free tree is cell-pair at $cd04")
  (check-equal? (memory-list use-case-1-b-state-after #xcd01 #xcd01)
                '(#x00)
                "refcount for cell-pair freed = 0")
  (check-equal? (memory-list use-case-1-b-state-after #xcfcd #xcfcd)
                '(#x08)
                "next free cell-pair on page $cd is still $08")

  (define use-case-1-c-code
    (append
     use-case-1-b-code ;; cell was freed and in the free tree
     (list
      ;; clear zp_ptr just to make sure
      (LDA !$00)
      (STA ZP_PTR)
      (STA ZP_PTR+1)

      ;; allocate a new cell (should reuse pair in free-tree)
      (JSR VM_ALLOC_CELL_PAIR))))

  (define use-case-1-c-state-after
    (run-code-in-test use-case-1-c-code))

  (check-equal? (memory-list use-case-1-c-state-after ZP_PTR (+ 1 ZP_PTR)) ;;
                '(#x04 #xcd )
                "allocated cell-pair is reused cell-pair of free tree")
  (check-equal? (memory-list use-case-1-c-state-after #xcec5 #xcec6) ;;
                '(#x00 #x00 )
                "root of free tree is initial again")
  (check-equal? (memory-list use-case-1-c-state-after #xcd01 #xcd01)
                '(#x00)
                "refcount for (reused) cell-pair = 1")
  (check-equal? (memory-list use-case-1-c-state-after #xcfcd #xcfcd)
                '(#x08)
                "next free cell-pair on page $cd is still $08"))

(module+ test #| use case: allocate, free, reallocate small list of cell-pairs |#
  (define use-case-2-a-code
    (list
     (JSR VM_ALLOC_CELL_PAIR)                               ;; zp_ptr = freshly allocated cell (cd04)
     (JSR VM_REFCOUNT_INCR_CELL_PAIR)                       ;; ref(zp_ptr) ++ (=1)
     ;; set cdr to nil
     (JSR VM_CELL_STACK_PUSH_NIL)                           ;; cell-stack <- push nil
     (JSR VM_CELL_STACK_WRITE_TOS_TO_CELL1_OF_ZP_PTR)       ;; (cdr zp_ptr) := nil
     ;; set car to int 0
     (JSR VM_CELL_STACK_PUSH_INT_0)                         ;; cell-stack <- push int0
     (JSR VM_CELL_STACK_WRITE_TOS_TO_CELL0_OF_ZP_PTR)       ;; (car zp_ptr) := int0

     (JSR VM_COPY_PTR_TO_PTR2)                              ;; zp_ptr2 := zp_ptr

     (JSR VM_ALLOC_CELL_PAIR)                               ;; zp_ptr = freshly allocated cell (cd08)
     (JSR VM_REFCOUNT_INCR_CELL_PAIR)                       ;; ref(zp_ptr) ++ (=1)
     ;; set cdr to zp_ptr2->
     (JSR VM_CELL_STACK_PUSH_ZP_PTR2)                       ;; cell-stack <- push zp_ptr2
     (JSR VM_CELL_STACK_WRITE_TOS_TO_CELL1_OF_ZP_PTR)       ;; (cdr zp_ptr) := tos (which is zp_ptr2)
     (JSR VM_CELL_STACK_POP__NO_GC)                         ;; just pop (no check, no gc)
     ;; set car to int0
     (JSR VM_CELL_STACK_WRITE_TOS_TO_CELL0_OF_ZP_PTR)       ;; (car zp_ptr) := tos (which is int0 again

     ;; now:
     ;;   zp_ptr[cd08|1] (int0 . ->[cd04|1](int0 . nil))
     ;; notation:
     ;;   [<mem-location>|<ref-count>]
     ;;   (<car-cell> . <cdr-cell>)
     ;;   intX, nil :: atomic value cells
     ;;   -> :: cell-ptr
     ))

  (define use-case-2-a-state-after
    (run-code-in-test use-case-2-a-code))

  (check-equal? (memory-list use-case-2-a-state-after ZP_PTR (+ 3 ZP_PTR))
                '(#x08 #xcd #x04 #xcd)
                "case 2a: zp_ptr -> $cd08, zp_ptr2 -> $cd04 = first two free cell-pairs on page $cd after initialization")
  (check-equal? (memory-list use-case-2-a-state-after #xcd01 #xcd0b)
                '(#x01 #x01 #x00      ;; refcounts
                  #x00 #x00 #x02 #x00 ;; tail cell
                  #x00 #x00 #x06 #xcd ;; head cell
                  )
                "case 2a: cell-pairs contain (int0 . -> next cell), (int 0 . nil)")

  (define use-case-2-b-code
    (append use-case-2-a-code ;; zp_ptr[cd08|1] (int0 . ->[cd04|1](int0 . nil))
            (list
             (JSR VM_REFCOUNT_DECR_CELL_PAIR)
             ;; now:
             ;;   free_tree -> [cd08|0] (int0 . ->[cd04|1] (int0 . nil))
             )))

  (define use-case-2-b-state-after
    (run-code-in-test use-case-2-b-code))

  (check-equal? (memory-list use-case-2-b-state-after #xcd01 #xcd0b)
                '(#x01 #x00 #x00      ;; refcounts
                  #x00 #x00 #x02 #x00 ;; cd04 (old tail of list)
                  #x00 #x00 #x06 #xcd ;; cd08 (old head of list)
                  )
                "case 2b: refcount cd08 = 0 (head), cd04 unchanged (tail)")
  (check-equal? (memory-list use-case-2-b-state-after #xcec5 #xcec6) ;;
                '(#x08 #xcd )
                "case 2b: root of free tree is cell-pair at $cd08")

  (define use-case-2-c-code
    (append use-case-2-b-code ;; free_tree -> [cd08|0] (int0 . ->[cd04|1] (int0 . nil))
            (list (LDA !$FF) ;; marker for debug, remove when done
                  (JSR VM_ALLOC_CELL_PAIR)
                  (JSR VM_REFCOUNT_INCR_CELL_PAIR)
                  ;; now:
                  ;;   zp_ptr = [cd08|1] not initialized
                  ;;   free_tree -> [cd04|0] (int0 . nil)
                  )))

  (define use-case-2-c-state-after
    (run-code-in-test use-case-2-c-code))

  (check-equal? (memory-list use-case-2-c-state-after ZP_PTR (+ 1 ZP_PTR))
                '(#x08 #xcd)
                "case 2c: zp_ptr -> $cd08, reallocated")
  (check-equal? (memory-list use-case-2-c-state-after #xcd01 #xcd0b)
                '(#x00 #x01 #x00      ;; refcounts
                  #x00 #x00 #x02 #x00 ;; tail cell
                  #x00 #x00 #x06 #xcd ;; head cell
                  )
                "case 2c: refcount cd08 = 1 reallocated, refcount cd04 = 0 (original tail, now in the free tree)")
  (check-equal? (memory-list use-case-2-c-state-after #xcec5 #xcec6) ;;
                '(#x04 #xcd )
                "case 2c: root of free tree is cell-pair at $cd04"))

;; ----------------------------------------
;; page type: call-frame page
;; => allocation/deallocation is always done on tos
;;    no need for a free list (stack structure is coded into the stack pages)
;;    need for max size left
;; memory layout of call frame page (organized in stack)
;;  00 : unused (page type)
;;  01 : previous page (just high byte), 00 for first stack page
;;  02 : first frame payload byte 0
;;  ... : first frame payload byte size-1
;;  free-1 : size of (prev) frame
;;  free : unused
;; ...ff : unused
;;
;; VM_FREE_SLOT_FOR_PAGE + pageidx: holds free-idx (initially 02) <- points to the first free byte (-1 = size of previous)


;; input: X = previous call stack page
;; output: X = page allocated
;;         page bitmap is adjusted
;;         page is initialized: previous call stack page is set
;;         VM_FREE_SLOT_FOR_PAGE is set to 02
;;         VM_FREE_CALL_STACK_PAGE is set to this allocated page
(define VM_ALLOC_PAGE__CALL_FRAME
  (list
   (label VM_ALLOC_PAGE__CALL_FRAME)
          (TXA)
          (PHA)

          (JSR VM_ALLOC_PAGE__PAGE_UNINIT) ;; A = new page

          ;; set this new page as the TOS for call stack allocation
          (STA VM_FREE_CALL_STACK_PAGE)

          ;; set zp_ptr to point to this page
          (STA ZP_PTR+1)
          (LDX !$00)
          (STX ZP_PTR)
          (TAX)

          (LDA !$02)
          (STA VM_FREE_SLOT_FOR_PAGE,x) ;; set first free slot to 02

          ;; initialize allocated page
          (PLA)
          (LDY !$01)
          (STA (ZP_PTR),y) ;; set previous page

          (RTS)))

(module+ test #| VM_ALLOC_PAGE__CALL_FRAME |#
  (define alloc-page-call-frame-code
    (list
     (LDX !$00) ;; previous not existent
     (JSR VM_ALLOC_PAGE__CALL_FRAME)))

  (define alloc-page-call-frame-state-after
    (run-code-in-test alloc-page-call-frame-code))

  (check-equal? (memory-list alloc-page-call-frame-state-after #xcd01 #xcd01)
                (list #x00)
                "point to previous call frame page is 00 (none)")
  (check-equal? (memory-list alloc-page-call-frame-state-after #xcfcd #xcfcd)
                (list #x02)
                "first free slot of page cd is 02")
  (check-equal? (memory-list alloc-page-call-frame-state-after #xcec1 #xcec1)
                (list #xcd)
                "tos for call frame ois page cd")

  (define alloc-second-page-call-frame-code
    (append
     alloc-page-call-frame-code
     (list
      (LDX !$cd) ;; previous
      (JSR VM_ALLOC_PAGE__CALL_FRAME))))

  (define alloc-second-page-call-frame-state-after
    (run-code-in-test alloc-second-page-call-frame-code))

  (check-equal? (memory-list alloc-second-page-call-frame-state-after #xcc01 #xcc01)
                (list #xcd)
                "point to previous call frame page is cd")
  (check-equal? (memory-list alloc-second-page-call-frame-state-after #xcfcc #xcfcc)
                (list #x02)
                "first free slot of page cc is 02")
  (check-equal? (memory-list alloc-second-page-call-frame-state-after #xcec1 #xcec1)
                (list #xcc)
                "tos for call frame ois page cc"))


;; input:  VM_FREE_CALL_STACK_PAGE
;;         VM_FREE_SLOT_FOR_PAGE,x
;; output: VM_FREE_CALL_STACK_PAGE
;;         VM_FREE_SLOT_FOR_PAGE,x
(define VM_POP_CALL_FRAME
  (list
   (label DEALLOCATE_PAGE__VM_POP_CALL_FRAME)
          (LDA !$00)
          (STA ZP_PTR)
          (LDY !$01)
          (LDA (ZP_PTR),y)  ;; A = previous page
          (STA VM_FREE_CALL_STACK_PAGE)
          (JSR VM_FREE_PAGE)

   ;; ----------------------------------------
   (label VM_POP_CALL_FRAME)
          (LDX VM_FREE_CALL_STACK_PAGE)
          (STX ZP_PTR+1)
          (LDA VM_FREE_SLOT_FOR_PAGE,x)
          (STA ZP_PTR)
          (CMP !$02)
          (BEQ DEALLOCATE_PAGE__VM_POP_CALL_FRAME)

          (DEC ZP_PTR) ;; point to size of previous record
          (LDY !$00)
          (CLC)
          (SBC (ZP_PTR),y) ;; a = size of previous
          (SBC !$01)
          (STA VM_FREE_SLOT_FOR_PAGE,x)
          (RTS)))

(module+ test #| vm_pop_call_frame |#
  (define test-pop-call-frame-code
    (list
     (LDA !$80)
     (JSR VM_ALLOC_CALL_FRAME)
     (LDA !$80)  ;; doesn't fit, second page is allocated
     (JSR VM_ALLOC_CALL_FRAME)
     (JSR VM_POP_CALL_FRAME)))

  (define test-pop-call-frame-state-after
    (run-code-in-test test-pop-call-frame-code))

  (check-equal? (memory-list test-pop-call-frame-state-after #xcfcc #xcfcc)
                (list #x02)
                "first free slot of page cc is (again) 02")
  (check-equal? (memory-list test-pop-call-frame-state-after #xcec1 #xcec1)
                (list #xcc)
                "tos for call frame is page cc")
  (check-equal? (memory-list test-pop-call-frame-state-after #xcc01 #xcc01)
                (list #xcd)
                "previous stack page is cd")

  (define test-pop-call-frame-2times-code
    (list
     (LDA !$80)
     (JSR VM_ALLOC_CALL_FRAME)
     (LDA !$80) ;; doesn't fit, second page is allocated
     (JSR VM_ALLOC_CALL_FRAME)
     (JSR VM_POP_CALL_FRAME)
     (JSR VM_POP_CALL_FRAME)))

  (define test-pop-call-frame-2times-state-after
    (run-code-in-test test-pop-call-frame-2times-code))

  (check-equal? (memory-list test-pop-call-frame-2times-state-after #xcfcd #xcfcd)
                (list #x02)
                "first free slot of page cd is (again) 02")
  (check-equal? (memory-list test-pop-call-frame-2times-state-after #xcec1 #xcec1)
                (list #xcd)
                "tos for call frame is page (again) cd")
  (check-equal? (memory-list test-pop-call-frame-2times-state-after #xcd01 #xcd01)
                (list #x00)
                "previous stack page is 00"))

;; input: A = size
;; output: ZP_PTR
(define VM_ALLOC_CALL_FRAME
  (list
   ;; ----------------------------------------
   (label VM_ALLOC_CALL_FRAME)
          (PHA)
          (LDX VM_FREE_CALL_STACK_PAGE) ;; get the tos page for call stacks
          (BEQ ALLOCATE_NEW_PAGE__VM_ALLOC_CALL_FRAME)

          (SEC) ;; add one additionally for storing the size
          (ADC VM_FREE_SLOT_FOR_PAGE,x)   ;; A = free + size + 1 == new free
          (BCC USE_GIVEN_PAGE__VM_ALLOC_CALL_FRAME)

   (label ALLOCATE_NEW_PAGE__VM_ALLOC_CALL_FRAME)
          (JSR VM_ALLOC_PAGE__CALL_FRAME)
          (PLA)
          (PHA)      ;; A = size
          (ADC !$03) ;; 03 = old-free (2) + size (A) + 1 = new free

   (label USE_GIVEN_PAGE__VM_ALLOC_CALL_FRAME)
          (LDY VM_FREE_SLOT_FOR_PAGE,x) ;; y = (old) free on page
          (STA VM_FREE_SLOT_FOR_PAGE,x) ;; set new first free slot

          (STX ZP_PTR+1) ;; x = stack page
          (STY ZP_PTR)   ;; y = (old) free on page = first byte allocated
          (PLA)          ;; A = size
          (TAY)          ;; Y = size
          (STA (ZP_PTR),y) ;; set size on last byte (for easy deallocation)
          (RTS)))

(module+ test #| vm_alloc_call_frame |#
  (define alloc-call-frame-code
    (list
     (LDA !$80)
     (JSR VM_ALLOC_CALL_FRAME)))

  (define alloc-call-frame-state-after
    (run-code-in-test alloc-call-frame-code))

  (check-equal? (memory-list alloc-call-frame-state-after #xcfcd #xcfcd)
                (list #x83)
                "first free slot of page cc is 83")
  (check-equal? (memory-list alloc-call-frame-state-after #xcec1 #xcec1)
                (list #xcd)
                "tos for call frame is page cd")
  (check-equal? (memory-list alloc-call-frame-state-after #xcd01 #xcd01)
                (list #x00)
                "previous stack page is 00")
  (check-equal? (memory-list alloc-call-frame-state-after #xcd82 #xcd82)
                (list #x80)
                "size of entry is 80")
  (check-equal? (memory-list alloc-call-frame-state-after ZP_PTR (add1 ZP_PTR))
                (list #x02 #xcd)
                "pointer to allocated frame")

  (define alloc-call-frame-2times-code
    (list
     (LDA !$80)
     (JSR VM_ALLOC_CALL_FRAME)
     (LDA !$80) ;; does not fit again in this call frame
     (JSR VM_ALLOC_CALL_FRAME)))

  (define alloc-call-frame-2times-state-after
    (run-code-in-test alloc-call-frame-2times-code))

  (check-equal? (memory-list alloc-call-frame-2times-state-after #xcfcc #xcfcc)
                (list #x83)
                "first free slot of page cc is 83")
  (check-equal? (memory-list alloc-call-frame-2times-state-after #xcec1 #xcec1)
                (list #xcc)
                "tos for call frame is page cc")
  (check-equal? (memory-list alloc-call-frame-2times-state-after #xcc01 #xcc01)
                (list #xcd)
                "previous stack page is cd")
  (check-equal? (memory-list alloc-call-frame-2times-state-after ZP_PTR (add1 ZP_PTR))
                (list #x02 #xcc)
                "pointer to allocated frame")

  (define alloc-call-frame-2times-fitting-code
    (list
     (LDA !$20)
     (JSR VM_ALLOC_CALL_FRAME)
     (LDA !$20)
     (JSR VM_ALLOC_CALL_FRAME)))

  (define alloc-call-frame-2times-fitting-state-after
    (run-code-in-test alloc-call-frame-2times-fitting-code))

  (check-equal? (memory-list alloc-call-frame-2times-fitting-state-after #xcfcd #xcfcd)
                (list #x44)
                "first free slot of page cd is 44")
  (check-equal? (memory-list alloc-call-frame-2times-fitting-state-after #xcec1 #xcec1)
                (list #xcd)
                "tos for call frame is page cd")
  (check-equal? (memory-list alloc-call-frame-2times-fitting-state-after #xcd22 #xcd22)
                (list #x20)
                "size of first allocation is 20")
  (check-equal? (memory-list alloc-call-frame-2times-fitting-state-after #xcd43 #xcd43)
                (list #x20)
                "size of second allocation is 20")
  (check-equal? (memory-list alloc-call-frame-2times-fitting-state-after ZP_PTR (add1 ZP_PTR))
                (list #x23 #xcd)
                "pointer to allocated frame"))

(define vm-memory-manager
  (append VM_MEMORY_MANAGEMENT_CONSTANTS
          VM_INITIALIZE_MEMORY_MANAGER
          ;; VM_ALLOC_PAGE_JUMP_TABLE
          ;; VM_ALLOC_PAGE

          VM_FREE_PAGE
          VM_ALLOC_PAGE__LIST_CELL_PAIRS
          VM_ALLOC_PAGE__PAGE_UNINIT
          VM_ALLOC_PAGE__CALL_FRAME
          VM_ALLOC_CELL_PAIR_ON_PAGE

          VM_REFCOUNT_DECR
          VM_REFCOUNT_DECR_CELL_PAIR
          VM_REFCOUNT_INCR_CELL_PAIR

          VM_ALLOC_CELL_PAIR
          VM_ALLOC_CALL_FRAME
          VM_POP_CALL_FRAME

          VM_FREE_NON_ATOMIC
          VM_FREE_CELL_PAIR

          VM_CELL_STACK_WRITE_INT_TO_TOS
          VM_CELL_STACK_WRITE_TOS_TO_CELLy_OF_ZP_PTR
          VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS
          VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRy
          VM_CELL_STACK_WRITE_ZP_PTRy_TO_TOS
          VM_CELL_STACK_POP
          VM_CELL_STACK_PUSH

          VM_COPY_PTR2_TO_PTR
          VM_COPY_PTR_TO_PTR2

          (list (org #xcec0))
          VM_INITIAL_MM_REGS
          (list (org #xced0))
          VM_FREE_PAGE_BITMAP
          ))

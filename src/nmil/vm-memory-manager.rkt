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

;; IDEA: zero page cell stack may not be beneficial (can be put in regular memory)
;;       LDA zeropage,x  consumes 4 clocks,  LDA absolute,x consumes 4 clocks too => no speed just size benefit (2 vs. 3 bytes)
;;       LDA zeropage and LDA absolute differs in speed => storing registers not accessed through index makes sense
;;       indirect addressing can only be done on zp => zp_ptr and zp_ptr2 make sense, too

;; IDEA: eval cell stack is held in pages growing from the bottom (to have continuous access)
;;       tos is a ptr on the zp, pointing to the tos cell
;;           LDY !$00
;;           LDA (TOS),y    ;; tagged low byte of cell
;;           INY ...
;;       call-params os a ptr on the zp, pointing into the first parameter on the eval cell stack
;;           LDY n * 2
;;           LDA (call-params),y ;; get n-th parametere tagged low byte of cell
;;       pop cell stack:
;;           LDA TOS
;;           BNE JUST_LOW
;;           DEC TOS+1
;;        JUST_LOW:
;;           DEC TOS
;;           DEC TOS
;;       push cell stack:
;;           INC TOS
;;           INC TOS
;;           BNE DONE
;;           INC TOS+1
;;        DONE:
;;       call frame is created by call, but there is not need to copy (portions of) the eval stack
;;       the call frame allocates space for locals and that's it (it's still a stack, so push/pop is supported)
;;       alternative to tos: function entry tos + index
;;         upon function entry, the fe tos is written (once) and a separate index is held in zero page
;;         such that fe0-tos + index = actual tos => push pop work in index only (restriction value stack w/i one page to 128 cells)
;;       alternative continuous stack space: allocate pages, but how? function call will fix the parameters => that part does not change
;;       within one function, one page is enough, but an almost full page could be a problem:
;;         one solution: page grows until function call, then, if page is free enough continues with the given page, if page is not enough allocated new one
;;         minimum free space = 16 cells? = 32 byte => 256 - 32 = 224 (roughly) are available, can be used in combination with call-frame
;;      zp_vm_params -> [params]            <- actually previous eval stack
;;        callframe:    -----               <- new allocation starts here
;;                      [old zp_vm_pc]      <- pointer to code to return to (zp_vm_pc)
;;                      [old zp_vm_locals]  <- used to restore zp_vm_locals
;;                      [old zp_vm_params]  <- used to restore zp_vm_params
;;      zp_vm_locals -> [locals]            <- fixed number of slots kept for function execution
;;         zp_vm_tos -> [eval-stack]        <- size must fit into page (16 cells) <- could later be derived from actual function implementation
;;                                            (part of the eval stack is then again [params] for the next function called)
;; call to function:
;;      new zp_vm_params = zp_vm_tos - 2 * n-params (of the function called)
;;      allocated call frame [call-back] = zp_vm_pc
;;      new zp_vm_locals = allocated call frame + 6
;;      new zp_vm_tos    = allocated call frame + 2 * (n-locals + 3)  (of the function called)
;;      zp_vm_pc         = function called
;;
;; return from function:
;;      zp_vm_params     = (new) zp_vm_locals - 2 * (n-params + 3) (of function returned to)
;;      zp_vm_locals     = ? <- must be saved too
;;      zp_vm_tos        = (old) zp_vm_params - 2
;;      zp_vm_pc         = call-frame [call-back] = ((old) zp_vm_locals-6)
;;
;; NEW STACK STRUCTURE:
;;      ZP_CELL_STACK_BASE_PTR -> points to the start of the stack of the current (executing) function
;;      ZP_CELL_STACK_TOS      -> offset for ZP_CELL_STACK_BASE_PTR, pointing to the tagged (low) byte, ff = empty, 01 = 1 el on stack, 03 = 2 el on stack ...
;;                                LDA (ZP_CELL_STACK_BASE_PTR),ZP_CELL_STACK_TOS   = tagged low byte
;;                                LDA (ZP_CELL_STACK_BASE_PTR),ZP_CELL_STACK_TOS-1 = high byte
;;                                => pushing is done in reverse order (push high byte first, then push tagged low byte)
;;                             reading tos:
;;                                LDY ZP_CELL_STACK_TOS;
;;                                LDA (ZP_CELL_STACK_BASE_PTR),y  ;; gets tagged low byte
;;                                DEY
;;                                LDA (ZP_CELL_STACK_BASE_PTR),y  ;; gets untagged high byte
;;
;; IDEA: use larger block page allocation (e.g. not used one page but 4 pages as a block => less waste

(require (only-in racket/format ~a))

(require "../6510.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader))
(require (only-in "../tools/6510-interpreter.rkt" peek-word-at-address))
(require (only-in "../ast/6510-calc-opcode-facades.rkt" LDA-immediate))
(require (only-in racket/list flatten take empty? drop))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in racket/port open-output-nowhere))
  (require (only-in "../tools/6510-disassembler.rkt" disassemble-bytes))
  (require (only-in "../tools/6510-debugger.rkt" run-debugger-on))

  (define (wrap-code-for-test bc)
    (append (list (org #xc000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER))
            bc
            (list (BRK))
            vm-memory-manager))

  (define (run-code-in-test bc (debug #f))
    (define wrapped-code (wrap-code-for-test bc))
    (define state-before
      (6510-load-multiple (initialize-cpu)
                          (assemble-to-code-list wrapped-code)))
    (if debug
        (run-debugger-on state-before)
        (parameterize ([current-output-port (open-output-nowhere)])
          (run-interpreter-on state-before)))))

(require (only-in "../tools/6510-interpreter.rkt" 6510-load 6510-load-multiple initialize-cpu run-interpreter run-interpreter-on memory-list cpu-state-accumulator peek))

(provide vm-memory-manager vm-stack->strings ast-const-get
          ZP_VM_PC
          ZP_LOCALS_PTR
          ZP_PARAMS_PTR
          ZP_CELL_STACK_TOS
          ZP_CELL_STACK_BASE_PTR)

;; produce strings describing the current cell-stack status
(define (vm-stack->strings state)
  (define stack-tos-idx (peek state ZP_CELL_STACK_TOS))
  (cond
    [(> stack-tos-idx #xf0) (list "stack is empty")]
    [else
     (define stack-ptr (peek-word-at-address state ZP_CELL_STACK_BASE_PTR))
     (define stack (memory-list state stack-ptr (+ 1 stack-tos-idx stack-ptr)))
     (define stack-values stack)
     (define stack-item-no (/ (add1 stack-tos-idx) 2))
     (cons (format "stack holds ~a ~a" stack-item-no (if (= 1 stack-item-no) "item" "items"))
           (reverse (map (lambda (pair) (vm-cell->string (cdr pair) (car pair))) (pairing stack-values))))]))

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

(define (vm-cells->strings byte-list (result (list)))
  (if (empty? byte-list)
      (reverse result)
      (vm-cells->strings
       (cddr byte-list)
       (cons (vm-cell->string (car byte-list)
                             (cadr byte-list))
             result))))

(module+ test #| vm-cells->strings |#
  (check-equal? (vm-cells->strings '(#x02 #x00 #x00 #x01))
                '("cell-pair-ptr $0000" "cell-int $0001")))

(module+ test #| vm-stack->string |#
  (define test-vm_stack_to_string-a-code
    (list (JSR VM_CELL_STACK_PUSH_NIL)
          (JSR VM_CELL_STACK_PUSH_NIL)
          (LDA !$01)
          (LDX !$03)
          (JSR VM_CELL_STACK_PUSH_INT)))

  (define test-vm_stack_to_string-a-state-after
    (run-code-in-test test-vm_stack_to_string-a-code))

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

;;   VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR :: Stack -> ZP_PTR (CELLx)
;;   VM_CELL_STACK_WRITE_TOS_TO_CELL0_OF_ZP_PTR :: Stack -> ZP_PTR (CELL0)
;;   VM_CELL_STACK_WRITE_TOS_TO_CELL1_OF_ZP_PTR :: Stack -> ZP_PTR (CELL1)
;;
;;   VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS :: ZP_PTR (CELLy) -> Stack
;;   VM_CELL_STACK_WRITE_CELL0_OF_ZP_PTR_TO_TOS :: ZP_PTR (CELL0) -> Stack
;;   VM_CELL_STACK_WRITE_CELL1_OF_ZP_PTR_TO_TOS :: ZP_PTR (CELL1) -> Stack
;;
;;   VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRx         :: Stack -> ZP_PTRx
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

   ;; these two must be adjacent (for some code to work)

   ;; (byte-const ZP_CELL_TOS              $d9)  ;; current offset to tos $fe = stack empty, $00 = cell0, $02 = cell1, $04 = cell2 ...

   ;; (byte-const ZP_CELL0                 $da) ;; low tagged, high  (there are no untagged values on the stack)
   ;; (byte-const ZP_CELL1                 $dc)
   ;; (byte-const ZP_CELL2                 $de)
   ;; (byte-const ZP_CELL3                 $e0)
   ;; (byte-const ZP_CELL4                 $e2)
   ;; (byte-const ZP_CELL5                 $e4)
   ;; (byte-const ZP_CELL6                 $e6)
   ;; (byte-const ZP_CELL7                 $e8)

   (byte-const ZP_CELL_STACK_TOS         $dc) ;; byte (fe = empty stack, 0 = first element, 2 = second element, 4 = third element ...)

   (byte-const ZP_PTR_TAGGED             $dd) ;; dd = low byte with tag bits , tagged low bytes are 2 appart, to use same offset as ZP_PTR <-> ZP_PTR2
   (byte-const ZP_PTR2_TAGGED            $df) ;; df = low byte with tag bits

   (byte-const ZP_TEMP                   $de) ;; may not be used after sub calls (just within a routine)

   ;; the following six bytes need to be continuous, since they are saved into the call frame
   (byte-const ZP_VM_PC                  $e0)
   (byte-const ZP_PARAMS_PTR             $e2) ;; pointer to first parameter in call-frame
   (byte-const ZP_LOCALS_PTR             $e4) ;; pointer to first local in call-frame
   (byte-const ZP_CELL_STACK_BASE_PTR    $e6) ;; e6..e7 (pointer to the base of the eval stack of the currently running function

   (byte-const ZP_CALL_FRAME             $f1) ;; f1..f2 <- may be not needed (zp_locals_ptr always = zp_call_frame + 6)

   (byte-const ZP_PTR                    $fb)   ;; fb = low byte (with out tag bits), fc = high byte,   tagged low byte is held in ZP_PTR_TAGGED
   (byte-const ZP_PTR2                   $fd)   ;; fd = low byte (with out tag bits), fe = high byte
   ))

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

(define ZP_PTR                  (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PTR"))
(define ZP_PTR_TAGGED           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PTR_TAGGED"))
(define ZP_PTR2                 (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PTR2"))
(define ZP_PTR2_TAGGED          (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PTR2_TAGGED"))
;; (define ZP_CELL_TOS             (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL_TOS"))
;; (define ZP_CELL0                (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL0"))
(define ZP_CALL_FRAME           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CALL_FRAME"))
(define ZP_CELL_STACK_BASE_PTR  (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL_STACK_BASE_PTR"))
(define ZP_CELL_STACK_TOS       (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_CELL_STACK_TOS"))
(define ZP_TEMP                 (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_TEMP"))
(define ZP_VM_PC                (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_VM_PC"))
(define ZP_LOCALS_PTR           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_LOCALS_PTR"))
(define ZP_PARAMS_PTR           (ast-const-get VM_MEMORY_MANAGEMENT_CONSTANTS "ZP_PARAMS_PTR"))

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
          (LDY ZP_CELL_STACK_TOS)
          (BMI STACK_EMPTY_ERROR__VM_CELL_STACK_POP)

          ;; now check whether deallocation needs to take place (that is the cell being popped is a ptr)
          (LDA (ZP_CELL_STACK_BASE_PTR),y) ;; tagged low byte
          (AND !$03)
          (BEQ DO_POP_2DEY__VM_CELL_STACK_POP) ;; is no pointer => jump

          ;; is a pointer?
          (LSR)
          (BCC IS_CELL_PAIR_PTR__VM_CELL_STACK_POP)

          ;; is a cell-ptr
          ;; not implemented yet: should decrement cell-ptr and check whether it can be garbage collected!

   (label STACK_EMPTY_ERROR__VM_CELL_STACK_POP)
          (BRK) ;; not implemented yet

   (label IS_CELL_PAIR_PTR__VM_CELL_STACK_POP)
          ;; move cell-pair-ptr (tos) -> zp_ptr
          (DEY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)  ;; high byte
          (BEQ DO_POP__VM_CELL_STACK_POP) ;; hight byte = 0 => is nil
          (STA ZP_PTR+1)      ;; to zp_ptr+1
          (INY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)  ;; tagged low byte
          (AND $fc)           ;; remove low 2 bits
          (STA ZP_PTR)        ;; to zp_ptr
          ;; no need for copying tagged low byte, since I already know it is a cell-pair-ptr
          (JSR VM_REFCOUNT_DECR_CELL_PAIR) ;; decrement and gc if necessary

   (label VM_CELL_STACK_POP__NO_GC) ;; entry for just popping!
          (LDY ZP_CELL_STACK_TOS)   ;; restore tos

   (label DO_POP_2DEY__VM_CELL_STACK_POP)
          (DEY)               ;; dex (such that in total x-=3)

   (label DO_POP__VM_CELL_STACK_POP)
          (DEY) ;; y was already decremented => dec 2x
          (STY ZP_CELL_STACK_TOS) ;; store new tos
          (RTS)))

(module+ test #| vm_cell_stack_pop |#

  (define test-vm_cell_stack_pop-a-state-after
    (run-code-in-test
     (list (JSR VM_CELL_STACK_PUSH_NIL)
           (JSR VM_CELL_STACK_POP)
           (JSR VM_CELL_STACK_POP) ;; stops at brk in this routine
           (LDA !$00)              ;; is never run
           (STA ZP_CELL_STACK_TOS))))

  (check-equal? (memory-list test-vm_cell_stack_pop-a-state-after ZP_CELL_STACK_TOS ZP_CELL_STACK_TOS)
                '(#xff)
                "tos = ff (empty)"))

;; push a cell in A/X (high/low) onto the cell-stack
;; input: stack
;;        X = low byte,  (tagged)
;;        A = high byte,
;; output: stack++[nil]
;; registers: A  ?
;;            Y  TOS+2
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
   ;;         flags: result of lda high byte of celly
   ;; check stack full!
   (label VM_CELL_STACK_PUSH_CELLy_OF_ZP_PTR)
          (LDA (ZP_PTR),y)
          (TAX)
          (INY)
          (LDA (ZP_PTR),y)
          (CLC)
          (BCC VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_ZP_PTR2)
          (LDY !$02)
          (BNE VM_CELL_STACK_PUSH_ZP_PTRy)
   (label VM_CELL_STACK_PUSH_ZP_PTR)
          (LDY !$00)

   ;; push zp_ptry (y=0 zp_ptr, y=2 zp_ptr1) onto cell-stack
   ;; input:  stack
   ;;         zp_ptry
   ;; output: stack++[zp_ptry] (uses zp_ptr_tagged, too)
   ;;         A  high byte of zp_ptr
   ;;         X  TOS
   ;;         Y  0|2 (orig Y * 2)
   ;; check stack full!
   (label VM_CELL_STACK_PUSH_ZP_PTRy)
          (LDX ZP_PTR_TAGGED,y)
          (LDA ZP_PTR+1,y)
          (CLC)
          (BCC VM_CELL_STACK_PUSH)

   ;; ints are saved high byte first, then low byte !!!!
   ;; input:  stack
   ;;         X = high byte of int (max 31 = $1f) (stored in low byte (tagged) position)
   ;;         A = low byte of int (0..255) (stored in high byte (untagged) position)
   ;; output: stack++[INT]
   ;; check stack full!
   (label VM_CELL_STACK_PUSH_INT)
          (TAY)
          (TXA)
          (ASL A)
          (ASL A)
          (AND !$7c)           ;; mask out top and two low bits!
          (TAX)
          (TYA)
          (CLC)
          (BCC VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_INT_m1)
          (LDA !$ff) ;; 1f << 2
          (LDX !$7c)
          (BNE VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_INT_2)
          (LDA !$02)
          (LDX !$00)
          (BEQ VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_INT_1)
          (LDA !$01)
          (LDX !$00)
          (BEQ VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_INT_0)
          (LDA !$00)
          (TAX)
          (BEQ VM_CELL_STACK_PUSH)

   (label VM_CELL_STACK_PUSH_NIL)
          (LDX !<TAGGED_NIL)
          (LDA !>TAGGED_NIL)

   ;; ---------------------------------------- (a=high byte, x=tagged low)
   (label VM_CELL_STACK_PUSH)
          (LDY ZP_CELL_STACK_TOS)

          ;; check that stack pointer does not run out of bound (over the page)
          (CPY !$fd)
          (BNE NO_ERROR__VM_CELL_STACK_PUSH)

          (BRK)

   (label NO_ERROR__VM_CELL_STACK_PUSH)
          (INY)
          (STA (ZP_CELL_STACK_BASE_PTR),y)       ;; write high byte! (untagged)
          (INY)
          (TXA)
          (STA (ZP_CELL_STACK_BASE_PTR),y)      ;; write low byte (tagged)
          (STY ZP_CELL_STACK_TOS)      ;; set new tos
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

;; input: A = lowbyte of int
;;        X = tagged highbyte of int
(define VM_CELL_STACK_WRITE_INT_TO_TOS
  (list
   (label VM_CELL_STACK_WRITE_INT_1_TO_TOS)
          (LDX !$01)
          (LDA !$00)
          (BEQ VM_CELL_STACK_WRITE_INT_TO_TOS)

   (label VM_CELL_STACK_WRITE_INT_0_TO_TOS)
          (LDA !$00)
          (TAX)

   ;; ----------------------------------------
   (label VM_CELL_STACK_WRITE_INT_TO_TOS)
          (LDY ZP_CELL_STACK_TOS)
          (ASL A)
          (ASL A)
          (AND !$7c)            ;; mask out top and two low bits!
          (STA (ZP_CELL_STACK_BASE_PTR),y)   ;; write tagged int high byte first
          (DEY)
          (TXA)
          (STA (ZP_CELL_STACK_BASE_PTR),y)   ;; write untagged int low byte
          (RTS)))

(module+ test #| vm_cell_push_int |#
  (define test-vm_cell_stack_push_int-a-state-after
    (run-code-in-test
     (list (JSR VM_CELL_STACK_PUSH_INT_m1)
           (LDA !$00) ;; -4096
           (LDX !$10)
           (JSR VM_CELL_STACK_PUSH_INT)
           (JSR VM_CELL_STACK_PUSH_INT_1)
           (JSR VM_CELL_STACK_PUSH_INT_0)
           (LDA !$ff) ;; 4095
           (LDX !$0f)
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
      (LDA !$12)
      (LDX !$02)
      (JSR VM_CELL_STACK_PUSH_INT)                      ;;  push cell-int $0212

      (JSR VM_ALLOC_CELL_PAIR)
      (JSR VM_REFCOUNT_INCR_CELL_PAIR)
      (LDX !$00)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR)  ;; write cell-int $0212 -> car of allocated cell-pair

      (JSR VM_CELL_STACK_POP)
      (JSR VM_CELL_STACK_PUSH_NIL)                      ;; push cell-pair-ptr nil

      (LDX !$02)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR)  ;; write cell-pair-ptr nil -> cdr of allocated cell-pair

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
                '(#x04 #xcc)
                "zp_ptr => cd04 (first free pair cell)")
  (check-equal? (memory-list test-vm_cell_stack_push_celly_to_zp_ptr-a-state-after #xcc04 #xcc07)
                '(#x08 #x12 #x02 #x00)
                "zp_ptr => (int530 . nil)"))

;; write TOS into celly of cell-pair pointer to by zp_ptr (y=0 car-cell, y=2 cdr-cell)
;; input:  cell-stack
;;         x register (0 = car-cell, 2 = cdr-cell)
;;         zp_ptr pointing to the cells to write
;; output: cell-stack unchanged
;;         zp_ptr celly is set to tos cell
;;         a = ?
;; no stack empty check!
(define VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR
  (list
   (label VM_CELL_STACK_WRITE_TOS_TO_CELL1_OF_ZP_PTR)
          (LDX !$02)
          (BNE VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR)
   (label VM_CELL_STACK_WRITE_TOS_TO_CELL0_OF_ZP_PTR)
          (LDX !$00)

   ;;------------------------------------------------
   (label VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR)
          ;; move just tagged low byte and high byte, since CELLy has no untagged byte!!
          (LDY ZP_CELL_STACK_TOS)

   (label VM_CELL_STACK_WRITE_TOS_TO_ZP_PTR__XY_SET)
          (STX ZP_TEMP)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)
          (PHA) ;; STA ZP_TEMP2
          (DEY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)
          (LDY ZP_TEMP)
          (INY)
          (STA (ZP_PTR),y) ;; cell high byte
          (DEY)
          (PLA) ;; LDA ZP_TEMP2
          (STA (ZP_PTR),y) ;; cell low byte
          (RTS)))

(module+ test #| vm_cell_stack_write_tos_to_cellx_of_zp_ptr |#
  (define test-vm_cell_stack_write_tos_to_cellx_of_zp_ptr-a-state-after
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CELL_PAIR)
      (JSR VM_REFCOUNT_INCR_CELL_PAIR)
      (LDX !$02)
      (LDA !$12)
      (JSR VM_CELL_STACK_PUSH_INT)
      (LDX !$00)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR)
      (JSR VM_CELL_STACK_POP)
      (JSR VM_CELL_STACK_PUSH_NIL)
      (LDX !$02)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR)
      (JSR VM_CELL_STACK_POP))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_write_tos_to_cellx_of_zp_ptr-a-state-after)
                '("stack is empty"))
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_cellx_of_zp_ptr-a-state-after ZP_PTR (+ 1 ZP_PTR))
                '(#x04 #xcc)
                "zp_ptr => cd04 (first free pair cell)")
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_cellx_of_zp_ptr-a-state-after #xcc04 #xcc07)
                '(#x08 #x12 #x02 #x00)
                "zp_ptr => (int530 . nil)"))

;; write zp_ptry (x=0 zp_ptr, x=2 zp_ptr1) into top of cell-stack
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
          (LDX !$02)
          (BNE VM_CELL_STACK_WRITE_ZP_PTRy_TO_TOS)
   (label VM_CELL_STACK_WRITE_ZP_PTR_TO_TOS)
          (LDX !$00)

   ;; ----------------------------------------
   (label VM_CELL_STACK_WRITE_ZP_PTRy_TO_TOS)
          (LDY ZP_CELL_STACK_TOS)

          (LDA ZP_PTR_TAGGED,x)
          (STA (ZP_CELL_STACK_BASE_PTR),y)    ;; tagged low byte
          (DEY)
          (LDA ZP_PTR+1,x) ;; high byte of ptr
          (STA (ZP_CELL_STACK_BASE_PTR),y)

          (RTS)))

(define VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS
  (list
   (label VM_CELL_STACK_WRITE_CELL1_OF_ZP_PTR_TO_TOS)
          (LDY !$02)
          (BNE VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS)
   (label VM_CELL_STACK_WRITE_CELL0_OF_ZP_PTR_TO_TOS)
          (LDY !$00)

   ;; ----------------------------------------
   (label VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS)
          ;; write tagged low byte and high byte
          (LDA (ZP_PTR),y) ;; cell low byte
          (STA ZP_TEMP)
          (INY)
          (LDA (ZP_PTR),y) ;; cell high byte
          (LDY ZP_CELL_STACK_TOS)
          (STA (ZP_CELL_STACK_BASE_PTR),y) ;; store high first
          (INY)
          (PLA)
          (STA (ZP_CELL_STACK_BASE_PTR),y) ;; store low there after
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
                  "cell-pair-ptr $cc04"))
  (check-equal? (memory-list test-vm_cell_stack_push_zp_ptry-a-state-after ZP_PTR (+ ZP_PTR 1))
                '(#x04 #xcc)
                "zp_ptr => cc04 (first free pair cell)")
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
      (LDY !$02)
      (JSR VM_CELL_STACK_PUSH_ZP_PTRy))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_push_zp_ptry-b-state-after)
                '("stack holds 1 item"
                  "cell-pair-ptr $cc04"))
  (check-equal? (memory-list test-vm_cell_stack_push_zp_ptry-b-state-after ZP_PTR2 (+ 1 ZP_PTR2))
                '(#x04 #xcc)
                "zp_ptr2 => cc04 (first free pair cell)")
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
(define VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRx
  (list
   (label VM_CELL_STACK_WRITE_TOS_TO_ZP_PTR2)
          (LDX !$02)
          (BNE VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRx)
   (label VM_CELL_STACK_WRITE_TOS_TO_ZP_PTR)
          (LDX !$00)

   ;;------------------------------------------------
   (label VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRx)
          (LDY ZP_CELL_STACK_TOS)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)    ;; low byte tagged
          (STA ZP_TEMP)
          (DEY)
          (LDA (ZP_CELL_STACK_BASE_PTR),y)    ;; high byte
          (STA ZP_PTR+1,x)
          (LDA ZP_TEMP)
          (AND !$fc)
          (STA ZP_PTR,x)

          (LDA ZP_TEMP)
          (STA ZP_PTR_TAGGED,x)
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

      (LDX !$00)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRx))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_write_tos_to_zp_ptry-a-state-after)
                '("stack holds 1 item"
                  "cell-pair-ptr $cc04"))
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_zp_ptry-a-state-after ZP_PTR (+ 1 ZP_PTR))
                '(#x04 #xcc)
                "zp_ptr => cc04 (first free pair cell)")
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

      (LDX !$02)
      (JSR VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRx))))

  (check-equal? (vm-stack->strings test-vm_cell_stack_write_tos_to_zp_ptry-b-state-after)
                '("stack holds 1 item"
                  "cell-pair-ptr $cc04"))
  (check-equal? (memory-list test-vm_cell_stack_write_tos_to_zp_ptry-b-state-after ZP_PTR2 (+ 1 ZP_PTR2))
                '(#x04 #xcc)
                "zp_ptr2 => cc04 (first free pair cell)")
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
           (LDA !$00)
           (TAY)
    (label VM_INITIALIZE_MEMORY_MANAGER__LOOP)
           ;; highbyte of this address should be using the constant NEXT_FREE_PAGE_PAGE
           ;; (STA $cf00,y) ;; encoded directly in the next couple of bytes
           ;; (car (ast-opcode-cmd-bytes (STA $cf00,y)))
           (byte 153 0) (byte-ref NEXT_FREE_PAGE_PAGE)
           (INY)
           (BNE VM_INITIALIZE_MEMORY_MANAGER__LOOP)

           ;; initialize cell stack
           (LDA !$20)
           (JSR  VM_ALLOC_CALL_FRAME)
           (STY ZP_PARAMS_PTR)
           (STY ZP_LOCALS_PTR)
           (STY ZP_CELL_STACK_BASE_PTR)
           (STX ZP_PARAMS_PTR+1)
           (STX ZP_LOCALS_PTR+1)
           (STX ZP_CELL_STACK_BASE_PTR+1)

           (LDX !$ff)          ;; negative and iny will produce 0
           (STX ZP_CELL_STACK_TOS)

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
                '(#x04 #xcc)
                "zp_ptr -> $cd04 = first free cell-pair on page $cd after initialization")
  (check-equal? (memory-list use-case-1-a-state-after #xcc01 #xcc01)
                '(#x01)
                "refcount for first cell-pair allocated = 1")
  (check-equal? (memory-list use-case-1-a-state-after #xcc04 #xcc07)
                '(#x00 #x00 #x00 #x00)
                "cell1=int0 cell2=int0")
  (check-equal? (memory-list use-case-1-a-state-after #xcfcc #xcfcc)
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

  (check-equal? (memory-list use-case-1-b-state-after #xcec5 #xcec6)
                '(#x04 #xcc )
                "root of free tree is cell-pair at $cd04")
  (check-equal? (memory-list use-case-1-b-state-after #xcc01 #xcc01)
                '(#x00)
                "refcount for cell-pair freed = 0")
  (check-equal? (memory-list use-case-1-b-state-after #xcfcc #xcfcc)
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

  (check-equal? (memory-list use-case-1-c-state-after ZP_PTR (+ 1 ZP_PTR))
                '(#x04 #xcc )
                "allocated cell-pair is reused cell-pair of free tree")
  (check-equal? (memory-list use-case-1-c-state-after #xcec5 #xcec6)
                '(#x00 #x00 )
                "root of free tree is initial again")
  (check-equal? (memory-list use-case-1-c-state-after #xcc01 #xcc01)
                '(#x00)
                "refcount for (reused) cell-pair = 1")
  (check-equal? (memory-list use-case-1-c-state-after #xcfcc #xcfcc)
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
     ;;   zp_ptr[cc08|1] (int0 . ->[cc04|1](int0 . nil))
     ;; notation:
     ;;   [<mem-location>|<ref-count>]
     ;;   (<car-cell> . <cdr-cell>)
     ;;   intX, nil :: atomic value cells
     ;;   -> :: cell-ptr
     ))

  (define use-case-2-a-state-after
    (run-code-in-test use-case-2-a-code))

  (check-equal? (memory-list use-case-2-a-state-after ZP_PTR (+ 3 ZP_PTR))
                '(#x08 #xcc #x04 #xcc)
                "case 2a: zp_ptr -> $cc08, zp_ptr2 -> $cc04 = first two free cell-pairs on page $cc after initialization")
  (check-equal? (memory-list use-case-2-a-state-after #xcc01 #xcc0b)
                '(#x01 #x01 #x00      ;; refcounts
                  #x00 #x00 #x02 #x00 ;; tail cell
                  #x00 #x00 #x06 #xcc ;; head cell
                  )
                "case 2a: cell-pairs contain (int0 . -> next cell), (int 0 . nil)")

  (define use-case-2-b-code
    (append use-case-2-a-code ;; zp_ptr[cc08|1] (int0 . ->[cc04|1](int0 . nil))
            (list
             (JSR VM_REFCOUNT_DECR_CELL_PAIR)
             ;; now:
             ;;   free_tree -> [cc08|0] (int0 . ->[cc04|1] (int0 . nil))
             )))

  (define use-case-2-b-state-after
    (run-code-in-test use-case-2-b-code))

  (check-equal? (memory-list use-case-2-b-state-after #xcc01 #xcc0b)
                '(#x01 #x00 #x00      ;; refcounts
                  #x00 #x00 #x02 #x00 ;; cc04 (old tail of list)
                  #x00 #x00 #x06 #xcc ;; cc08 (old head of list)
                  )
                "case 2b: refcount cd08 = 0 (head), cc04 unchanged (tail)")
  (check-equal? (memory-list use-case-2-b-state-after #xcec5 #xcec6) ;;
                '(#x08 #xcc )
                "case 2b: root of free tree is cell-pair at $cc08")

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
                '(#x08 #xcc)
                "case 2c: zp_ptr -> $cc08, reallocated")
  (check-equal? (memory-list use-case-2-c-state-after #xcc01 #xcc0b)
                '(#x00 #x01 #x00      ;; refcounts
                  #x00 #x00 #x02 #x00 ;; tail cell
                  #x00 #x00 #x06 #xcc ;; head cell
                  )
                "case 2c: refcount cc08 = 1 reallocated, refcount cc04 = 0 (original tail, now in the free tree)")
  (check-equal? (memory-list use-case-2-c-state-after #xcec5 #xcec6) ;;
                '(#x04 #xcc )
                "case 2c: root of free tree is cell-pair at $cc04"))

;; input:  A = size (needs to include 32 bytes cell-stack + 8 byte (pc, old params, old locals, old cell stack base ptr) + 2 * #locals
;;         ZP_CELL_STACK_BASE_PTR
;;         ZP_CELL_STACK_TOS
;;         VM_FREE_CALL_STACK_PAGE
;; output: ZP_CALL_FRAME,
;;         Y/X=low-byte, high-byte
;; info: back linking previous page is automatically done through zp_params_ptr (pointing to the previous page, if so)
(define VM_ALLOC_CALL_FRAME
  (list
   ;; ----------------------------------------
   (label VM_ALLOC_CALL_FRAME)
          (STA ZP_TEMP)
          (LDX VM_FREE_CALL_STACK_PAGE) ;; get the tos page for call stacks
          (BEQ ALLOCATE_NEW_PAGE__VM_ALLOC_CALL_FRAME)

          (LDA ZP_CELL_STACK_TOS) ;; make sure to have Y loaded if page is reused
          (CLC)
          (ADC ZP_CELL_STACK_BASE_PTR)
          (TAY) ;; y = tos + base_ptr
          (CLC)
          (ADC ZP_TEMP)
          (BCC USE_GIVEN_PAGE__VM_ALLOC_CALL_FRAME) ;; still on same page

          ;; if this call frame (size) does not fit on existing page, continue to allocate a new page

   (label ALLOCATE_NEW_PAGE__VM_ALLOC_CALL_FRAME)
          (JSR VM_ALLOC_PAGE__PAGE_UNINIT) ;; A = new page
          ;; set this new page as the TOS for call stack allocation
          (STA VM_FREE_CALL_STACK_PAGE)
          (TAX)
          (LDY !$01) ;; first free byte is 02, but y is incremented

   (label USE_GIVEN_PAGE__VM_ALLOC_CALL_FRAME)
          (STX ZP_CALL_FRAME+1) ;; x = stack page
          (INY)
          (STY ZP_CALL_FRAME)   ;; new call frame start

          (RTS)))

(module+ test #| vm_alloc_call_frame |#
  (define alloc-call-frame-code
    (list
     (LDA !$80)
     (JSR VM_ALLOC_CALL_FRAME)))

  (define alloc-call-frame-state-after
    (run-code-in-test alloc-call-frame-code))

  (check-equal? (memory-list alloc-call-frame-state-after ZP_CALL_FRAME (add1 ZP_CALL_FRAME))
                (list #x02 #xcd)
                "pointer to allocated frame")

  (define alloc-call-frame-2times-code
    (list
     (LDA !$80)
     (JSR VM_ALLOC_CALL_FRAME)
     (LDA ZP_CALL_FRAME)
     (STA ZP_CELL_STACK_BASE_PTR)
     (LDA !$80) ;; does not fit again in this call frame
     (STA ZP_CELL_STACK_TOS) ;;
     (JSR VM_ALLOC_CALL_FRAME)))

  (define alloc-call-frame-2times-state-after
    (run-code-in-test alloc-call-frame-2times-code))

  (check-equal? (memory-list alloc-call-frame-2times-state-after ZP_CALL_FRAME (add1 ZP_CALL_FRAME))
                (list #x02 #xcc)
                "pointer to allocated frame")

  (define alloc-call-frame-2times-fitting-code
    (list
     (LDA !$20)
     (JSR VM_ALLOC_CALL_FRAME)
     (LDA ZP_CALL_FRAME)
     (STA ZP_CELL_STACK_BASE_PTR)
     (LDA !$1F) ;; stack used up $20 slots
     (STA ZP_CELL_STACK_TOS)
     (LDA !$20)
     (JSR VM_ALLOC_CALL_FRAME)))

  (define alloc-call-frame-2times-fitting-state-after
    (run-code-in-test alloc-call-frame-2times-fitting-code))

  (check-equal? (memory-list alloc-call-frame-2times-fitting-state-after ZP_CALL_FRAME (add1 ZP_CALL_FRAME))
                (list #x22 #xcd)
                "pointer to allocated frame"))

(define VM_CELL_STACK_WRITE_TOS_TO_PARAM_0
  (list
   (label VM_CELL_STACK_WRITE_TOS_TO_PARAM_0)
          (RTS)))


;; input:   A = number of parameters on the stack to be used in this call frame
;;          x = number of locals to allocate on call frame
;;          zp_cell_frame = freshly allocated frame that has min size x*2 + 6 + 32
;;          zp_vm_pc          -> saved into cell frame (for pop)
;;          zp_vm_params_ptr  -> saved into cell frame (for pop)
;;          zp_vm_locals_ptr  -> saved into cell frame (for pop)
;; oputput: zp_vm_params_ptr = pointer into the previous cell-stack
;;          xp_vm_locals_ptr = pointer into the current call frame locals
;;          zp_cell_stack_base_ptr = point into the current call frame (right after locals)
;;          zp_cell_stack_tos = 0
;; NO CHECK IS DONE, zp_vm_pc is not overwritten
(define VM_SAVE_EXEC_STATE_TO_CALL_FRAME
  (list
   (label VM_SAVE_EXEC_STATE_TO_CALL_FRAME)
          (ASL A) ;; # params * 2 (number of bytes)
          (STA ZP_TEMP)
          (LDA ZP_CELL_STACK_TOS)
          (SEC)
          (SBC ZP_TEMP)
          (STA ZP_TEMP) ;; keep offset to cell-stack-base-ptr for new parameter-ptr

          ;; copy 8 bytes
          (LDY !$07)
   (label LOOP__VM_SAVE_EXEC_STATE_TO_CALL_FRAME)
          (LDA ZP_VM_PC,y)
          (STA (ZP_CALL_FRAME),y)
          (DEY)
          (BPL LOOP__VM_SAVE_EXEC_STATE_TO_CALL_FRAME)

          ;; set new parameter pointer
          (LDA ZP_CELL_STACK_BASE_PTR+1)
          (STA ZP_PARAMS_PTR+1)
          (LDA ZP_CELL_STACK_BASE_PTR)
          (SEC)
          (ADC ZP_TEMP)
          (STA ZP_PARAMS_PTR)

          ;; set new ZP_LOCALS_PTR and ZP_CELL_STACK_BASE_PTR
          (LDA ZP_CALL_FRAME+1)
          (STA ZP_LOCALS_PTR+1)
          (STA ZP_CELL_STACK_BASE_PTR+1)
          (LDA ZP_CALL_FRAME)
          (CLC)
          (ADC !$08)
          (STA ZP_LOCALS_PTR)

          (TXA)
          (ASL A) ;; # of locals *2 = bytes
          (ADC ZP_LOCALS_PTR)
          (STA ZP_CELL_STACK_BASE_PTR)

          (LDA !$ff)
          (STA ZP_CELL_STACK_TOS) ;; set this one to 0

          (RTS)))

(module+ test #| vm_save_exec_state_to_call_frame |#

  ;; before
  ;; cd04 <- ZP_PARAMS_PTR
  ;; cd08 <- ZP_CALL_FRAME
  ;; cd10 <- ZP_LOCALS_PTR
  ;; cd16 <- ZP_CELL_STACK_BASE_PTR
  ;; cd1b <- ZP_CELL_STACK_TOS (base ptr + 05, 3 elements on stack)
  ;; cc06 <- ZP_VM_PC

  ;; after
  ;; cd18 <- ZP_PARAMS_PTR (2 params, cd18..cd19, cd1a..cd1b)
  ;; cd1c <- ZP_CALL_FRAME
  ;; cd24 <- ZP_LOCALS_PTR
  ;; cd2a <- ZP_CELL_STACK_BASE_PTR
  ;; cd29 <- ZP_CELL_STACK_TOS ($ff)
  ;; cc06 <- ZP_VM_PC (unchanged)

  (define test-save-exec-state-code
    (list
     ;; fill pointers that will be saved
     (LDX !$04)
     (STX ZP_PARAMS_PTR) ;; $04
     (LDX !$08)
     (STX ZP_CALL_FRAME)
     (LDX !$10)
     (STX ZP_LOCALS_PTR) ;; $10
     (LDX !$16)
     (STX ZP_CELL_STACK_BASE_PTR)
     (LDX !$05)
     (STX ZP_CELL_STACK_TOS)
     (INX)
     (STX ZP_VM_PC)     ;; $17

     (LDX ZP_CALL_FRAME+1) ;; $cd
     (STX ZP_PARAMS_PTR+1)
     (STX ZP_LOCALS_PTR+1)
     (STX ZP_CELL_STACK_BASE_PTR+1)
     (DEX)                      ;; $cc
     (STX ZP_VM_PC+1)

     ;; allocate new call frame space that can hold $20 bytes
     (LDA !$20)
     (JSR VM_ALLOC_CALL_FRAME)

     (LDA !$02) ;; two params
     (LDX !$03) ;; three locals
     (JSR VM_SAVE_EXEC_STATE_TO_CALL_FRAME)
     ))

  (define test-save-exec-state-state-after
    (run-code-in-test test-save-exec-state-code))

  (check-equal? (memory-list test-save-exec-state-state-after ZP_PARAMS_PTR (add1 ZP_PARAMS_PTR))
                (list #x18 #xcd))
  (check-equal? (memory-list test-save-exec-state-state-after ZP_CALL_FRAME (add1 ZP_CALL_FRAME))
                (list #x1c #xcd))
  (check-equal? (memory-list test-save-exec-state-state-after ZP_LOCALS_PTR (add1 ZP_LOCALS_PTR))
                (list #x24 #xcd))
  (check-equal? (memory-list test-save-exec-state-state-after ZP_CELL_STACK_BASE_PTR (add1 ZP_CELL_STACK_BASE_PTR))
                (list #x2a #xcd))
  (check-equal? (memory-list test-save-exec-state-state-after ZP_CELL_STACK_TOS ZP_CELL_STACK_TOS)
                (list #xff))
  (check-equal? (memory-list test-save-exec-state-state-after ZP_VM_PC (add1 ZP_VM_PC))
                (list #x06 #xcc)))

;; zp_params_ptr := zp_cell_stack_base_ptr + zp_cell_stack_tos
;; if zp_params_ptr is on a different page than zp_call_frame,
;;    free page of zp_call_frame
;;      vm_free_call_stack_page = page of zp_params_ptr
;;      vm_free_slot_for_page,x = ??
;; restore zp_params_ptr
;; restore zp_locals_ptr
;; restore zp_vm_pc
;; zp_call_frame = zp_locals_ptr - 6
;;
;; input:  VM_FREE_CALL_STACK_PAGE
;;         VM_FREE_SLOT_FOR_PAGE,x
;; output: VM_FREE_CALL_STACK_PAGE
;;         VM_FREE_SLOT_FOR_PAGE,x
(define VM_POP_CALL_FRAME
  (list
   (label VM_POP_CALL_FRAME)
          (JSR VM_CELL_STACK_WRITE_TOS_TO_PARAM_0)

          (LDA ZP_CALL_FRAME+1)         ;; get current page
          (CMP ZP_PARAMS_PTR+1)         ;; compare with old page
          (BEQ SAME_PAGE__VM_POP_CALL_FRAME)

          ;; deallocate current page first
          (JSR VM_FREE_PAGE)
          (LDA ZP_PARAMS_PTR+1)         ;; get old page
          (STA VM_FREE_CALL_STACK_PAGE) ;; store old page as current (free) page

   (label SAME_PAGE__VM_POP_CALL_FRAME)
          ;; start to calculate tos of cell-stack
          (LDA ZP_PARAMS_PTR)
          (STA ZP_CELL_STACK_TOS) ;; needs to subtract base ptr (which is available after the following copy loop

          (LDY !$07)

   (label LOOP__VM_POP_CALL_FRAME)
          (LDA (ZP_CALL_FRAME),y)
          (STA ZP_VM_PC,y)
          (DEY)
          (BPL LOOP__VM_POP_CALL_FRAME)

          ;; finish calc of new stack
          (LDA ZP_CELL_STACK_TOS)
          (SEC)
          (SBC ZP_CELL_STACK_BASE_PTR)
          (STA ZP_CELL_STACK_TOS)
          (INC ZP_CELL_STACK_TOS) ;; point to the tagged lowbyte

          ;; set ZP_CALL_FRAME
          (LDA ZP_LOCALS_PTR+1)
          (STA ZP_CALL_FRAME+1)
          (LDA ZP_LOCALS_PTR)
          (SEC)
          (SBC !$08)
          (STA ZP_CALL_FRAME)

          (RTS)))

(module+ test #| vm_pop_call_frame |#
  ;; before
  ;; cd18 <- ZP_PARAMS_PTR (2 params, cd18..cd19, cd1a..cd1b)
  ;; cd1c <- ZP_CALL_FRAME
  ;; cd24 <- ZP_LOCALS_PTR
  ;; cd2a <- ZP_CELL_STACK_BASE_PTR
  ;; cd29 <- ZP_CELL_STACK_TOS ($ff)
  ;; cc06 <- ZP_VM_PC (unchanged)

  ;; after
  ;; cd04 <- ZP_PARAMS_PTR
  ;; cd08 <- ZP_CALL_FRAME
  ;; cd10 <- ZP_LOCALS_PTR
  ;; cd16 <- ZP_CELL_STACK_BASE_PTR
  ;; cd19 <- ZP_CELL_STACK_TOS (base ptr + 03, 2 elements on stack, 1 remaining + 1 result)
  ;; cc06 <- ZP_VM_PC

  (define test-pop-call-frame-code
    (list
     ;; fill pointers that will be saved
     (LDX !$04)
     (STX ZP_PARAMS_PTR) ;; $04
     (LDX !$08)
     (STX ZP_CALL_FRAME)
     (LDX !$10)
     (STX ZP_LOCALS_PTR) ;; $10
     (LDX !$16)
     (STX ZP_CELL_STACK_BASE_PTR)
     (LDX !$05)
     (STX ZP_CELL_STACK_TOS)
     (INX)
     (STX ZP_VM_PC)     ;; $17

     (LDX ZP_CALL_FRAME+1) ;; $cd
     (STX ZP_PARAMS_PTR+1)
     (STX ZP_LOCALS_PTR+1)
     (STX ZP_CELL_STACK_BASE_PTR+1)
     (DEX)                      ;; $cc
     (STX ZP_VM_PC+1)

     ;; allocate new call frame space that can hold $20 bytes
     (LDA !$20)
     (JSR VM_ALLOC_CALL_FRAME)

     (LDA !$02) ;; two params
     (LDX !$03) ;; three locals
     (JSR VM_SAVE_EXEC_STATE_TO_CALL_FRAME)

     (JSR VM_POP_CALL_FRAME)
     ))

  (define test-pop-call-frame-state-after
    (run-code-in-test test-pop-call-frame-code))

  (check-equal? (memory-list test-pop-call-frame-state-after ZP_PARAMS_PTR (add1 ZP_PARAMS_PTR))
                (list #x04 #xcd))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_CALL_FRAME (add1 ZP_CALL_FRAME))
                (list #x08 #xcd))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_LOCALS_PTR (add1 ZP_LOCALS_PTR))
                (list #x10 #xcd))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_CELL_STACK_BASE_PTR (add1 ZP_CELL_STACK_BASE_PTR))
                (list #x16 #xcd))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_CELL_STACK_TOS ZP_CELL_STACK_TOS)
                (list #x03))
  (check-equal? (memory-list test-pop-call-frame-state-after ZP_VM_PC (add1 ZP_VM_PC))
                (list #x06 #xcc)))

(define vm-memory-manager
  (append VM_MEMORY_MANAGEMENT_CONSTANTS
          VM_INITIALIZE_MEMORY_MANAGER
          ;; VM_ALLOC_PAGE_JUMP_TABLE
          ;; VM_ALLOC_PAGE

          VM_FREE_PAGE
          VM_ALLOC_PAGE__LIST_CELL_PAIRS
          VM_ALLOC_PAGE__PAGE_UNINIT
          ;; VM_ALLOC_PAGE__CALL_FRAME
          VM_ALLOC_CELL_PAIR_ON_PAGE

          VM_REFCOUNT_DECR
          VM_REFCOUNT_DECR_CELL_PAIR
          VM_REFCOUNT_INCR_CELL_PAIR

          VM_ALLOC_CELL_PAIR

          VM_ALLOC_CALL_FRAME
          VM_SAVE_EXEC_STATE_TO_CALL_FRAME
          VM_POP_CALL_FRAME

          VM_FREE_NON_ATOMIC
          VM_FREE_CELL_PAIR

          VM_CELL_STACK_WRITE_TOS_TO_PARAM_0

          VM_CELL_STACK_WRITE_INT_TO_TOS
          VM_CELL_STACK_WRITE_TOS_TO_CELLx_OF_ZP_PTR
          VM_CELL_STACK_WRITE_CELLy_OF_ZP_PTR_TO_TOS
          VM_CELL_STACK_WRITE_TOS_TO_ZP_PTRx
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

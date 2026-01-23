#lang racket/base

(provide vm-call-frame->strings              ;; provide the call frame as string
         vm-call-frame-return-pc             ;; get the pc of the code that is returned to

         VM_POP_CALL_FRAME                   ;; pop the topmost call frame
         ;; VM_REFCOUNT_DECR_CURRENT_LOCALS     ;; decrement refcounts of all locals (where applicable) (can be done as part of the pop frame always)
         VM_PUSH_CALL_FRAME                  ;; push a call frame onto the call frame stack, initializes locals for the function being called
         VM_ALLOC_CALL_FRAME                 ;; allocate a new call frame (used to init call frame)
         VM_INIT_CALL_FRAME_STACK

         vm-call-frame-code)                 ;; all code of this module

#|

  implementation of a call frame stack for vm calls
  implemented as stack with fat/slim stack frames
  using a cell-stack

 |#

(require (only-in racket/list range)
         (only-in racket/string string-join)
         "../../6510.rkt"
         (only-in "../../6510-utils.rkt"
                  word->hex-string
                  byte->hex-string)
         (only-in "../../tools/6510-interpreter.rkt"
                  peek-word-at-address
                  peek)
         (only-in "../../tools/data-tools.rkt"
                  high-byte
                  bytes->int)
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
         (only-in "../vm-inspector-utils.rkt"
                  vm-cell->string)
         (only-in "../vm-interpreter-loop.rkt"
                  ZP_VM_PC)
         (only-in "./vm-cell-stack.rkt"
                  INIT_CELLSTACK_PAGE_X
                  vm-cell-stack-code)
         (only-in "./vm-m1-slots.rkt"
                  DEC_REFCNT_M1_SLOT_RZ
                  vm-m1-slot-code)
         (only-in "./vm-memory-map.rkt"
                  VM_MEMORY_MANAGEMENT_CONSTANTS)
         [only-in "./vm-memory-map.rkt"
                  ZP_CALL_FRAME_LB
                  ZP_CALL_FRAME_HB
                  ZP_FUNC_PTR
                  ZP_LOCALS_LB_PTR
                  ZP_LOCALS_HB_PTR
                  ZP_LOCALS_TOP_MARK
                  ZP_CALL_FRAME_TOP_MARK]
         (only-in "./vm-pages.rkt"
                  VM_ALLOCATE_NEW_PAGE
                  VM_DEALLOCATE_PAGE
                  VM_INIT_PAGE_MEMORY_MANAGER
                  vm-pages-code)
         (only-in "./vm-register-functions.rkt"
                  vm-register-functions-code))

(module+ test #| after mem init |#
  (require "../../6510-test-utils.rkt"
           (only-in "../../ast/6510-relocator.rkt"
                    code-len)
           (only-in "../../tools/6510-interpreter.rkt"
                    memory-list)
           (only-in "../test-utils.rkt"
                    regression-test)
           (only-in "../vm-inspector-utils.rkt"
                    vm-page->strings)
           (only-in "../vm-interpreter-loop.rkt"
                    VM_INTERPRETER_ZP)
           (only-in "./vm-memory-manager-test-utils.rkt"
                    run-code-in-test-on-code
                    remove-labels-for))

  (define PAGE_AVAIL_0 #xcf)
  (define PAGE_AVAIL_0_W #xcf00)
  (define PAGE_AVAIL_1 #xce)
  (define PAGE_AVAIL_1_W #xce00)

  (define PAGE_CALL_FRAME #xcf)
  (define PAGE_CALL_FRAME_W #xcf00)
  (define PAGE_CALL_FRAME_HB #xce)
  (define PAGE_LOCALS_LB #xcf)
  (define PAGE_LOCALS_HB #xce)

  (define (wrap-code-for-test bc complete-code (mocked-code-list (list)))
    (append
            (list
             (org #xa000)
             (JSR VM_INIT_PAGE_MEMORY_MANAGER_N20)
             (label TEST_ENTRY))
            bc
            (list (BRK))
            VM_MEMORY_MANAGEMENT_CONSTANTS
            vm-pages-code
            vm-cell-stack-code
            vm-m1-slot-code
            vm-register-functions-code
            (remove-labels-for complete-code (filter (lambda (ast-cmd) (ast-label-def-cmd? ast-cmd)) mocked-code-list))
            VM_INTERPRETER_ZP
            (list (label VM_INTERPRETER_OPTABLE))))

  (define (run-code-in-test bc (debug #f) #:mock (mocked-code-list (list)))
    (run-code-in-test-on-code 
     (wrap-code-for-test bc vm-call-frame-code mocked-code-list)
     debug)))

(define (vm-call-frame-return-pc state)
  (define call-frame-lb-ptr (peek-word-at-address state ZP_CALL_FRAME_LB))
  (define top-mark          (peek state ZP_CALL_FRAME_TOP_MARK))
  (define pc-offset         (peek state (+ call-frame-lb-ptr top-mark 1)))
  (define pc-page           (peek state (+ call-frame-lb-ptr top-mark 2)))
  (bytes->int pc-offset pc-page))

;; report call-frame specific data:
;;   return-pc
;;   return-function-ptr
;;   return-locals-ptr  (lb, hb)
(define (vm-call-frame-tos->string state #:locals (locals-too #t) #:locals-addr (locals-addr #f))
  (define call-frame-lb-ptr (peek-word-at-address state ZP_CALL_FRAME_LB))
  (define call-frame-hb-ptr (peek-word-at-address state ZP_CALL_FRAME_HB))
  (define top-mark          (peek state ZP_CALL_FRAME_TOP_MARK))
  (define pc-offset         (peek state (+ call-frame-lb-ptr top-mark 1)))
  (define locals-offset     (peek state (+ call-frame-hb-ptr top-mark 1)))
  (define locals-lb-page    (high-byte call-frame-lb-ptr))
  (define locals-hb-page    (high-byte call-frame-hb-ptr))
  (define locals-lb-ptr     (bytes->int locals-offset locals-lb-page)) ;; TODO respect page change (current topmark < locals_offset)
  (define locals-hb-ptr     (bytes->int locals-offset locals-hb-page)) ;; TODO respect page change
  (define pc-page           (peek state (+ call-frame-lb-ptr top-mark 2)))
  (define pc                (+ pc-offset (* 256 pc-page)))
  (define func-offset       (peek state (+ call-frame-hb-ptr top-mark 2)))
  (define func-page         (+ pc-page (if (> func-offset pc-offset) -1 0)))
  (define func              (+ func-offset (* 256 func-page)))
  (append (list (format "return pc:         $~a" (word->hex-string pc))
                (format "return func-ptr:   $~a" (word->hex-string func))
                (if (zero? locals-offset)
                    "return locals-ptr: undefined"
                    (format "return locals-ptr: $~a, $~a (lb, hb)"
                            (word->hex-string locals-lb-ptr)
                            (word->hex-string locals-hb-ptr))))

          (if locals-too
              (vm-call-frame-locals->string state #:addr locals-addr)
              (list))))

;; provider string list for all locals of the current running function in the call frame
(define (vm-call-frame-locals->string state #:addr (addr #f))
  (define call-frame-lb-ptr (peek-word-at-address state ZP_CALL_FRAME_LB))
  (define call-frame-hb-ptr (peek-word-at-address state ZP_CALL_FRAME_HB))
  (define top-mark          (peek state ZP_CALL_FRAME_TOP_MARK))
  (define pc-offset         (peek state (+ call-frame-lb-ptr top-mark 1)))
  (define pc-page           (peek state (+ call-frame-lb-ptr top-mark 2)))
  (define func-offset       (peek state (+ call-frame-hb-ptr top-mark 2)))
  (define func-page         (+ pc-page (if (> func-offset pc-offset) -1 0)))
  (define func              (+ func-offset (* 256 func-page)))
  (define locals-n          (bitwise-and #x0f (peek state func)))
  (if (zero? locals-n)
      (list "no-locals")
      (map (lambda (local)
             (define lb-loc (+ call-frame-lb-ptr 3 local top-mark))
             (define hb-loc (+ call-frame-hb-ptr 3 local top-mark))
             (if addr
                 (format
                  "local ~a: ~a (located lb: $~a, hb: $~a)"
                  local
                  (vm-cell->string (peek state lb-loc)
                                  (peek state hb-loc))
                  (word->hex-string lb-loc)
                  (word->hex-string hb-loc))
                 (format
                  "local ~a: ~a"
                  local
                  (vm-cell->string (peek state lb-loc)
                                  (peek state hb-loc)))))
           (range locals-n))))

;; write call stack status
(define (vm-call-frame->strings state)
  (append
   (list (format "call-frame-ptr:    $~a, $~a (lb, hb), topmark: ~a"
                 (word->hex-string (peek-word-at-address state ZP_CALL_FRAME_LB))
                 (word->hex-string (peek-word-at-address state ZP_CALL_FRAME_HB))
                 (byte->hex-string (peek state ZP_CALL_FRAME_TOP_MARK)))
         (format "program-counter:   $~a" (word->hex-string (peek-word-at-address state ZP_VM_PC)))
         (format "function-ptr:      $~a" (word->hex-string (peek-word-at-address state ZP_FUNC_PTR)))
         (format "locals-ptr:        $~a, $~a (lb, hb)"
                 (word->hex-string (bytes->int (peek state ZP_LOCALS_LB_PTR) (peek state (add1 ZP_LOCALS_LB_PTR))))
                 (word->hex-string (bytes->int (peek state ZP_LOCALS_HB_PTR) (peek state (add1 ZP_LOCALS_HB_PTR))))))
   (if (= #xfe (peek state ZP_CALL_FRAME_TOP_MARK))
       (list) ;; nothing on the stack
       (vm-call-frame-tos->string state #:locals #f))))

;; allocate a new call frame,
;; close the current call frame (set top mark)
;; and initialize the page accordingly
;; input:  ZP_CALL_FRAME_xB (LB and HB)
;; output: ZP_CALL_FRAME_xB (LB and HB)
;;         ZP_LOCALS_LB_PTR+1 (just the page)
;;         ZP_LOCALS_HB_PTR+1 (just the page)
;;         ZP_CALL_FRAME_TOP_MARK
;;         Y index for first byte available as payload (03) = ZP_CALL_FRAME_TOP_MARK
;;         the two allocated pages are initialized (@0 = page type, @ff = previous pages)
(define VM_INIT_CALL_FRAME_STACK '())
(define-vm-function-wol VM_ALLOC_CALL_FRAME
  (list
   (label VM_INIT_CALL_FRAME_STACK)
          (LDX !$00)
          (STX ZP_CALL_FRAME_LB+1)
          (STX ZP_CALL_FRAME_HB+1)

   (label VM_ALLOC_CALL_FRAME)
          ;; allocate completely new page of type $18
          (LDA !$18)
          (JSR VM_ALLOCATE_NEW_PAGE)
          (TXA)

          ;; init page as new call frame page (00 = page type, 01 = previous call frame page, 02 = top mark [uninitialized until full], rest uninitialized)
          (LDX ZP_CALL_FRAME_LB+1)         ;; keep old page in X
          (STA ZP_CALL_FRAME_LB+1)         ;; write new page into call-frame-ptr
          (STA ZP_LOCALS_LB_PTR+1)         ;; make sure locals page is set, too

          (LDY !$ff)                       ;; y = $ff
          (TXA)
          (STA (ZP_CALL_FRAME_LB),y)       ;; set previous page to current page of call frame

          (LDA !$18)
          (JSR VM_ALLOCATE_NEW_PAGE)
          (TXA)

          ;; init page as new call frame page (00 = page type, 01 = previous call frame page, 02 = top mark [uninitialized until full], rest uninitialized)
          (LDX ZP_CALL_FRAME_HB+1)         ;; keep old page in X
          (STA ZP_CALL_FRAME_HB+1)         ;; write new page into call-frame-ptr
          (STA ZP_LOCALS_HB_PTR+1)         ;; make sure locals page is set, too

          (LDY !$ff)                       ;; y = $ff
          (TXA)
          (STA (ZP_CALL_FRAME_HB),y)       ;; set previous page to current page of call frame

          (DEY)                            ;; y = $fe
          (STY ZP_CALL_FRAME_TOP_MARK)     ;; write top mark
          (RTS)))

(module+ test #| alloc call frame |#
  (define alloc-call-frame-test
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CALL_FRAME))
     #f))

  (regression-test
   alloc-call-frame-test
   "initial alloc call frame"
   (check-equal? (memory-list alloc-call-frame-test (+ 1 ZP_CALL_FRAME_HB))
                 (list PAGE_AVAIL_1)
                 "call frame hb on page")
   (check-equal? (memory-list alloc-call-frame-test (+ 1 ZP_CALL_FRAME_LB))
                 (list PAGE_AVAIL_0)
                 "call frame lb on page")
   (check-equal? (memory-list alloc-call-frame-test ZP_CALL_FRAME_TOP_MARK)
                 (list #xfe)
                 "topmark is set to $FE")
   (check-equal? (vm-page->strings alloc-call-frame-test PAGE_AVAIL_1)
                 (list "page-type:      call-frame page"
                       "previous page:  $00")
                 "call frame page (hb)")
   (check-equal? (vm-page->strings alloc-call-frame-test PAGE_AVAIL_0)
                 (list "page-type:      call-frame page"
                       "previous page:  $00")
                 "call frame page (lb)"))

  (define alloc-call-frame-2-test
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CALL_FRAME)
      (JSR VM_ALLOC_CALL_FRAME))
     #f))

  (regression-test
   alloc-call-frame-2-test
   "alloc multiple call frame"
   (check-equal? (memory-list alloc-call-frame-2-test (+ 1 ZP_CALL_FRAME_HB))
                 (list (- PAGE_AVAIL_1 2))
                 "call frame hb on page")
   (check-equal? (memory-list alloc-call-frame-2-test (+ 1 ZP_CALL_FRAME_LB))
                 (list (- PAGE_AVAIL_0 2))
                 "call frame lb on page")
   (check-equal? (memory-list alloc-call-frame-2-test ZP_CALL_FRAME_TOP_MARK)
                 (list #xfe)
                 "topmark is set to $FE")
   (check-equal? (vm-page->strings alloc-call-frame-2-test (- PAGE_AVAIL_1 2))
                 (list "page-type:      call-frame page"
                       (format "previous page:  $~a" (byte->hex-string PAGE_AVAIL_1)))
                 "call frame page (hb)")
   (check-equal? (vm-page->strings alloc-call-frame-2-test (- PAGE_AVAIL_0 2))
                 (list "page-type:      call-frame page"
                       (format "previous page:  $~a" (byte->hex-string PAGE_AVAIL_0)))
                 "call frame page (lb)")))

;; push a call frame and allocate X locals
;; input:  ZP_CALL_FRAME_xB
;;         ZP_CALL_FRAME_TOP_MARK
;;         ZP_LOCALS_LB_PTR
;;         ZP_CALL_STACK_TOP_MARK
;;         ZP_FUNC_PTR
;;         ZP_VM_PC
;;         ZP_RP <- ptr to function called
;; output: ZP_CALL_FRAME_xB
;;         ZP_CALL_FRAME_TOP_MARK
;;         ZP_LOCALS_xB_PTR      (just the offset, page also if a page change is necessary)
;;         additionally:
;;           TODO
;; NOTE: for a complete call,
;;           1. push call frame (use this method)
;;           2. set zp_vm_pc to new function
(define-vm-function VM_PUSH_CALL_FRAME
  (list
          ;; check if it fits
          (LDA ZP_CALL_FRAME_TOP_MARK)
          (LDY !$00)
          (SEC)
          (SBC (ZP_RP),y) ;; number of locals of function jumped to
          (BEQ DOES_NOT_FIT__)
          (BCC DOES_NOT_FIT__)
          (TAY)
          (LDA ZP_VM_PC+1) ;; code page
          (STA (ZP_CALL_FRAME_LB),y)
          (LDA ZP_FUNC_PTR)
          (STA (ZP_CALL_FRAME_HB),y)
          (DEY)
          (BEQ DOES_NOT_FIT__)
          (LDA ZP_VM_PC)
          (STA (ZP_CALL_FRAME_LB),y)
          (LDA ZP_LOCALS_LB_PTR)
          (STA (ZP_CALL_FRAME_HB),y)
          (DEY)
          (STY ZP_CALL_FRAME_TOP_MARK)

          (INY)
          (INY)
          (INY)
          (STY ZP_LOCALS_LB_PTR)
          (STY ZP_LOCALS_HB_PTR)
          (RTS)

   (label DOES_NOT_FIT__)
          (JSR VM_ALLOC_CALL_FRAME)
          (JSR VM_PUSH_CALL_FRAME)      ;; push call frame onto newly allocated pages
          (LDA ZP_CALL_FRAME_LB+1)      ;; copy new pages into page part of locals ptrs
          (STA ZP_LOCALS_LB_PTR+1)
          (LDA ZP_CALL_FRAME_HB+1)
          (STA ZP_LOCALS_HB_PTR+1)
          (RTS)))

#|
    stack grows top down

        | offset | description (lb) | (hb)                |                             |
        |--------+------------------+---------------------+-----------------------------|
        | FF     | previous page    |                     |                             |
        |        | . . .            |                     | <-- start of previous frame |
        |        | local #n-1       |                     |                             |
        |        | . . .            |                     |                             |
        |        | local #0         |                     | <-- ZP_LOCALS_xB_PTR        |
        |        | prev. page       | prev. func offset   |                             |
        |        | prev. pc offset  | prev. locals offset |                             |
        |        |                  |                     | <-- ZP_CALL_FRAME_TOP_MARK  |
        |        | . . .            |                     |                             |
        | 00     | page type        | page type           |                             |

 |#

(module+ test #| VM_PUSH_CALL_FRAME |#
  (define push-call-frame-test
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CALL_FRAME)

      ;; initialize function
      (LDX !$90)
      (STX ZP_FUNC_PTR+1)
      (STX ZP_FUNC_PTR)      ;; func ptr = $a0a0
      (INX)
      (STX ZP_VM_PC+1)
      (LDA !$06)
      (STA $9090)            ;; #locals = 6
      (STA ZP_VM_PC)         ;; pc = $A106

      ;; run test
      (JSR VM_PUSH_CALL_FRAME))
     ))

  (regression-test
   push-call-frame-test
   "push one call frame on empty call frame stack"
   (check-equal? (vm-call-frame-tos->string push-call-frame-test)
                 (list "return pc:         $9106"
                       "return func-ptr:   $9090"
                       "return locals-ptr: undefined"
                       "local 0: ptr NIL"
                       "local 1: ptr NIL"
                       "local 2: ptr NIL"
                       "local 3: ptr NIL"
                       "local 4: ptr NIL"
                       "local 5: ptr NIL")
                 "list all elements of the call frame"))

  (define push-call-frame-2-test
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CALL_FRAME)

      ;; initialize function
      (LDX !$90)
      (STX ZP_FUNC_PTR+1)
      (STX ZP_FUNC_PTR)      ;; func ptr = $a0a0
      (INX)
      (STX ZP_VM_PC+1)
      (LDA !$06)
      (STA $9090)            ;; #locals = 6
      (STA ZP_VM_PC)         ;; pc = $A106

      ;; first allocation
      (JSR VM_PUSH_CALL_FRAME)

      (LDX !$80)
      (STX ZP_FUNC_PTR+1)
      (STX ZP_FUNC_PTR)      ;; func ptr = $a0a0
      (STX ZP_RP)
      (STX ZP_RP+1)
      (INX)
      (STX ZP_VM_PC+1)
      (LDA !$04)
      (STA $8080)            ;; #locals = 4
      (STA ZP_VM_PC)         ;; pc = $A106


      (JSR VM_PUSH_CALL_FRAME)

      ;; set some locals
      ;; write byte $ff into local index 2
      (LDA !$01)
      (LDY !$02)
      (STA (ZP_LOCALS_HB_PTR),y)
      (STA (ZP_LOCALS_LB_PTR),y))
     #f))

  (regression-test
   push-call-frame-2-test
   "push one call frame on empty call frame stack"
   (check-equal? (vm-call-frame-tos->string push-call-frame-2-test)
                 (list "return pc:         $8104"
                       "return func-ptr:   $8080"
                       (format "return locals-ptr: $~aff, $~aff (lb, hb)" (byte->hex-string PAGE_CALL_FRAME) (byte->hex-string PAGE_CALL_FRAME_HB))
                       "local 0: ptr NIL"
                       "local 1: ptr NIL"
                       "local 2: byte $01"
                       "local 3: ptr NIL")
                 "list all elements of the call frame")))

;; pop the current call frame
;; input:  zp_call_frame_top_mark
;;         zp_call_frame
;; output: zp_call_frame_top_mark
;;         zp_call_frame
;;         zp_vm_pc
;;         zp_func_ptr
;;         zp_locals_lb_ptr
;;         zp_locals_hb_ptr
;;         if slow frame is popped, additionally:
;;            zp_cell_stack_lb_ptr
;;            zp_cell_stack_hb_ptr
;;            zp_cell_stack_tos
;; NOTE: pop call completely restores the invocation frame
;;       it does no GC check on locals!
(define-vm-function-wol VM_POP_CALL_FRAME
  (list
   ;; (label SWITCH_TO_PREVIOUS_STACK_PAGE__)
   ;;        (BRK) ;; TODO

   (label VM_POP_CALL_FRAME)
          (JSR VM_REFCOUNT_DECR_CURRENT_LOCALS)

          (LDY ZP_CALL_FRAME_TOP_MARK)
          ;; (CPY !$FE)
          ;; (BEQ SWITCH_TO_PREVIOUS_STACK_PAGE__) ;; should never happen

          (INY)
          (LDA (ZP_CALL_FRAME_LB),y)    ;; previous pc offset
          (STA ZP_VM_PC)
          (LDA (ZP_CALL_FRAME_HB),y)    ;; previous locals ptr offset
          (STA ZP_LOCALS_LB_PTR)
          (STA ZP_LOCALS_HB_PTR)

          (INY)

          ;; now remove locals from top mark
          (TYA)
          (STY ZP_TEMP)
          (CLC)
          (LDY !$00)
          (ADC (ZP_FUNC_PTR),y) ;; number of locals of currently executing function
          (STA ZP_CALL_FRAME_TOP_MARK)
          (LDY ZP_TEMP)

          (LDA (ZP_CALL_FRAME_LB),y)    ;; previous pc code page
          (STA ZP_VM_PC+1)
          (TAX)
          (LDA (ZP_CALL_FRAME_HB),y)    ;; previous function ptr offset
          (STA ZP_FUNC_PTR)

          (CMP ZP_VM_PC)                ;; c = ZP_FUNC_PTR >= ZP_VM_PC
          (BCC PC_ON_SAME_PAGE__)       ;; pc offset > func ptr offset => function starts on same page
          (DEX)                         ;; func ptr offset > pc offset => function starts on previous page
   (label PC_ON_SAME_PAGE__)
          (STX ZP_FUNC_PTR+1)

          (LDA ZP_CALL_FRAME_TOP_MARK)
          (CMP !$FE)
          (BNE LOCALS_STAY_ON_PAGE__)

          (LDY !$ff)
          (LDA (ZP_CALL_FRAME_LB),y)
          (BEQ LOCALS_STAY_ON_PAGE__) ;; is the last stack page, don't go further back!

          ;; switch to previous call frame page

          ;; find out top mark of previous page
          ;; locals offset - 3
          (LDA ZP_LOCALS_LB_PTR)
          (CLC)
          (SBC !$03)
          (STA ZP_CALL_FRAME_TOP_MARK)

          ;; keep old pages of zp_call_frame_ to free them
          (LDA ZP_CALL_FRAME_LB+1)
          (PHA)
          (LDA ZP_CALL_FRAME_HB+1)
          (PHA)

          (LDA (ZP_CALL_FRAME_LB),y)
          (STA ZP_CALL_FRAME_LB+1) ;; store previous page (lb)
          (STA ZP_LOCALS_LB_PTR+1)
          (LDA (ZP_CALL_FRAME_HB),y)
          (STA ZP_CALL_FRAME_HB+1) ;; store previous page (hb)
          (STA ZP_LOCALS_HB_PTR+1)

          ;; free the previous pages again
          (PLA)
          (TAX)
          (JSR VM_DEALLOCATE_PAGE)      ;; pages can be freed, since initialization is not expensive
          (PLA)
          (TAX)
          (JSR VM_DEALLOCATE_PAGE)

   (label LOCALS_STAY_ON_PAGE__)

          (RTS)))


(module+ test #| pop call frame |#
  (define pop-call-frame-test
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CALL_FRAME)

      ;; initialize function
      (LDX !$90)
      (STX ZP_FUNC_PTR+1)
      (STX ZP_FUNC_PTR)      ;; func ptr = $9090
      (STX ZP_RP)
      (STX ZP_RP+1)
      (INX)
      (STX ZP_VM_PC+1)
      (LDA !$06)
      (STA $9090)            ;; #locals = 6
      (STA ZP_VM_PC)         ;; pc = $9106

      (LDA ZP_CALL_FRAME_LB+1)
      (STA ZP_LOCALS_LB_PTR+1)
      (LDA ZP_CALL_FRAME_HB+1)
      (STA ZP_LOCALS_HB_PTR+1)
      (LDA !$fe)
      (STA ZP_LOCALS_LB_PTR)
      (STA ZP_LOCALS_HB_PTR)

      ;; push frame
      (JSR VM_PUSH_CALL_FRAME)

      ;; clear registers
      (LDA !$00)
      ;; (STA ZP_FUNC_PTR)
      ;; (STA ZP_FUNC_PTR+1)
      (STA ZP_VM_PC)
      (STA ZP_VM_PC+1)
      (STA ZP_LOCALS_HB_PTR)
      (STA ZP_LOCALS_HB_PTR+1)
      (STA ZP_LOCALS_LB_PTR)
      (STA ZP_LOCALS_LB_PTR+1)

      ;; pop frame
      (JSR VM_POP_CALL_FRAME))
     #f))

  (regression-test
   pop-call-frame-test
   "check simple push and pop"
   (check-equal? (memory-list pop-call-frame-test ZP_FUNC_PTR (+ 1 ZP_FUNC_PTR))
                 (list #x90 #x90)
                 "function pointer is restored")
   (check-equal? (memory-list pop-call-frame-test ZP_VM_PC (+ 1 ZP_VM_PC))
                 (list #x06 #x91))
   (check-equal? (memory-list pop-call-frame-test ZP_LOCALS_LB_PTR (+ 1 ZP_LOCALS_LB_PTR))
                 (list #xfe #x00)
                 "page is not restored, just the offset (since no page change has taken place)")
   (check-equal? (memory-list pop-call-frame-test ZP_LOCALS_HB_PTR (+ 1 ZP_LOCALS_HB_PTR))
                 (list #xfe #x00)
                 "page is not restored, just the offset (since no page change has taken place)"))

  (define pop-call-frame-2-test
    (run-code-in-test
     (list
      (JSR VM_ALLOC_CALL_FRAME)

      ;; initialize function
      (LDX !$90)
      (STX ZP_FUNC_PTR+1)
      (STX ZP_FUNC_PTR)      ;; func ptr = $9090
      (STX ZP_RP)
      (STX ZP_RP+1)
      (INX)
      (STX ZP_VM_PC+1)
      (LDA !$06)
      (STA $9090)            ;; #locals = 6
      (STA ZP_VM_PC)         ;; pc = $A106

      (LDA ZP_CALL_FRAME_LB+1)
      (STA ZP_LOCALS_LB_PTR+1)
      (LDA ZP_CALL_FRAME_HB+1)
      (STA ZP_LOCALS_HB_PTR+1)
      (LDA !$fe)
      (STA ZP_LOCALS_LB_PTR)
      (STA ZP_LOCALS_HB_PTR)

      (LDA !$07)
      (STA ZP_CALL_FRAME_TOP_MARK)

      ;; push frame
      (JSR VM_PUSH_CALL_FRAME)

      ;; clear registers
      (LDA !$00)
      (STA ZP_VM_PC)
      (STA ZP_VM_PC+1)
      (STA ZP_LOCALS_HB_PTR)
      (STA ZP_LOCALS_HB_PTR+1)
      (STA ZP_LOCALS_LB_PTR)
      (STA ZP_LOCALS_LB_PTR+1)

      ;; pop frame
      (JSR VM_POP_CALL_FRAME))
     #f))

  (regression-test
   pop-call-frame-2-test
   "check push that needs new page and pop"
   (check-equal? (memory-list pop-call-frame-2-test ZP_FUNC_PTR (+ 1 ZP_FUNC_PTR))
                 (list #x90 #x90)
                 "function pointer is restored")
   (check-equal? (memory-list pop-call-frame-2-test ZP_VM_PC (+ 1 ZP_VM_PC))
                 (list #x06 #x91))
   (check-equal? (memory-list pop-call-frame-2-test ZP_LOCALS_LB_PTR (+ 1 ZP_LOCALS_LB_PTR))
                 (list #xfe #xcf)
                 "page and offset is restored (since page change has taken place)")
   (check-equal? (memory-list pop-call-frame-2-test ZP_LOCALS_HB_PTR (+ 1 ZP_LOCALS_HB_PTR))
                 (list #xfe #xce)
                 "page and offset is restored (since page change has taken place)")))

;; @DC-FUN: VM_REFCOUNT_DECR_CURRENT_LOCALS, group: gc
;; decrement the refcount to all locals that are not initial (e.g. upon leaving a function)
(define-vm-function VM_REFCOUNT_DECR_CURRENT_LOCALS
  (list
          (LDY !$00)
          (LDA (ZP_FUNC_PTR),y) ;; # of current locals
          (AND !$0f)        ;; mask low nibble
          (BEQ DONE__)
          (TAY) ;; y = number of locals of current tunction
          ;; loop over locals -> rt, decr refcount
          (DEY)
   (label LOOP__)
          (LDA (ZP_LOCALS_LB_PTR),y)
          (BEQ NEXT_ITER__) ;; nil, next iteration
          (STA ZP_RZ)
          (LSR)
          (BCS S0_NEXT_ITER__) ;; definitely no pointer since lowest bit is set
          (LDA (ZP_LOCALS_HB_PTR),y)
          (STA ZP_RZ+1)
          (STY COUNTER__)
          (JSR DEC_REFCNT_M1_SLOT_RZ)
          (LDY COUNTER__)
   (label S0_NEXT_ITER__)
          (LDA !$00)
   (label NEXT_ITER__)
          (STA (ZP_LOCALS_LB_PTR),y)
          ;; (STA (ZP_LOCALS_HB_PTR),y) ;; clear of low byte should be enough
          (DEY)
          (BPL LOOP__) ;; bpl ok, since y must be < 16 (initially)
   (label DONE__)
          (RTS)

   (label COUNTER__)
          (byte 0)))


(module+ test #| free locals |#
)

(define vm-call-frame-code
  (append VM_ALLOC_CALL_FRAME                              ;; allocate a new call frame, storing current top mark on previous frame (if existent)
          VM_PUSH_CALL_FRAME                               ;; push a new frame, respecting X = locals needed and vm_pc to decide whether fast or slow frames are used
          VM_POP_CALL_FRAME                                ;; pop last pushed frame, checking whether slow or fast frame is on top of call frame stack
          VM_REFCOUNT_DECR_CURRENT_LOCALS))

#;(module+ test #| vm-call-frame |#
  (inform-check-equal? (code-len (flatten vm-call-frame-code))
                       263
                       "estimated code length of call-frame runtime"))

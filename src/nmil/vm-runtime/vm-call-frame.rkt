#lang racket/base

(provide vm-call-frame-code
         vm-call-frame->strings
         vm-call-frames->string

         VM_POP_CALL_FRAME
         VM_REFCOUNT_DECR_CURRENT_LOCALS
         VM_ALLOC_LOCALS
         VM_PUSH_CALL_FRAME
         VM_INITIALIZE_CALL_FRAME)

#|

  implementation of a call frame for vm calls
  implemented as stack with fat/slim stack frames
  using a cell-stack

|#

(require "../../6510.rkt"
         (only-in "../../tools/6510-interpreter.rkt"
                  peek-word-at-address
                  peek)
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
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
         (only-in "./vm-memory-map.rkt"
                  ZP_CALL_FRAME
                  ZP_FUNC_PTR
                  ZP_LOCALS_LB_PTR
                  ZP_LOCALS_HB_PTR
                  ZP_LOCALS_TOP_MARK
                  ZP_CALL_FRAME_TOP_MARK)
         (only-in "./vm-pages.rkt"
                  VM_ALLOCATE_NEW_PAGE
                  VM_DEALLOCATE_PAGE
                  VM_INITIALIZE_PAGE_MEMORY_MANAGER
                  vm-pages-code)
         (only-in "./vm-register-functions.rkt"
                  vm-register-functions-code))

(module+ test #| after mem init |#
  (require (only-in "../../ast/6510-relocator.rkt"
                    code-len))

  (define PAGE_AVAIL_0 #xcc)
  (define PAGE_AVAIL_0_W #xcc00)
  (define PAGE_AVAIL_1 #xcb)
  (define PAGE_AVAIL_1_W #xcb00)

  (define PAGE_CALL_FRAME #xcf)
  (define PAGE_CALL_FRAME_W #xcf00)
  (define PAGE_LOCALS_LB #xcd)
  (define PAGE_LOCALS_HB #xce))

(module+ test
  (require "../../6510-test-utils.rkt"
           (only-in "../../tools/6510-interpreter.rkt"
                    memory-list)
           (only-in "../vm-interpreter-loop.rkt"
                    VM_INTERPRETER_ZP)
           (only-in "./vm-memory-manager-test-utils.rkt"
                    run-code-in-test-on-code
                    remove-labels-for))

  (define (wrap-code-for-test bc complete-code (mocked-code-list (list)))
    (append
            (list
             (org #xa000)
             (JSR VM_INITIALIZE_PAGE_MEMORY_MANAGER_N20)
             (JSR VM_INITIALIZE_CALL_FRAME)
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

(define (vm-call-frames->string state)
  (define call-frame-page (peek state (add1 ZP_CALL_FRAME)))
  (define call-frame-ptr (peek-word-at-address state ZP_CALL_FRAME))
  (define return-address (peek-word-at-address state call-frame-ptr))
  (define top-mark       (peek state ZP_CALL_FRAME_TOP_MARK))
  (define slow-fast-ind  (peek state (bytes->int (sub1 top-mark) call-frame-page)))
  (cond [(= 0 (bitwise-and #xfe slow-fast-ind))
         ;; fat frame
         (define locals-ptr-low (peek state (+ 3 call-frame-ptr)))
         (define locals-lb-page (peek state (+ 4 call-frame-ptr)))
         (define locals-hb-page (peek state (+ 5 call-frame-ptr)))
         (define func-ptr-low   (peek state (+ 6 call-frame-ptr)))
         (list (format "fat-frame ($~a..$~a)" (format-hex-word call-frame-ptr) (format-hex-word (+ 7 call-frame-ptr)))
               (format "return-pc:           $~a" (format-hex-word return-address))
               (format "return-function-ptr: $~a" (format-hex-word (bytes->int func-ptr-low (- (high-byte return-address) slow-fast-ind))))
               (format "return-locals-ptr:   $~a, $~a (lb,hb)"
                       (format-hex-word (bytes->int locals-ptr-low locals-lb-page))
                       (format-hex-word (bytes->int locals-ptr-low locals-hb-page))))
         ]
        [else
         ;; slim frame
         (define func-ptr-low   (peek state (+ 2 call-frame-ptr)))
         (define locals-ptr-low (peek state (+ 3 call-frame-ptr)))
         (list (format "slim-frame ($~a..$~a)" (format-hex-word call-frame-ptr) (format-hex-word (+ 3 call-frame-ptr)))
               (format "return-pc:           $~a" (format-hex-word return-address))
               (format "return-function-ptr: $~a" (format-hex-word (bytes->int func-ptr-low (high-byte return-address))))
               (format "return-locals-ptr:   $~a, $~a (lb,hb)"
                       (format-hex-word (bytes->int locals-ptr-low (peek state (add1 ZP_LOCALS_LB_PTR))))
                       (format-hex-word (bytes->int locals-ptr-low (peek state (add1 ZP_LOCALS_HB_PTR))))))
         ]))

;; write call stack status
(define (vm-call-frame->strings state)
  (append
   (list (format "call-frame-ptr:   $~a, topmark: ~a" (format-hex-word (peek-word-at-address state ZP_CALL_FRAME)) (format-hex-byte (peek state ZP_CALL_FRAME_TOP_MARK)))
         (format "program-counter:  $~a" (format-hex-word (peek-word-at-address state ZP_VM_PC)))
         (format "function-ptr:     $~a" (format-hex-word (peek-word-at-address state ZP_FUNC_PTR)))
         (format "locals-ptr:       $~a, $~a (lb, hb), topmark: ~a"
                 (format-hex-word (bytes->int (peek state ZP_LOCALS_LB_PTR) (peek state (add1 ZP_LOCALS_LB_PTR))))
                 (format-hex-word (bytes->int (peek state ZP_LOCALS_HB_PTR) (peek state (add1 ZP_LOCALS_HB_PTR))))
                 (format-hex-byte (peek state ZP_LOCALS_TOP_MARK)))
         )
   (if (= #x03 (peek state ZP_CALL_FRAME_TOP_MARK))
       (list)
       (vm-call-frames->string state))))


;; @DC-FUN: ALLOC_LOCALS_STACK, group: call_frame
;; allocate two pages for a cell-stack, store previous ZP_LOCALS_xB ptrs into these
;; and initialise the new cell-stack
;; input:  ZP_LOCALS_LB_PTR  ptr to old locals cell-stack
;;         ZP_LOCALS_HB_PTR
;; usage:  A X Y
;; output: ZP_LOCALS_LB_PTR  ptr to new locals cell-stack
;;         ZP_LOCALS_HB_PTR
(define-vm-function-wol ALLOC_LOCALS_STACK
  (list
   (label ALLOC_FIRST_LOCALS_STACK)
          (LDX !$00)
          (STX ZP_LOCALS_LB_PTR+1)
          (STX ZP_LOCALS_HB_PTR+1)

   (label ALLOC_LOCALS_STACK)
          (JSR VM_ALLOCATE_NEW_PAGE)
          (LDA ZP_LOCALS_HB_PTR+1) ;; previous locals hb page
          (STX ZP_LOCALS_HB_PTR+1)
          (JSR INIT_CELLSTACK_PAGE_X)

          (JSR VM_ALLOCATE_NEW_PAGE)
          (LDA ZP_LOCALS_LB_PTR+1) ;; previous locals lb page
          (STX ZP_LOCALS_LB_PTR+1)
          (JMP INIT_CELLSTACK_PAGE_X)))

;; @DC-FUN: VM_INITIALIZE_CALL_FRAME, group: call_frame
;; initialise first call frame
;; input:  -
;; usage:  A X Y
;; output: ZP_LOCALS_LB_PTR
;;         ZP_LOCALS_HB_PTR
;;         ZP_LOCALS_TOP_MARK
;;         ZP_CALL_FRAME
;;         ZP_CALL_FRAME_TOP_MARK
(define-vm-function VM_INITIALIZE_CALL_FRAME
  (list
          ;; init current call frame (initial) no popping
          (LDA !$00)
          (STA ZP_CALL_FRAME+1)         ;; this ensures that NO previous call frame page is heeded!
          (STA ZP_CALL_FRAME_TOP_MARK)
          (JSR VM_ALLOC_CALL_FRAME)   ;; call-frame initialized (top mark set, too)

          ;; alloc locals
          (JSR ALLOC_FIRST_LOCALS_STACK)

          (LDA !$03) ;; payload start for locals
          (STA ZP_LOCALS_LB_PTR)
          (STA ZP_LOCALS_HB_PTR)
          (STA ZP_LOCALS_TOP_MARK)
          (RTS)))


;; @DC-FUN: VM_ALLOC_CALL_FRAME, group: call_frame
;; allocate a new call frame,
;; close the current call frame (set top mark)
;; and initialize the page accordingly
;; input:  ZP_CALL_FRAME
;;         ZP_CALL_FRAME_TOP_MARK
;; output: ZP_CALL_FRAME
;;         ZP_CALL_FRAME_TOP_MARK
;;         Y index for first byte available as payload (03) = ZP_CALL_FRAME_TOP_MARK
(define-vm-function VM_ALLOC_CALL_FRAME
  (list
          (LDA ZP_CALL_FRAME+1)
          (BEQ NO_SAVE_OF_OLD_FRAME_DATA__)

          ;; make sure to write old top mark to old call frame page (if existent)
          (LDA !$00)
          (STA ZP_CALL_FRAME)           ;; clear low byte if pointer
          (LDY !$02)
          (LDA ZP_CALL_FRAME_TOP_MARK)
          (STA (ZP_CALL_FRAME),y)       ;; write (old) top mark at current page $02

   (label NO_SAVE_OF_OLD_FRAME_DATA__)
          ;; allocate completely new page
          (JSR VM_ALLOCATE_NEW_PAGE)
          (TXA)

          ;; init page as new call frame page (00 = page type, 01 = previous call frame page, 02 = top mark [uninitialized until full], rest uninitialized)
          (LDX ZP_CALL_FRAME+1)         ;; keep old page in X
          (STA ZP_CALL_FRAME+1)         ;; write new page into call-frame-ptr
          (LDA !$18)
          (LDY !$00)
          (STA (ZP_CALL_FRAME),y)       ;; set page type to #b0001 1000  (call frame page)
          (INY)
          (TXA)
          (STA (ZP_CALL_FRAME),y)       ;; set previous page to current page of call frame
          (INY)
          (INY)
          (STY ZP_CALL_FRAME_TOP_MARK)  ;; write top mark
          (STY ZP_CALL_FRAME)           ;; write new 03 into lowbyte of call-frame
          (RTS)))

;; @DC-FUN: VM_PUSH_CALL_FRAME, group: call_frame
;; push a call frame and allocate X locals
;; input:  X = number of locals to allocate on locals frame
;;         zp_call_frame
;;         zp_call_frame_top_mark
;;         zp_locals_lb_ptr
;;         zp_cell_stack_tos
;;         zp_func_ptr
;;         zp_vm_pc
;; output: zp_call_frame
;;         zp_call_frame_top_mark
;;         additionally:
;;            if call frame needs to be put on a new page:
;;               $02 on old page holds top mark of old call stack
;; NOTE: for a complete call,
;;           1. push call frame (use this method)
;;           2. allocated # of locals needed, set zp_vm_locals_lb_ptr and zp_vm_locals_hb_ptr
;;           3. set zp_vm_pc to new function
(define-vm-function VM_PUSH_CALL_FRAME
  (list
          ;; check for fast frames
          ;;    push: possible if - vm_pc and func-ptr share the same page
          (LDA ZP_VM_PC+1)
          (CMP ZP_FUNC_PTR+1)
          (BNE SLOW_FRAME__)
          ;; check not necessary, stack keeps its state, pushing, popping takes care of cell-stack ptr
          ;; ;;                      - cell-stack does not overflow (has 16 entries reserve)
          ;; (LDA ZP_EVAL_STACK_TAIL_TOP)
          ;; (CMP !$F0)
          ;; (BPL SLOW_FRAME__VM_PUSH_CALL_FRAME)
          ;;                      - locals do not overflow (has reserves to hold functions' need)
          (TXA)                  ;; x = number of locals for this function
          (CLC)
          (ADC ZP_LOCALS_LB_PTR)
          (BCS SLOW_FRAME__)

          ;; do fast frame
          ;; alloc for fast frame
          ;; ensure call frame allows 4 additional bytes
          (LDA ZP_CALL_FRAME_TOP_MARK)
          (TAY)
          (ADC !$04)
          (BCC NO_NEW_PAGE_FOR_FAST_CALL_FRAME__)

          (JSR VM_ALLOC_CALL_FRAME)                  ;; now allocate a new page

   (label NO_NEW_PAGE_FOR_FAST_CALL_FRAME__)
          (STY ZP_CALL_FRAME)                          ;; set lowbyte of call frame

          ;; now copy fast frame values
          (LDY !$00)
          ;;                             |                 vm pc                   |
          (LDA ZP_VM_PC)
          (STA (ZP_CALL_FRAME),y)
          (INY)
          (LDA ZP_VM_PC+1)
          (STA (ZP_CALL_FRAME),y)
          (INY)
          ;;                             | func-ptr low-byte | locals-ptr low byte |
          (LDA ZP_FUNC_PTR)
          (STA (ZP_CALL_FRAME),y)
          (INY)
          (LDA ZP_LOCALS_LB_PTR)
          (STA (ZP_CALL_FRAME),y)
          (INY)

          (TYA)
          (CLC)
          (ADC ZP_CALL_FRAME)
          (STA ZP_CALL_FRAME_TOP_MARK)
          (RTS)

   (label SLOW_FRAME__)
          ;; alloc for slow frame
          ;; ensure call frame allows 10 additional bytes
          (LDA ZP_CALL_FRAME_TOP_MARK)
          (TAY)
          (CLC)
          (ADC !$08) ;; length of slow call frame
          (BCC NO_NEW_PAGE_FOR_SLOW_CALL_FRAME__)

          (JSR VM_ALLOC_CALL_FRAME)
          ;; alloc call frame returns y with topmark
          ;; (LDY ZP_CALL_FRAME_TOP_MARK) ;; first payload byte available on call frame page

   (label NO_NEW_PAGE_FOR_SLOW_CALL_FRAME__)
          (STY ZP_CALL_FRAME)

          ;; now copy slow frame values

          ;; len of slow call frame
          (LDA !$08)

          ;; set new top mark
          (CLC)
          (ADC ZP_CALL_FRAME)
          (STA ZP_CALL_FRAME_TOP_MARK)

          (LDY !$07) ;; copy 08 bytes starting at 07

          ;; set encoded page for function pointer
          (SEC)
          (LDA ZP_VM_PC+1)
          (SBC ZP_FUNC_PTR+1)
          ;; (AND !$01)                    ;; make sure that only bit 0 can be set (should not happen, since function code may max spread over two pages!)
          (STA (ZP_CALL_FRAME),y)         ;; y currently = 7
          (DEY)

          ;; copy rest of zero page values
          ;; naiive copy: 7 * 5 bytes = 35 bytes, 7 * 9 cycles = 63 cycles
          ;; looped copy: 10 bytes + 7 table bytes, 7 * 18 cycles = 126 cycles
    (label LOOP_ZP_COPY__)
          (LDX SLOW_STACK_ZP_LOCATIONS,y)
          (LDA $00,x)
          (STA (ZP_CALL_FRAME),y)        ;; y currently = 6..0  (7 bytes) copied
          (DEY)
          (BPL LOOP_ZP_COPY__)           ;; >= 0? -> loop

          (RTS)

   (label SLOW_STACK_ZP_LOCATIONS) ;; processed in reverse order, starting with offset+08 (used be vm_pop_call_frame_n, too!!)
          (byte-ref ZP_VM_PC)               ;; |                   vm pc                     |
          (byte-ref ZP_VM_PC+1)
          (byte-ref ZP_TEMP)                ;; |   reserved           | locals ptr shared lb |
          (byte-ref ZP_LOCALS_LB_PTR)
          (byte-ref ZP_LOCALS_LB_PTR+1)     ;; | locals-lb page       | locals-hb-page       |
          (byte-ref ZP_LOCALS_HB_PTR+1)
          (byte-ref ZP_FUNC_PTR)         ;; |  func-ptr  low       | $00 / $01            | func-ptr could be encoded into: lowbyte, highbyte =  vm_pc page + $00/$01 (of byte 4 in this stack) <- would save two bytes of stack size
          ))

(module+ test #| VM_PUSH_CALL_FRAME |#
  (define push-call-frame-n-fit-page-prep-code
    (list
       ;; set complete vm state to values to be pushed (mostly done by call stack init)
      (LDY !$09) ;; 09
      (STY ZP_FUNC_PTR+1)            ;; share same page
      (STY ZP_VM_PC+1)
      (INY) ;; 0a
      (STY ZP_FUNC_PTR)              ;; => zp_vm_func_ptr $090a
      (INY) ;; 0b
      (STY ZP_VM_PC)                    ;; => zp_vm_pc $090b
      ))

  (define push-call-frame-n-fit-page-prep-state
    (run-code-in-test push-call-frame-n-fit-page-prep-code))

  (check-equal? (vm-call-frame->strings push-call-frame-n-fit-page-prep-state)
                (list (format "call-frame-ptr:   $~a03, topmark: 03" (format-hex-byte PAGE_CALL_FRAME))
                      "program-counter:  $090b"
                      "function-ptr:     $090a"
                      (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03" (format-hex-byte PAGE_LOCALS_LB) (format-hex-byte PAGE_LOCALS_HB)))
                "prepared call-frame is initial (and empty)")
  (check-equal? (peek push-call-frame-n-fit-page-prep-state ZP_CALL_FRAME_TOP_MARK)
                #x03
                "top mark points to the first free byte on the call frame stack")

  (define push-call-frame-n-fit-page-code
    (append push-call-frame-n-fit-page-prep-code
             (list
              (LDX !$02) ;; need 4 local cells
              (JSR VM_PUSH_CALL_FRAME)
              (LDA !$02)
              (JSR VM_ALLOC_LOCALS))))
  (define push-call-frame-n-fit-page-state
    (run-code-in-test push-call-frame-n-fit-page-code))

  (check-equal? (memory-list push-call-frame-n-fit-page-state (+ PAGE_CALL_FRAME_W #x03) (+ PAGE_CALL_FRAME_W #x06))
                (list #x0b #x09
                      #x0a
                      #x03)
                "the call frame is a fast frame (4 bytes) holding the full pc (2b), low byte of function-ptr and low byte of locals-ptr")
  (check-equal? (vm-call-frame->strings push-call-frame-n-fit-page-state)
                (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
                      "program-counter:  $090b"
                      "function-ptr:     $090a"
                      (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 05" (format-hex-byte PAGE_LOCALS_LB) (format-hex-byte PAGE_LOCALS_HB))
                      (format "slim-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
                      "return-pc:           $090b"
                      "return-function-ptr: $090a"
                      (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)" (format-hex-byte PAGE_LOCALS_LB) (format-hex-byte PAGE_LOCALS_HB)))
                "call-frame is pointing to the current one push ")
  (check-equal? (peek push-call-frame-n-fit-page-state ZP_CALL_FRAME_TOP_MARK)
                #x07
                "top mark points to the first free byte on the call frame stack (past the one pushed)")

  (define push-call-frame-misfit-page-sl-frame-0-code
    (append push-call-frame-n-fit-page-code ;; one frame has been pushed
             (list
              (LDA !$fb)
              (STA ZP_CALL_FRAME_TOP_MARK) ;; set mark such that push will need new page
              (LDX !$00)
              (JSR VM_PUSH_CALL_FRAME)
              (LDA !$00)
              (JSR VM_ALLOC_LOCALS)
              (INC ZP_VM_PC+1)             ;; pc on other page => need for slow call frame

              (LDX !$03) ;; need 3 local cells
              (JSR VM_PUSH_CALL_FRAME)
              (LDA !$03)
              (JSR VM_ALLOC_LOCALS))))

  (define push-call-frame-misfit-page-sl-frame-0-state
    (run-code-in-test push-call-frame-misfit-page-sl-frame-0-code))

  (check-equal? (vm-call-frame->strings push-call-frame-misfit-page-sl-frame-0-state)
                (list (format "call-frame-ptr:   $~a03, topmark: 0b" (format-hex-byte PAGE_AVAIL_0))
                      "program-counter:  $0a0b"
                      "function-ptr:     $090a"
                      (format "locals-ptr:       $~a05, $~a05 (lb, hb), topmark: 08" (format-hex-byte PAGE_LOCALS_LB) (format-hex-byte PAGE_LOCALS_HB))
                      (format "fat-frame ($~a03..$~a0a)" (format-hex-byte PAGE_AVAIL_0) (format-hex-byte PAGE_AVAIL_0))
                      "return-pc:           $0a0b"
                      "return-function-ptr: $090a"
                      (format "return-locals-ptr:   $~a05, $~a05 (lb,hb)" (format-hex-byte PAGE_LOCALS_LB) (format-hex-byte PAGE_LOCALS_HB))))
  (check-equal? (peek push-call-frame-misfit-page-sl-frame-0-state ZP_CALL_FRAME_TOP_MARK)
                #x0b
                "top mark points to the first free byte on the call frame stack (past the one pushed)")
  (check-equal? (memory-list push-call-frame-misfit-page-sl-frame-0-state (+ PAGE_AVAIL_0_W #x03) (+ PAGE_AVAIL_0_W #x04))
                (list #x0b #x0a)
                "the call frame is a slow frame (8 bytes)
                 holding the full pc (2b)")
  (check-equal? (memory-list push-call-frame-misfit-page-sl-frame-0-state (+ PAGE_AVAIL_0_W #x06) (+ PAGE_AVAIL_0_W #x0a))
                (list #x05
                      PAGE_LOCALS_LB
                      PAGE_LOCALS_HB
                      #x0a
                      #x01)
                "the call frame is a slow frame (8 bytes)
                 low byte of locals-ptr")

  (define push-call-frame-misfit-page-sl-frame-1-code
    (append push-call-frame-n-fit-page-code ;; one frame has been pushed
             (list
              (LDA !$fe)
              (STA ZP_CALL_FRAME_TOP_MARK) ;; set mark such that push will need new page
              (STA ZP_LOCALS_HB_PTR)
              (STA ZP_LOCALS_LB_PTR)       ;; locals don't fit on same page => need for slow call frame
              (STA ZP_LOCALS_TOP_MARK)

              (LDX !$03) ;; need 3 local cells
              (JSR VM_PUSH_CALL_FRAME)
              (LDA !$03)
              (JSR VM_ALLOC_LOCALS))))
  (define push-call-frame-misfit-page-sl-frame-1-state
    (run-code-in-test push-call-frame-misfit-page-sl-frame-1-code))

  (check-equal? (vm-call-frame->strings push-call-frame-misfit-page-sl-frame-1-state)
                (list (format "call-frame-ptr:   $~a03, topmark: 0b" (format-hex-byte PAGE_AVAIL_0))
                      "program-counter:  $090b"
                      "function-ptr:     $090a"
                      (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 06" (format-hex-byte (- PAGE_LOCALS_LB 3)) (format-hex-byte (- PAGE_LOCALS_HB 3)))
                      (format  "fat-frame ($~a03..$~a0a)" (format-hex-byte PAGE_AVAIL_0) (format-hex-byte PAGE_AVAIL_0))
                      "return-pc:           $090b"
                      "return-function-ptr: $090a"
                      (format "return-locals-ptr:   $~afe, $~afe (lb,hb)" (format-hex-byte PAGE_LOCALS_LB) (format-hex-byte PAGE_LOCALS_HB))))
  (check-equal? (peek push-call-frame-misfit-page-sl-frame-1-state ZP_CALL_FRAME_TOP_MARK)
                #x0b
                "top mark points to the first free byte on the call frame stack (past the one pushed)")
  (check-equal? (memory-list push-call-frame-misfit-page-sl-frame-1-state (+ PAGE_AVAIL_0_W #x03) (+ PAGE_AVAIL_0_W #x04))
                (list #x0b #x09)
                "the call frame is a slow frame (8 bytes)
                 holding the full pc $090b")
  (check-equal? (memory-list push-call-frame-misfit-page-sl-frame-1-state (+ PAGE_AVAIL_0_W #x06) (+ PAGE_AVAIL_0_W #x0a))
                (list #xfe
                      PAGE_LOCALS_LB
                      PAGE_LOCALS_HB
                      #x0a
                      #x00)
                "the call frame is a slow frame (8 bytes)
                 low byte of locals-ptr"))

;; @DC-FUN: VM_POP_CALL_FRAME, group: call_frame
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
(define-vm-function VM_POP_CALL_FRAME
  (list
   ;; TODO: check whether locals need to move to new page (free locals page)

          (LDA ZP_CALL_FRAME_TOP_MARK)
          (CMP !$03)                                     
          (BNE NO_PAGE_CHANGE__)

          ;; if top mark points to base, the previous call frame page must be loaded
          (LDA ZP_CALL_FRAME+1) ;; get previous page
          (TAX)                 ;; into X

          ;; get previous call frame page
          (LDA !$00)
          (STA ZP_CALL_FRAME)
          (LDY !$01)
          (LDA (ZP_CALL_FRAME),y) ;; get previous page
          (STA ZP_CALL_FRAME+1)
          (INY)
          (LDA (ZP_CALL_FRAME),y) ;; get top mark from previous page
          (STA ZP_CALL_FRAME_TOP_MARK)

          ;; reconstruct old call-frame after page change
          (TAY)
          (JSR LOCAL__FS_CALL_FRAME_ADJUSTMENT__)
          (STA ZP_CALL_FRAME) ;; do this so zp_call_frame behaves as if no page change took place (will be put into top mark eventually)

          ;; free old call frame page!
          (JSR VM_DEALLOCATE_PAGE)
          ;; (TXA)
          ;; (JSR FREE_PAGE_A)

          (LDA ZP_CALL_FRAME_TOP_MARK)
   (label NO_PAGE_CHANGE__)
          ;; current frame fast?
          (SEC)
          (SBC ZP_CALL_FRAME)
          (CMP !$04)
          (BNE SLOW_FRAME__)

          ;; is a fast frame
          (LDY !$00)
          (LDA (ZP_CALL_FRAME),y)
          (STA ZP_VM_PC)
          (INY)
          (LDA (ZP_CALL_FRAME),y)
          (STA ZP_VM_PC+1)
          (STA ZP_FUNC_PTR+1)
          (INY)
          (LDA (ZP_CALL_FRAME),y)
          (STA ZP_FUNC_PTR)
          (INY)
          (LDA (ZP_CALL_FRAME),y)
          (STA ZP_LOCALS_LB_PTR)
          (STA ZP_LOCALS_HB_PTR)
          (BNE RECONSTRUCT_CALL_FRAME_AND_TOP_MARK__)  ;; locals ptr never points in lowbyte to 0

   (label SLOW_FRAME__)
          (LDA ZP_LOCALS_LB_PTR+1) ;; old locals lb page
          (STA ZP_TEMP)            ;; remember locals lb page
          (LDA ZP_LOCALS_HB_PTR+1) ;; old locals lb page
          (STA ZP_TEMP2)           ;; remember locals lb page

          (LDY !$06)                    ;; copy 7 bytes

   (label LOOP_ZP_RESTORE__)
          (LDX SLOW_STACK_ZP_LOCATIONS,y)
          (LDA (ZP_CALL_FRAME),y)
          (STA $00,x)
          (DEY)
          (BPL LOOP_ZP_RESTORE__)

          (LDA ZP_LOCALS_LB_PTR+1)
          (CMP ZP_TEMP)
          (BEQ NO_LOCALS_PAGE_CHANGE__)

          ;; free the old locals page (lb and hb) and set the top mark accordingly
          (LDX ZP_TEMP)
          (JSR VM_DEALLOCATE_PAGE)
          ;; (LDA ZP_TEMP)
          ;; (JSR FREE_PAGE_A)
          (LDX ZP_TEMP2)
          (JSR VM_DEALLOCATE_PAGE)
          ;; (LDA ZP_TEMP2)
          ;; (JSR FREE_PAGE_A)

          ;; restore topmark of new and current locals page
          (LDA !$00)
          (STA ZP_TEMP)
          (LDA ZP_LOCALS_LB_PTR+1)
          (STA ZP_TEMP+1)
          (LDY !$02)
          (LDA (ZP_TEMP),y) ;; old top mark of this page,
          (STA ZP_LOCALS_TOP_MARK)

   (label NO_LOCALS_PAGE_CHANGE__)

          ;; now get function pointer page from encoded byte
          (LDY !$07)                    ;; index of last byte in slow call frame
          (LDA ZP_VM_PC+1)              ;; was just set by copy loop before
          (SEC)
          (SBC (ZP_CALL_FRAME),y)       ;; contains either $00 or $01 since this is a slow frame
          (STA ZP_FUNC_PTR+1)           ;; page of func ptr is page of pc - $00/$01

          ;; make sure the locals ptr are synchronized (only locals-lb-ptr is put on stack completely)
          (LDA ZP_LOCALS_LB_PTR)
          (STA ZP_LOCALS_HB_PTR)

   (label RECONSTRUCT_CALL_FRAME_AND_TOP_MARK__)
          (LDA ZP_CALL_FRAME)                   ;; old low byte of call frame
          (STA ZP_CALL_FRAME_TOP_MARK)          ;; set top mark 
          (CMP !$03) ;; alread at bottom => no sub
          (BEQ NO_SUB__)     ;; 03 indicates page change of call frame

          (LDA !$00)
          (STA ZP_CALL_FRAME)                   ;; zero low byte to use topmark as index
          (LDY ZP_CALL_FRAME_TOP_MARK)          ;; get index to top
          (JSR LOCAL__FS_CALL_FRAME_ADJUSTMENT__)

   (label NO_SUB__)
          (STA ZP_CALL_FRAME)
          (RTS)


   ;; calc zp_call_frame by looking at the call-frame right below zp_call_frame_top
   ;; Y = top mark
   ;; ZP_CALL_FRAME (00 page)
   ;; fast frame = there are bits set other than just bit0
   ;; slow frame = only bit0 is used (all else is 0)
   (label LOCAL__FS_CALL_FRAME_ADJUSTMENT__)
          (DEY)                                                 ;; index one behind top mark
          (LDA (ZP_CALL_FRAME),y)                               ;; load that value
          (AND !$fe)                                            ;; all but bit0  are 0?
          (BNE LOCAL__IS_FAST_CALL_FRAME__) ;; => slow frame

          (LDA ZP_CALL_FRAME_TOP_MARK)
          (SEC)
          (SBC !$08)
          (RTS)

   (label LOCAL__IS_FAST_CALL_FRAME__)
          (LDA ZP_CALL_FRAME_TOP_MARK)
          (SEC)
          (SBC !$04)
          (RTS)))

;; (module+ test #| pop call frame n |#
;;   (define pop-call-frame-n-fast-cf-code
;;     (append push-call-frame-n-fit-page-prep-code
;;             (list
;;              (LDX !$04)
;;              (JSR VM_PUSH_CALL_FRAME)
;;              (LDX !$04)
;;              (JSR VM_PUSH_CALL_FRAME)

;;              ;; now change pc
;;              (LDY !$91)
;;              (STY ZP_VM_PC)
;;              (INY)
;;              (STY ZP_VM_PC+1)
;;              (INY)
;;              ;; function pointer
;;              (STY ZP_FUNC_PTR)
;;              (INY)
;;              (STY ZP_FUNC_PTR+1)
;;              (INY)
;;              ;; and lowbyte of locals ptr
;;              (STY ZP_LOCALS_LB_PTR)
;;              (INY)
;;              (STY ZP_LOCALS_HB_PTR)

;;              (JSR VM_POP_CALL_FRAME)
;;              )))
;;   (define pop-call-frame-n-fast-cf-state
;;     (run-code-in-test pop-call-frame-n-fast-cf-code))

;;   (check-equal? (peek pop-call-frame-n-fast-cf-state ZP_CALL_FRAME_TOP_MARK)
;;                 #x07)
;;   (check-equal? (vm-call-frame->strings pop-call-frame-n-fast-cf-state)
;;                 (list (format "call-frame-ptr:   $~a03, topmark: 07" (format-hex-byte PAGE_CALL_FRAME))
;;                       "program-counter:  $090b"
;;                       "function-ptr:     $090a"
;;                       (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03" (format-hex-byte PAGE_LOCALS_LB) (format-hex-byte PAGE_LOCALS_HB))
;;                       (format "fast-frame ($~a03..$~a06)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
;;                       "return-pc:           $090b"
;;                       "return-function-ptr: $090a"
;;                       (format "return-locals-ptr:   $~a03, $~a03 (lb,hb)" (format-hex-byte PAGE_LOCALS_LB) (format-hex-byte PAGE_LOCALS_HB)))
;;                 "restore original call-frame-ptr,
;;                          program counter,
;;                          functions-ptr
;;                          and locals pointer")

;;   (define pop-call-frame-n-slow-cf-code
;;     (append push-call-frame-misfit-page-sl-frame-0-code
;;             ;; now the call frame is set to
;;             ;; (list "call-frame-ptr:   $xx03"   <-- slow frame with 8 bytes
;;             ;;       "program-counter:  $0a0b"
;;             ;;       "function-ptr:     $090a"
;;             ;;       "locals-ptr:       $xx03, $x03 (lb, hb)")
;;             (list
;;              ;; overwrite call-frame data: pc, function pointer and locals ptr (hi/lo bytes)
;;              (LDY !$21)
;;              (STY ZP_VM_PC)
;;              (INY)
;;              (STY ZP_VM_PC+1)
;;              (INY)
;;              (STY ZP_FUNC_PTR)
;;              (INY)
;;              (STY ZP_FUNC_PTR+1)
;;              (INY)
;;              (STY ZP_LOCALS_LB_PTR)
;;              (STY ZP_LOCALS_HB_PTR)
;;              (INY)
;;              (STY ZP_LOCALS_LB_PTR+1)
;;              (INY)
;;              (STY ZP_LOCALS_HB_PTR+1)

;;              ;; restore should restore all pointers of the pushed call-frame
;;              (JSR VM_POP_CALL_FRAME))))
;;   (define pop-call-frame-n-slow-cf-state
;;     (run-code-in-test pop-call-frame-n-slow-cf-code))

;;   (check-equal? (vm-call-frame->strings pop-call-frame-n-slow-cf-state)
;;                 (list (format "call-frame-ptr:   $~a03, topmark: 03" (format-hex-byte PAGE_AVAIL_0))
;;                       "program-counter:  $0a0b"
;;                       "function-ptr:     $090a"
;;                       (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03" (format-hex-byte PAGE_LOCALS_LB) (format-hex-byte PAGE_LOCALS_HB))))
;;   (check-equal? (peek pop-call-frame-n-slow-cf-state ZP_CALL_FRAME_TOP_MARK)
;;                 #x03
;;                 "top mark points to the first free byte on the call frame stack")

;;   (define pop-call-frame-n-slow-2-cf-code
;;     (append pop-call-frame-n-slow-cf-code
;;             ;; now the call frame is set to
;;             ;; (list "call-frame-ptr:   $xx03"   <-- no frame here any more, needs to go back to previous page
;;             ;;       "program-counter:  $090b"
;;             ;;       "function-ptr:     $090a"
;;             ;;       "locals-ptr:       $xx03, $xx03 (lb, hb)")
;;             (list
;;              (LDA !$02)
;;              (ast-bytes-cmd '() (list #x8d #xfa PAGE_CALL_FRAME)) ;; store 02 right behind top mark => ensure this is detected as fast frame
;;              (JSR VM_POP_CALL_FRAME)
;;              )))
;;   (define pop-call-frame-n-slow-2-cf-state
;;     (run-code-in-test pop-call-frame-n-slow-2-cf-code))

;;   (check-equal? (vm-call-frame->strings pop-call-frame-n-slow-2-cf-state)
;;                 (list (format "call-frame-ptr:   $~af7, topmark: fb" (format-hex-byte PAGE_CALL_FRAME))
;;                       "program-counter:  $090b"
;;                       "function-ptr:     $090a"
;;                       (format "locals-ptr:       $~a03, $~a03 (lb, hb), topmark: 03" (format-hex-byte PAGE_LOCALS_LB) (format-hex-byte PAGE_LOCALS_HB))
;;                       (format "fast-frame ($~af7..$~afa)" (format-hex-byte PAGE_CALL_FRAME) (format-hex-byte PAGE_CALL_FRAME))
;;                       "return-pc:           $0000"
;;                       "return-function-ptr: $0000"
;;                       (format "return-locals-ptr:   $~a02, $~a02 (lb,hb)" (format-hex-byte PAGE_LOCALS_LB) (format-hex-byte PAGE_LOCALS_HB)))
;;                 "f7 = fb (top mark) - 4 (slow frame)")
;;   (check-equal? (peek pop-call-frame-n-slow-2-cf-state ZP_CALL_FRAME_TOP_MARK)
;;                 #xfb
;;                 "top mark points to the first free byte on the call frame stack"))

;; @DC-FUN: VM_ALLOC_LOCALS, group: call_frame
;; allocate A locals onto the locals stack
;; input:  A number of locals needed by this function
;;         zp_locals_lb_ptr
;;         zp_locals_hb_ptr
;;         zp_locals_top_mark
;; output: A  new topmark
;;         zp_locals_lb_ptr
;;         zp_locals_hb_ptr
;;         zp_locals_top_mark
;; uses:   A, X, Y
(define-vm-function VM_ALLOC_LOCALS
  (list
          (TAX)                          ;; save original number of locals for after new page allocation (if necessary)
          (CLC)
          (ADC ZP_LOCALS_TOP_MARK)      
          (BCS ALLOCATE_NEW_LOCALS_PAGES__)

   (label STAY_ON_PAGE__)               ;; a holds new top mark
          (LDX ZP_LOCALS_TOP_MARK)      ;; x no longer needed, now loading current topmark
          (STX ZP_LOCALS_LB_PTR)
          (STX ZP_LOCALS_HB_PTR)        
          (STA ZP_LOCALS_TOP_MARK)
          (RTS)

   (label ALLOCATE_NEW_LOCALS_PAGES__)  ;; x holds number of parameters
          ;; store old topmark on page
          (LDA ZP_LOCALS_TOP_MARK)      ;; a can be discarded
          (LDY !$02)
          (STY ZP_LOCALS_LB_PTR)
          (STY ZP_LOCALS_HB_PTR)        ;; actually on lb is necessary, room for optimization
          (LDY !$00)
          (STA (ZP_LOCALS_LB_PTR),y)    ;; store topmark @02
          (STA (ZP_LOCALS_HB_PTR),y)    ;; actually on lb is necessary, room for optimization
          
          (STX ZP_LOCALS_TOP_MARK)      ;; remember X (number of parameters)

          ;; prepare call to new pages allocation
          (JSR ALLOC_LOCALS_STACK)

          ;; now set the locals pointers to local#0 and set the top mark
          (LDA !$03)
          (STA ZP_LOCALS_LB_PTR)
          (STA ZP_LOCALS_HB_PTR)   ;; new locals start here
          (CLC)
          (ADC ZP_LOCALS_TOP_MARK) ;; add number of locals allocated
          (STA ZP_LOCALS_TOP_MARK) ;; store top mark
          (RTS)))

(module+ test #| alloc locals |#
  (skip (check-equal? #t #f)))

;; @DC-FUN: VM_FREE_LOCALS, group: gc
;; input:  A number of locals to keep after free (locals of current function?)
;;         zp_locals_lb_ptr
;;         zp_locals_hb_ptr
;;         zp_locals_top_mark
;; output: A  new topmark
;;         zp_locals_lb_ptr
;;         zp_locals_hb_ptr
;;         zp_locals_top_mark
;; uses:   A, X, Y
(define-vm-function VM_FREE_LOCALS
  (list
          (LDY ZP_LOCALS_LB_PTR)
          (CPY !$03)
          (BEQ FREE_LOCALS_PAGES__)

   (label STAY_ON_PAGE__)
          (STA ZP_LOCALS_TOP_MARK)      ;; keep # parameters to keep
          (LDA ZP_LOCALS_LB_PTR)    
          (TAX)                         ;; keep old locals lb ptr lowbyte for new top mark
          (SEC)
          (SBC ZP_LOCALS_TOP_MARK)      ;; A = A (lb ptr low byte) - # params
          (STA ZP_LOCALS_LB_PTR)
          (STA ZP_LOCALS_HB_PTR)
          (STX ZP_LOCALS_TOP_MARK)
          (RTS)

   (label FREE_LOCALS_PAGES__)          ;; a = number of parameters
          (STA ZP_LOCALS_TOP_MARK)      ;; keep #

          ;; free the pages
          (LDX ZP_LOCALS_LB_PTR+1)
          (JSR VM_DEALLOCATE_PAGE)
          (LDX ZP_LOCALS_HB_PTR+1)
          (JSR VM_DEALLOCATE_PAGE)
          ;; (LDA ZP_LOCALS_LB_PTR+1)
          ;; (JSR FREE_PAGE_A)
          ;; (LDA ZP_LOCALS_HB_PTR+1)
          ;; (JSR FREE_PAGE_A)

          ;; get previous page (using data from the just freed pages, so make sure no one interferes!)
          (LDY !$01)
          (STA ZP_LOCALS_LB_PTR)
          (STA ZP_LOCALS_HB_PTR)
          (LDY !$00)
          (LDA (ZP_LOCALS_LB_PTR),y)
          (STA ZP_LOCALS_LB_PTR+1)
          (LDA (ZP_LOCALS_HB_PTR),y)
          (STA ZP_LOCALS_HB_PTR+1)

          ;; now get old top mark
          (INY)
          (LDA (ZP_LOCALS_LB_PTR),y)
          (SEC)
          (SBC ZP_LOCALS_TOP_MARK)
          (STA ZP_LOCALS_TOP_MARK)
          (STA ZP_LOCALS_LB_PTR)
          (STA ZP_LOCALS_HB_PTR)

          (RTS)))


;; @DC-FUN: VM_REFCOUNT_DECR_CURRENT_LOCALS, group: gc
;; decrement the refcount to all locals that are not initial (e.g. upon leaving a function)
(define-vm-function VM_REFCOUNT_DECR_CURRENT_LOCALS
  (list
          (LDA ZP_LOCALS_LB_PTR)
          (STA ZP_LOCALS_TOP_MARK) ;; restore top mark
          (LDY !$00)
          (LDA (ZP_FUNC_PTR),y)
          (AND !$0f)
          (TAY) ;; y = number of locals of current tunction
          ;; loop over locals -> rt, decr refcount
          (DEY)
          (BMI DONE__)
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
          (STA (ZP_LOCALS_HB_PTR),y)
          (DEY)
          (BPL LOOP__)
   (label DONE__)
          (RTS)

   (label COUNTER__)
          (byte 0)))


(module+ test #| free locals |#
  (skip (check-equal? #t #f)))

(define vm-call-frame-code
  (append VM_INITIALIZE_CALL_FRAME
          ALLOC_LOCALS_STACK
          VM_ALLOC_CALL_FRAME                              ;; allocate a new call frame, storing current top mark on previous frame (if existent)
          VM_PUSH_CALL_FRAME                               ;; push a new frame, respecting X = locals needed and vm_pc to decide whether fast or slow frames are used
          VM_POP_CALL_FRAME                                ;; pop last pushed frame, checking whether slow or fast frame is on top of call frame stack
          VM_ALLOC_LOCALS
          VM_FREE_LOCALS
          VM_REFCOUNT_DECR_CURRENT_LOCALS))

(module+ test #| vm-call-frame |#
  (inform-check-equal? (code-len (flatten vm-call-frame-code))
                       620
                       "estimated code length of call-frame runtime"))

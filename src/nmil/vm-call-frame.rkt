#lang racket/base

#|

implementation of list primitives (car, cdr, cons) using 6510 assembler routines

|#

(require (only-in racket/list flatten take))

(require "../6510.rkt")
(require "../util.rkt")
(require (only-in "../ast/6510-assembler.rkt" assemble assemble-to-code-list translate-code-list-for-basic-loader))
(require (only-in "../tools/6510-interpreter.rkt" peek-word-at-address cpu-state-clock-cycles peek))

(require (only-in "./vm-memory-manager.rkt"
                  ZP_CALL_FRAME
                  ZP_VM_PC
                  ZP_VM_FUNC_PTR
                  ZP_LOCALS_LB_PTR
                  ZP_LOCALS_HB_PTR
                  ZP_CALL_FRAME_TOP_MARK
                  vm-memory-manager
                  vm-stack->strings
                  vm-page->strings
                  vm-regt->string
                  vm-deref-cell-pair-w->string))


(module+ test #| after mem init |#
  (define PAGE_AVAIL_0 #x97)
  (define PAGE_AVAIL_0_W #x9700)
  (define PAGE_AVAIL_1 #x96)
  (define PAGE_AVAIL_1_W #x9600))

(module+ test
  (require "../6510-test-utils.rkt")
  (require (only-in "./vm-memory-manager-test-utils.rkt" run-code-in-test-on-code remove-labels-for))

  (define (wrap-code-for-test bc complete-code (mocked-code-list (list)))
    (append (list (org #xc000)
                  (JSR VM_INITIALIZE_MEMORY_MANAGER)
                  (JSR VM_INITIALIZE_CALL_FRAME))
            bc
            (list (BRK))
            (remove-labels-for complete-code (filter (lambda (ast-cmd) (ast-label-def-cmd? ast-cmd)) mocked-code-list))))

  (define (run-code-in-test bc (debug #f) #:mock (mocked-code-list (list)))
    (run-code-in-test-on-code 
     (wrap-code-for-test bc vm-call-frame mocked-code-list)
     debug)))


(require (only-in "../tools/6510-interpreter.rkt" 6510-load 6510-load-multiple initialize-cpu run-interpreter run-interpreter-on memory-list cpu-state-accumulator))

(provide vm-call-frame vm-call-frame->strings)

;; write call stack status
(define (vm-call-frame->strings state)
  (list (format "call-frame-ptr:   $~a" (format-hex-word (peek-word-at-address state ZP_CALL_FRAME)))
        (format "program-counter:  $~a" (format-hex-word (peek-word-at-address state ZP_VM_PC)))
        (format "function-ptr:     $~a" (format-hex-word (peek-word-at-address state ZP_VM_FUNC_PTR)))
        (format "locals-ptr:       $~a, $~a (lb, hb)"
                (format-hex-word (bytes->int (peek state ZP_LOCALS_LB_PTR) (peek state (add1 ZP_LOCALS_LB_PTR))))
                (format-hex-word (bytes->int (peek state ZP_LOCALS_HB_PTR) (peek state (add1 ZP_LOCALS_HB_PTR)))))
        ;; (format "cell-stack start: $~a,$~a"
        ;;         (format-hex-word (bytes->int (peek state ZP_CELL_STACK_LB_PTR) (peek state (add1 ZP_CELL_STACK_LB_PTR))))
        ;;         (format-hex-word (bytes->int (peek state ZP_CELL_STACK_HB_PTR) (peek state (add1 ZP_CELL_STACK_HB_PTR)))))
        ;; (format "tos index:        $~a" (format-hex-byte (peek state ZP_CELL_STACK_TOS)))
        ))



(define VM_INITIALIZE_CALL_FRAME
  (list
   (label VM_INITIALIZE_CALL_FRAME)
          ;; init current call frame (initial) no popping
          (LDA !$00)
          (STA ZP_CALL_FRAME+1)         ;; this ensures that NO previous call frame page is heeded!
          (STA ZP_CALL_FRAME_TOP_MARK)
          (JSR VM_ALLOC_CALL_FRAME_N)

          ;; alloc locals
          (LDX !$00)
          (LDY !$00)
          (JSR VM_ALLOC_CELL_STACK_PAGES)
          (STX ZP_LOCALS_LB_PTR+1)
          (STY ZP_LOCALS_HB_PTR+1)
          (LDA !$03) ;; payload start for locals
          (STA ZP_LOCALS_LB_PTR)
          (STA ZP_LOCALS_HB_PTR)
          (STA ZP_LOCALS_TOP_MARK)
          (RTS)))


;; close the current call frame (set top mark)
;; allocate a new call frame
;; and initialize the page accordingly
;; input:  ZP_CALL_FRAME
;;         ZP_CALL_FRAME_TOP_MARK
;; output: ZP_CALL_FRAME
;;         ZP_CALL_FRAME_TOP_MARK
;;         Y index for first byte available as payload (03) = ZP_CALL_FRAME_TOP_MARK
(define VM_ALLOC_CALL_FRAME_N
  (list
   (label VM_ALLOC_CALL_FRAME_N)
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
          (JSR VM_ALLOC_PAGE__PAGE_UNINIT)

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
(define VM_PUSH_CALL_FRAME_N
(flatten
  (list
   (label VM_PUSH_CALL_FRAME_N)
          ;; check for fast frames
          ;;    push: possible if - vm_pc and func-ptr share the same page
          (LDA ZP_VM_PC+1)
          (CMP ZP_VM_FUNC_PTR+1)
          (BNE SLOW_FRAME__VM_PUSH_CALL_FRAME_N)
          ;; check not necessary, stack keeps its state, pushing, popping takes care of cell-stack ptr
          ;; ;;                      - cell-stack does not overflow (has 16 entries reserve)
          ;; (LDA ZP_CELL_STACK_TOS)
          ;; (CMP !$F0)
          ;; (BPL SLOW_FRAME__VM_PUSH_CALL_FRAME_N)
          ;;                      - locals do not overflow (has reserves to hold functions' need)
          (TXA)                  ;; x = number of locals for this function
          (CLC)
          (ADC ZP_LOCALS_LB_PTR)
          (BCS SLOW_FRAME__VM_PUSH_CALL_FRAME_N)

          ;; do fast frame
          ;; alloc for fast frame
          ;; ensure call frame allows 4 additional bytes
          (LDA ZP_CALL_FRAME_TOP_MARK)
          (TAY)
          (ADC !$04)
          (BCC NO_NEW_PAGE_FOR_FAST_CALL_FRAME__VM_PUSH_CALL_FRAME_N)

          (JSR VM_ALLOC_CALL_FRAME_N)                  ;; now allocate a new page

   (label NO_NEW_PAGE_FOR_FAST_CALL_FRAME__VM_PUSH_CALL_FRAME_N)
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
          (LDA ZP_VM_FUNC_PTR)
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

   (label SLOW_FRAME__VM_PUSH_CALL_FRAME_N)
          ;; alloc for slow frame
          ;; ensure call frame allows 10 additional bytes
          (LDA ZP_CALL_FRAME_TOP_MARK)
          (TAY)
          (CLC)
          (ADC !$08) ;; length of slow call frame
          (BCC NO_NEW_PAGE_FOR_SLOW_CALL_FRAME__VM_PUSH_CALL_FRAME_N)

          (JSR VM_ALLOC_CALL_FRAME_N)
          ;; alloc call frame returns y with topmark
          ;; (LDY ZP_CALL_FRAME_TOP_MARK) ;; first payload byte available on call frame page

   (label NO_NEW_PAGE_FOR_SLOW_CALL_FRAME__VM_PUSH_CALL_FRAME_N)
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
          (SBC ZP_VM_FUNC_PTR+1)
          ;; (AND !$01)                    ;; make sure that only bit 0 can be set (should not happen, since function code may max spread over two pages!)
          (STA (ZP_CALL_FRAME),y)         ;; y currently = 7
          (DEY)

          ;; copy rest of zero page values
          ;; naiive copy: 7 * 5 bytes = 35 bytes, 7 * 9 cycles = 63 cycles
          ;; looped copy: 10 bytes + 7 table bytes, 7 * 18 cycles = 126 cycles
    (label LOOP_ZP_COPY__VM_PUSH_CALL_FRAME_N)
          (LDX SLOW_STACK_ZP_LOCATIONS,y)
          (LDA $00,x)
          (STA (ZP_CALL_FRAME),y)        ;; y currently = 6..0  (7 bytes) copied
          (DEY)
          (BPL LOOP_ZP_COPY__VM_PUSH_CALL_FRAME_N)           ;; >= 0? -> loop

          (RTS)

   (label SLOW_STACK_ZP_LOCATIONS) ;; processed in reverse order, starting with offset+08 (used be vm_pop_call_frame_n, too!!)
          (byte-ref ZP_VM_PC)               ;; |                   vm pc                     |
          (byte-ref ZP_VM_PC+1)
          (byte-ref ZP_TEMP)                ;; |   reserved           | locals ptr shared lb |
          (byte-ref ZP_LOCALS_LB_PTR)
          (byte-ref ZP_LOCALS_LB_PTR+1)     ;; | locals-lb page       | locals-hb-page       |
          (byte-ref ZP_LOCALS_HB_PTR+1)
          (byte-ref ZP_VM_FUNC_PTR)         ;; |  func-ptr  low       | $00 / $01            | func-ptr could be encoded into: lowbyte, highbyte =  vm_pc page + $00/$01 (of byte 4 in this stack) <- would save two bytes of stack size
          )))

(module+ test #| VM_PUSH_CALL_FRAME_N |#
  (define push-call-frame-n-fit-page-prep-code
    (list
       ;; set complete vm state to values to be pushed
      (JSR VM_ALLOC_PAGE__PAGE_UNINIT)
      (STA ZP_CALL_FRAME+1)             ;;         zp_call_frame
      (LDY !$03)
      (STY ZP_CALL_FRAME)               ;; => zp_call_frame $cc03
      (STY ZP_CALL_FRAME_TOP_MARK)      ;; => zp_call_frame_top_mark $03
      (INY) ;;04
      (STY ZP_LOCALS_LB_PTR)
      (STY ZP_LOCALS_HB_PTR)            ;; same index (low enough for alloc to stay on page)
      (INY) ;; 05
      (STY ZP_LOCALS_LB_PTR+1)          ;; => zp_locals_lb_ptr $0504
      (INY) ;; 06
      (STY ZP_LOCALS_HB_PTR+1)          ;; => zp_locals_hb_ptr $0604
      (INY) ;; 07
      (STY ZP_CELL_STACK_LB_PTR)
      (STY ZP_CELL_STACK_HB_PTR)        ;; same index (low enough for alloc to stay on page)
      (INY) ;; 08
      (STY ZP_CELL_STACK_LB_PTR+1)      ;; => zp_cell_stack_hb_ptr $0807
      (INY) ;; 09
      (STY ZP_CELL_STACK_HB_PTR+1)      ;; => zp_cell_stack_hb_ptr $0907
      (STY ZP_CELL_STACK_TOS)
      (STY ZP_VM_FUNC_PTR+1)            ;; share same page
      (STY ZP_VM_PC+1)
      (INY) ;; 0a
      (STY ZP_VM_FUNC_PTR)              ;; => zp_vm_func_ptr $090a
      (INY) ;; 0b
      (STY ZP_VM_PC)                    ;; => zp_vm_pc $090b

      ;;  ;; push the call frame
      ;; (LDX !$04) ;; reserve 4 local cells
      ;; (JSR VM_PUSH_CALL_FRAME_N)
      ))

  (define push-call-frame-n-fit-page-prep-state
    (run-code-in-test push-call-frame-n-fit-page-prep-code))

  (check-equal? (vm-call-frame->strings push-call-frame-n-fit-page-prep-state)
                (list (format "call-frame-ptr:   $~a03" (format-hex-byte PAGE_AVAIL_0))
                      "program-counter:  $090b"
                      "function-ptr:     $090a"
                      "locals-ptr:       $0504, $0604 (lb, hb)")
                "prepared call-frame is initial (and empty)")
  (check-equal? (peek push-call-frame-n-fit-page-prep-state ZP_CALL_FRAME_TOP_MARK)
                #x03
                "top mark points to the first free byte on the call frame stack")

  (define push-call-frame-n-fit-page-code
    (append push-call-frame-n-fit-page-prep-code
             (list
              (LDX !$04) ;; need 4 local cells
              (JSR VM_PUSH_CALL_FRAME_N))))
  (define push-call-frame-n-fit-page-state
    (run-code-in-test push-call-frame-n-fit-page-code))

  (check-equal? (memory-list push-call-frame-n-fit-page-state (+ PAGE_AVAIL_0_W #x03) (+ PAGE_AVAIL_0_W #x06))
                (list #x0b #x09
                      #x0a
                      #x04)
                "the call frame is a fast frame (4 bytes) holding the full pc (2b), low byte of function-ptr and low byte of locals-ptr")
  (check-equal? (vm-call-frame->strings push-call-frame-n-fit-page-state)
                (list (format "call-frame-ptr:   $~a03" (format-hex-byte PAGE_AVAIL_0))
                      "program-counter:  $090b"
                      "function-ptr:     $090a"
                      "locals-ptr:       $0504, $0604 (lb, hb)")
                "call-frame is pointing to the current one push ")
  (check-equal? (peek push-call-frame-n-fit-page-state ZP_CALL_FRAME_TOP_MARK)
                #x07
                "top mark points to the first free byte on the call frame stack (past the one pushed)")

  (define push-call-frame-misfit-page-sl-frame-0-code
    (append push-call-frame-n-fit-page-code ;; one frame has been pushed
             (list
              (LDA !$fb)
              (STA ZP_CALL_FRAME_TOP_MARK) ;; set mark such that push will need new page
              (JSR VM_PUSH_CALL_FRAME_N)
              (INC ZP_VM_PC+1)             ;; pc on other page => need for slow call frame

              (LDX !$03) ;; need 3 local cells
              (JSR VM_PUSH_CALL_FRAME_N))))
  (define push-call-frame-misfit-page-sl-frame-0-state
    (run-code-in-test push-call-frame-misfit-page-sl-frame-0-code))

  (check-equal? (vm-call-frame->strings push-call-frame-misfit-page-sl-frame-0-state)
                (list (format "call-frame-ptr:   $~a03" (format-hex-byte PAGE_AVAIL_1))
                      "program-counter:  $0a0b"
                      "function-ptr:     $090a"
                      "locals-ptr:       $0504, $0604 (lb, hb)"))
  (check-equal? (peek push-call-frame-misfit-page-sl-frame-0-state ZP_CALL_FRAME_TOP_MARK)
                #x0b
                "top mark points to the first free byte on the call frame stack (past the one pushed)")
  (check-equal? (memory-list push-call-frame-misfit-page-sl-frame-0-state (+ PAGE_AVAIL_1_W #x03) (+ PAGE_AVAIL_1_W #x04))
                (list #x0b #x0a)
                "the call frame is a slow frame (8 bytes)
                 holding the full pc (2b)")
  (check-equal? (memory-list push-call-frame-misfit-page-sl-frame-0-state (+ PAGE_AVAIL_1_W #x06) (+ PAGE_AVAIL_1_W #x0a))
                (list #x04
                      #x05
                      #x06
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

              (LDX !$03) ;; need 3 local cells
              (JSR VM_PUSH_CALL_FRAME_N))))
  (define push-call-frame-misfit-page-sl-frame-1-state
    (run-code-in-test push-call-frame-misfit-page-sl-frame-1-code))

  (check-equal? (vm-call-frame->strings push-call-frame-misfit-page-sl-frame-1-state)
                (list (format "call-frame-ptr:   $~a03" (format-hex-byte PAGE_AVAIL_1))
                      "program-counter:  $090b"
                      "function-ptr:     $090a"
                      "locals-ptr:       $05fe, $06fe (lb, hb)"))
  (check-equal? (peek push-call-frame-misfit-page-sl-frame-1-state ZP_CALL_FRAME_TOP_MARK)
                #x0b
                "top mark points to the first free byte on the call frame stack (past the one pushed)")
  (check-equal? (memory-list push-call-frame-misfit-page-sl-frame-1-state (+ PAGE_AVAIL_1_W #x03) (+ PAGE_AVAIL_1_W #x04))
                (list #x0b #x09)
                "the call frame is a slow frame (8 bytes)
                 holding the full pc $090b")
  (check-equal? (memory-list push-call-frame-misfit-page-sl-frame-1-state (+ PAGE_AVAIL_1_W #x06) (+ PAGE_AVAIL_1_W #x0a))
                (list #xfe
                      #x05
                      #x06
                      #x0a
                      #x00)
                "the call frame is a slow frame (8 bytes)
                 low byte of locals-ptr"))


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
(define VM_POP_CALL_FRAME_N
  (list
   (label VM_POP_CALL_FRAME_N)
          ;; current frame fast?
          (SEC)
          (LDA ZP_CALL_FRAME_TOP_MARK)
          (SBC ZP_CALL_FRAME)
          (CMP !$04)
          (BNE SLOW_FRAME__VM_POP_CALL_FRAME_N)

          ;; is a fast frame
          (LDY !$00)
          (LDA (ZP_CALL_FRAME),y)
          (STA ZP_VM_PC)
          (INY)
          (LDA (ZP_CALL_FRAME),y)
          (STA ZP_VM_PC+1)
          (STA ZP_VM_FUNC_PTR+1)
          (INY)
          (LDA (ZP_CALL_FRAME),y)
          (STA ZP_VM_FUNC_PTR)
          (INY)
          (LDA (ZP_CALL_FRAME),y)
          (STA ZP_LOCALS_LB_PTR)
          (STA ZP_LOCALS_HB_PTR)
          (BCS RECONSTRUCT_CALL_FRAME_AND_TOP_MARK__VM_POP_CALL_FRAME_N)  ;; carry is always clear (since iny stays below 256)

   (label SLOW_FRAME__VM_POP_CALL_FRAME_N)
          (LDY !$06)                    ;; copy 7 bytes

   (label LOOP_ZP_RESTORE__VM_POP_CALL_FRAME_N)
          (LDX SLOW_STACK_ZP_LOCATIONS,y)
          (LDA (ZP_CALL_FRAME),y)
          (STA $00,x)
          (DEY)
          (BPL LOOP_ZP_RESTORE__VM_POP_CALL_FRAME_N)

          ;; now get function pointer page from encoded byte
          (LDY !$07)                    ;; index of last byte in slow call frame
          (LDA ZP_VM_PC+1)              ;; was just set by copy loop before
          (SEC)
          (SBC (ZP_CALL_FRAME),y)       ;; contains either $00 or $01 since this is a slow frame
          (STA ZP_VM_FUNC_PTR+1)           ;; page of func ptr is page of pc - $00/$01

          ;; make sure the locals ptr are synchronized (only locals-lb-ptr is put on stack completely)
          (LDA ZP_LOCALS_LB_PTR)
          (STA ZP_LOCALS_HB_PTR)

   (label RECONSTRUCT_CALL_FRAME_AND_TOP_MARK__VM_POP_CALL_FRAME_N)
          (LDA ZP_CALL_FRAME)
          (CMP !$03)
          (BNE STAY_ON_CALL_FRAME_PAGE__VM_POP_CALL_FRAME_N)

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
          (STA ZP_CALL_FRAME) ;; do this so zp_call_frame behaves as if no page change took place (will be put into top mark eventually)

          ;; free old call frame page!
          (TXA)
          (JSR VM_FREE_PAGE)

          ;; continue with previous call frame recon

   (label STAY_ON_CALL_FRAME_PAGE__VM_POP_CALL_FRAME_N)
          ;; one one below top mark
          (LDA ZP_CALL_FRAME+1)
          (STA LOAD_TOP_M1_BYTE__VM_POP_CALL_FRAME_N+2)
          (LDY ZP_CALL_FRAME_TOP_MARK)
          (DEY)
   (label LOAD_TOP_M1_BYTE__VM_POP_CALL_FRAME_N)
          (LDA $c000,y)
          (AND !$fe)
          (BEQ PREVIOUS_IS_SLOW_CALL_FRAME__VM_POP_CALL_FRAME_N)

          ;; previous is fast frame
          (LDA !$04) ;; length of fast frame
          (BNE SET_TOP_MARK_AND_FRAME__VM_POP_CALL_FRAME_N)

   (label PREVIOUS_IS_SLOW_CALL_FRAME__VM_POP_CALL_FRAME_N)
          (LDA !$08) ;; length of slow frame

   (label SET_TOP_MARK_AND_FRAME__VM_POP_CALL_FRAME_N)
          (STA ZP_TEMP)
          (LDA ZP_CALL_FRAME)
          (STA ZP_CALL_FRAME_TOP_MARK)
          (SEC)
          (SBC ZP_TEMP)                 ;; temp holds len of call frame (04 = fast, 0a = slow)
          (STA ZP_CALL_FRAME)
          (RTS)))

(module+ test #| pop call frame n |#
  (define pop-call-frame-n-fast-cf-code
    (append push-call-frame-n-fit-page-prep-code
            (list
             (LDX !$04)
             (JSR VM_PUSH_CALL_FRAME_N)
             (LDX !$04)
             (JSR VM_PUSH_CALL_FRAME_N)

             ;; now change pc
             (LDY !$91)
             (STY ZP_VM_PC)
             (INY)
             (STY ZP_VM_PC+1)
             (INY)
             ;; function pointer
             (STY ZP_VM_FUNC_PTR)
             (INY)
             (STY ZP_VM_FUNC_PTR+1)
             (INY)
             ;; and lowbyte of locals ptr
             (STY ZP_LOCALS_LB_PTR)
             (INY)
             (STY ZP_LOCALS_HB_PTR)

             (JSR VM_POP_CALL_FRAME_N)
             )))
  (define pop-call-frame-n-fast-cf-state
    (run-code-in-test pop-call-frame-n-fast-cf-code))

  (check-equal? (peek pop-call-frame-n-fast-cf-state ZP_CALL_FRAME_TOP_MARK)
                #x07)
  (check-equal? (vm-call-frame->strings pop-call-frame-n-fast-cf-state)
                (list (format "call-frame-ptr:   $~a03" (format-hex-byte PAGE_AVAIL_0))
                      "program-counter:  $090b"
                      "function-ptr:     $090a"
                      "locals-ptr:       $0504, $0604 (lb, hb)")
                "restore original call-frame-ptr,
                         program counter,
                         functions-ptr
                         and locals pointer")

  (define pop-call-frame-n-slow-cf-code
    (append push-call-frame-misfit-page-sl-frame-0-code
            ;; now the call frame is set to
            ;; (list "call-frame-ptr:   $cb03"   <-- slow frame with 8 bytes
            ;;       "program-counter:  $0a0b"
            ;;       "function-ptr:     $090a"
            ;;       "locals-ptr:       $0504, $0604 (lb, hb)")
            (list
             ;; overwrite call-frame data: pc, function pointer and locals ptr (hi/lo bytes)
             (LDY !$21)
             (STY ZP_VM_PC)
             (INY)
             (STY ZP_VM_PC+1)
             (INY)
             (STY ZP_VM_FUNC_PTR)
             (INY)
             (STY ZP_VM_FUNC_PTR+1)
             (INY)
             (STY ZP_LOCALS_LB_PTR)
             (STY ZP_LOCALS_HB_PTR)
             (INY)
             (STY ZP_LOCALS_LB_PTR+1)
             (INY)
             (STY ZP_LOCALS_HB_PTR+1)

             ;; restore should restore all pointers of the pushed call-frame
             (JSR VM_POP_CALL_FRAME_N)
             )))
  (define pop-call-frame-n-slow-cf-state
    (run-code-in-test pop-call-frame-n-slow-cf-code))

  (check-equal? (vm-call-frame->strings pop-call-frame-n-slow-cf-state)
                (list (format "call-frame-ptr:   $~afb" (format-hex-byte PAGE_AVAIL_0))
                      "program-counter:  $0a0b"
                      "function-ptr:     $090a"
                      "locals-ptr:       $0504, $0604 (lb, hb)"))
  (check-equal? (peek pop-call-frame-n-slow-cf-state ZP_CALL_FRAME_TOP_MARK)
                #xff
                "top mark points to the first free byte on the call frame stack (past the one pushed)"))

;; input:  A number of locals needed by this function
;;         zp_locals_lb_ptr
;;         zp_locals_hb_ptr
;;         zp_locals_top_mark
;; output: A  new topmark
;;         zp_locals_lb_ptr
;;         zp_locals_hb_ptr
;;         zp_locals_top_mark
;; uses:   A, X, Y
(define VM_ALLOC_LOCALS
  (list
   (label VM_ALLOC_LOCALS)
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
          (LDX ZP_LOCALS_LB_PTR+1)
          (LDY ZP_LOCALS_HB_PTR+1)
          (JSR VM_ALLOC_CELL_STACK_PAGES)
          (STX ZP_LOCALS_LB_PTR+1)
          (STY ZP_LOCALS_PTR+1)

          ;; now set the locals pointers to local#0 and set the top mark
          (LDA !$03)
          (STA ZP_LOCALS_LB_PTR)
          (STA ZP_LOCALS_HB_PTR)   ;; new locals start here
          (CLC)
          (ADC ZP_LOCALS_TOP_MARK) ;; add number of locals allocated
          (STA ZP_LOCALS_TOP_MARK) ;; store top mark
          (RTS)))

(module+ test #| alloc locals |#
  (check-equal? #t #f))

;; input:  A number of locals to keep after free (locals of current function?)
;;         zp_locals_lb_ptr
;;         zp_locals_hb_ptr
;;         zp_locals_top_mark
;; output: A  new topmark
;;         zp_locals_lb_ptr
;;         zp_locals_hb_ptr
;;         zp_locals_top_mark
;; uses:   A, X, Y
(define VM_FREE_LOCALS
  (list
   (label VM_FREE_LOCALS)
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
          (LDA ZP_LOCALS_LB_PTR+1)
          (JSR VM_FREE_PAGE)
          (LDA ZP_LOCALS_HB_PTR+1)
          (JSR VM_FREE_PAGE)

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

(module+ test #| free locals |#
  (check-equal? #t #f))

(define vm-call-frame
  (append vm-memory-manager
          VM_INITIALIZE_CALL_FRAME
          VM_ALLOC_CALL_FRAME_N                              ;; allocate a new call frame, storing current top mark on previous frame (if existent)
          VM_PUSH_CALL_FRAME_N                               ;; push a new frame, respecting X = locals needed and vm_pc to decide whether fast or slow frames are used
          VM_POP_CALL_FRAME_N                                ;; pop last pushed frame, checking whether slow or fast frame is on top of call frame stack
          VM_ALLOC_LOCALS
          VM_FREE_LOCALS
          ))

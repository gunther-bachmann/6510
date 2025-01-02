#lang racket/base

(require "../6510.rkt")
(require "../6510-test-utils.rkt")

(require (only-in "../6510-utils.rkt" absolute))
(require (only-in "../ast/6510-assembler.rkt"
                  assemble
                  assemble-to-code-list
                  translate-code-list-for-basic-loader
                  org-for-code-seq))
(require (only-in "../ast/6510-resolver.rkt" ->resolved-decisions label-instructions))
(require (only-in "../ast/6510-relocator.rkt" label-string-offsets))
(require (only-in "../tools/6510-interpreter.rkt"
                  cpu-state-program-counter
                  cpu-state-clock-cycles
                  peek-word-at-address
                  6510-load-multiple
                  initialize-cpu
                  peek
                  6510-load
                  run-interpreter
                  run-interpreter-on
                  memory-list
                  cpu-state-accumulator))
(require (only-in "../tools/6510-debugger.rkt"
                  run-debugger-on
                  dispatch-debugger-command
                  debugger--run
                  debugger--push-breakpoint
                  debugger--remove-breakpoints))
(require (only-in "../tools/6510-debugger-shared.rkt" debug-state-states breakpoint))
(require (only-in "../util.rkt" bytes->int format-hex-byte format-hex-word))
(require (only-in "./vm-bc-disassembler.rkt"
                  disassembler-byte-code--byte-count
                  disassemble-byte-code))
(require (only-in "./vm-memory-manager.rkt"
                  vm-cell-at-nil?
                  vm-page->strings
                  vm-stack->strings
                  vm-regt->string
                  vm-cell-at->string
                  vm-cell->string
                  vm-deref-cell-pair-w->string
                  VM_QUEUE_ROOT_OF_CELL_PAIRS_TO_FREE

                  ast-const-get
                  ZP_RT
                  ZP_VM_PC
                  ZP_LOCALS_LB_PTR
                  ZP_LOCALS_HB_PTR
                  ZP_VM_FUNC_PTR
                  ZP_CALL_FRAME
                  ZP_CELL_STACK_TOS
                  ZP_CELL_STACK_LB_PTR
                  ZP_CELL_STACK_HB_PTR))
(require (only-in "./vm-call-frame.rkt" vm-call-frame->strings))


(require (only-in racket/string string-prefix? string-join))
(require (only-in racket/format ~a))
(require (only-in racket/match match-let))
(require (only-in racket/list flatten take empty? range))
(require (only-in racket/port open-output-nowhere))

(provide run-bc-wrapped-in-test- vm-next-instruction-bytes)


(define (print-list-of-labels label-list label-offsets)
  (unless (empty? label-list)
    (define str (symbol->string (car label-list)))
    (displayln (format "~a : ~a" str (number->string (hash-ref label-offsets str) 16)))
    (print-list-of-labels (cdr label-list) label-offsets)))

(define (vm-pc state)
  (absolute (peek state (add1 ZP_VM_PC))
            (peek state ZP_VM_PC)))

(define (vm-next-instruction-bytes state (n 1))
  (memory-list state
               (vm-pc state)
               (sub1 (+ n (vm-pc state)))))

(define bc-debugger--instruction-breakpoint-name "stop on bytecode interpreter")

(define (vm-local->string state num)
  (define num-i
    (if (string? num)
        (string->number num 16)
        num))
  (format "local #$~a: ~a"
          (format-hex-byte num-i)
          (vm-cell->string
           (peek state (+ num-i (peek-word-at-address state ZP_LOCALS_LB_PTR)))
           (peek state (+ num-i (peek-word-at-address state ZP_LOCALS_HB_PTR))))))

(define (vm-list->strings state address (string-list '()))
  (cond [(= address #x0001) ;; this is the nil ptr
         (reverse string-list)]
        [else
         (unless (= (bitwise-and #x03 address) #x01)
           (raise-user-error (format "address is not a cell-pair-ptr ~a" (format-hex-word address))))
         (define cell-cdr (peek-word-at-address state (+ address 2)))
         (unless (= (bitwise-and #x03 cell-cdr) #x01)
           (raise-user-error (format "cdr cell is not a cell-pair-ptr => this is no list ~a" (format-hex-word cell-cdr)) ))
         (if (vm-cell-at-nil? state address)
             (reverse string-list)
             (vm-list->strings state
                              cell-cdr
                              (cons (vm-cell-at->string state address)
                                    string-list)))]))
(define (debugger--bc-help d-state)
  (for-each displayln (list "bc commands"
                            ""
                            "? | h    print this help screen"
                            "q        quit debugger"
                            ""
                            "ps       print the stack"
                            "pf       print the call frame"
                            "pfn      print running function meta data"
                            "pl n     print the n-th local (of the call frame)"
                            "pml adr  print memory location as list (must be the address of a cell-pair!)"
                            "rt       print register t"
                            "ruc      run until next call or return instruction"
                            "rur      run until returned from current call"
                            "s        run one step"
                            "so       step over the current bc (even calls)"
                            "sa adr   stop at bytecode at the given adr (not implemented)"
                            "sab byte stop at the given bytecode (not implemented)"
                            "^        (prefix) pass the following commands to the assembly debugger"))
  d-state)

(define (debugger--disassemble d-state (offset 0))
  (define c-state (car (debug-state-states d-state)))
  (define pc (+ (peek-word-at-address c-state ZP_VM_PC) offset))
  (define bc (peek c-state pc))
  (define bc_p1 (peek c-state (add1 pc)))
  (define bc_p2 (peek c-state (+ 2 pc)))
  (format "$~a: $~a ~a"
          (format-hex-word pc)
          (~a (string-join (take (map format-hex-byte
                                      (list bc bc_p1 bc_p2))
                                 (disassembler-byte-code--byte-count bc))
                           " $")
              #:min-width 10)
          (disassemble-byte-code bc bc_p1 bc_p2)))

(define (debugger--disassemble-lines d-state (lines 10) (offset 0))
  (when (> lines 0)
    (displayln (debugger--disassemble d-state offset))
    (define c-state (car (debug-state-states d-state)))
    (debugger--disassemble-lines
     d-state
     (- lines 1)
     (+ offset
        (disassembler-byte-code--byte-count (peek c-state (+ offset (peek-word-at-address c-state ZP_VM_PC))))))))

(define (debugger--bc-run-until d-state interpreter-loop-adr condition-fn)
  (debugger--push-breakpoint
   (debugger--remove-breakpoints
    (debugger--run
     (debugger--push-breakpoint
      (debugger--remove-breakpoints d-state bc-debugger--instruction-breakpoint-name)
      (lambda (bc-state)
        (and
         (condition-fn bc-state)
         (= (cpu-state-program-counter bc-state)
            interpreter-loop-adr)))
      "next instruction is a call or return"
      #f))
    "next instruction is a call or return")
   (lambda (lc-state)
     (eq? (cpu-state-program-counter lc-state)
          interpreter-loop-adr))
   bc-debugger--instruction-breakpoint-name
   #f))

(define (debugger--bc-dispatcher- interpreter-loop-adr)
  (lambda (command d-state)
    (define c-state (car (debug-state-states d-state)))
    (define pa-regex #px"^pa ([[:xdigit:]])$")
    (define pl-regex #px"^pl ([[:xdigit:]])$")
    (define pml-regex #px"^pml ([[:xdigit:]]*)$")
    (cond [(or (string=? command "?") (string=? command "h")) (debugger--bc-help d-state)]
          [(string=? command "pp") (debugger--disassemble-lines d-state) d-state]
          [(string=? command "ps") (begin (for-each (lambda (str) (displayln str)) (vm-stack->strings c-state)) d-state)]
          [(string=? command "pt") (begin (displayln (format "rt: ~a" (vm-regt->string c-state))) d-state)]
          [(string=? command "pfn") (begin
                                      (define func-ptr (peek-word-at-address c-state ZP_VM_FUNC_PTR))
                                      (displayln (format "function-ptr: $~a" (format-hex-word func-ptr)))
                                      (define locals-num (peek c-state func-ptr))
                                      (displayln (format "locals used : ~a" (number->string locals-num)))
                                      (for-each  displayln (map (lambda (n) (vm-local->string c-state n)) (range locals-num)))
                                      d-state)]
          [(string=? command "s") (debugger--run d-state #t)]
          [(string=? command "pf") (begin (for-each displayln (vm-call-frame->strings c-state)) d-state)]
          [(regexp-match? pl-regex command)
           (match-let (((list _ num) (regexp-match pl-regex command)))
             (begin (displayln (vm-local->string c-state num))
                    d-state))]
          [(regexp-match? pml-regex command)
           (match-let (((list _ num) (regexp-match pml-regex command)))
             (begin (displayln (format "list at $~a > ~a" num (vm-list->strings c-state (string->number num 16))))
                    d-state))]
          [(string=? command "ruc")
           (debugger--bc-run-until d-state interpreter-loop-adr
                                   (lambda (bc-state)
                                     (memq (peek bc-state (peek-word-at-address bc-state ZP_VM_PC))
                                           '(#x34 ;; call
                                             #x35 ;; tail call
                                             ))))]
          [(string=? command "so")
           (cond [(= #x34 (peek c-state (peek-word-at-address c-state ZP_VM_PC)))
                  ;; step over, since it is a call!
                  (define state-after-call (debugger--run d-state #t))
                  (define nc-state (car (debug-state-states state-after-call)))
                  (define parent-pc (peek-word-at-address nc-state (peek-word-at-address nc-state ZP_CALL_FRAME)))
                  (display (format "running until hitting byte code at $~a ..." (format-hex-word parent-pc)))
                  (debugger--bc-run-until state-after-call interpreter-loop-adr
                                          (lambda (bc-state)
                                            (= (peek-word-at-address bc-state ZP_VM_PC) parent-pc)))]
                 [else (debugger--run d-state #t)])]
          [(string=? command "rur")
           ;; get previous (stored vm_pc, to which to return to)
           (define parent-pc (peek-word-at-address c-state (peek-word-at-address c-state ZP_CALL_FRAME)))
           (display (format "running until hitting byte code at $~a ..." (format-hex-word parent-pc)))
           (debugger--bc-run-until d-state interpreter-loop-adr
                                   (lambda (bc-state)
                                     (= (peek-word-at-address bc-state ZP_VM_PC) parent-pc)))]
          [(string-prefix? command "^")
           (dispatch-debugger-command (substring command 1) d-state) ]
          [else
           (displayln "dispatching -> assembler-debugger")
           (dispatch-debugger-command command d-state)])))

(define (debugger--bc-interactor interpreter-loop-adr)
  (list
   `(dispatcher . ,(debugger--bc-dispatcher- interpreter-loop-adr))
   `(prompter . ,(lambda (d-state) (format "BC [~x] > " (length (debug-state-states d-state)))))
   `(pre-prompter . ,(lambda (d-state) (string-append "\n" (debugger--disassemble d-state))))))

(define (run-bc-wrapped-in-test- bc wrapped-code (debug #f))
  ;; (define wrapped-code (wrap-bytecode-for-test bc))
  (define org-code-start (org-for-code-seq wrapped-code))
  (define resolved-dec (->resolved-decisions (label-instructions wrapped-code) wrapped-code))
  (define label->offset (label-string-offsets org-code-start resolved-dec))
  (define interpreter-loop (hash-ref label->offset "VM_INTERPRETER"))
  (define state-before
    (6510-load-multiple (initialize-cpu)
                        (assemble-to-code-list wrapped-code)))
  (if debug
      (run-debugger-on state-before
                       ""
                       #t
                       (list
                        (breakpoint bc-debugger--instruction-breakpoint-name
                                    (lambda (lc-state)
                                      (eq? (cpu-state-program-counter lc-state)
                                           interpreter-loop))
                                    #f))
                       (debugger--bc-interactor interpreter-loop)
                       #t)
      (parameterize ([current-output-port (open-output-nowhere)])
        (run-interpreter-on state-before))))

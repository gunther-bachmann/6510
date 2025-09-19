#lang racket/base

(require (only-in ansi-color
                  with-colors
                  foreground-color
                  color-displayln)
         racket/exn
         (only-in racket/format
                  ~a)
         (only-in racket/list
                  flatten
                  take
                  empty?
                  range
                  drop)
         (only-in racket/match
                  match-let)
         (only-in racket/port
                  open-output-nowhere)
         racket/set
         (only-in racket/string
                  string-prefix?
                  string-join)
         "../6510-test-utils.rkt"
         (only-in "../6510-utils.rkt"
                  absolute)
         (only-in "../6510-utils.rkt"
                  low-byte
                  high-byte)
         "../6510.rkt"
         (only-in "../ast/6510-assembler.rkt"
                  assemble
                  assemble-to-code-list
                  new-assemble-to-code-list
                  assembly-code-list-org-code-sequences
                  assembly-code-list-labels
                  translate-code-list-for-basic-loader
                  org-for-code-seq)
         (only-in "../ast/6510-relocator.rkt"
                  label-string-offsets)
         (only-in "../ast/6510-resolver.rkt"
                  ->resolved-decisions
                  label-instructions)
         (only-in "../tools/6510-debugger-shared.rkt"
                  debug-state-states
                  debug-state-tracepoints
                  debug-state-labels
                  tracepoint-description
                  breakpoint
                  tracepoint
                  debug-state)
         (only-in "../tools/6510-debugger.rkt"
                  run-debugger-on
                  dispatch-debugger-command
                  debugger--run
                  push-debugger-interactor
                  debugger--assembler-interactor
                  debugger--push-breakpoint
                  debugger--remove-breakpoints)
         (only-in "../tools/6510-interpreter.rkt"
                  cpu-state-program-counter
                  cpu-state-clock-cycles
                  peek-word-at-address
                  6510-load-multiple
                  exn:fail:cpu-interpreter?
                  exn:fail:cpu-interpreter-cpu-state
                  initialize-cpu
                  peek
                  poke
                  6510-load
                  run-interpreter
                  run-interpreter-on
                  memory-list
                  cpu-state-accumulator)
         (only-in "../util.rkt"
                  bytes->int
                  format-hex-byte
                  format-hex-word)
         (only-in "./vm-bc-disassembler.rkt"
                  disassembler-byte-code--byte-count
                  disassemble-byte-code)
         (only-in "./vm-call-frame.rkt"
                  vm-call-frame->strings)
         (only-in "./vm-inspector-utils.rkt"
                  shorten-cell-string
                  shorten-cell-strings
                  vm-cell-at-nil?
                  vm-page->strings
                  vm-stack->strings
                  vm-regt->string
                  vm-cell-at->string
                  vm-cell->string
                  vm-deref-cell-pair-w->string)
         (only-in "./vm-memory-map.rkt"
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

(provide run-bc-wrapped-in-test-
         vm-next-instruction-bytes
         vm-cell-pairs-used-info
         vm-list->strings
         vm-cell-pair-pages
         vm-cell-pairs-free-in-page
         vm-cell-pairs-used-num-in-page)


(define (print-list-of-labels label-list label-offsets)
  (unless (empty? label-list)
    (define str (symbol->string (car label-list)))
    (displayln (format "~a : ~a" str (number->string (hash-ref label-offsets str) 16)))
    (print-list-of-labels (cdr label-list) label-offsets)))

;; is the given page (only the high byte of an address) a page of cell-pairs?
(define (vm-cell-pair-page? state page)
  (= #x40 (bitwise-and #xc0 (peek state (arithmetic-shift page 8)))))

;; is the given page (only the high byte of an address) a page of cells?
(define (vm-cell-page? state page)
  (= #x80 (bitwise-and #x80 (peek state (arithmetic-shift page 8)))))

;; is the given page (only the high byte of an address) a m1 page?
(define (vm-m1-page? state page)
  (= #x00 (bitwise-and #xe8 (peek state (arithmetic-shift page 8)))))

;; given that this page is a m1 page, what profile is used
(define (vm-profile-of-m1-page state page)
  (bitwise-and #x07 (peek state (arithmetic-shift page 8))))

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
           (peek state (+ num-i (peek-word-at-address state ZP_LOCALS_HB_PTR)))
           state
           #t)))

(define (vm-list->strings state address (string-list '()) (follow #f))
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
                              (cons (vm-cell-at->string state address #f follow)
                                    string-list)
                              follow))]))
(define (debugger--bc-help d-state)
  (with-colors
    'green
    (lambda ()
      (for-each displayln (list "bc commands"
                                ""
                                "? | h                 print this help screen"
                                "q                     quit debugger"
                                ""
                                "bt n                  go back to persisted debugger state n"
                                "page | pg n           print data to page n"
                                "ps                    print the stack"
                                "pf                    print the call frame"
                                "pfn                   print running function meta data"
                                "pl <n?>               print the n-th local (of the call frame), or all of them"
                                "pml adr               print cell at memory location as list (must be the address of a cell-pair!)"
                                "ppml adr              pretty print cell at memory location as list (must be the address of a cell-pair!)"
                                "pma adr               print cell at memory location (can be anything)"
                                "ppma adr              pretty print cell at memory location (can be anything)"
                                "pp <B?> <A?>          pretty print the next B (hex) commands starting at address A (hex)" 
                                "pt                    print cell register t"
                                "rbc <bc> <bc>? <bc>?  run a byte code command on current virtual machine"
                                "ruc                   run until next call or return instruction"
                                "rur                   run until returned from current call"
                                "run                   run up to next breakpoint set"
                                "trace on | to         switch bc level tracing on"
                                "s                     run one step"
                                "so                    step over the current bc (even calls)"
                                "stop pc = <A>         stop byte code interpretation at address <A>"
                                "clear stop pc = <A>   clear that breakpoint"
                                "sab byte              stop at the given bytecode (not implemented)"
                                "dive                  push assembler interactor"
                                "^                     (prefix) pass the following commands to the assembly debugger"))))
  d-state)

;; run a byte command (made up of bc-code) on the current machine without changing the pc
(define (run-bc-command bc-codes d-state interpreter-loop-adr)
  (define c-state-num (length (debug-state-states d-state)))
  (define c-state (car (debug-state-states d-state)))
  (cond [(not (= interpreter-loop-adr (cpu-state-program-counter c-state)))
         ;; if not on the interpreter loop, restoring won't be able to construct the right cpu state
         (with-colors 'red (lambda () (displayln "rbc only available if currently in the bc interpreter loop!")))
         d-state]
        [else
         (color-displayln (format "running bytecodes '~a' on current machine..." (apply disassemble-byte-code bc-codes)))
         (define pc (peek-word-at-address c-state ZP_VM_PC))
         (define old-bcs (map (lambda (n) (peek c-state (+ n pc))) (list 0 1 2)))
         (define new-state (apply poke (cons c-state (cons pc bc-codes))))
         (define new-d-state (struct-copy debug-state d-state
                                          [states (cons new-state (debug-state-states d-state))]))
         (define state-run (wrap-into-bc-states new-d-state (debugger--run new-d-state #t) interpreter-loop-adr))
         (define c-state-run (car (debug-state-states state-run)))
         ;; now restore old pc and byte code
         (define old-pc-state (poke c-state-run ZP_VM_PC (low-byte pc) (high-byte pc)))
         (define restored-state (apply poke (cons old-pc-state (cons pc old-bcs))))
         (define final-d-state 
           (struct-copy debug-state state-run
                        [states (cons restored-state                               
                                      (drop (debug-state-states state-run)
                                            (- (length (debug-state-states state-run)) c-state-num)))]))
         final-d-state]))

(define (debugger--disassemble c-state (offset 0) #:labels (labels (hash)))
  (define pc (+ (peek-word-at-address c-state ZP_VM_PC) offset))
  (define bc (peek c-state pc))
  (define bc_p1 (peek c-state (add1 pc)))
  (define bc_p2 (peek c-state (+ 2 pc)))
  (format "~a: ~a ~a"
          (format-hex-word pc)
          (~a (string-join (take (map format-hex-byte
                                      (list bc bc_p1 bc_p2))
                                 (disassembler-byte-code--byte-count bc))
                           " ")
              #:min-width 10)
          (disassemble-byte-code bc bc_p1 bc_p2 #:labels labels)))

(define (debugger--disassemble-lines d-state (lines 10) (offset 0))
  (when (> lines 0)
    (color-displayln (debugger--disassemble (car (debug-state-states d-state)) offset #:labels (debug-state-labels d-state)))
    (define c-state (car (debug-state-states d-state)))
    (debugger--disassemble-lines
     d-state
     (- lines 1)
     (+ offset
        (disassembler-byte-code--byte-count (peek c-state (+ offset (peek-word-at-address c-state ZP_VM_PC))))))))

;; run until a breakpoint other than the regular bc instruction break point hits
(define (debugger--bc-run d-state interpreter-loop-adr)
  (debugger--push-breakpoint
   (debugger--run
    (debugger--remove-breakpoints d-state bc-debugger--instruction-breakpoint-name))   
   (lambda (lc-state)
     (eq? (cpu-state-program-counter lc-state)
          interpreter-loop-adr))
   bc-debugger--instruction-breakpoint-name
   #f))

;; run until the given condition hits (or any other break point) other than the regular bc instruction level break
;; remove the breakpoint with this condition again (regardless which breakpoint actually triggered)
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


(define (wrap-into-bc-states previous-d-state run-d-state bc-loop-address)
  (define states-to-keep (debug-state-states previous-d-state))
  (define old-count (length states-to-keep))
  (define all-new-states (debug-state-states run-d-state))
  (define new-count (length all-new-states))
  (define new-states (take all-new-states (- new-count old-count)))
  (define filtered-new-states
    (filter (lambda (c-state)
              (= (cpu-state-program-counter c-state)
                 bc-loop-address))
             new-states))
  ;; (displayln (format "old-count ~a\nnew-count ~a\nfiltered-new-states ~a"
  ;;                    old-count new-count (length filtered-new-states)))
  (struct-copy debug-state run-d-state
               [states (append filtered-new-states states-to-keep)]))

(define (createBcTracepoint interpreter-loop-adr)
  (tracepoint
   "bc step trace"
   (lambda (c-state)
     (= (cpu-state-program-counter c-state)
        interpreter-loop-adr))
   (lambda (c-state)
     (define stack-len (sub1 (length (vm-stack->strings c-state 20))))
     (define top (vm-regt->string c-state))
     (color-displayln
      (format "~a\t~a ~a" 
              (~a (debugger--disassemble c-state)
                  #:align 'left
                  #:width 40
                  #:pad-string " ")              
              top
              (if (> stack-len 1)
                  (~a (format "(... ~a more)" (sub1 stack-len))
                      #:align 'right
                      #:width (max 0 (- 40 (string-length top)))
                      #:pad-string " ")
                  ""))))
   #f))

(define (debugger--bc-dispatcher- interpreter-loop-adr)
  (lambda (command d-state)
    (define c-state (car (debug-state-states d-state)))
    (define page-regex #px"^(page|pg) *([[:xdigit:]]{2})$")
    (define pa-regex #px"^pa ([[:xdigit:]])$")
    (define pl-regex #px"^pl *([[:xdigit:]])?$")
    (define pml-regex #px"^pml ([[:xdigit:]]*)$")
    (define ppml-regex #px"^ppml ([[:xdigit:]]*)$")
    (define pma-regex #px"^pma ([[:xdigit:]]*)$")
    (define ppma-regex #px"^ppma ([[:xdigit:]]*)$")
    (define pp-regex #px"^pp *([[:xdigit:]]{1,2})? *([[:xdigit:]]{1,4})?$")
    (define stop-pc-regex #px"^(clear *)?stop *pc *= *([[:xdigit:]]{1,4})$")
    (define bt-regex #px"^bt *([[:xdigit:]]{1,4})$")
    (define rbc-regex #px"^rbc *([[:xdigit:]]{2}) *(([[:xdigit:]]{2}) *([[:xdigit:]]{2})?)?")
    (foreground-color 'yellow)
    (begin0
        (with-handlers ([exn:fail? (lambda (e)
                                     (color-displayln (format "ignored exception (~a)" (exn->string e)))
                                     d-state)])
          (cond [(or (string=? command "?") (string=? command "h")) (debugger--bc-help d-state)]
                [(string=? command "dive") (push-debugger-interactor debugger--assembler-interactor d-state)]
                [(string=? command "fl") (color-displayln (vm-cell-pair-free-list-info c-state)) d-state]
                [(or (string=? command "trace on")
                    (string=? command "to"))
                 (struct-copy debug-state d-state
                              [tracepoints (cons (createBcTracepoint interpreter-loop-adr)
                                                 (debug-state-tracepoints d-state))])]
                [(or (string=? command "trace off")
                    (string=? command "tf"))
                 (struct-copy debug-state d-state
                              [tracepoints (filter (lambda (tp)
                                                     (not (string=? (tracepoint-description tp)
                                                                  "bc step trace")))
                                                 (debug-state-tracepoints d-state))])]
                [(regexp-match? rbc-regex command)
                 (match-let (((list _ bc0 _ bc1 bc2) (regexp-match rbc-regex command)))
                   (run-bc-command (map (lambda (nstr)
                                          (string->number nstr 16))
                                        (filter (lambda (val) val) (list bc0 bc1 bc2)))
                                   d-state
                                   interpreter-loop-adr))]
                [(regexp-match? pp-regex command)
                 (match-let (((list _ len address) (regexp-match pp-regex command)))
                   (begin
                     (debugger--disassemble-lines 
                      d-state
                      (if len (string->number len 16) 10)
                      (if address
                          (- (string->number address 16)
                             (peek-word-at-address c-state ZP_VM_PC))
                          0))
                     d-state))]
                [(regexp-match? bt-regex command)
                 (match-let (((list _ num) (regexp-match bt-regex command)))
                   (define drop-num (- (length (debug-state-states d-state)) (if num (string->number num 16) 1)))
                   (cond [(or (< drop-num 0) (>= drop-num (length (debug-state-states d-state))))  
                          (begin (with-colors 'red (lambda () (displayln "cannot go to that point in time"))) d-state)]
                         [else
                          (struct-copy debug-state d-state
                                       [states (drop (debug-state-states d-state) drop-num)])]))]
                [(string=? command "ps") (begin (color-displayln (string-join (vm-stack->strings c-state) "\n  ")) d-state)]
                [(string=? command "pt") (begin (color-displayln (format "rt: ~a" (vm-regt->string c-state))) d-state)]
                [(string=? command "pfn") (begin
                                            (define func-ptr (peek-word-at-address c-state ZP_VM_FUNC_PTR))
                                            (color-displayln (format "function-ptr: $~a" (format-hex-word func-ptr)))
                                            (define locals-num (peek c-state func-ptr))
                                            (color-displayln (format "locals used : ~a" (number->string locals-num)))
                                            (for-each  color-displayln (map (lambda (n) (vm-local->string c-state n)) (range locals-num)))
                                            d-state)]
                [(string=? command "s")
                 (wrap-into-bc-states d-state (debugger--run d-state #t) interpreter-loop-adr)]
                [(string=? command "pf") (begin (for-each color-displayln (vm-call-frame->strings c-state)) d-state)]
                [(regexp-match? page-regex command)
                 (match-let (((list _ _ page) (regexp-match page-regex command)))
                   (define page-num (string->number page 16))
                   (map color-displayln (vm-page->strings c-state page-num))
                   (cond [(vm-cell-pair-page? c-state page-num)
                          (map color-displayln (vm-cell-pairs-used-info c-state (string->number page 16)))]
                          [(vm-cell-page? c-state page-num)
                           (color-displayln "no more details for cell pages")]
                          [(vm-m1-page? c-state page-num)
                           (color-displayln (format "no more details for m1 pages (profile ~a)" (vm-profile-of-m1-page c-state page-num)))]
                          [else (color-displayln "no more details for this page type")]))
                 d-state]
                [(regexp-match? pl-regex command)
                 (match-let (((list _ num) (regexp-match pl-regex command)))
                   (cond [(string? num)
                          (color-displayln (vm-local->string c-state num))]
                         [else
                          (define func-ptr (peek-word-at-address c-state ZP_VM_FUNC_PTR))
                          (define locals-num (peek c-state func-ptr))
                          (color-displayln (format "there are ~a locals in this function:" locals-num))
                          (when (> locals-num 0)
                            (map
                             (lambda (n)
                               (color-displayln (vm-local->string c-state n)))
                             (range locals-num)))
                          ])
                   d-state)]
                [(regexp-match? pml-regex command)
                 (match-let (((list _ num) (regexp-match pml-regex command)))
                   (begin (color-displayln (string-join (vm-list->strings c-state (string->number num 16)) " "))
                          d-state))]
                [(regexp-match? ppml-regex command)
                 (match-let (((list _ num) (regexp-match ppml-regex command)))
                   (begin (color-displayln (string-join (shorten-cell-strings (vm-list->strings c-state (string->number num 16) (list) #t)) " "))
                          d-state))]
                [(regexp-match? pma-regex command)
                 (match-let (((list _ num) (regexp-match pma-regex command)))
                   (begin
                     (define low (peek c-state (string->number num 16)))
                     (define high (peek c-state (add1 (string->number num 16))))
                     (color-displayln (vm-cell->string low high c-state #t))
                     d-state))]
                [(regexp-match? ppma-regex command)
                 (match-let (((list _ num) (regexp-match ppma-regex command)))
                   (begin
                     (define low (peek c-state (string->number num 16)))
                     (define high (peek c-state (add1 (string->number num 16))))
                     (color-displayln (shorten-cell-string (vm-cell->string low high c-state #t)))
                     d-state))]
                [(string=? command "ruc")
                 (wrap-into-bc-states
                  d-state
                  (debugger--bc-run-until d-state interpreter-loop-adr
                                          (lambda (bc-state)
                                            (memq (peek bc-state (peek-word-at-address bc-state ZP_VM_PC))
                                                  '(#x34 ;; call
                                                    #x35 ;; tail call
                                                    ))))
                  interpreter-loop-adr)]
                [(or (string=? command "run")
                    (string=? command "r"))
                 (wrap-into-bc-states d-state (debugger--bc-run d-state interpreter-loop-adr) interpreter-loop-adr)]
                [(string=? command "so")
                 (cond [(= #x34 (peek c-state (peek-word-at-address c-state ZP_VM_PC)))
                        ;; step over, since it is a call!
                        (define state-after-call (debugger--run d-state #t))
                        (define nc-state (car (debug-state-states state-after-call)))
                        (define parent-pc (peek-word-at-address nc-state (peek-word-at-address nc-state ZP_CALL_FRAME)))
                        (color-display (format "running until hitting byte code at $~a ..." (format-hex-word parent-pc)))
                        (wrap-into-bc-states d-state
                                             (debugger--bc-run-until state-after-call interpreter-loop-adr
                                                                     (lambda (bc-state)
                                                                       (= (peek-word-at-address bc-state ZP_VM_PC) parent-pc)))
                                             interpreter-loop-adr)]
                       [else (debugger--run d-state #t)])]
                [(regexp-match? stop-pc-regex command)
                 (match-let (((list _ cl value) (regexp-match stop-pc-regex command)))
                   (cond [cl
                          (begin
                            (color-displayln (format "clear breakpoint at byte code pc = $~a" value))
                            (debugger--remove-breakpoints d-state (format "stop at bc pc = $~a" value)))]
                         [else
                          (begin                      
                            (color-displayln (format "set breakpoint at bc pc = $~a" value))
                            (debugger--push-breakpoint
                             d-state
                             (lambda (lc-state)
                               (and (= (peek-word-at-address lc-state ZP_VM_PC)
                                     (string->number value 16))
                                  (= (cpu-state-program-counter lc-state)
                                     interpreter-loop-adr)))
                             (format "stop at bc pc = $~a" value)))]))]
                [(string=? command "rur")
                 ;; get previous (stored vm_pc, to which to return to)
                 (define parent-pc (peek-word-at-address c-state (peek-word-at-address c-state ZP_CALL_FRAME)))
                 (color-display (format "running until hitting byte code at $~a ..." (format-hex-word parent-pc)))
                 (wrap-into-bc-states
                  d-state
                  (debugger--bc-run-until d-state interpreter-loop-adr
                                          (lambda (bc-state)
                                            (= (peek-word-at-address bc-state ZP_VM_PC) parent-pc)))
                  interpreter-loop-adr)]
                [(string-prefix? command "^")
                 (dispatch-debugger-command (substring command 1) d-state) ]
                [else
                 (color-displayln "dispatching -> assembler-debugger")
                 (dispatch-debugger-command command d-state)]))
      (foreground-color 'b-white))))

(define (debugger--bc-interactor interpreter-loop-adr)
  (list
   `(dispatcher . ,(debugger--bc-dispatcher- interpreter-loop-adr))
   `(prompter . ,(lambda (d-state) (format "BC [~x] > " (length (debug-state-states d-state)))))
   `(pre-prompter . ,(lambda (d-state) (string-append "\n" (debugger--disassemble (car (debug-state-states d-state)) #:labels (debug-state-labels d-state)))))))


;; get the number of pages not in the free list nor allocated (totally untouched/free)
(define (vm-free-pages-num state (page-map-start #xcf00))
  (foldl (lambda (idx acc)
           (define page-status (peek state (+ page-map-start idx)))
           (+ acc (if (= #xff page-status) 1 0)))
         0
         (range 256)))

(define (vm-cell-pair-pages- state page (result (list)))
  (cond [(= 0 page) result]
        [else
         (define next-page (peek state (bitwise-ior (arithmetic-shift page 8) #xff)))
         (vm-cell-pair-pages-
          state next-page
          (cons page result))]))

;; get number of used cell pairs on the page
(define (vm-cell-pairs-used-num-in-page state page)
  (bitwise-and #x3f (peek state (arithmetic-shift page 8))))

(define (vm-cell-pairs-free-in-page- state page free-cp-off (result (list)))
  (cond [(= 0 free-cp-off) result]
        [else
         (define next-free-cp-off (peek state (bytes->int free-cp-off page)))
         (vm-cell-pairs-free-in-page-
          state
          page
          next-free-cp-off
          (cons free-cp-off result))]))
;; get a list of offsets in the current page of free cells
(define (vm-cell-pairs-free-in-page state page)
  (vm-cell-pairs-free-in-page- state page (peek state (bytes->int page #xcf))))

;; return a list of cell-pair offset that are on the page (used and free ones)
(define (vm-cell-pairs-on-page-)
  `(#x05 #x09 ,@(map (lambda (n) (+ (* 4 n) #x41)) (range 47))))

(define (vm-cell-pairs-used-on-page state page)
  (set->list
   (set-subtract
    (list->set (vm-cell-pairs-on-page-))
    (list->set (vm-cell-pairs-free-in-page state page)))))

(define (vm-cell-pairs-used-info state page)
  (map (lambda (offset)
         (vm-cell->string offset page state #t))
       (sort (vm-cell-pairs-used-on-page state page) <)))

;; get list of pages used for cell-pairs
(define (vm-cell-pair-pages state)
  (define page-w-free-cell-pairs (peek state #xcec3))
  (vm-cell-pair-pages- state page-w-free-cell-pairs))

(define (vm-cell-pair-free-list- state free-cell-pair-adr (result (list)))
  (cond [(= 0 (bitwise-and #xff free-cell-pair-adr)) (reverse result)]
        [else
         (define next (peek-word-at-address state free-cell-pair-adr))
         (vm-cell-pair-free-list- state next (cons free-cell-pair-adr result))]))

;; give a list of pointers to free cell-pairs that are free for reallocation
(define (vm-cell-pair-free-list-info state)
  (define first-free (peek-word-at-address state #xcecc))
  (define free-adr-list (vm-cell-pair-free-list- state first-free))
  (cond [(empty? free-adr-list) "free-list: empty"]
        [else
         (string-append
          "free-list: $"
          (string-join
           (map format-hex-word 
                free-adr-list)
           ", $"))]))

(define (run-bc-wrapped-in-test- bc wrapped-code (debug #f))
  ;; (define wrapped-code (wrap-bytecode-for-test bc))
  (define org-code-start (org-for-code-seq wrapped-code))
  (define resolved-dec (->resolved-decisions (label-instructions wrapped-code) wrapped-code))
  (define label->offset (label-string-offsets org-code-start resolved-dec))
  (define interpreter-loop (hash-ref label->offset "VM_INTERPRETER"))
  (define assembly (new-assemble-to-code-list wrapped-code))
  (define state-before
    (6510-load-multiple (initialize-cpu)
                        (assembly-code-list-org-code-sequences assembly)))
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
                       (list)
                       (debugger--bc-interactor interpreter-loop)
                       #t
                       #:labels (assembly-code-list-labels assembly))
      (with-handlers ([exn:fail:cpu-interpreter?
                       (lambda (e)
                         (displayln "exception raised, continue in debugger")
                         (run-debugger-on (exn:fail:cpu-interpreter-cpu-state e) "" #t)
                         )])
        (parameterize ([current-output-port (open-output-nowhere)])
          (run-interpreter-on state-before)))))

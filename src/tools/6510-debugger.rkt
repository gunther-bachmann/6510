#lang racket
#|

 allow to interactively debug 6510 opcodes (makeing use of readline)

 (run-debugger org raw-bytes)

 |#

(require (rename-in  racket/contract [define/contract define/c]))
(require readline/readline)
(require threading)
(require "../6510-utils.rkt")
(require "6510-interpreter.rkt")
(require (only-in "../asm/6510-parser.rkt" asm->ast))
(require (only-in "6510-disassembler.rkt" disassemble disassemble-single code-bytes))
(require (only-in "../ast/6510-resolver.rkt" commands->bytes))
(require (only-in racket/fixnum fx+))
(require (only-in "../ast/6510-command.rkt" ast-command?))
(require (only-in "../ast/6510-assembler.rkt" assemble))
(require (only-in "./6510-screen-display.rkt" 6510-debugger--print-string))
(require (only-in "./6510-debugger-sync-source.rkt"
                  load-source-map
                  6510-debugger--remove-all-addresses-on-source
                  6510-debugger--show-disassembly-on-source-lines
                  6510-debugger--show-address-on-source-lines
                  6510-debugger--move-cursor-to-source-line
                  6510-debugger--remove-disassembly-on-source-lines
                  pc-source-map-entry?
                  pc-source-map-entry-file
                  pc-source-map-entry-line))
(require (only-in "./6510-processor-display.rkt"
                  6510-debugger--proc-buffer-display
                  6510-debugger--proc-buffer-kill))
(require (only-in "./6510-emacs-integration.rkt"
                  6510-debugger--has-single-step-cap
                  6510-debugger--has-output-cap
                  6510-debugger--has-proc-display-cap))
(require "6510-debugger-shared.rkt")

(provide run-debugger
         run-debugger-on
         debugger--assembler-interactor
         dispatch-debugger-command
         debugger--run
         debugger--push-breakpoint
         debugger--remove-breakpoints)

(module+ test
  (require threading)
  (require rackunit))

;; TODO allow execution of arbirtray racket with values explicitly set into the namespace to evaluate breakpoint conditions (or do calculations)
;; (namespace-set-variable-value! 'some 25)
;; (namespace-variable-value 25)
;; (eval '(+ some 5)) ;; => 30

(define debugger-max-uninterrupted-steps 4096)

;; set or clear flags
(define/c (modify-flag set flag c-state)
  (-> boolean? string? cpu-state? cpu-state?)
  (cond [(string=? flag "c")
         (if set (set-carry-flag c-state) (clear-carry-flag c-state))]
        [(string=? flag "n")
         (if set (set-negative-flag c-state) (clear-negative-flag c-state))]
        [(string=? flag "i")
         (if set (set-interrupt-flag c-state) (clear-interrupt-flag c-state))]
        [(string=? flag "b")
         (if set (set-brk-flag c-state) (clear-brk-flag c-state))]
        [(string=? flag "v")
         (if set (set-overflow-flag c-state) (clear-overflow-flag c-state))]
        [(string=? flag "z")
         (if set (set-zero-flag c-state) (clear-zero-flag c-state))]
        [else c-state]))

(define/c (debugger--pretty-print address len d-state)
  (-> (or/c string? false?) (or/c string? false?) debug-state? debug-state?)
  (define c-state (car (debug-state-states d-state)))
  (display (disassemble c-state
                        (if address (string->number address 16)  (cpu-state-program-counter c-state))
                        (if len (string->number len 16) 1)))
  d-state)

(define/c (debugger--print-memory address len d-state)
  (-> (or/c string? false?) (or/c string? false?) debug-state? debug-state?)
  (define c-state (car (debug-state-states d-state)))
  (define adr (if address (string->number address 16)  (cpu-state-program-counter c-state)))
  (displayln (memory->string adr
                            (+ -1 adr (if len (string->number len 16) 1))
                            c-state))
  d-state)

(define/c (debugger--set-memory address value d-state)
  (-> (or/c string? false?) (or/c string? false?) debug-state? debug-state?)
  (define c-state (car (debug-state-states d-state)))
  (struct-copy debug-state d-state 
               [states (cons (poke c-state (string->number address 16) (string->number value 16))
                             (debug-state-states d-state))]) )

(define/c (debugger--set-accumulator value d-state)
  (-> (or/c string? false?) debug-state? debug-state?)
  (define c-state (car (debug-state-states d-state)))
  (struct-copy debug-state d-state 
               [states (cons (with-accumulator c-state (string->number value 16))
                             (debug-state-states d-state))]))

(define/c (debugger--set-program-counter word-value d-state)
  (->  (or/c string? false?) debug-state? debug-state?)
  (define c-state (car (debug-state-states d-state)))
  (struct-copy debug-state d-state 
               [states (cons (with-program-counter c-state (string->number word-value 16))
                             (debug-state-states d-state))]))

(define/c (debugger--set-or-clear-flag set-or-clear flag d-state)
  (-> string? string? debug-state? debug-state?)
  (define c-state (car (debug-state-states d-state)))
  (struct-copy debug-state d-state 
               [states (cons (modify-flag (string-prefix? set-or-clear "s") flag c-state)
                             (debug-state-states d-state))]))

(define/c (instruction-byte-len c-state)
  (-> cpu-state? exact-positive-integer?)
  (let-values (((_ commandBytes) (disassemble-single c-state))) commandBytes))

(define/c (debugger--execute-command command force d-state)
  (-> string? boolean? debug-state? debug-state?)
  (with-handlers ([exn:fail? ;; catch parse error of the 6510 opcode
                   (lambda (e) (displayln e) d-state)])
    (define c-state (car (debug-state-states d-state)))
    (let ([byteList (compile-opcodes (string-trim command))])
      (cond [(or force
                 (eq? (instruction-byte-len c-state)
                      (length byteList)))
             (struct-copy debug-state d-state
                          [states (cons (-pokem c-state (cpu-state-program-counter c-state) byteList)
                                        (debug-state-states d-state))])]
            [else
             (begin (displayln "byte length differs (use xf to force)")
                    d-state)]))))

(define/c (debugger--remove-breakpoints d-state description)
  (-> debug-state? string? debug-state?)
  (struct-copy debug-state d-state
               [breakpoints (dropf (debug-state-breakpoints d-state)
                                   (lambda (breakpoint)
                                     (string=?
                                      description
                                      (breakpoint-description breakpoint))))]))

(define/c (debugger--push-breakpoint d-state fun description (verbose #t))
  (->* [debug-state? any/c string?] [boolean?] debug-state?)
  (struct-copy debug-state d-state
               [breakpoints (cons (breakpoint description fun verbose)
                                  (debug-state-breakpoints d-state))]))

(define/c (debugger--pop-breakpoint d-state)
  (-> debug-state? debug-state?)
  (struct-copy debug-state d-state [breakpoints (cdr (debug-state-breakpoints d-state))]))

(define/c (debugger--run d-state (display #t))
  (->* [debug-state?] [boolean?] debug-state?)
  (define c-states (debug-state-states d-state))
  (define next-states (cons (execute-cpu-step (car c-states) #t (debug-state-output-function d-state)) c-states))
  (let-values (((breakpoint new-states) (run-until-breakpoint next-states (debug-state-breakpoints d-state) 0 (debug-state-output-function d-state))))
    (when (and breakpoint display (breakpoint-verbose breakpoint))
      (displayln (format "hit breakpoint ~a" (breakpoint-description breakpoint))))
    (struct-copy debug-state d-state [states new-states])))

(define/c (print-latest-cpu-state d-state)
  (-> debug-state? debug-state?)
  (printf "\t\t") (print-state (car (debug-state-states d-state)) #t)
  d-state)

(define/c (next-cpu-step? byte-code cpu-state)
  (-> byte/c cpu-state? boolean?)
  (= (peek-pc cpu-state) byte-code))

;; run len steps (1 if len is false)
(define/c (debugger--run-steps d-state len (depth 0) (follow-jsr #f) (i-call #t))
  (->* [debug-state? exact-nonnegative-integer?] [exact-nonnegative-integer? boolean? boolean?] debug-state?)
  (define c-states (debug-state-states d-state))
  (define breakpoint (if i-call  #f (breakpoint-hits (car c-states) (debug-state-breakpoints d-state))))
  (when breakpoint
    (printf "breakpoint hit: ~a\n" (breakpoint-description breakpoint)))
  (cond [(or (and (<= len 0) (or (not follow-jsr) (= 0 depth)))
            breakpoint)
         d-state]
        [else
         (let ([next-step-jsr (next-cpu-step? #x20 (car c-states))]
               [next-step-rts (next-cpu-step? #x60 (car c-states))]
               [sp            (cpu-state-stack-pointer (car c-states))]
               [new-states    (cons (execute-cpu-step (car c-states) #t (debug-state-output-function d-state)) c-states)])
           (debugger--run-steps
            (struct-copy debug-state d-state
                         [states new-states])
            (- len 1)
            (+ depth
               (if (and (> sp (cpu-state-stack-pointer (car new-states)))
                      next-step-jsr)
                   1
                   0)
               (if next-step-rts -1 0))
            follow-jsr
            #f))]))

(define/c (debugger--help d-state)
  (-> debug-state? debug-state?)
  (define help #<<EOF
pp <B?> <A?>          pretty print the next B (hex) commands starting at address A (hex)
pm <A?> <B?>          print memory starting @A (hex) for B bytes (hex)
set mem <A> = <B>     set memory @A (hex) to byte B (hex)
step <N?>             step forward (single or N times)
step over <N?>        step over the next N commands, without following sub routine jumps (jsr)
back <N?>             backward step (single or N times)
set a = <B>           set accumulator to byte B (hex)
set pc = <A>          set program counter to address A (hex)
xf? { <command> }     eXecute Force? the given 6510 command (force if byte len differs)
stop pc = <A>         set break point to stop at pc = address A (hex)
stop a = <B>          stop when accumulator is set to (hex) B
stop sp = <B>         stop when stack pointer is set to (hex) B
list stops            list active breakpoints
commit <B>?           keep only the last 10 | B (hex) states
clear flag [cbnvzi]   clear flag (Carry,Break,Negative,Overflow,Zero,Interrupt)
set flag [cbnvzi]     set flag (Carry,Break,Negative,Overflow,Zero,Interrupt)
run                   run until a break point is hit
inc pc                increment program counter (e.g. to step over a BRK instruction)
p                     print cpu state
clear stops           clear all breakpoints
q                     quit
.                     repeat last command (multiple dots repeat multiple times)
EOF
    )
  (displayln help)
  d-state)

;; decode the given debugger command and dispatch to the debugger
(define/c (dispatch-debugger-command command d-state)
  (-> string? debug-state? debug-state?)
  (define c-states (debug-state-states d-state))
  (define c-state (car c-states))
  (define pp-regex #px"^pp *([[:xdigit:]]{1,2})? *([[:xdigit:]]{1,4})?$")
  (define pm-regex #px"^pm *([[:xdigit:]]{1,4})? *([[:xdigit:]]{1,2})?$")
  (define sm-regex #px"^s(et)? *m(em)? *([[:xdigit:]]{1,4}) *= *([[:xdigit:]]{1,2})$")
  (define s-regex #px"^s(tep)? *([[:xdigit:]]{1,2})?$")
  (define so-regex #px"^s(tep)? *o(ver)? *([[:xdigit:]]{1,2})?$")
  (define b-regex #px"^b(ack)? *([[:xdigit:]]{1,2})?$")
  (define sa-regex #px"^s(et)? *a *= *([[:xdigit:]]{1,2})$")
  (define spc-regex #px"^s(et)? *pc *= *([[:xdigit:]]{1,4})$")
  (define x-regex #px"^xf? *\\{(.*)\\}$")
  (define stop-pc-regex #px"^(clear *)?stop *pc *= *([[:xdigit:]]{1,4})$")
  (define stop-a-regex #px"^(clear *)?stop *a *= *([[:xdigit:]]{1,2})$")
  (define stop-sp-regex #px"^(clear *)?stop *sp *= *([[:xdigit:]]{1,2})$")
  (define list-bp-regex #px"^l(ist)? *s(tops)?$")
  (define commit-regex #px"^commit *([[:xdigit:]]{1,2})?$")
  (define flags-regex #px"^(s(et)?|c(lear)?) *f(lag)? *([cbnvzi])$")
  (define run-regex #px"^r(un)?")
  (define incpc_regex #px"^i(nc)? *p(c)?")
  (define options-refex #px"^t(oggle)? o(ption)? (verbose-step)")
  (cond [(or (string=? command "?") (string=? command "h")) (debugger--help d-state)]
        ;; increment pc (to step over brk for example)
        [(regexp-match? incpc_regex command)
         (struct-copy debug-state d-state
                      [states (cons (with-program-counter c-state (fx+ 1 (cpu-state-program-counter c-state))) c-states)])]
        ;; b - go back in history
        [(regexp-match? b-regex command)
         (match-let (((list _ _ value) (regexp-match b-regex command)))
           (define states (debug-state-states d-state))
           (struct-copy debug-state d-state [states (list-tail states (min (- (length states) 1) (if value (string->number value 16) 1)))]))]
        ;; s - single step
        [(regexp-match? s-regex command)
         (match-let (((list _ _ len) (regexp-match s-regex command)))
           (~>> d-state
               (debugger--run-steps _ (if len (string->number len 16) 1))))]
        ;; so - step over
        [(regexp-match? so-regex command)
         (match-let (((list _ _ _ len) (regexp-match so-regex command)))
           (~>> d-state
               (debugger--run-steps _ (if len (string->number len 16) 1) 0 #t)             
               (debugger--pretty-print #f "1" _)
               (print-latest-cpu-state _)))]
        ;; p - print processor state
        [(string=? command "p") (print-state c-state) d-state]
        ;; pp - disassemble (pretty print)
        [(regexp-match? pp-regex command)
         (match-let (((list _ len address) (regexp-match pp-regex command)))
           (begin0
               (debugger--pretty-print address len d-state)
             (displayln "")))]
        ;; pm - print memory
        [(regexp-match? pm-regex command)
         (match-let (((list _ address len) (regexp-match pm-regex command)))
           (debugger--print-memory address len d-state))]
        ;; sm - set memory
        [(regexp-match? sm-regex command)
         (match-let (((list _ _ _ address value) (regexp-match sm-regex command)))
           (debugger--set-memory address value d-state))]
        ;; sa - set accumulator
        [(regexp-match? sa-regex command)
         (match-let (((list _ _ value) (regexp-match sa-regex command)))
           (debugger--set-accumulator value d-state))]
        ;; spc - set program counter
        [(regexp-match? spc-regex command)
         (match-let (((list _ _ value) (regexp-match spc-regex command)))
           (debugger--set-program-counter value d-state))]
        ;; [cs]f[nvibcz] - clear / set flag
        [(regexp-match? flags-regex command)
         (match-let (((list _ set-or-clear _ _ _ flag) (regexp-match flags-regex command)))
           (debugger--set-or-clear-flag set-or-clear flag d-state))]
        ;; x - execute / compile 6510 opcode
        [(regexp-match? x-regex command)
         (match-let (((list _ value) (regexp-match x-regex command)))
           (debugger--execute-command value (string-prefix? command "xf") d-state))]
        ;; stop - stop at program counter (breakpoint)
        [(regexp-match? stop-pc-regex command)
         (match-let (((list _ cl value) (regexp-match stop-pc-regex command)))
           (cond [cl
                  (begin
                    (displayln (format "clear breakpoint at pc = $~a" value))
                    (debugger--remove-breakpoints d-state (format "stop at pc = $~a" value)))]
                 [else
                  (begin
                    (displayln (format "set breakpoint at pc = $~a" value))
                    (debugger--push-breakpoint d-state
                                               (lambda (lc-state)
                                                 (eq? (cpu-state-program-counter lc-state)
                                                      (string->number value 16)))
                                               (format "stop at pc = $~a" value)))]))]
        ;; stop a=ff - stop at accumulator = ff
        [(regexp-match? stop-a-regex command)
         (match-let (((list _ cl value) (regexp-match stop-a-regex command)))
           (cond [cl
                  (begin
                    (displayln (format "clear breakpoint at accumluator = $~a" value))
                    (debugger--remove-breakpoints d-state (format "stop when register A = $~a" value)))]
                 [else
                  (begin
                    (displayln (format "set breakpoint at accumluator = $~a" value))
                    (debugger--push-breakpoint d-state
                                               (lambda (lc-state)
                                                 (eq? (cpu-state-accumulator lc-state)
                                                      (string->number value 16)))
                                               (format "stop when register A = $~a" value)))]))]
        ;; stop sp=ff - stop at stack pointer = fff
        [(regexp-match? stop-sp-regex command)
         (match-let (((list _ cl value) (regexp-match stop-sp-regex command)))
           (cond [cl
                  (begin
                    (displayln (format "clear breakpoint at sp = $(1)~a" value))
                    (debugger--remove-breakpoints d-state (format "stop when sp = $~a" value)))]
                 [else
                  (begin
                    (displayln (format "set breakpoint at sp = $(1)~a" value))
                    (debugger--push-breakpoint d-state
                                               (lambda (lc-state)
                                                 (eq? (cpu-state-stack-pointer lc-state)
                                                      (string->number value 16)))
                                               (format "stop when sp = $~a" value)))]))]
        ;; list stop
        [(regexp-match? list-bp-regex command)
         (begin
           (for ([description (map breakpoint-description (debug-state-breakpoints d-state))])
             (displayln description))
           d-state)]
        ;; stop cf = 0 - stop when carry is cleared
        ;; stop zf = 1 - stop when zero is set
        ;; stop x = 20 - stop if x (turns) 20
        ;; stop y = 21 - stop if y (turns) 21
        ;; clear - clear breakpoints
        [(string=? command "clear stops")
         (struct-copy debug-state d-state [breakpoints '()])]
        ;; commit - commit change states
        [(regexp-match? commit-regex command)
         (match-let (((list _ value) (regexp-match commit-regex command)))
           (define states (debug-state-states d-state))
           (struct-copy debug-state d-state
                        [states (take states (min (length states) (string->number (or value "0a") 16)))]))]
        ;; r - run
        [(regexp-match? run-regex command) (debugger--run d-state)]
        [else (begin (unless (zero? (string-length command))
                     (displayln "(unknown command, enter '?' to get help)") )
                   d-state)]))

(define/c (create-disassemble-annotation-string c-state)
  (-> cpu-state? string?)
  (define address (cpu-state-program-counter c-state))
  (let-values (((str len) (disassemble-single c-state address)))
    (define code-byte-str (code-bytes c-state address len))
    (format "~a: ~a" (~a code-byte-str #:width 8 #:right-pad-string " ") str)))

(struct emacs-capabilities
  (print-proc-status
   sync-step-with-source
   output)
  #:transparent)

(define/c (collect-emacs-capabilities file-name)
  (-> string? emacs-capabilities?)
  (emacs-capabilities
   (6510-debugger--has-proc-display-cap)
   (and (non-empty-string? file-name) (file-exists? file-name) (6510-debugger--has-single-step-cap file-name))
   (6510-debugger--has-output-cap)))

(define/c (run-debugger--prepare-emacs-integration capabilities file-name d-state)
  (-> emacs-capabilities? string? debug-state? any/c)
  (when (emacs-capabilities-sync-step-with-source capabilities)
    (6510-debugger--remove-all-addresses-on-source file-name)
    (6510-debugger--show-address-on-source-lines file-name (debug-state-pc-source-map d-state))))

(define/c (run-debugger--step-update-emacs-integration capabilities s-entry d-state)
  (-> emacs-capabilities? (or/c pc-source-map-entry? #f) debug-state? any/c)
  (cond [(and (emacs-capabilities-sync-step-with-source capabilities)
            s-entry)
         (6510-debugger--show-disassembly-on-source-lines (pc-source-map-entry-file s-entry)
                                                          (pc-source-map-entry-line s-entry)
                                                          (create-disassemble-annotation-string (car (debug-state-states d-state))))]
        [else
         (display (apply (debug-state-pre-prompter d-state) (list d-state)))
         (displayln "")])
  (when (emacs-capabilities-print-proc-status capabilities)
    (6510-debugger--proc-buffer-display d-state)))

(define/c (run-debugger--step-cleanup-emacs-integration capabilities s-entry)
  (-> emacs-capabilities? (or/c pc-source-map-entry? #f) any/c)
  '()
  ;; (when (and s-entry (emacs-capabilities-sync-step-with-source capabilities))
  ;;   (when s-entry (6510-debugger--remove-disassembly-on-source-lines (pc-source-map-entry-file s-entry))))
  )

(define/c (run-debugger--cleanup-emacs-integration capabilities file-name)
  (-> emacs-capabilities? string?  any/c)
  (when (emacs-capabilities-print-proc-status capabilities)
    (6510-debugger--proc-buffer-kill))
  (when (emacs-capabilities-sync-step-with-source capabilities)
    (6510-debugger--remove-all-addresses-on-source file-name)))


(define (debugger--assembler-prompter d-state)
  (format "6510 [~x] > " (length (debug-state-states d-state))))

(define (debugger--assembler-pre-prompter d-state)
  (define c-state (car (debug-state-states d-state)))
  (disassemble c-state (cpu-state-program-counter c-state) 1))

;; definition of an repl interactor for regular assembly level debug repl sessions
(define debugger--assembler-interactor
  (list
   `(prompter . ,debugger--assembler-prompter)
   `(dispatcher . ,dispatch-debugger-command)
   `(pre-prompter . ,debugger--assembler-pre-prompter)))

;; run an read eval print loop debugger on the passed program
(define/c (run-debugger-on state (file-name "") (verbose #t) (breakpoints '()) (interactor debugger--assembler-interactor) (run #f))
  (->* [cpu-state?] [string? boolean? (listof breakpoint?) (listof any/c) boolean?] any/c)
  (define capabilities (collect-emacs-capabilities file-name))
  (define file-does-exist (and (non-empty-string? file-name) (file-exists? file-name)))
  (define d-state
    (debug-state (list state)
                 breakpoints
                 (if file-does-exist
                   (load-source-map file-name)
                   (hash))
                 (if (emacs-capabilities-output capabilities)
                     6510-debugger--print-string
                     debugger-output-function)
                 (dict-ref interactor 'prompter debugger--assembler-prompter)
                 (dict-ref interactor 'dispatcher dispatch-debugger-command)
                 (dict-ref interactor 'pre-prompter debugger--assembler-pre-prompter)))

  (when file-does-exist
    (run-debugger--prepare-emacs-integration capabilities file-name d-state))
  (if run
      (run-debugger--repl (debugger--run d-state verbose) capabilities)
      (run-debugger--repl d-state capabilities))
  (when file-does-exist
    (run-debugger--cleanup-emacs-integration capabilities file-name)))

(define/c (run-debugger org program (file-name "") (verbose #t))
  (->* [word/c (listof (or/c byte/c ast-command?))] [string? boolean?] any/c)
  (define raw-bytes (if (ast-command? (car program))
                        (assemble org program)
                        program))
  (when verbose
    (displayln (format "loading program into debugger at ~a" org))
    (displayln "enter '?' to get help, enter 'q' to quit"))
  (run-debugger-on (6510-load (initialize-cpu) org raw-bytes) file-name verbose))

;; execute the debugger repl
(define/c (run-debugger--repl initial-d-state capabilities)
  (-> debug-state? emacs-capabilities? any/c)
  (define d-state (struct-copy debug-state initial-d-state))
  (define previous-input '())
  (for ([_ (in-naturals)])
    (define s-entry (hash-ref (debug-state-pc-source-map d-state)
                              (cpu-state-program-counter (car (debug-state-states d-state)))
                              #f))
    (run-debugger--step-update-emacs-integration capabilities s-entry d-state)    
    (display (apply (debug-state-prompter d-state) (list d-state)))
    (define input (begin (readline "")))
    (run-debugger--step-cleanup-emacs-integration capabilities s-entry)
    #:break (string=? input "q")
    (cond [(and previous-input (regexp-match #rx"^\\.+$" input))
           (set! d-state (run-debugger--dispatch-debugger-command-n-times previous-input d-state (string-length input)))]
          [else
           (begin
             (set! previous-input input)
             (set! d-state (apply (debug-state-dispatcher d-state) (list input d-state))))])))

;; dispatch the given debugger command n-times starting with the current debug-state
(define/c (run-debugger--dispatch-debugger-command-n-times command d-state (n 1))
  [->* [string? debug-state?] [nonnegative-integer?] debug-state?]
  (let ([local-d-state (struct-copy debug-state d-state)])
    (for ([_ (in-range n)])
      (set! local-d-state (apply (debug-state-dispatcher d-state) (list  command local-d-state))))
    local-d-state))

;; (run-debugger #xc000 (list #xa9 #x41 #x48 #x20 #xd2 #xff #x00))

;; return nil or the breakpoint that hit
;; breakpoint is a function that takes a single argument the state
(define/c (breakpoint-hits c-state breakpoints)
  (-> cpu-state? list? (or/c boolean? breakpoint?))
  (cond [(empty? breakpoints)
         #f]
        [else
         (begin
           (if (apply
                (breakpoint-fn (car breakpoints))
                (list c-state))
               (car breakpoints)
               (breakpoint-hits c-state (cdr breakpoints))))]))

;; is the debugger at a rts command and the stack is at the bottom (#xff)?
(define/c (-debugger-at-root-rts c-state)
  (-> cpu-state? boolean?)
  (and (eq? 96 (peek-pc c-state)) (eq? #xff (cpu-state-stack-pointer c-state))))

;; is the debugger at a brk command?
(define/c (-debugger-at-brk c-state)
  (-> cpu-state? boolean?)
  (zero? (peek-pc c-state)))

(define (debugger-output-function str)
  (display str))

;; run until a break point hits or the cpu is on a BRK statement
(define/c (run-until-breakpoint states breakpoints (steps 0) (string-output-function debugger-output-function))
  (->* [(listof cpu-state?) list?] [nonnegative-integer? (-> string? any/c)] [values (or/c boolean? breakpoint?) (listof cpu-state?)])
  (let ([breakpoint (breakpoint-hits (car states) breakpoints)])
    (cond [(or breakpoint
           (-debugger-at-brk (car states))
           (-debugger-at-root-rts (car states)))
           (values breakpoint states)]
          [(> steps debugger-max-uninterrupted-steps)
           (values breakpoint states)]
          [else
           (run-until-breakpoint
            (cons (execute-cpu-step (car states) #t string-output-function) states)
            breakpoints
            (fx+ 1 steps)
            string-output-function)])))

(define/c (compile-opcodes str)
  (-> string? (listof byte/c))
  (commands->bytes 0 (asm->ast str)))

(module+ test #| assemble/dissassemble roundrip |#

  ;; test that every opcode from $00 .. $ff will disassemble to a string
  ;; that, if compiled, will produce the same bytes
  ;; this ensures the symmetry of assembler and disassembler
  (for ([opcode (range #x00 #x100)])
    (define state-with-opcode
      (~> (initialize-cpu)
         (6510-load _ #xc000 `(,opcode #x10 #x20))
         (with-program-counter _ #xc000)))
    (define-values (mnemonic bytes) (disassemble-single state-with-opcode))
    (with-handlers
      ((exn:fail?
        (lambda (e) (displayln
                (format "error: ~a,\nfailure on: opcode: ~a, mnemonic: '~a'"
                        e opcode mnemonic)))))
      (let ([roundtrip-bytes (compile-opcodes mnemonic)])
        (check-equal?
         (length roundtrip-bytes)
         bytes
         "the disassembler reports different number of bytes than the assembler produces")
        (check-equal?
         roundtrip-bytes
         (take `(,opcode #x10 #x20)
               (length roundtrip-bytes))
         (format "assembler/disassemble roundtrip failed\nopcode: ~a, mnemonic: ~a, bytes: ~a"
                 opcode mnemonic roundtrip-bytes))))))

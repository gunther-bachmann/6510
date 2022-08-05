#lang racket
(require (rename-in  racket/contract [define/contract define/c]))
(require readline/readline)
(require "6510-interpreter.rkt")
(require "6510-parser.rkt")
(require "6510-utils.rkt")
(require "6510-disassembler.rkt")

(provide run-debugger)

;; TODO allow execution of arbirtray racket with values explicitly set into the namespace to evaluate breakpoint conditions (or do calculations)
;; (namespace-set-variable-value! 'some 25)
;; (namespace-variable-value 25)
;; (eval '(+ some 5)) ;; => 30

;; enable/disable breakpoints
;; give breakpoints names
;; list breakpoints
;; delete single breakpoints (by name)

;; set or clear flags
(define/c (modify-flag set flag c-state)
  (-> boolean? string? cpu-state? cpu-state?)
  (cond ((string=? flag "c")
         (if set (set-carry-flag c-state) (clear-carry-flag c-state)))
        ((string=? flag "n")
         (if set (set-negative-flag c-state) (clear-negative-flag c-state)))
        ((string=? flag "i")
         (if set (set-interrupt-flag c-state) (clear-interrupt-flag c-state)))
        ((string=? flag "b")
         (if set (set-brk-flag c-state) (clear-brk-flag c-state)))
        ((string=? flag "v")
         (if set (set-overflow-flag c-state) (clear-overflow-flag c-state)))
        ((string=? flag "z")
         (if set (set-zero-flag c-state) (clear-zero-flag c-state)))
        (#t c-state)))

(struct debug-state (states breakpoints)
  #:guard ((listof cpu-state?) list?))

(define/c (debugger--help d-state)
  (-> debug-state? debug-state?)
  (define help #<<EOF
pm <A> <B>           print memory starting @A (hex) for B bytes (hex)
sm <A> = <B>          set memory @A (hex) to byte B (hex)
sa = <B>              set accumulator to byte B (hex)
spc = <A>             set program counter to address A (hex)
s                     single step forward
b                     backward step
p                     print cpu state
pp <B?> <A?>          pretty print the next B (hex) commands starting at address A (hex)
xf? { <command> }     eXecute Force? the given 6510 command (force if byte len differs)
stop pc = <A>         set break point to stop at pc = address A (hex)
cf[cbnvzi]            clear flag (Carry,Break,Negative,Overflow,Zero,Interrupt)
sf[cbnvzi]            set flag (Carry,Break,Negative,Overflow,Zero,Interrupt)
clear                 clear all breakpoints
commit <B>?           keep only the last 10 | B (hex) states
r                     run until a break point is hit
c                     continue over the currently halted on breakpoint
q                     quit
EOF
    )
  (displayln help)
  d-state) 

(define/c (debugger--pretty-print address len d-state)
  (-> (or/c string? false?) (or/c string? false?) debug-state? debug-state?)
  (define c-state (car (debug-state-states d-state)))
  (displayln (disassemble c-state
                          (if address (string->number address 16)  (cpu-state-program-counter c-state))
                          (if len (string->number len 16) 1)))
  d-state)

(define/c (debugger--print-memory address len d-state)
  (-> (or/c string? false?) (or/c string? false?) debug-state? debug-state?)
  (define c-state (car (debug-state-states d-state)))
  (displayln (memory->string (string->number address 16)
                            (+ -1 (string->number address 16) (string->number len 16))
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
               [states (cons (modify-flag (string=? set-or-clear "s") flag c-state)
                             (debug-state-states d-state))]))

(define/c (debugger--execute-command command force d-state)
  (-> string? boolean? debug-state? debug-state?)
  (with-handlers ([exn:fail? ;; catch parse error of the 6510 opcode
                   (lambda (e)
                     (displayln e)
                     d-state)])
    (define c-state (car (debug-state-states d-state)))
    (let ((byteList (compile-opcode (string-trim command))))
      (if (or force
             (eq? (let-values (((_ commandBytes) (disassemble-single c-state))) commandBytes)
                  (length byteList)))
          (struct-copy debug-state d-state
                       [states (cons (-pokem c-state (cpu-state-program-counter c-state) byteList)
                                     (debug-state-states d-state))])
          (begin (displayln "byte length differs (use xf to force)")
                 d-state)))))

(define/c (debugger--set-breakpoint d-state fun)
  (-> debug-state? any/c debug-state?)
  (struct-copy debug-state d-state
               [breakpoints (cons fun
                                  (debug-state-breakpoints d-state))]))

(define/c (debugger--run d-state)
  (-> debug-state? debug-state?)
  (let-values (((breakpoint new-states) (run-until-breakpoint (debug-state-states d-state) (debug-state-breakpoints d-state))))
    (struct-copy debug-state d-state [states new-states])))
(define/c (debugger--continue d-state)
  (-> debug-state? debug-state?)
  (define c-states (debug-state-states d-state))
  (define next-states (cons (execute-cpu-step (car c-states)) c-states))
  (let-values (((breakpoint new-states) (run-until-breakpoint next-states (debug-state-breakpoints d-state))))
    (struct-copy debug-state d-state [states new-states])))

(define/c (dispatch-debugger-command command d-state)
  (-> string? debug-state? debug-state?)
  (define c-states (debug-state-states d-state))
  (define pp-regex #px"^pp *([[:xdigit:]]{1,2})? *([[:xdigit:]]{1,4})?")
  (define pm-regex #px"^pm *([[:xdigit:]]{1,4}) *([[:xdigit:]]{1,2})")
  (define sm-regex #px"^sm *([[:xdigit:]]{1,4}) *= *([[:xdigit:]]{1,2})")
  (define b-regex #px"^b *([[:xdigit:]]{1,2})?")
  (define sa-regex #px"^sa *= *([[:xdigit:]]{1,2})$")
  (define spc-regex #px"^spc *= *([[:xdigit:]]{1,4})$")
  (define x-regex #px"^xf? *\\{(.*)\\}$")
  (define stop-pc-regex #px"^stop *pc *= *([[:xdigit:]]{1,4})$")
  (define stop-a-regex #px"^stop *a *= *([[:xdigit:]]{1,2})$")
  (define stop-sp-regex #px"^stop *sp *= *([[:xdigit:]]{1,2})$")
  (define commit-regex #px"^commit *([[:xdigit:]]{1,2})?")
  (define flags-regex #px"^([sc])f([cbnvzi])$")
  (cond ((or (string=? command "?") (string=? command "h")) (debugger--help d-state))
        ;; ((string=? command "b") (struct-copy debug-state d-state [states (cdr c-states)]))
        ((regexp-match? b-regex command)
         (match-let (((list _ value) (regexp-match b-regex command)))
           (define states (debug-state-states d-state))
           (struct-copy debug-state d-state [states (list-tail states (min (- (length states) 1) (if value (string->number value 16) 1)))])))
        ((string=? command "s") (struct-copy debug-state d-state [states (cons (execute-cpu-step (car c-states)) c-states)]))
        ((string=? command "p") (displayln "") (print-state (car c-states)) d-state)
        ;; pp - disassemble (pretty print)
        ((regexp-match? pp-regex command)
         (match-let (((list _ len address) (regexp-match pp-regex command)))
           (debugger--pretty-print address len d-state)))
        ;; pm - print memory
        ((regexp-match? pm-regex command)
         (match-let (((list _ address len) (regexp-match pm-regex command)))
           (debugger--print-memory address len d-state)))
        ;; sm - set memory
        ((regexp-match? sm-regex command)
         (match-let (((list _ address value) (regexp-match sm-regex command)))
           (debugger--set-memory address value d-state)))
        ;; sa - set accumulator
        ((regexp-match? sa-regex command)
         (match-let (((list _ value) (regexp-match sa-regex command)))
           (debugger--set-accumulator value d-state)))
        ;; spc - set program counter
        ((regexp-match? spc-regex command)
         (match-let (((list _ value) (regexp-match spc-regex command)))
           (debugger--set-program-counter value d-state)))
        ;; [cs]f[nvibcz] - clear / set flag
        ((regexp-match? flags-regex command)
         (match-let (((list _ set-or-clear flag) (regexp-match flags-regex command)))
           (debugger--set-or-clear-flag set-or-clear flag d-state)))
        ;; x - execute / compile 6510 opcode
        ((regexp-match? x-regex command)
         (match-let (((list _ value) (regexp-match x-regex command)))
           (debugger--execute-command value (string-prefix? command "xf") d-state)))
        ;; stop - stop at program counter (breakpoint)
        ((regexp-match? stop-pc-regex command)
         (match-let (((list _ value) (regexp-match stop-pc-regex command)))
           (displayln (format "set breakpoint at pc = $~a" value))
           (debugger--set-breakpoint d-state
                                     (lambda (state)
                                       (eq? (cpu-state-program-counter state)
                                            (string->number value 16))))))
        ;; stop a=ff - stop at accumulator = ff
        ((regexp-match? stop-a-regex command)
         (match-let (((list _ value) (regexp-match stop-a-regex command)))
           (displayln (format "set breakpoint at accumluator = $~a" value))
           (debugger--set-breakpoint d-state
                                     (lambda (state)
                                       (eq? (cpu-state-accumulator state)
                                            (string->number value 16))))))
        ;; stop sp=ff - stop at stack pointer = fff
        ((regexp-match? stop-sp-regex command)
         (match-let (((list _ value) (regexp-match stop-sp-regex command)))
           (displayln (format "set breakpoint at sp = $(1)~a" value))
           (debugger--set-breakpoint d-state
                                     (lambda (state)
                                       (eq? (cpu-state-stack-pointer state)
                                            (string->number value 16))))))
        ;; stop cf = 0 - stop when carry is cleared
        ;; stop zf = 1 - stop when zero is set
        ;; stop x = 20 - stop if x (turns) 20
        ;; stop y = 21 - stop if y (turns) 21
        ;; clear - clear breakpoints
        ((string=? command "clear")
         (struct-copy debug-state d-state [breakpoints '()]))
        ;; commit - commit change states
        ((regexp-match? commit-regex command)
         (match-let (((list _ value) (regexp-match commit-regex command)))
           (define states (debug-state-states d-state))
           (struct-copy debug-state d-state [states (take states (min (length states) (string->number value 16)))])))
        ;; r - run
        ((string=? command "r") (debugger--run d-state))
        ;; c - continue over current breakpoint to the next one
        ((string=? command "c") (debugger--continue d-state))
        ;; so :: step over (jsr)
        (#t (begin (displayln "? unknown command?") d-state))))

;; run an read eval print loop debugger on the passed program
(define/c (run-debugger org raw-bytes)
  (-> word/c (listof byte/c) any/c)
  (displayln (format "loading program into debugger at ~a" org))
  (define d-state (debug-state (list (6510-load (initialize-cpu) org raw-bytes)) '()))
  (readline ">")
  (for ([_ (in-naturals)])
    (displayln "")
    (display (format "Step-Debugger[~a]> " (length (debug-state-states d-state))))
    (define input (begin (readline ">")))
    #:break (string=? input "q")
    (set! d-state
          (dispatch-debugger-command input d-state))))

;; (run-debugger #xc000 (list #xa9 #x41 #x48 #x20 #xd2 #xff #x00))

;; return nil or the breakpoint that hit
;; breakpoint is a function that takes a single argument the state
(define/c (breakpoint-hits state breakpoints)
  (-> cpu-state? list? any/c)
  (if (empty? breakpoints)
      #f
      (begin
        (if (apply
             (car breakpoints)
             (list state))
            (car breakpoints)
            (breakpoint-hits state (cdr breakpoints))))))

;; run until a break point hits or the cpu is on a BRK statement
(define (run-until-breakpoint states breakpoints)
  (-> cpu-state? list? (values any/c (listof cpu-state?)))
  (let ((breakpoint (breakpoint-hits (car states) breakpoints)))
    (if (or breakpoint (eq? 0 (peek-pc (car states))))
        (begin
          (displayln (format "hit breakpoint ~a" breakpoint))
          (values breakpoint states))
        (begin
          (let ((next-states (cons (execute-cpu-step (car states)) states)))            
            (run-until-breakpoint next-states breakpoints))))))

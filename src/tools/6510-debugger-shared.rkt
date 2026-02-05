#lang racket
#|

 common structures shared by some debugger tool files

 |#

(require (only-in "../tools/data-tools.rkt" word/c))
(require (only-in "./6510-interpreter.rkt" cpu-state?))
(require (only-in "./6510-debugger-sync-source.rkt" pc-source-map-entry?))

(provide (struct-out breakpoint)
         (struct-out tracepoint)
         (struct-out debug-state))

(struct breakpoint (description fn verbose keep-on-dive)
  #:transparent
  #:guard (struct-guard/c string? any/c boolean? boolean?))

(struct tracepoint (description fn output-fn verbose keep-on-dive)
  #:transparent
  #:guard (struct-guard/c string? any/c any/c boolean? boolean?))

(struct debug-state
  (states
   breakpoints
   tracepoints
   pc-source-map
   output-function
   prompter
   dispatcher
   pre-prompter
   ident
   interactor-queue
   labels
   program-counter)
  #:guard (struct-guard/c (listof cpu-state?)
                          (listof breakpoint?)
                          (listof tracepoint?)
                          (hash/c nonnegative-integer? pc-source-map-entry?)
                          (-> string? any/c)
                          (-> any/c string?)        ;; any/c is actually debug-state? 
                          (-> string? any/c any/c)  ;; any/c is actually debug-state?
                          (-> any/c string?)        ;; any/c is actually debug-state?
                          symbol?                   ;; identity of this interactor
                          (listof any/c)            ;; interactors
                          (hash/c string? word/c)    ;; resolved labels
                          (-> any/c nonnegative-integer?)
                          ))

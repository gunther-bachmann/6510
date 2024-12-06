#lang racket
#|

 common structures shared by some debugger tool files

 |#

(require (only-in "./6510-interpreter.rkt" cpu-state?))
(require (only-in "./6510-debugger-sync-source.rkt" pc-source-map-entry?))

(provide (struct-out breakpoint)
         (struct-out debug-state))

(struct breakpoint (description fn verbose)
  #:transparent
  #:guard (struct-guard/c string? any/c boolean?))

(struct debug-state (states breakpoints pc-source-map output-function prompter dispatcher pre-prompter)
  #:guard (struct-guard/c (listof cpu-state?)
                          (listof breakpoint?)
                          (hash/c nonnegative-integer? pc-source-map-entry?)
                          (-> string? any/c)
                          (-> any/c string?)        ;; any/c is actually debug-state? 
                          (-> string? any/c any/c)  ;; any/c is actually debug-state?
                          (-> any/c string?)        ;; any/c is actually debug-state? 
                          ))

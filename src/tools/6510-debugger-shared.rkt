#lang racket
#|

 common structures shared by some debugger tool files

 |#

(require (only-in "./6510-interpreter.rkt" cpu-state?))
(require (only-in "./6510-debugger-sync-source.rkt" pc-source-map-entry?))

(provide (struct-out breakpoint)
         (struct-out debug-state))

(struct breakpoint (description fn)
  #:transparent
  #:guard (struct-guard/c string? any/c))

(struct debug-state (states breakpoints pc-source-map)
  #:guard (struct-guard/c (listof cpu-state?)
                          (listof breakpoint?)
                          (hash/c nonnegative-integer? pc-source-map-entry?)))

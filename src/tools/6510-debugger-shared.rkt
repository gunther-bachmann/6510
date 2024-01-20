#lang racket
#|


 |#

(require "../6510-utils.rkt")
(require "6510-interpreter.rkt")
(require (only-in "./6510-debugger-sync-source.rkt" remove-overlay-source load-source-map overlay-source pc-source-map-entry? pc-source-map-entry-file pc-source-map-entry-line))

(provide (struct-out breakpoint)
         (struct-out debug-state))

(struct breakpoint (description fn)
  #:transparent
  #:guard (struct-guard/c string? any/c))

(struct debug-state (states breakpoints pc-source-map)
  #:guard (struct-guard/c (listof cpu-state?)
                          (listof breakpoint?)
                          (hash/c nonnegative-integer? pc-source-map-entry?)))

#lang racket

(provide (struct-out compile-ctx))

(struct compile-ctx
  (strings)
  #:transparent
  #:guard (struct-guard/c (listof string?)))

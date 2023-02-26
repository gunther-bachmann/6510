#lang racket/base

(provide project-root)

;; needed by reader to calculate the actual module path to require into the generated code
(define project-root (expand-user-path (string->path "~/repo/+1/6510/")))

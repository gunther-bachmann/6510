#lang racket/base

(provide project-root)

(define project-root (expand-user-path (string->path "~/repo/+1/6510/")))

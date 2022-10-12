#lang racket

(require "6510-alt-utils.rkt")
(require "6510-alt-addressing.rkt")

(provide BCC BCS BEQ BMI BNE BPL BVC BVS) 

(module+ test
  (require "6510-test-utils.rkt"))

;;--------------------------------------------------------------------------------
;; https://docs.racket-lang.org/reference/syntax-util.html
;; (format-id ...)

;; https://blog.racket-lang.org/2011/04/writing-syntax-case-macros.html
;;--------------------------------------------------------------------------------

(define-opcode BCC ((relative . #x90)))
(define-opcode BCS ((relative . #xb0)))
(define-opcode BEQ ((relative . #xf0)))

(module+ test #| BEQ |#
  (check-equal? (BEQ $10)
                '(rel-opcode #xf0 #x10))
  (check-equal? (BEQ some)
                '(rel-opcode #xf0 (resolve-relative "some"))))


(define-opcode BMI ((relative . #x30)))

(define-opcode BNE ((relative . #xd0)))

(define-opcode BPL ((relative . #x10)))

(define-opcode BVC ((relative . #x50)))

(define-opcode BVS ((relative . #x70)))


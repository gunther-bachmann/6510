#lang racket

(require "6510-addressing.rkt")

(provide TAX TAY TSX TXA TXS TYA)

(define-opcode TAX ((implicit . #xaa)))

(define-opcode TAY ((implicit . #xa8)))

(define-opcode TSX ((implicit . #xba)))

(define-opcode TXA ((implicit . #x8a)))

(define-opcode TXS ((implicit . #x9a)))

(define-opcode TYA ((implicit . #x98)))


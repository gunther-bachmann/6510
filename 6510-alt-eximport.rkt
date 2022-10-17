#lang racket

(require "6510-test-utils.rkt")

(provide import-table-bytes export-table-bytes)

(define (import-table-bytes offset collected-entries label-offsets commands)
  '())

(define (export-table-bytes offset collected-entries label-offsets commands)
  '())

;; import table format
;; offset           data
;; 0                rel-position-low, rel-position-high, : position where the value has to be written to
;; 2                width (byte), (if width = 1 0:lowbyte 1:highbyte)?,
;; 3/4              strlen (byte)
;; 4/5              string 
;; 4/5+strlen       ...next entry

;; export table format
;; offset           data
;; 0                width (byte), (if width = 2 0: absolute-const, 1:relative-load-address)?.
;; 1/2              value-low|rel-position-low (optional value-high|rel-position-high)
;; 1/2+width        strlen (byte)
;; 2/3+width        string
;; 1/2+width+strlen ... next entry

'((provide-word aout)
  (provide-byte char)
  (byte-const char $65)
  (label aout)
  (LDA !char)
  (JMP $FFD2))

'((require-word aout)
  (require-byte char)
  (LDX !$05)
  (label repeat)
  (JSR aout)
  (DEX)
  (BNE repeat)
  (CLC)
  (LDA !char)
  (ADC !#01)
  (JSR $FFD2)
  (BRK))


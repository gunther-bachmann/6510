#lang racket

(provide parse-number-string low-byte high-byte absolute)

(define (number-has-prefix? number-string)
  (string-contains? "%$" (substring number-string 0 1)))

(define (prefix->number-base prefix)
  (case prefix
    [("$") 16]
    [("%") 2]
    [else 10]))

(define (parse-number-string number-string)
  (exact-floor (string->number (substring  number-string (if (number-has-prefix? number-string) 1 0))
                               (prefix->number-base (substring number-string 0 1)))))

(define (low-byte absolute)
  (bitwise-and #xFF absolute))

(define (high-byte absolute)
  (bitwise-and #xFF (arithmetic-shift absolute -8)))

(define (absolute high low)
  (bitwise-ior (arithmetic-shift high 8) low))

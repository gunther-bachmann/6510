#lang racket

(module+ test
  (require rackunit))

(provide parse-number-string low-byte high-byte absolute word byte 6510-label-string? is-immediate-number? is-number?)

(define (number-has-prefix? number-string)
  (string-contains? "%$" (substring number-string 0 1)))

(define (6510-label-string? value)
  (and (string? value)
       (regexp-match? #rx"^(:[A-Z][A-Z0-9]*)$"
                      (string-upcase value))))

(module+ test
  (check-true (6510-label-string? ":some"))
  (check-true (6510-label-string? ":some12"))
  (check-false (6510-label-string? ":12some"))
  (check-false (6510-label-string? ":123"))
  (check-true (6510-label-string? ":s1ome"))
  (check-false (6510-label-string? "some"))
  (check-false (6510-label-string? ":"))
  (check-true (6510-label-string? ":s")))

(define (6510-number-string? value)
  (regexp-match? #rx"^(\\%(0|1)+|\\$([0-9]|A|B|C|D|E|F)+|[0-9]+)$"
                 value))

(module+ test
  (check-true (6510-number-string? "234"))
  (check-true (6510-number-string? "%1001100"))
  (check-true (6510-number-string? "$234ABF"))
  (check-false (6510-number-string? "-234"))
  (check-false (6510-number-string? "$234AG"))
  (check-false (6510-number-string? "%120")))

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

(define (word value)
  (bitwise-and #xffff value))

(define (byte value)
  (bitwise-and #xff value))

(define (is-number? string)
  (regexp-match? #rx"^([0-9]+|\\$[0-9a-fA-F]+|\\%[0-1]+)$" string))

(define (is-immediate-number? string)
  (and (equal? (substring string 0 1) "#")
       (is-number? (substring string 1))))

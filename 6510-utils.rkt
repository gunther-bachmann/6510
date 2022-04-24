#lang racket

(module+ test
  (require rackunit))

(provide two-complement-of parse-number-string low-byte high-byte absolute word byte 6510-label-string? is-immediate-number? 6510-number-string?)

;; is the given number-string prefixed with a valid number base prefix?
(define (number-has-prefix? number-string)
  (string-contains? "%$" (substring number-string 0 1)))

;; is the given string a valid label?
(define (6510-label-string? value)
  (and (string? value)
     (regexp-match? #rx"^(:[A-Z][A-Z0-9]*)$"
                    (string-upcase value))))

(module+ test #| 6510-label-string? |#
  (check-true (6510-label-string? ":some"))
  (check-true (6510-label-string? ":some12"))
  (check-false (6510-label-string? ":12some"))
  (check-false (6510-label-string? ":123"))
  (check-true (6510-label-string? ":s1ome"))
  (check-false (6510-label-string? "some"))
  (check-false (6510-label-string? ":"))
  (check-true (6510-label-string? ":s")))

;; is the given string a valid binary, hex or regular number
(define (6510-number-string? value)
  (and (string? value)
     (regexp-match? #rx"^(\\%(0|1)+|\\$([0-9]|A|B|C|D|E|F)+|[0-9]+)$"
                    value)))

(module+ test #| 6510-number-string? |#
  (check-true (6510-number-string? "234"))
  (check-true (6510-number-string? "%1001100"))
  (check-true (6510-number-string? "$234ABF"))
  (check-false (6510-number-string? "-234"))
  (check-false (6510-number-string? "$234AG"))
  (check-false (6510-number-string? "%120")))

;; get the number base of a valid number string
(define (prefix->number-base prefix)
  (case prefix
    [("$") 16]
    [("%") 2]
    [else 10]))

;; parse a number (binary, hex ...) string and return the number
(define (parse-number-string number-string)
  (exact-floor (string->number (substring  number-string (if (number-has-prefix? number-string) 1 0))
                               (prefix->number-base (substring number-string 0 1)))))

(module+ test #| parse-number-string |#
  (check-eq? (parse-number-string "%101")
             5)
  (check-eq? (parse-number-string "$17")
             23)
  (check-eq? (parse-number-string "102")
             102))

;; get the low byte of a (2 byte) number
(define (low-byte absolute)
  (bitwise-and #xFF absolute))

;; get the high byte of a (2 byte) number
(define (high-byte absolute)
  (bitwise-and #xFF (arithmetic-shift absolute -8)))

;; construct a 2 byte number for high byte and low byte
(define (absolute high low)
  (bitwise-ior (arithmetic-shift high 8) low))

;; restrict value to a 2 byte value (cutting off other bits)
(define (word value)
  (bitwise-and #xffff value))

;; restrict value to a 1 byte value (cutting off other bits)
(define (byte value)
  (bitwise-and #xff value))

;; return two complement of the given (possibly negative) number
(define (two-complement-of num)
  (when (or (> -127 num) (< 127 num)) (error "num out of range"))
  (if (< num 0)
      (+ 256 num)
      num))

;; if the given string an immediate string, (6510 number string prefixed by '#')?
(define (is-immediate-number? string)
  (and (string? string)
       (equal? (substring string 0 1) "#")
       (6510-number-string? (substring string 1))))

(module+ test #| is-immediate-number? |#
  (check-true (is-immediate-number? "#$2001"))
  (check-true (is-immediate-number? "#%10"))
  (check-true (is-immediate-number? "#17"))
  (check-false (is-immediate-number? "$2001"))
  (check-false (is-immediate-number? "# $2001"))
  (check-false (is-immediate-number? "#&2001")))

#lang racket
(require (rename-in  racket/contract [define/contract define/c]))

(module+ test
  (require rackunit))

(provide byte/c
         word/c
         two-complement-of
         decimal-from-two-complement
         parse-number-string
         low-byte
         high-byte
         absolute
         word
         byte
         6510-label-string?
         6510-label-byte-string?
         6510-label-immediate-byte-string?
         is-immediate-number?
         6510-number-string?)

(define/c (in-word-range? word)
  (-> exact-integer? boolean?)
  (and (<= word 65535) (>= word 0)))

(define/c (in-byte-range? byte)
  (-> exact-integer? boolean?)
  (and (<= byte 255) (>= byte 0)))

(define byte/c (and/c exact-nonnegative-integer? in-byte-range?))

(define word/c (and/c exact-nonnegative-integer? in-word-range?))


;; is the given number-string prefixed with a valid number base prefix?
(define (number-has-prefix? number-string)
  (string-contains? "%$" (substring number-string 0 1)))

;; is the given string a valid label (for word labels or relatives)?
(define (6510-label-string? value)
  (and (string? value)
     (regexp-match? #rx"^(:[A-Z][A-Z0-9]*)$"
                    (string-upcase value))))

;; is the given string a valid label (for byte labels)?
;; TODO: use other information to decide on type of the label use
(define (6510-label-byte-string? value)
  (and (string? value)
     (regexp-match? #rx"^(:[A-Z][A-Z0-9]*(-H|-L))$"
                    (string-upcase value))))

(define (6510-label-immediate-byte-string? value)
  (and (string? value)
     (regexp-match? #rx"^#(:[A-Z][A-Z0-9]*(-H|-L))$"
                    (string-upcase value))))

(module+ test #| 6510-label-string? |#
  (check-true (6510-label-string? ":some"))
  (check-true (6510-label-string? ":some12"))
  (check-false (6510-label-string? ":12some"))
  (check-false (6510-label-string? ":123"))
  (check-true (6510-label-string? ":s1ome"))
  (check-false (6510-label-string? "some"))
  (check-false (6510-label-string? ":"))
  (check-true (6510-label-string? ":s"))
  (check-false (6510-label-string? ":s-H"))
  (check-true (6510-label-byte-string? ":s-H"))
  (check-true (6510-label-byte-string? ":s-L"))
  (check-false (6510-label-byte-string? ":s-x")))

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
  (when (or (> -128 num) (< 127 num)) (error "num out of range"))
  (if (< num 0)
      (+ 256 num)
      num))

(define (decimal-from-two-complement num)
  (when (or (> 0 num) (< 256 num)) (error "num out of range"))
  (define abs-val (bitwise-and num #x7f))
  (if (> num 127)
      (- abs-val #x80)
      abs-val))

(module+ test #| two-complements |#
  (check-eq? (two-complement-of -1)
             #xff)
  (check-eq? (two-complement-of -128)
             #x80)
  (check-eq? (two-complement-of -2)
             #xfe)
  (check-eq? (two-complement-of 0)
             0)
  (check-eq? (two-complement-of 1)
             1)
  (check-eq? (two-complement-of 127)
             #x7f)

  (for ((b  (range -128 127)))
    (check-eq? (decimal-from-two-complement (two-complement-of b))
               b)))

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

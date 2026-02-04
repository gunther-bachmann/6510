#lang racket/base

(provide
 bit0?                          ;; is bit0 set in value?
 bit7?                          ;; is bit7 set in value?
 byte                           ;; restrict value to a 1 byte value (cutting off other bits)
 byte/c                         ;; is the given value within byte range (used be contracts)
 bytes->int                     ;; convert two bytes to int, low the high
 int->bytes                     ;; convert word to a list of low, high bytes
 decimal-from-two-complement    ;; give a signed decimal from a byte in two complement encoding
 high-byte                      ;; get the high byte of a (2 byte) number
 in-byte-range?                 ;; is this exact integer in byte range?
 in-word-range?                 ;; is this exact integer in word range?
 low-byte                       ;; get the low byte of a (2 byte) number
 not-bit0?                      ;; is bit0 unset in value?
 not-bit7?                      ;; is bit7 unset in value?
 two-complement-of              ;; return two complement of the given (possibly negative) number
 word                           ;; restrict value to a 2 byte value (cutting off other bits)
 word/c                         ;; is the given value within word range (used be contracts)
)

(require (only-in racket/contract define/contract -> and/c listof))

(module+ test
  (require (only-in racket/list range)
           "../6510-test-utils.rkt"))


;; is this exact integer in word range?
(define/contract (in-word-range? word)
  (-> exact-integer? boolean?)
  (and (<= word 65535) (>= word 0)))

;; is this exact integer in byte range?
(define/contract (in-byte-range? byte)
  (-> exact-integer? boolean?)
  (and (<= byte 255) (>= byte 0)))

(define byte/c (and/c exact-nonnegative-integer? in-byte-range?))

(define word/c (and/c exact-nonnegative-integer? in-word-range?))

;; is bit7 set in value?
(define/contract (bit7? value)
  (-> exact-nonnegative-integer? boolean?)
  (not (not-bit7? value)))

;; is bit0 set in value?
(define/contract (bit0? value)
  (-> exact-nonnegative-integer? boolean?)
  (not (not-bit0? value)))

;; is bit7 blank in value?
(define/contract (not-bit7? value)
  (-> exact-nonnegative-integer? boolean?)
  (zero? (bitwise-and #x80 value)))

;; is bit0 blank in value?
(define/contract (not-bit0? value)
  (-> exact-nonnegative-integer? boolean?)
  (zero? (bitwise-and #x1 value)))


;; get the low byte of a (2 byte) number
(define/contract (low-byte absolute)
  (-> exact-integer? byte/c)
  (bitwise-and #xFF absolute))

(module+ test #| low-byte, high-byte, bytes->int |#
  (check-equal? (low-byte #xA5FE)
                #xFE)
  (check-equal? (high-byte #xA5FE)
                #xA5)
  (check-equal? (bytes->int #xFE #xA5)
                #xA5FE))

;; get the high byte of a (2 byte) number
(define/contract (high-byte absolute)
  (-> exact-integer? byte/c)
  (bitwise-and #xFF (arithmetic-shift absolute -8)))

;; restrict value to a 2 byte value (cutting off other bits)
(define/contract (word value)
  (-> exact-integer? word/c)
  (bitwise-and #xffff value))

;; restrict value to a 1 byte value (cutting off other bits)
(define/contract (byte value)
  (-> exact-integer? byte/c)
  (bitwise-and #xff value))

;; return two complement of the given (possibly negative) number
(define/contract (two-complement-of num)
  (-> exact-integer? byte/c)
  (when (or (> -128 num) (< 127 num)) (error "num out of range"))
  (if (< num 0)
      (+ 256 num)
      num))

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
             #x7f))

(define/contract (decimal-from-two-complement num)
  (-> byte/c exact-integer?)
  (when (or (> 0 num) (< 256 num)) (error "num out of range"))
  (define abs-val (bitwise-and num #x7f))
  (if (> num 127)
      (- abs-val #x80)
      abs-val))

(module+ test #| decimal-from-two-complements |#
  (for ([b (range -128 127)])
    (check-eq? (decimal-from-two-complement (two-complement-of b))
               b)))

(define/contract (bytes->int low high)
  (-> byte/c byte/c word/c)
  (bitwise-xor low (arithmetic-shift high 8)))

(module+ test #| bytes->int |#
  (check-equal? (bytes->int #x00 #x00)
                #x0000)
  (check-equal? (bytes->int #xff #x00)
                #x00ff)
  (check-equal? (bytes->int #x01 #xb0)
                #xb001))

(define/contract (int->bytes word-value)
  (-> word/c (listof byte?))
  (list (low-byte word-value) (high-byte word-value)))

(module+ test #| int->bytes |#
  (check-equal? (int->bytes #xb000)
                (list #x00 #xb0))
  (check-equal? (int->bytes #x0000)
                (list #x00 #x00))
  (check-equal? (int->bytes #xb0ff)
                (list #xff #xb0)))

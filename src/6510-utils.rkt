#lang racket
#|

 provide utilities for byte/word manipulation and more

 |#

(require (rename-in  racket/contract [define/contract define/c]))

(module+ test
  (require "6510-test-utils.rkt"))

(provide 6510-number-string?
         absolute
         byte
         byte->hex-string
         byte/c
         decimal-from-two-complement
         high-byte
         is-immediate-number?
         low-byte
         parse-number-string
         two-complement-of
         word
         word->hex-string
         word/c
         in-word-range?
         in-byte-range?
         ->string
         base-label-str)

;; convert given element to string
(define/c (->string el)
  (-> (or/c string? char? symbol? number? syntax? list?) string?)
  (cond [(string? el) el]
        [(char? el) (string el)]
        [(symbol? el) (symbol->string el)]
        [(number? el) (number->string el)]
        [(syntax? el) (->string (syntax->datum el))]
        [(keyword? el) (keyword->string el)]
        [(list? el)
         (cond [(equal? (car el) 'unquote)
                (format ",~a" (cadr el))]
               [else (format "(~a)" (string-join (map (λ (iel) (->string iel)) el) ""))])]
        [else (raise-argument-error '->string (format "cannot convert ~a to string" el))]))

(module+ test #| ->string |#
  (check-equal? (->string '(($1000)))
                "(($1000))")
  (check-equal? (->string '(unquote x))
                ",x")
  (check-equal? (->string 'some)
                "some")
  (check-equal? (->string "some")
                "some")
  (check-equal? (->string 45)
                "45")
  (check-equal? (->string #'45)
                "45"))

;; is this exact integer in word range?
(define/c (in-word-range? word)
  (-> exact-integer? boolean?)
  (and (<= word 65535) (>= word 0)))

;; is this exact integer in byte range?
(define/c (in-byte-range? byte)
  (-> exact-integer? boolean?)
  (and (<= byte 255) (>= byte 0)))

(define byte/c (and/c exact-nonnegative-integer? in-byte-range?))

(define word/c (and/c exact-nonnegative-integer? in-word-range?))

;; is the given number-string prefixed with a valid number base prefix?
(define/c (number-has-prefix? number-string)
  (-> string? boolean?)
  (string-contains? "%$" (substring number-string 0 1)))

;; get the base label string (removing any prefix of > or <)
(define/c (base-label-str full-label)
  (-> string? string?)
  (cond [(or (eq? #\> (string-ref full-label 0))
            (eq? #\< (string-ref full-label 0)))
         (substring full-label 1)]
        [else full-label]))

(module+ test #| base-label-str |#
  (check-equal? (base-label-str "hello")
                "hello")
  (check-equal? (base-label-str ">hello")
                "hello")
  (check-equal? (base-label-str "<hello")
                "hello"))

;; is the given string a valid binary, hex or regular number
(define/c (6510-number-string? value)
  (-> string? boolean?)
  (and (string? value)
     (regexp-match? #rx"^(\\%(0|1)+|\\$[0-9A-Fa-f]+|[0-9]+)$"
                    value)))

(module+ test #| 6510-number-string? |#
  (check-true (6510-number-string? "234"))
  (check-true (6510-number-string? "%1001100"))
  (check-true (6510-number-string? "$234ABf"))
  (check-false (6510-number-string? "-234"))
  (check-false (6510-number-string? "$234AG"))
  (check-false (6510-number-string? "%120")))

;; get the number base of a valid number string
(define/c (prefix->number-base prefix)
  (-> string? exact-nonnegative-integer?)
  (case prefix
    [("$") 16]
    [("%") 2]
    [else 10]))

;; parse a number (binary, hex ...) string and return the number
(define/c (parse-number-string number-string)
  (-> string? exact-integer?)
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
(define/c (low-byte absolute)
  (-> exact-integer? byte/c)
  (bitwise-and #xFF absolute))

;; get the high byte of a (2 byte) number
(define/c (high-byte absolute)
  (-> exact-integer? byte/c)
  (bitwise-and #xFF (arithmetic-shift absolute -8)))

;; construct a 2 byte number for high byte and low byte
(define/c (absolute high low)
  (-> byte/c byte/c word/c)
  (bitwise-ior (arithmetic-shift high 8) low))

;; restrict value to a 2 byte value (cutting off other bits)
(define/c (word value)
  (-> exact-integer? word/c)
  (bitwise-and #xffff value))

;; restrict value to a 1 byte value (cutting off other bits)
(define/c (byte value)
  (-> exact-integer? byte/c)
  (bitwise-and #xff value))

;; return two complement of the given (possibly negative) number
(define/c (two-complement-of num)
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

(define/c (decimal-from-two-complement num)
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

;; transform a byte to a 2 digit hex string
(define/c (byte->hex-string num)
  (-> byte/c string?)
  (~a (number->string num 16)
      #:width 2 #:left-pad-string "0" #:align 'right))

;; transform a word (2 bytes) to a 4 digit hex string
(define/c (word->hex-string num)
  (-> word/c string?)
  (~a (number->string num 16)
      #:width 4 #:left-pad-string "0" #:align 'right))

(module+ test #| byte->hex-string, word->hex-string |#
  (check-equal? (byte->hex-string #x00) "00")
  (check-equal? (byte->hex-string #x01) "01")
  (check-equal? (byte->hex-string #x7f) "7f")
  (check-equal? (byte->hex-string #x80) "80")
  (check-equal? (byte->hex-string #x81) "81")
  (check-equal? (byte->hex-string #xa0) "a0")
  (check-equal? (byte->hex-string #xff) "ff")
  (check-equal? (word->hex-string #x0000) "0000")
  (check-equal? (word->hex-string #x0001) "0001")
  (check-equal? (word->hex-string #x0020) "0020")
  (check-equal? (word->hex-string #x0300) "0300")
  (check-equal? (word->hex-string #x4000) "4000")
  (check-equal? (word->hex-string #xffff) "ffff")
  (check-equal? (word->hex-string #x9999) "9999")
  (check-equal? (word->hex-string #x5e5f) "5e5f"))

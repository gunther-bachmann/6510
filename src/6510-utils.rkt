#lang racket
#|

 provide utilities for byte/word manipulation and more

 |#

(require (rename-in  racket/contract [define/contract define/c])
         (only-in "tools/data-tools.rkt" byte/c word/c))

(module+ test
  (require "6510-test-utils.rkt"))

(provide 6510-number-string?
         byte->hex-string
         is-immediate-number?
         parse-number-string
         word->hex-string
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

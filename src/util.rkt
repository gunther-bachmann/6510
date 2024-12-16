#lang typed/racket/base

(require/typed racket/struct (struct->list (Any -> (Listof Any))))
(require (only-in racket/format ~a))

(provide nested->list
         low-byte
         high-byte
         bytes->int
         format-hex-word
         format-hex-byte)

;; convert a deeply nested structure into a list that can easily be matched
(define (nested->list (deeply-nested : Any)) : Any
  (cond
    [(struct? deeply-nested)
     (define-values (info _a) (struct-info deeply-nested))
     (unless info (raise-user-error "is no struct"))
     (define-values (struct-name _b _c _d _e _f _g _h) (struct-type-info info))
     (cons struct-name (nested->list (struct->list deeply-nested)))]

    [(list? deeply-nested)
     (map nested->list deeply-nested)]

    [(hash? deeply-nested)
     (foldl (lambda ((key : Any) (acc-hash-list : (Listof Any)))
              (append acc-hash-list (list key (nested->list (hash-ref deeply-nested key)))))
            (list 'hash) (hash-keys deeply-nested))]

    [else deeply-nested]))

(module+ test #| require test utils |#
  ;; (require "../6510-test-utils.rkt")
  (require typed/rackunit)

  (require/typed racket/struct (struct->list (Any -> (Listof Any))))

  (check-equal? (nested->list (hash 'a 1))
                (list 'hash 'a 1)))

(define (low-byte (int : Integer)) : Byte
  (define lb (bitwise-and #xff int))
  (if (byte? lb)
      lb
      (raise-user-error "low-byte error")))

(define (high-byte (int : Integer)) : Byte
  (define hb (arithmetic-shift int -8))
  (if (byte? hb)
      hb
      (raise-user-error "high-byte error")))

(define (bytes->int (low : Byte) (high : Byte)) : Integer
  (bitwise-xor low (arithmetic-shift high 8)))

(module+ test #| low-byte, high-byte, bytes->int |#
  (check-equal? (low-byte #xA5FE)
                #xFE)
  (check-equal? (high-byte #xA5FE)
                #xA5)
  (check-equal? (bytes->int #xFE #xA5)
                #xA5FE))

;; format a hexadecimal byte
(define (format-hex-byte (byte : Byte))
  (~a (number->string byte 16) #:width 2 #:align 'right #:pad-string "0"))

(define (format-hex-word (word : Integer))
  (~a (number->string word 16) #:width 4 #:align 'right #:pad-string "0"))

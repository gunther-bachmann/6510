#lang typed/racket/base

(require/typed racket/struct (struct->list (Any -> (Listof Any))))

(provide nested->list)

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


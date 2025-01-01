#lang racket/base

(require (only-in racket/list empty? permutations dropf drop))

#|

TODO: make all recursive functions tail-callable
      implement
      - btree-add-value-before :: add given value before the given path
      - btree<-list :: creating a balanced tree with all elements in the ordered list
      - btree->list :: creating an ordered list of elements
      - btree-remove :: remove value from given path in tree
                       (since nodes may be rewritten, return path the next || last)
      - btree-root-of-path :: give the root of the current tree (referenced through this patH)

simplest valued tree

case A
   o
  / \
 P   T

case B
   o
  / \
 P   nil


add X after P:
  case A:          case B
      o                o
    /   \             / \
   P     o           P   X
        / \
       X   T

add X after T (only case A):
  case B:
      o
    /   \
   P     o
        / \
       T   X

add X before P:
  case A:          case B
      o                o
    /   \             / \
   o     T           X   P
  / \
 X   P 

add X before T (only case A)
  case A:
      o
    /   \
   P     o
        / \       
       X   T

strategies for deleting:
  if right node: just set nil
  if left node and right is not nil: swap
  if both are nil delete parent node (recurse)

strategies for finding next/prev

  next:
    if left node and has right node: find first of right node
    if left node and has no right node
       or if right node: go up until coming from a left node (and right node != null), get first of right node

  prev:
   if right node: find last of left node
   if left node: go up until coming from a right node. get last of left node


  first of node:
    keep going left to first leaf


  last of node:
    keep going right to first leaf, if leaf is nil, take last of left of same level
    
    
invariants:
  - left is never nil, tree is reorganized if left is deleted!
  - left may be subtree or leaf value
  - right may be nil, subtree or leaf value


 |#
(module+ test #| btree add value|#
  (require rackunit))

(define (btree-make-root value)
  (cons value null))

(module+ test #| btree add value|#
  (check-equal? (btree-make-root 1)
                (cons 1 null)))

(define (btree-value? node)
  (or (string? node) (integer? node)))

(define (btree-node? node)
  (pair? node))

(define (btree-validate node (print-error #f))
  (define is-pair-or-value (or (btree-node? node) (btree-value? node)))

  (when (and print-error (not is-pair-or-value))
    (displayln (format "validation failed: is pair or value: ~a" node)))

  (cond [(pair? node)
         (define car-is-not-nil (not (empty? (car node))))

         (when (and print-error (not car-is-not-nil))
           (displayln (format "validation failed: car is not nil: ~a" node)))

         (define left-is-valid
           (btree-validate (car node) print-error))

         (define right-is-valid
           (if (empty? (cdr node))
               #t
               (btree-validate (cdr node) print-error)))

         (and is-pair-or-value
            car-is-not-nil
            left-is-valid
            right-is-valid)]
        [else
         (and is-pair-or-value)]))

(module+ test #| |#
  (check-true (btree-validate (btree-make-root 1) #t)))

(define (btree-depth node)
  (cond [(not (pair? node)) 0]
        [else
         (define l (car node))
         (define r (cdr node))
         (add1 (max (btree-depth l) (btree-depth r)))]))

(module+ test #| btree-depth |#
  (check-equal? (btree-depth (btree-make-root 1))
                1)
  (check-equal? (btree-depth (btree-make-root "1"))
                1)

  (check-equal? (btree-depth '(((1 . ()) . 3) . (4 . 5)))
                3))

(define (btree-path-to-first node (path (list)))
  (cond [(btree-value? node) path]
        [else (btree-path-to-first (car node) (cons (cons -1 node) path))]))

(module+ test #| btree-path-to-first |#
  (check-equal? (btree-path-to-first (btree-make-root "1"))
                '((-1 . ("1" . ()))))

  (check-equal? (btree-path-to-first '(((1 . ()) . 3) . (4 . 5)))
                '((-1 . (1 . ()))
                  (-1 . ((1 . ()) . 3))
                  (-1 . (((1 . ()) . 3) . (4 . 5))))))

(define (btree-path-to-last  node (path (list)))
  (cond [(btree-value? node) path]
        [(empty? (cdr node)) (btree-path-to-last (car node) (cons (cons -1 node) path))]
        [else (btree-path-to-last (cdr node) (cons (cons 1 node) path))]))

(module+ test #| btree-path-to-last |#
  (check-equal? (btree-path-to-last (btree-make-root "1"))
                '((-1 . ("1" . ()))))

  (check-equal? (btree-path-to-last '(((1 . 2) . 3) . (4 . 5)))
                '((1 . (4 . 5))
                  (1 . (((1 . 2) . 3) . (4 . 5)))))
  (check-equal? (btree-path-to-last '(((1 . 2) . 3) . (4 . ())))
                '((-1 . (4 . ()))
                  (1 . (((1 . 2) . 3) . (4 . ()))))))

(define (btree-node-for-path path)
  (cond [(empty? path) '()]
        [(= -1 (caar path)) (car (cdar path))]
        [(= 1 (caar path)) (cdr (cdar path))]
        [else (raise-user-error (format "btree path may only contain 1 | -1:" path))]))
 
(module+ test #| btree-node-for-path |#
  (check-equal? (btree-node-for-path '((-1 . ("1" . ()))))
                "1")

  (check-equal? (btree-node-for-path '((1 . (4 . 5))
                                       (1 . (((1 . 2) . 3) . (4 . 5)))))
                5)
  (check-equal? (btree-node-for-path '((1 . (((1 . 2) . 3) . (4 . 5)))))
                '(4 . 5))
  (check-equal? (btree-node-for-path '((-1 . (4 . ()))
                                       (1 . (((1 . 2) . 3) . (4 . ())))))
                4))

(define (btree-prev path)
  (cond [(empty? path)
         '()]

        [(= 1 (caar path))
         ;; left must not be empyt => no additional check
         (define top-most-relevant  path)
         (append (btree-path-to-last (cadar top-most-relevant))
                 (cons (cons -1 (cdar top-most-relevant))
                       (cdr top-most-relevant)))]

        [(= -1 (caar path))
         (define top-most-relevant (dropf (cdr path)
                                          (lambda (pe) (= -1 (car pe))
                                                  ;; left must not be empyt => no further check
                                            )))
         (if (empty? top-most-relevant)
             '()
             (append (btree-path-to-last (cadar top-most-relevant))
                     (cons (cons -1 (cdar top-most-relevant))
                           (cdr top-most-relevant))))]

        [else (raise-user-error "unknown case")]))

(module+ test #| btree-prev |#
  (check-equal? (btree-prev '((-1 . (5 . ()))))
                '()
                "if no left is present any level firther up, result is '()")

  (check-equal? (btree-prev '((1 . (5 . 6))))
                '((-1 . (5 . 6)))
                "choose last of left if current path element is right")

  (check-equal? (btree-prev '((1 . (((4 . (5 . 6)) . ()) . 7))))
                '((1 . (5 . 6))
                  (1 . (4 . (5 . 6)))
                  (-1 . ((4 . (5 . 6)) . ()))
                  (-1 . (((4 . (5 . 6)) . ()) . 7)))
                "choose last of left if current path is right")

  (check-equal? (btree-prev '((-1 . (6 . 7))
                              (1 . (((1 . 2) . ()) . (6 . 7)))
                              (-1 . ((((1 . 2) . ()) . (6 . 7)) . ()))
                              (-1 . (((((1 . 2) . ()) . (6 . 7)) . ()) . 8))
                              ;; (-1 . ((((((1 . 2) . ()) . (6 . 7)) . ()) . 8) . 9) )
                              ;; (-1 . (((((((1 . 2) . ()) . (6 . 7)) . ()) . 8) . 9) . 10) )
                              ))
               '((1 . (1 . 2))
                 (-1 . ((1 . 2) . ()))
                 (-1 . (((1 . 2) . ()) . (6 . 7)))
                 (-1 . ((((1 . 2) . ()) . (6 . 7)) . ()))
                 (-1 . (((((1 . 2) . ()) . (6 . 7)) . ()) . 8) ))))

(define (btree-next path)
  (cond [(empty? path)
         '()]

        [(and (= -1 (caar path))
            (not (empty? (cddar path))))
         (define top-most-relevant path)
         (append (btree-path-to-first (cddar top-most-relevant))
                 (cons (cons 1 (cdar top-most-relevant))
                       (cdr top-most-relevant)))]

        [(or (and (= -1 (caar path))
               (empty? (cddar path)))
            (= 1 (caar path)))
         (define top-most-relevant (dropf (cdr path)
                                          (lambda (pe) (or (= 1 (car pe))
                                                     (empty? (cdr (cdr pe)))))))
         (if (empty? top-most-relevant)
             '()
             (append (btree-path-to-first (cddar top-most-relevant))
                     (cons (cons 1 (cdar top-most-relevant))
                           (cdr top-most-relevant))))]

        [else (raise-user-error "unknown case")]))

(module+ test #| btree-next |#
  (check-equal? (btree-next '((-1 . (5 . ()))))
                '()
                "if right is nil and there is no way to go one level up, result is '()")

  (check-equal? (btree-next '((-1 . (5 . ()))
                              (-1 . ((5 . ()) . ()))
                              (-1 . (((5 . ()) . ()) . ()))))
                '()
                "if right is nil and all the way to the root there is no right, result is '()")

  (check-equal? (btree-next '((-1 . (5 . 6))))
                '((1 . (5 . 6)))
                "no need to go any level up, just select right")

  (check-equal? (btree-next '((-1 . (5 . ()))
                              (-1 . ((5 . ()) . (6 . ())))))
                '((-1 . (6 . ()))
                  (1 . ((5 . ()) . (6 . ()))))
                "go one level up (since right is '()), replace -1 with 1 and select first of subtree (6 . ())")


  (check-equal? (btree-next '((-1 . (5 . ()))
                              (-1 . ((5 . ()) . (((6 . 7) . 9) . ())))))
                '((-1 . (6 . 7))
                  (-1 . ((6 . 7) . 9))
                  (-1 . (((6 . 7) . 9) . ()))
                  (1 . ((5 . ()) . (((6 . 7) . 9) . ()))))
                "go one level up (since right is '()), replace -1 with 1 and select first of subtree (((6 . 7) . 9) . ())")

  (check-equal? (btree-next '((-1 . (5 . ()))
                              (-1 . ((5 . ()) . ()))
                              (-1 . (((5 . ()) . ()) . ()))
                              (-1 . ((((5 . ()) . ()) . ()) . 8))))
                '((1 . ((((5 . ()) . ()) . ()) . 8)))
                "go all the way up to the root and then select path to 8 as next")

  (check-equal? (btree-next '((-1 . (5 . ()))
                              (-1 . ((5 . ()) . ()))
                              (-1 . (((5 . ()) . ()) . ()))
                              (-1 . ((((5 . ()) . ()) . ()) . ((8 . 9) . (10 11))))))
                '((-1 . (8 . 9))
                  (-1 . ((8 . 9) . (10 11)))
                  (1 . ((((5 . ()) . ()) . ()) . ((8 . 9) . (10 11)))))
                "skip over all levels that have () as right, selecting first of subtree ((8 . 9) . (10 11))")

  (check-equal? (btree-next '((1 . (8 . 9))
                              (-1 . ((8 . 9) . ((10 . 11) . ()))) ;; level X
                              (1 . (7 . ((8 . 9) . ((10 . 11) . ()))))))
                '((-1 . (10 . 11))
                  (-1 . ((10 . 11) . ()))
                  (1 . ((8 . 9) . ((10 . 11) . ())))
                  (1 . (7 . ((8 . 9) . ((10 . 11) . ())))))
                "go up to level X, replace -1 with 1 and search first in that subtree ((10 . 11) . ())"))


;; TODO: implement tail recursive!
;; replace new nodes up the tree, making the tree persistent
;; balanced: O(lg N), worst case O(N)
(define (recursive-rebuild-path-at-with path repl-node)
  (cond [(empty? path) '()]
        [(= (caar path) -1)
         (define new-node (cons repl-node (cddar path)))
         (define new-pe
           (cons (caar path) new-node))
         (cons new-pe
               (recursive-rebuild-path-at-with (cdr path) new-node))]
        [(= (caar path) 1)
         (define new-node (cons (cadar path) repl-node))
         (define new-pe
           (cons (caar path) new-node))
         (cons new-pe
               (recursive-rebuild-path-at-with (cdr path) new-node))]
        [else (raise-user-error "unknown case")]))

(module+ test #| recursive-rebuild-path-at-with |#
  (check-equal? (recursive-rebuild-path-at-with
                 '((-1 . (5 . 6))
                   (-1 . ((5 . 6) . 7))
                   (1 . (3 . ((5 . 6) . 7)))
                   (-1 . ((3 . ((5 . 6) . 7)) . 8)))
                 '(4 . 5))

                '((-1 . ((4 . 5) . 6))
                   (-1 . (((4 . 5) . 6) . 7))
                   (1 . (3 . (((4 . 5) . 6) . 7)))
                   (-1 . ((3 . (((4 . 5) . 6) . 7)) . 8)))

                "replace node 5 with (4 . 5) and rewrite all nodes up to the root"))

;; add value after the given path, returning the new path (with the tail holding the new root, because tree is persistent)
;; balanced: O(lg N), worst case O(N)
(define (btree-add-value-after value path)
  (cond [(empty? path) (raise-user-error "path may not be empty")]
        [(and (= -1 (caar path))
            (empty? (cddar path)))
         (cons
          (cons 1 (cons (cadar path) value))
          (cdr path))]
        [(and (= -1 (caar path))
            (not (empty? (cddar path))))
         (define new-right-node (cons value (cddar path)))
         (define repl-node (cons (cadar path) new-right-node))
         (cons
          (cons -1 new-right-node)          
          (cons (cons 1 repl-node)
                (recursive-rebuild-path-at-with (cdr path) repl-node)))]
        [(= 1 (caar path))
         (define new-right-node (cons (cddar path) value))
         (define repl-node (cons (cadar path) new-right-node))
         (cons
          (cons 1 new-right-node)
          (cons (cons 1 repl-node)
                (recursive-rebuild-path-at-with (cdr path) repl-node)))]
        [else (raise-user-error "unknown case")]))

(module+ test #| add value after |#
  (check-equal? (btree-add-value-after 5 '((-1 . (4 . ()))))
                '((1 . (4 . 5))))

  (check-equal? (btree-add-value-after 5 '((-1 . (4 . 6))))
                '((-1 . (5 . 6))
                  (1 . (4 . (5 . 6)))))
  (check-equal? (btree-add-value-after 5 '((-1 . (4 . (6 . 7)))))
                '((-1 . (5 . (6 . 7)))
                  (1 . (4 . (5 . (6 . 7))))))

  (check-equal? (btree-add-value-after 7 '((1 . (5 . 6))))
                '((1 . (6 . 7))
                  (1 . (5 . (6 . 7)))))

  (check-equal? (btree-add-value-after 7 '((1 . (5 . 6))
                                           (1 . (4 . (5 . 6)))
                                           (1 . (3 . (4 . (5 . 6))))))
                '((1 . (6 . 7))
                  (1 . (5 . (6 . 7)))
                  (1 . (4 . (5 . (6 . 7))))
                  (1 . (3 . (4 . (5 . (6 . 7))))))
                "update of new cons cells needs to go up to the root (persistent tree)!"))

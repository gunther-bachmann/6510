#lang racket/base

(require (only-in racket/list empty? permutations dropf drop))

#|

TODO: make all recursive functions tail-callable

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

(define (btree-depth node (right-list (list)) (depth 0) (max-depth 0))
  (cond [(and (not (pair? node))
            (empty? right-list))
         (max depth max-depth)]
        [(not (pair? node))
         (btree-depth (caar right-list) (cdr right-list) (cdar right-list) (max depth max-depth))]
        [else
         (define l (car node))
         (define r (cdr node))
         (btree-depth l (cons (cons r (add1 depth)) right-list) (add1 depth) max-depth)]))

(module+ test #| btree-depth |#
  (check-equal? (btree-depth "1")
                0)
  (check-equal? (btree-depth (btree-make-root 1))
                1)
  (check-equal? (btree-depth (btree-make-root "1"))
                1)
  (check-equal? (btree-depth '(((1 . ()) . 3) . (4 . 5)))
                3)
  (check-equal? (btree-depth '((3 . ((4 . (5 . ((6 . 7) . ()))) . ())) . 8))
                7)
  (check-equal? (btree-depth '((1 . (2 . 3))))
                3)
  (check-equal? (btree-depth '((1 . ((2 . ()) . (((3 . 4) . ()) . ()))) . 5))
                6)
  (check-equal? (btree-depth '((3 . ((4 . (5 . ((6 . ()) . (7 . (8 . (9 . 9)))))) . ())) . 10))
                9))

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


;; replace new nodes up the tree, making the tree persistent
;; balanced: O(lg N), worst case O(N)
(define (recursive-rebuild-path-with path repl-node (result (list)))
  (cond [(empty? path) (reverse result)]
        [(= (caar path) -1)
         (define new-node (cons repl-node (cddar path)))
         (define new-pe
           (cons (caar path) new-node))                 
         (recursive-rebuild-path-with (cdr path) new-node (cons new-pe result))]
        [(= (caar path) 1)
         (define new-node (cons (cadar path) repl-node))
         (define new-pe
           (cons (caar path) new-node))                 
         (recursive-rebuild-path-with (cdr path) new-node (cons new-pe result))]
        [else (raise-user-error "unknown case")]))

(module+ test #| recursive-rebuild-path-at-with |#
  (check-equal? (recursive-rebuild-path-with
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
         (define new-node (cons (cadar path) value))
         (cons
          (cons 1 new-node)
          (recursive-rebuild-path-with (cdr path) new-node))]
        [(and (= -1 (caar path))
            (not (empty? (cddar path))))
         (define new-right-node (cons value (cddar path)))
         (define repl-node (cons (cadar path) new-right-node))
         (cons
          (cons -1 new-right-node)          
          (cons (cons 1 repl-node)
                (recursive-rebuild-path-with (cdr path) repl-node)))]
        [(= 1 (caar path))
         (define new-right-node (cons (cddar path) value))
         (define repl-node (cons (cadar path) new-right-node))
         (cons
          (cons 1 new-right-node)
          (cons (cons 1 repl-node)
                (recursive-rebuild-path-with (cdr path) repl-node)))]
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

(define (btree-add-value-before value path)
  (cond [(empty? path) (raise-user-error "path may not be empty")]
        [(and (= -1 (caar path))
            (empty? (cddar path)))
         (define new-node (cons value (cadar path)))
         (cons
          (cons -1 new-node)
          (recursive-rebuild-path-with (cdr path) new-node))]
        [(and (= -1 (caar path))
            (not (empty? (cddar path))))
         (define new-left-node (cons value (cadar path)))
         (define repl-node (cons new-left-node (cddar path)))
         (cons
          (cons -1 new-left-node)
          (cons (cons -1 repl-node)
                (recursive-rebuild-path-with (cdr path) repl-node)))]
        [(= 1 (caar path))
         (define new-right-node (cons value (cddar path)))
         (define repl-node (cons (cadar path) new-right-node))
         (cons
          (cons -1 new-right-node)
          (cons (cons 1 repl-node)
                (recursive-rebuild-path-with (cdr path) repl-node)))]
        [else (raise-user-error "unknown case")]))

(module+ test #| add value before |#
  (check-equal? (btree-add-value-before 5 '((-1 . (6 . ()))))
                '((-1 . (5 . 6)))
                "replace null with value and make new node")

  (check-equal? (btree-add-value-before 5 '((-1 . (6 . ()))
                                            ( 1 . (3 . (6 . ())))
                                            (-1 . ((3 . (6 . ())) . 7))))
                '((-1 . (5 . 6))
                  ( 1 . (3 . (5 . 6)))
                  (-1 . ((3 . (5 . 6)) . 7)))
                "replace null with value, make new node and replace all up to the root")

  (check-equal? (btree-add-value-before 5 '((-1 . (6 . 7))))
                '((-1 . (5 . 6))
                  (-1 . ((5 . 6) . 7)))
                "replace old node 6 with (5 . 6)")

  (check-equal? (btree-add-value-before 5 '((-1 . (6 . 7))
                                            ( 1 . (3 . (6 . 7)))
                                            (-1 . ((3 . (6 . 7)) . 8))))
                '((-1 . (5 . 6))
                  (-1 . ((5 . 6) . 7))
                  ( 1 . (3 . ((5 . 6) . 7)))
                  (-1 . ((3 . ((5 . 6) . 7)) . 8)))
                "replace old node 6 with (5 . 6) and replace all up to the root")

  (check-equal? (btree-add-value-before 5 '((1 . (4 . 6))))
                '((-1 . (5 . 6))
                  ( 1 . (4 . (5 . 6))))
                "replace node 6 with (5 . 6)")

  (check-equal? (btree-add-value-before 5 '(( 1 . (4 . 6))
                                            (-1 . ((4 . 6) . 7))
                                            ( 1 . (3 . ((4 . 6) . 7)))
                                            (-1 . ((3 . ((4 . 6) . 7)) . 8))))
                '((-1 . (5 . 6))
                  ( 1 . (4 . (5 . 6)))
                  (-1 . ((4 . (5 . 6)) . 7))
                  ( 1 . (3 . ((4 . (5 . 6)) . 7)))
                  (-1 . ((3 . ((4 . (5 . 6)) . 7)) . 8)))
                "replace node 6 with (5 . 6) and replace all up to the root"))

(define (btree->list node (result (list)) (btree-prefix (list)))
  (cond [(and (empty? node)
            (not (empty? btree-prefix)))
         (btree->list (car btree-prefix) result (cdr btree-prefix))]
        [(empty? node) result]
        [(btree-value? node) (btree->list '() (cons node result) btree-prefix)]
        [(btree-node? node)         
         (btree->list (cdr node) result (cons (car node) btree-prefix))]
        [else (raise-user-error "unknown case")]))

(module+ test #| btree->list |#
  (check-equal? (btree->list '(1 . ()))
                '(1))

  (check-equal? (btree->list '((3 . ((4 . (5 . ((6 . 7) . ()))) . ())) . 8))
                '(3 4 5 6 7 8)))

(define (btree<-nodes nodes (result (list)))
  (cond 
    [(and (empty? nodes) (empty? result)) '()]
    [(and (empty? nodes)
        (not (empty? result))
        (empty? (cdr result))) 
     (car result)]
    [(and (empty? nodes)
        (not (empty? result)))
     (btree<-nodes (reverse result))]
    [(empty? (cdr nodes))
     (btree<-nodes (cdr nodes) (cons (cons (car nodes) '()) result))]
    [else
     (btree<-nodes (cddr nodes)
                  (cons (cons (car nodes) (cadr nodes)) result))]))

(module+ test #| <-nodes |#
  (check-equal? (btree<-nodes '(1))
                '(1 . ()))

  (check-equal? (btree<-nodes '(1 2 3))
                '((1 . 2) . (3 . ())))

  (check-equal? (btree<-nodes '(1 2 3 4))
                '((1 . 2) . (3 . 4)))

  (check-equal? (btree<-nodes '(1 2 3 4 5))
                '(((1 . 2) . (3 . 4)) . ((5 . ()) . ())))

  (check-equal? (btree<-nodes '(1 2 3 4 5 6))
                '(((1 . 2) . (3 . 4)) . ((5 . 6) . ())))

  (check-equal? (btree<-nodes '(1 2 3 4 5 6 7))
                '(((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . ()))))

  (check-equal? (btree<-nodes '(1 2 3 4 5 6 7 8))
                '(((1 . 2) . (3 . 4)) . ((5 . 6) . (7 . 8)))))

(define (btree-remove-value-at path (result (list)) (old-next (list)))
  (cond
    [(and (empty? path)
        (not (empty? old-next)))
     (define node (btree-node-for-path result))
     (define next-node-path (btree-next result))
     (if (or (eq? node old-next)
            (empty? next-node-path))
         result
         next-node-path)]
    [(empty? path) result]
    [(= 1 (caar path))
     (define new-node (cons (cadar path) '()))
     (btree-remove-value-at
      (list)
      (cons
       (cons -1 new-node)
       (recursive-rebuild-path-with (cdr path) new-node))
      old-next)]
    [(and (= -1 (caar path))
        (not (empty? (cddar path))))
     (define new-node (cons (cddar path) '()))
     (btree-remove-value-at
      (list)
      (cons
       (cons -1 new-node)
       (recursive-rebuild-path-with (cdr path) new-node))
      old-next)]
    [(and (= -1 (caar path))
        (empty? (cddar path))
        (empty? (cdr path)))
     '()]
    [(and (= -1 (caar path))
        (empty? (cddar path))
        (not (empty? (cdr path))))
     (define old-next-node (btree-node-for-path (btree-next path)))
     (btree-remove-value-at (cdr path) (list) old-next-node)]
    [else (raise-user-error "unknown case")]))

(module+ test #| remove-value |#
  (check-equal? (btree-remove-value-at '((1 . (5 . 6))))
                '((-1 . (5 . ())))
                "replace (5 . 6) with (5 . ())")

  (check-equal? (btree-remove-value-at '(( 1 . (5 . 6))
                                         (-1 . ((5 . 6) . 7))
                                         ( 1 . (4 . ((5 . 6) . 7)))
                                         (-1 . ((4 . ((5 . 6) . 7)) . 8))))
                '((-1 . (5 . ()))
                  (-1 . ((5 . ()) . 7))
                  ( 1 . (4 . ((5 . ()) . 7)))
                  (-1 . ((4 . ((5 . ()) . 7)) . 8)))
                "replace (5 . 6) with (5 . ()) and do replace recursively up to the root")

  (check-equal? (btree-remove-value-at '((-1 . (5 . 6))))
                '((-1 . (6 . ())))
                "replace (5 . 6) with (6 . ())")

  (check-equal? (btree-remove-value-at '((-1 . (5 . 6))
                                         (-1 . ((5 . 6) . 7))
                                         ( 1 . (4 . ((5 . 6) . 7)))
                                         (-1 . ((4 . ((5 . 6) . 7)) . 8))))
                '((-1 . (6 . ()))
                  (-1 . ((6 . ()) . 7))
                  ( 1 . (4 . ((6 . ()) . 7)))
                  (-1 . ((4 . ((6 . ()) . 7)) . 8)))
                "replace (5 . 6) with (6 . ()) and do replace recursively up to the root")

  (check-equal? (btree-remove-value-at '((-1 . (5 . ()))))
                '())

  (check-equal? (btree-remove-value-at '((-1 . (5 . ()))
                                         ( 1 . (4 . (5 . ())))
                                         (-1 . ((4 . (5 . ())) . 6))
                                         ( 1 . (3 . ((4 . (5 . ())) . 6)))))
                '(( 1 . ((4 . ()) . 6))
                  ( 1 . (3 . ((4 . ()) . 6))))
                "make sure to select next node and resursively replace nodes up to the root")

  (check-equal? (btree-remove-value-at '((-1 . (5 . ()))
                                         (-1 . ((5 . ()) . ()))
                                         (-1 . (((5 . ()) . ()) . ()))))
                '()
                "if this is the last node to be deleted, return nil")

  (check-equal? (btree-remove-value-at '((-1 . ((5 . ()) . 6))))
                '((-1 . (6 . ())))
                "if the node deleted has a next, return that one")

  (check-equal? (btree-remove-value-at '((-1 . (5 . ()))
                                         (-1 . ((5 . ()) . 6))))
                '((-1 . (6 . ())))
                "if the node deleted has a next, return that one")

  (check-equal? (btree-remove-value-at '((-1 . (5 . ()))
                                         (-1 . ((5 . ()) . ()))
                                         (-1 . (((5 . ()) . ()) . ()))
                                         (-1 . ((((5 . ()) . ()) . ()) . 6))))
                '((-1 . (6 . ())))
                "if the node deleted has a next, return that one")

  (check-equal? (btree-remove-value-at '((-1 . (5 . ()))
                                         (-1 . ((5 . ()) . ()))
                                         (-1 . (((5 . ()) . ()) . ()))
                                         (-1 . ((((5 . ()) . ()) . ()) . 6))
                                         ( 1 . (4 . ((((5 . ()) . ()) . ()) . 6)))
                                         ( 1 . (3 . (4 . ((((5 . ()) . ()) . ()) . 6))))
                                         (-1 . ((3 . (4 . ((((5 . ()) . ()) . ()) . 6))) . 7))))
                '((-1 . (6 . ()))
                  ( 1 . (4 . (6 . ())))
                  ( 1 . (3 . (4 . (6 . ()))))
                  (-1 . ((3 . (4 . (6 . ()))) . 7)))
                "if the node deleted has a next, return that one and recursively replace up to root"))

(define (btree-root-of-path path)
  (cond [(empty? path) '()]
        [(not (empty? (cdr path))) (btree-root-of-path (cdr path))]
        [else (cdar path)]))

(module+ test #| root of path |#
  (check-equal? (btree-root-of-path '())
                '())

  (check-equal? (btree-root-of-path '(( 1 . (5 . 6))
                                      (-1 . ((5 . 6) . (7 . 8)))
                                      ( 1 . (4 . ((5 . 6) . (7 . 8))))
                                      (-1 . ((4 . ((5 . 6) . (7 . 8))) . 9))))
                '((4 . ((5 . 6) . (7 . 8))) . 9)))

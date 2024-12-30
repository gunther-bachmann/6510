#lang racket/base

(require (only-in racket/list empty? permutations dropf drop))

#|

simplest valued tree

   o
  / \
 1   nil

add 2 after 1

   o
  / \
 1   2

add 4 after 2

     o
    / \
   o   4
  / \
 1   2

add 5 after 4                  alternative

      o                                o
    /   \                             / \
   o     o                           o   5
  / \   / \                         / \  
 1   2 4   5                       o   4 
                                  / \
                                 1   2


add 3 after 2 (before 4)        alternative           alternative      alternative
        o                             o                    o                    o     
      /   \                         /   \                /   \                /   \   
    o      o                      o      o             o      o             o      o  
   / \    / \                    / \    / \           / \    / \           / \    / \ 
  o   3  4   5                  1   o  4   5         1   2  o   5         o   3  4   5
 / \                               / \                     / \           / \          
1   2                             2   3                   3   4         1   2         
push down existing             create new node o
node           o               and replace    / \
add new node  / \              2 with it     2   3
  o          1   2
 / \
    3

add 0 before 1              alternative

       ...                         ...   
     /                         /     
    o                         o      
   / \                       / \     
  0   o                     o   2    
     / \                   / \   
    1   2                 0   1  


strategies for adding:
  add after leaf x:  x is left, has right = nil, set right to new value
  add after leaf x:  x is left, alt1: create new node with leaf x as left and the new value as r and replace original leaf x with it
  add after leaf x:  x is right, alt1:

strategies for deleting:
  if right node: just set nil
  if left, right is not nil: swap
  if both are nil delete parent node (recurse)

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
  (cond [(btree-value? node) (reverse path)]
        [else (btree-path-to-first (car node) (cons -1 path))]))

(module+ test #| btree-path-to-first |#
  (check-equal? (btree-path-to-first (btree-make-root "1"))
                (list -1))

  (check-equal? (btree-path-to-first '(((1 . ()) . 3) . (4 . 5)))
                (list -1 -1 -1)))

(define (btree-path-to-last  node (path (list)))
  (cond [(btree-value? node) (reverse path)]
        [(empty? (cdr node)) (btree-path-to-last (car node) (cons -1 path))]
        [else (btree-path-to-last (cdr node) (cons 1 path))]))

(module+ test #| btree-path-to-last |#
  (check-equal? (btree-path-to-last (btree-make-root "1"))
                (list -1))

  (check-equal? (btree-path-to-last '(((1 . 2) . 3) . (4 . 5)))
                (list 1 1))
  (check-equal? (btree-path-to-last '(((1 . 2) . 3) . (4 . ())))
                (list 1 -1)))

(define (btree-node-for-path node path)
  (cond [(empty? path) node]
        [(= -1 (car path)) (btree-node-for-path (car node) (cdr path))]
        [(= 1 (car path)) (btree-node-for-path (cdr node) (cdr path))]
        [else (raise-user-error (format "btree path may only contain 1 | -1:" path))]))

(module+ test #| btree-node-for-path |#
  (check-equal? (btree-node-for-path (btree-make-root "1")
                                     (list -1))
                "1")

  (check-equal? (btree-node-for-path '(((1 . 2) . 3) . (4 . 5))
                                     (list 1 1))
                5)
  (check-equal? (btree-node-for-path '(((1 . 2) . 3) . (4 . 5))
                                     (list 1))
                '(4 . 5))
  (check-equal? (btree-node-for-path '(((1 . 2) . 3) . (4 . ()))
                                     (list 1 -1))
                4))

;; return nodes traversed by the given path
;; (returning root + nodes which will be 1 element more than in path)
(define (btree-nodes-for-path node path (path-nodes (list)))
  (cond
    [(empty? path) (reverse (cons node path-nodes))]
    [(= -1 (car path)) (btree-nodes-for-path (car node) (cdr path) (cons node path-nodes))]
    [(= 1 (car path)) (btree-nodes-for-path (cdr node) (cdr path) (cons node path-nodes))]
    [else (raise-user-error (format "btree path may only contain 1 | -1:" path))]))

(module+ test #| btree-nodes-for-path |#
  (check-equal? (drop (btree-nodes-for-path (btree-make-root "1")
                                            (list -1))
                      1)
                '("1"))

  (check-equal? (drop (btree-nodes-for-path '(((1 . 2) . 3) . (4 . 5))
                                            (list 1 1))
                      1)
                '((4 . 5)
                  5))
  (check-equal? (drop (btree-nodes-for-path '(((1 . 2) . 3) . (4 . 5))
                                            (list 1))
                      1)
                '((4 . 5)))
  (check-equal? (drop (btree-nodes-for-path '(((1 . 2) . 3) . (4 . ()))
                                            (list 1 -1))
                      1)
                '((4)
                  4)))

;; get first non nil next node in the tree
(define (btree-next root-node path (path-nodes (list)))
  (define rev-path (reverse path))
  (define rev-non-1-path (dropf rev-path (lambda (pe) (= 1 pe))))
  (define p-nodes
    (cond [(empty? path-nodes) (btree-nodes-for-path root-node path)]
          [else path-nodes]))

  (cond [(empty? path) '()]
        [(empty? rev-non-1-path) '()]
        [(= 1 (car rev-path)) ;; path backtracked to first -1
         (btree-next root-node (reverse (cons 1 (cdr rev-non-1-path))))
         ]
        [else (btree-path-to-first root-node (reverse (cons 1 (cdr rev-non-1-path))))])
  
  '())

(module+ test #| btree-next |#
  (check-equal? (btree-next '(1 . ())
                            '(-1))
                '())

  (check-equal? (btree-next '(1 . 2)
                            '(-1))
                '(1))

  ;; (check-equal? (btree-next '(((1 . 2) . 3) . (4 . 5))
  ;;                           (list 1 1) ;; points to 5
  ;;                           )
  ;;               '())

  ;; (check-equal? (btree-next '(((1 . 2) . 3) . (4 . ()))
  ;;                           (list 1 -1) ;; points to 4
  ;;                           )
  ;;               '())

  ;; (check-equal? (btree-next '(((1 . 2) . 3) . (4 . ()))
  ;;                           (list -1 -1 -1) ;; points to 1
  ;;                           )
  ;;               (list -1 -1 1) ;; points to 2
  ;;               )

  ;; (check-equal? (btree-next '(((1 . ()) . 3) . (4 . ()))
  ;;                           (list -1 -1 -1) ;; points to 1
  ;;                           )
  ;;               (list -1 1) ;; points to 3
  ;;               )
  )


;; (define (btree-validate node (print-error #f))
;;   (define node-must-be-pair (mpair? node))
;;   (when (and print-error (not node-must-be-pair))
;;     displayln (format "failed rule: node must be pair, ~a" node))

;;   (define lb-node (mcdr node))
;;   (define l (mcar lb-node))
;;   (define r (mcdr lb-node))

;;   (define comparator-must-be-int (integer? (mcar node)))
;;   (when (and print-error (not comparator-must-be-int))
;;     (displayln (format "failed rule: comparator must be an integer, ~a" node)))

;;   (define left-is-integer-or-pair
;;     (or (integer? l)
;;       (and (mpair? l)
;;          (btree-validate l print-error))))
;;   (when (and print-error (not left-is-integer-or-pair))
;;     (displayln (format "failed rule: left is integer or pair, ~a" node)))

;;   (define right-is-integer-or-pair-or-empty
;;     (or (integer? l)
;;       (empty? r)
;;       (and (mpair? l)
;;          (btree-validate l print-error))))
;;   (when (and print-error (not right-is-integer-or-pair-or-empty))
;;     (displayln (format "failed rule: right is integer or pair or empty, ~a" node)))

;;   (define max-of-left-matches-c-value
;;     (= (mcar node)
;;        (btree-max-value l)))
;;   (when (and print-error (not max-of-left-matches-c-value))
;;     (displayln (format "failed rule: max of left matches c-value ~a" node)))

;;   (and
;;    node-must-be-pair
;;    comparator-must-be-int
;;    left-is-integer-or-pair
;;    right-is-integer-or-pair-or-empty
;;    max-of-left-matches-c-value))


;; (define (btree-add-value la-node value)
;;   (define lb-node (mcdr la-node))
;;   (define l (mcar lb-node))
;;   (define r (mcdr lb-node))
;;   (cond
;;     ;; A.1
;;     [(and (not (mpair? l))
;;         (empty? r)
;;         (< value (mcar la-node)))
;;      (mcons value (mcons value l))]
;;     ;; A.2
;;     [(and ;; (not (mpair? l))
;;         (empty? r)
;;         (<= (mcar la-node) value))
;;      (mcons (mcar la-node) (mcons l value))]
;;     ;; B.1
;;     [(and (not (mpair? l))
;;         (not (mpair? r))
;;         (< value (mcar la-node)))
;;      (mcons (mcar la-node) (mcons (mcons value (mcons value l)) r))]
;;     ;; B.2
;;     [(and (not (mpair? l))
;;         (not (mpair? r))
;;         (< value r))
;;      (mcons value (mcons (mcons l (mcons l value)) r))]
;;     ;; B.3
;;     [(and (not (mpair? l))
;;         (not (mpair? r))
;;         (< r value))
;;      (mcons r (mcons (mcons l (mcons l r)) value))]
;;     ;; C.1
;;     ;; [(and (mpair? l)
;;     ;;     (empty? r)
;;     ;;     (< value (mcar la-node)))
;;     ;;  (mcons (mcar la-node) (mcons (btree-add-value l value) r))]
;;     ;; C.2
;;     ;; [(and (mpair? l)
;;     ;;     (empty? r)
;;     ;;     (<= (mcar la-node) value))
;;     ;;  (mcons (mcar la-node) (mcons l value))]
;;     ;; D.1
;;     [(and (mpair? l)
;;         ;; (not (mpair? r))
;;         (< value (mcar la-node)))
;;      (mcons (mcar la-node) (mcons (btree-add-value l value) r))]
;;     ;; D.2
;;     [(and (mpair? l)
;;         (not (mpair? r))
;;         (<= (mcar la-node) value)
;;         (< value r))
;;      (mcons (mcar la-node) (mcons l (mcons value (mcons value r))))]
;;     ;; D.3
;;     [(and (mpair? l)
;;         (not (mpair? r))
;;         (<= (mcar la-node) value)
;;         (< r value))
;;      (mcons (mcar la-node) (mcons l (mcons r (mcons r value))))]
;;     ;; E.1
;;     ;; [(and (mpair? l)
;;     ;;     (mpair? r)
;;     ;;     (< value (mcar l)))
;;     ;;  (mcons (mcar la-node) (mcons (btree-add-value l value) r))]
;;     [(and (mpair? l)
;;         (mpair? r)
;;         (<= (mcar l) value)
;;         (< value (btree-max-value r)))
;;      (mcons (mcar la-node) (mcons l (btree-add-value r value)))]
;;     [(and (mpair? l)
;;         (mpair? r)
;;         (<= (btree-max-value r) value))
;;      (mcons (mcar la-node) (mcons l (mcons (btree-max-value r) (mcons r value))))]
;;     [else (raise-user-error "unknown case")]))

;; (define (btree-to-list node (result (list)))
;;   (cond [(empty? node) result]
;;         [(not (mpair? node)) (cons node result)]
;;         [else
;;          (define lb-node (mcdr node))
;;          (define l (mcar lb-node))
;;          (define r (mcdr lb-node))
;;          (cond [(not (mpair? l)) (cons l (btree-to-list r))]
;;                [else (append (btree-to-list l) (btree-to-list r))])]))

;; (define (btree-to-balanced-tree--values-to-nodes values (node-list '()))
;;   (cond [(empty? values) (reverse node-list)]
;;         [(and (car values)
;;             (not (empty? (cdr values))))
;;           (btree-to-balanced-tree--values-to-nodes
;;            (cddr values)
;;            (cons (mcons (car values) (mcons (car values) (cadr values))) node-list))
;;          ]
;;         [else
;;          (btree-to-balanced-tree--values-to-nodes
;;           (cdr values)
;;           (cons (btree-make-root (car values)) node-list))]))

;; (define (btree-to-balanced-tree--combine-nodes nodes (result-nodes '()))
;;   (cond [(and (not (empty? nodes))
;;             (empty? (cdr nodes))
;;             (empty? result-nodes))
;;          (car nodes)]
;;         [(empty? nodes)
;;          (btree-to-balanced-tree--combine-nodes (reverse result-nodes))]
;;         [(and (not (empty? nodes))
;;             (car nodes)
;;             (not (empty? (cdr nodes))))
;;          (btree-to-balanced-tree--combine-nodes
;;           (cddr nodes)
;;           (cons
;;            (mcons (btree-max-value (car nodes)) (mcons (car nodes) (cadr nodes)))
;;            result-nodes))
;;          ]
;;         [else
;;          (btree-to-balanced-tree--combine-nodes
;;           (cdr nodes)
;;           (cons
;;            (car nodes)
;;            result-nodes))]))

;; (define (btree-to-balanced-tree values)
;;   (btree-to-balanced-tree--combine-nodes
;;    (btree-to-balanced-tree--values-to-nodes values)))

;; (module+ test #| btree add value |#
;;   (check-equal?
;;    (btree-to-balanced-tree (list 1))
;;    (mcons 1 (mcons 1 '())))
;;   (check-true (btree-validate (btree-to-balanced-tree (list 1))))
;;   (check-equal? (btree-depth (btree-to-balanced-tree (list 1)))
;;                 1)

;;   (check-equal?
;;    (btree-to-balanced-tree (list 1 2))
;;    (mcons 1 (mcons 1 2)))
;;   (check-true (btree-validate (btree-to-balanced-tree (list 1 2))))
;;   (check-equal? (btree-depth (btree-to-balanced-tree (list 1 2)))
;;                 1)

;;   (check-equal?
;;    (btree-to-balanced-tree (list 1 2 3))
;;    (mcons 2 (mcons (mcons 1 (mcons 1 2)) (mcons 3 (mcons 3 '())))))
;;   (check-true (btree-validate (btree-to-balanced-tree (list 1 2 3))))
;;   (check-equal? (btree-depth (btree-to-balanced-tree (list 1 2 3)))
;;                 2)

;;   (check-equal?
;;    (btree-to-balanced-tree (list 1 2 3 4))
;;    (mcons 2 (mcons (mcons 1 (mcons 1 2)) (mcons 3 (mcons 3 4)))))
;;   (check-true (btree-validate (btree-to-balanced-tree (list 1 2 3 4))))
;;   (check-equal? (btree-depth (btree-to-balanced-tree (list 1 2 3 4)))
;;                 2)

;;   (check-equal?
;;    (btree-to-balanced-tree (list 1 2 3 4 5))
;;    (mcons 4 (mcons (mcons 2 (mcons (mcons 1 (mcons 1 2)) (mcons 3 (mcons 3 4)))) (mcons 5 (mcons 5 '())))))
;;   (check-true (btree-validate (btree-to-balanced-tree (list 1 2 3 4 5))))
;;   (check-equal? (btree-depth (btree-to-balanced-tree (list 1 2 3 4 5)))
;;                 3)

;;   (check-equal?
;;    (btree-to-balanced-tree (list 1 2 3 4 5 6))
;;    (mcons 4 (mcons (mcons 2 (mcons (mcons 1 (mcons 1 2)) (mcons 3 (mcons 3 4)))) (mcons 5 (mcons 5 6)))))
;;   (check-true (btree-validate (btree-to-balanced-tree (list 1 2 3 4 5 6))))
;;   (check-equal? (btree-depth (btree-to-balanced-tree (list 1 2 3 4 5 6)))
;;                 3)

;;   (check-equal?
;;    (btree-to-balanced-tree (list 1 2 3 4 5 6 7))
;;    (mcons 4 (mcons (mcons 2 (mcons (mcons 1 (mcons 1 2)) (mcons 3 (mcons 3 4)))) (mcons 6 (mcons (mcons 5 (mcons 5 6)) (mcons 7 (mcons 7 '())))))))
;;   (check-true (btree-validate (btree-to-balanced-tree (list 1 2 3 4 5 6 7))))
;;   (check-equal? (btree-depth (btree-to-balanced-tree (list 1 2 3 4 5 6 7)))
;;                 3)

;;   (check-equal?
;;    (btree-to-balanced-tree (list 1 2 3 4 5 6 7 8))
;;    (mcons 4 (mcons (mcons 2 (mcons (mcons 1 (mcons 1 2)) (mcons 3 (mcons 3 4)))) (mcons 6 (mcons (mcons 5 (mcons 5 6)) (mcons 7 (mcons 7 8)))))))
;;   (check-true (btree-validate (btree-to-balanced-tree (list 1 2 3 4 5 6 7 8))))
;;   (check-equal? (btree-depth (btree-to-balanced-tree (list 1 2 3 4 5 6 7 8)))
;;                 3)

;;   (check-true (btree-validate (mcons 5 (mcons 5 null))))
;;   (check-equal? (btree-add-value (mcons 5 (mcons 5 null)) 3)
;;                 (mcons 3 (mcons 3 5)))
;;   (check-true (btree-validate (mcons 3 (mcons 3 5))))

;;   (check-true (btree-validate (mcons 2 (mcons 2 null))))
;;   (check-equal? (btree-add-value (mcons 2 (mcons 2 null)) 3)
;;                 (mcons 2 (mcons 2 3)))
;;   (check-true (btree-validate (mcons 2 (mcons 2 3))))


;;   (check-true (btree-validate (mcons 4 (mcons 4 5))))
;;   (check-equal? (btree-add-value (mcons 4 (mcons 4 5)) 3)
;;                 (mcons 4 (mcons (mcons 3 (mcons 3 4)) 5)))
;;   (check-true (btree-validate (mcons 4 (mcons (mcons 3 (mcons 3 4)) 5))))


;;   (check-true (btree-validate (mcons 2 (mcons 2 5))))
;;   (check-equal? (btree-add-value (mcons 2 (mcons 2 5)) 3)
;;                 (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5)))
;;   (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5))))


;;   (check-true (btree-validate (mcons 1 (mcons 1 2))))
;;   (check-equal? (btree-add-value (mcons 1 (mcons 1 2)) 3)
;;                 (mcons 2 (mcons (mcons 1 (mcons 1 2)) 3)))
;;   (check-true (btree-validate (mcons 2 (mcons (mcons 1 (mcons 1 2)) 3))))


;;   (check-true (btree-validate (mcons 5 (mcons (mcons 4 (mcons 4 5)) null))))
;;   (check-equal? (btree-add-value (mcons 5 (mcons (mcons 4 (mcons 4 5)) null)) 3)
;;                 (mcons 5 (mcons (mcons 4 (mcons (mcons 3 (mcons 3 4)) 5)) null)))
;;   (check-true (btree-validate (mcons 5 (mcons (mcons 4 (mcons (mcons 3 (mcons 3 4)) 5)) null))))


;;   (check-true (btree-validate (mcons 5 (mcons (mcons 2 (mcons 2 5)) null))))
;;   (check-equal? (btree-add-value (mcons 5 (mcons (mcons 2 (mcons 2 5)) null)) 3)
;;                 (mcons 5 (mcons (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5)) null)))
;;   (check-true (btree-validate (mcons 5 (mcons (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5)) null))))


;;   (check-true (btree-validate (mcons 5 (mcons (mcons 2 (mcons 2 5)) null))))
;;   (check-equal? (btree-add-value (mcons 5 (mcons (mcons 2 (mcons 2 5)) null)) 6)
;;                 (mcons 5 (mcons (mcons 2 (mcons 2 5)) 6)))
;;   (check-true (btree-validate (mcons 5 (mcons (mcons 2 (mcons 2 5)) 6))))


;;   (check-true (btree-validate (mcons 5 (mcons (mcons 2 (mcons 2 5)) 6))))
;;   (check-equal? (btree-add-value (mcons 5 (mcons (mcons 2 (mcons 2 5)) 6)) 3)
;;                 (mcons 5 (mcons (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5)) 6)))
;;   (check-true (btree-validate (mcons 5 (mcons (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5)) 6))))


;;   (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) 6))))
;;   (check-equal? (btree-add-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) 6)) 5)
;;                 (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))))
;;   (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6))))))


;;   (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5))))
;;   (check-equal? (btree-add-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5)) 6)
;;                 (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))))
;;   (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6))))))


;;   (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6))))))
;;   (check-equal? (btree-add-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))) 1)
;;                 (mcons 3 (mcons (mcons 2 (mcons (mcons 1 (mcons 1 2)) 3)) (mcons 5 (mcons 5 6)))))
;;   (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons (mcons 1 (mcons 1 2)) 3)) (mcons 5 (mcons 5 6))))))


;;   (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6))))))
;;   (check-equal? (btree-add-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))) 4)
;;                 (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons (mcons 4 (mcons 4 5)) 6)))))
;;   (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons (mcons 4 (mcons 4 5)) 6))))))


;;   (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6))))))
;;   (check-equal? (btree-add-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))) 7)
;;                 (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 6 (mcons (mcons 5 (mcons 5 6)) 7)))))
;;   (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 6 (mcons (mcons 5 (mcons 5 6)) 7))))))


;;   (check-true (btree-validate (mcons 4 (mcons (mcons 1 (mcons (mcons 0 (mcons 0 1)) (mcons 2 (mcons 2 4)))) 5))))
;;   (check-equal? (btree-add-value (mcons 4 (mcons (mcons 1 (mcons (mcons 0 (mcons 0 1)) (mcons 2 (mcons 2 4)))) 5)) 3)
;;                 (mcons 4 (mcons (mcons 1 (mcons (mcons 0 (mcons 0 1)) (mcons 3 (mcons (mcons 2 (mcons 2 3)) 4)))) 5)))
;;   (check-true (btree-validate (mcons 4 (mcons (mcons 1 (mcons (mcons 0 (mcons 0 1)) (mcons 3 (mcons (mcons 2 (mcons 2 3)) 4)))) 5))))

;;   ;; ;; costly test, enable for major changes
;;   ;; (for-each
;;   ;;  (lambda (number-list)
;;   ;;    (define btree (btree-build-with number-list))
;;   ;;    (unless (btree-validate btree #t)
;;   ;;      (displayln (format "original number list: ~a" number-list))
;;   ;;      (displayln (format "failed to validate tree: ~a" btree))))
;;   ;;  (permutations (list 1 2 3 4 5 6 7 8 9 10)))

;;   (check-equal?
;;    (btree-to-list (btree-build-with (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
;;    (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))

;;   (check-equal?
;;    (btree-to-list (btree-build-with (list 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)))
;;    (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))

;;   (check-equal?
;;    (btree-to-list (btree-build-with (list 12 3 16 1 5 2 11 10 8 7 13 15 4 14 6 9)))
;;    (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))

;;   (check-equal? (btree-to-list (mcons 4 (mcons (mcons 1 (mcons (mcons 0 (mcons 0 1)) (mcons 3 (mcons (mcons 2 (mcons 2 3)) 4)))) 5)))
;;                 (list 0 1 2 3 4 5))

;;   (check-equal? (btree-depth (mcons 4 (mcons (mcons 1 (mcons (mcons 0 (mcons 0 1)) (mcons 3 (mcons (mcons 2 (mcons 2 3)) 4)))) 5)))
;;                 4)

;;   (check-equal? (btree-depth (mcons 1 (mcons (mcons 0 (mcons 0 1)) (mcons 3 (mcons (mcons 2 (mcons 2 3)) 4)))))
;;                 3)

;;   (check-equal? (btree-depth (mcons 3 (mcons (mcons 2 (mcons 2 3)) 4)))
;;                 2)

;;   (check-equal? (btree-depth (mcons 5 (mcons 5 null)))
;;                 1)

;;   (check-equal? (btree-depth (mcons 5 (mcons 5 6)))
;;                 1))


;; (define (btree-build-with values)
;;   (foldl (lambda (node val)
;;            (btree-add-value val node))
;;          (btree-make-root (car values))
;;          (cdr values)))

;; (define (btree-max-value la-node)
;;   (cond [(empty? la-node) null]
;;         [(not (mpair? la-node)) la-node]
;;         [else
;;          (define cv (mcar la-node))
;;          (define r (mcdr (mcdr la-node)))
;;          (cond [(empty? r) cv]
;;                [else (btree-max-value r)])]))

;; (module+ test #| btree max value |#
;;   (check-equal? (btree-max-value (mcons 5 (mcons 5 null)))
;;                 5)

;;   (check-equal? (btree-max-value (mcons 2 (mcons 2 null)))
;;                 2)

;;   (check-equal? (btree-max-value (mcons 4 (mcons 4 5)))
;;                 5)

;;   (check-equal? (btree-max-value (mcons 2 (mcons 2 5)))
;;                 5)

;;   (check-equal? (btree-max-value (mcons 1 (mcons 1 2)))
;;                 2)

;;   (check-equal? (btree-max-value (mcons 5 (mcons (mcons 4 (mcons 4 5)) null)))
;;                 5)

;;   (check-equal? (btree-max-value (mcons 5 (mcons (mcons 2 (mcons 2 5)) null)))
;;                 5)

;;   (check-equal? (btree-max-value (mcons 5 (mcons (mcons 2 (mcons 2 5)) null)))
;;                 5)

;;   (check-equal? (btree-max-value (mcons 5 (mcons (mcons 2 (mcons 2 5)) 6)))
;;                 6)

;;   (check-equal? (btree-max-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) 6)))
;;                 6)

;;   (check-equal? (btree-max-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5)))
;;                 5)

;;   (check-equal? (btree-max-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))))
;;                 6)

;;   (check-equal? (btree-max-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))))
;;                 6)

;;   (check-equal? (btree-max-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))))
;;                 6))

;; ;; INFO: trails are interesting for implementing a persistent version of this tree
;; ;; return the trail to the given value,
;; ;; car of the trail holds the searched value (either in car or in cdr position)
;; (define (btree-trail-to la-node value (trail '()))
;;   (cond [(<= value (mcar la-node)) ;; search left sub tree
;;          (btree-left-trail-to (mcdr la-node) value (cons 'cdr (cons la-node trail)))]
;;         [else
;;          (btree-right-trail-to (mcdr la-node) value (cons 'cdr (cons la-node trail)))]))

;; (define (btree-left-trail-to lb-node value trail)
;;   (define new-trail (cons lb-node trail))
;;   (cond [(and (integer? (mcar lb-node))
;;             (= (mcar lb-node) value))
;;          (cons 'car new-trail)]
;;         [(and (integer? (mcdr lb-node))
;;             (= (mcdr lb-node) value))
;;          (cons 'cdr new-trail)]
;;         [(mpair? (mcar lb-node))
;;          (btree-trail-to (mcar lb-node) value (cons 'car new-trail))]
;;         [else '()]))

;; (define (btree-right-trail-to lb-node value trail)
;;   (define new-trail (cons lb-node trail))
;;   (cond [(and (integer? (mcdr lb-node))
;;             (= (mcdr lb-node) value))
;;          (cons 'cdr new-trail)]
;;         [(mpair? (mcdr lb-node)) (btree-trail-to (mcdr lb-node) value (cons 'cdr new-trail))]
;;         [else '()]))

;; (module+ test
;;   (require rackunit)

;;   (define tree0 (mcons 1 (mcons 1 '())))

;;   (check-equal? (btree-trail-to tree0 1)
;;                 (list 'car (mcdr tree0) ;; cdr tree0  is lb-node
;;                       'cdr tree0))     ;; tree0      is la-node

;;   (check-equal? (btree-trail-to tree0 2)
;;                 '())

;;   (define tree12 (mcons 2 (mcons 1 2)))

;;   (check-equal? (btree-trail-to tree12 1)
;;                 (list 'car (mcdr tree12)
;;                       'cdr tree12))

;;   (check-equal? (btree-trail-to tree12 2)
;;                 (list 'cdr (mcdr tree12)
;;                       'cdr tree12))

;;   (check-equal? (btree-trail-to tree12 3)
;;                 '())

;;   (define tree123 (mcons 2 (mcons (mcons 2 (mcons 1 2)) 3)))

;;   (check-equal? (btree-trail-to tree123 1)
;;                 (list 'car (mcdr (mcar (mcdr tree123)))
;;                       'cdr (mcar (mcdr tree123))
;;                       'car (mcdr tree123)
;;                       'cdr tree123))

;;   (check-equal? (btree-trail-to tree123 2)
;;                 (list 'cdr (mcdr (mcar (mcdr tree123)))
;;                       'cdr (mcar (mcdr tree123))
;;                       'car (mcdr tree123)
;;                       'cdr tree123))

;;   (check-equal? (btree-trail-to tree123 3)
;;                 (list 'cdr (mcdr tree123)
;;                       'cdr tree123))

;;   (check-equal? (btree-trail-to tree123 4)
;;                 '())

;;   (define tree12345 (mcons 2 (mcons (mcons 2 (mcons 1 2)) (mcons 4 (mcons (mcons 4 (mcons 3 4)) 5)))))

;;   (check-equal? (btree-trail-to tree12345 1)
;;                 (list 'car (mcdr (mcar (mcdr tree12345)))
;;                       'cdr (mcar (mcdr tree12345))
;;                       'car (mcdr tree12345)
;;                       'cdr tree12345))

;;   (check-equal? (btree-trail-to tree12345 2)
;;                 (list 'cdr (mcdr (mcar (mcdr tree12345)))
;;                       'cdr (mcar (mcdr tree12345))
;;                       'car (mcdr tree12345)
;;                       'cdr tree12345))

;;   (check-equal? (btree-trail-to tree12345 3)
;;                 (list 'car (mcdr (mcar (mcdr (mcdr (mcdr tree12345)))))
;;                       'cdr (mcar (mcdr (mcdr (mcdr tree12345))))
;;                       'car (mcdr (mcdr (mcdr tree12345)))
;;                       'cdr (mcdr (mcdr tree12345))
;;                       'cdr (mcdr tree12345)
;;                       'cdr tree12345))

;;   (check-equal? (btree-trail-to tree12345 4)
;;                 (list 'cdr (mcdr (mcar (mcdr (mcdr (mcdr tree12345)))))
;;                       'cdr (mcar (mcdr (mcdr (mcdr tree12345))))
;;                       'car (mcdr (mcdr (mcdr tree12345)))
;;                       'cdr (mcdr (mcdr tree12345))
;;                       'cdr (mcdr tree12345)
;;                       'cdr tree12345))

;;   (check-equal? (btree-trail-to tree12345 5)
;;                 (list 'cdr (mcdr (mcdr (mcdr tree12345)))
;;                       'cdr (mcdr (mcdr tree12345))
;;                       'cdr (mcdr tree12345)
;;                       'cdr tree12345))

;;   (check-equal? (btree-trail-to tree12345 6)
;;                 '())
;;   )

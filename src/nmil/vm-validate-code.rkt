#lang racket/base


#|

(balanced tree for editor with 200 lines of 30 bytes each)
=> 200 nodes with values (lb nodes) * 4  + 255 + 127  (la nodes) * 4 (size) + 200 * 30
   (+ (* 6 (+ 200 255 127))
      (* 200 30))
   = 9492 bytes (* 1.6 6000)

la-node: car: value for comparison (car of pointed to lb-node contains values <= this value)
          cdr: pointer to lb-node 

 lb-node: car: ptr to la-node | value
          cdr: ptr to la-node | value | NIL


 la-nodes are modified, if max needs to change

 la-nodes:     

  (A)          (B)           (C)           (D)         (E) 
      o            o             o             o           o          
     / \          / \           / \           / \         / \     
    V1  NIL      V1  V2      la-N  NIL     la-N  V    la-N1 la-N2

(A.1) v < V1 :  o   (update lb-node above to hold comparison value v) 
               / \
              v   V1

(A.2) v > V1 :  o   
               / \
             V1   v

(B.1) v < V1:      o      (update lb-nodes above to hold comparison value v)
                  / \
             c=v o   V2
                 |
                 o
                / \
               v   V1

(B.2) V1 < v < V2:       o    
                        / \
                  c=V1 o   V2
                       |
                       o
                      / \
                    V1   v

(B.3) V2 < v:        o   (update lb-nodes above to hold comparison value V2  
                    / \
              c=V2 o   v 
                   |
                   o
                  / \
                V1   V2

(C.1) v < V(la-N): go into la-N and recurse

(C.2) v > V(la-N):       o
                        / \
                     la-N  v

(D.1) v < V(la-N): go into la-N and recurse

(D.2) V(la-N) < v < V:       o    
                            / \
                         la-N  o c=v
                               |      
                               o
                              / \
                             v   V

(D.3) V < v:         o       
                    / \      
                 la-N  o c=V
                       |     
                       o
                      / \
                     V   v

(E.1) v < V(la-N1): go into la-N1 and recurse

(E.2) V(la-N1) < v < V(la-N2): go into la-N2 and recurse

(E.3) V(la-N2) < v:        o       
                          / \
                      la-N1  o c=V(la-N2)
                             |     
                             o
                            / \
                        la-N2  v

 |#

(define (btree-make-root value)
  (mcons value (mcons value '())))

(module+ test #| btree add value|#
  (check-equal? (btree-make-root 1)
                (mcons 1 (mcons 1 '()))))

;; INFO: trails are interesting for implementing a persistent version of this tree
;; return the trail to the given value,
;; car of the trail holds the searched value (either in car or in cdr position)
(define (btree-trail-to la-node value (trail '()))
  (cond [(<= value (mcar la-node)) ;; search left sub tree
         (btree-left-trail-to (mcdr la-node) value (cons 'cdr (cons la-node trail)))]
        [else
         (btree-right-trail-to (mcdr la-node) value (cons 'cdr (cons la-node trail)))]))

(define (btree-left-trail-to lb-node value trail)
  (define new-trail (cons lb-node trail))
  (cond [(and (integer? (mcar lb-node))
            (= (mcar lb-node) value))
         (cons 'car new-trail)]
        [(and (integer? (mcdr lb-node))
            (= (mcdr lb-node) value))
         (cons 'cdr new-trail)]
        [(mpair? (mcar lb-node))
         (btree-trail-to (mcar lb-node) value (cons 'car new-trail))]
        [else '()]))

(define (btree-right-trail-to lb-node value trail)
  (define new-trail (cons lb-node trail))
  (cond [(and (integer? (mcdr lb-node))
            (= (mcdr lb-node) value))
         (cons 'cdr new-trail)]
        [(mpair? (mcdr lb-node)) (btree-trail-to (mcdr lb-node) value (cons 'cdr new-trail))]
        [else '()]))

(module+ test
  (require rackunit)

  (define tree0 (mcons 1 (mcons 1 '())))

  (check-equal? (btree-trail-to tree0 1)
                (list 'car (mcdr tree0) ;; cdr tree0  is lb-node
                      'cdr tree0))     ;; tree0      is la-node 

  (check-equal? (btree-trail-to tree0 2)
                '())

  (define tree12 (mcons 2 (mcons 1 2)))

  (check-equal? (btree-trail-to tree12 1)
                (list 'car (mcdr tree12)
                      'cdr tree12))

  (check-equal? (btree-trail-to tree12 2)
                (list 'cdr (mcdr tree12)
                      'cdr tree12))

  (check-equal? (btree-trail-to tree12 3)
                '())

  (define tree123 (mcons 2 (mcons (mcons 2 (mcons 1 2)) 3)))

  (check-equal? (btree-trail-to tree123 1)
                (list 'car (mcdr (mcar (mcdr tree123)))
                      'cdr (mcar (mcdr tree123))
                      'car (mcdr tree123)
                      'cdr tree123))

  (check-equal? (btree-trail-to tree123 2)
                (list 'cdr (mcdr (mcar (mcdr tree123)))
                      'cdr (mcar (mcdr tree123))
                      'car (mcdr tree123)
                      'cdr tree123))

  (check-equal? (btree-trail-to tree123 3)
                (list 'cdr (mcdr tree123)
                      'cdr tree123))

  (check-equal? (btree-trail-to tree123 4)
                '())

  (define tree12345 (mcons 2 (mcons (mcons 2 (mcons 1 2)) (mcons 4 (mcons (mcons 4 (mcons 3 4)) 5)))))

  (check-equal? (btree-trail-to tree12345 1)
                (list 'car (mcdr (mcar (mcdr tree12345)))
                      'cdr (mcar (mcdr tree12345))
                      'car (mcdr tree12345)
                      'cdr tree12345))

  (check-equal? (btree-trail-to tree12345 2)
                (list 'cdr (mcdr (mcar (mcdr tree12345)))
                      'cdr (mcar (mcdr tree12345))
                      'car (mcdr tree12345)
                      'cdr tree12345))

  (check-equal? (btree-trail-to tree12345 3)
                (list 'car (mcdr (mcar (mcdr (mcdr (mcdr tree12345)))))
                      'cdr (mcar (mcdr (mcdr (mcdr tree12345))))
                      'car (mcdr (mcdr (mcdr tree12345)))
                      'cdr (mcdr (mcdr tree12345))
                      'cdr (mcdr tree12345)
                      'cdr tree12345))

  (check-equal? (btree-trail-to tree12345 4)
                (list 'cdr (mcdr (mcar (mcdr (mcdr (mcdr tree12345)))))
                      'cdr (mcar (mcdr (mcdr (mcdr tree12345))))
                      'car (mcdr (mcdr (mcdr tree12345)))
                      'cdr (mcdr (mcdr tree12345))
                      'cdr (mcdr tree12345)
                      'cdr tree12345))

  (check-equal? (btree-trail-to tree12345 5)
                (list 'cdr (mcdr (mcdr (mcdr tree12345)))
                      'cdr (mcdr (mcdr tree12345))
                      'cdr (mcdr tree12345)
                      'cdr tree12345))

  (check-equal? (btree-trail-to tree12345 6)
                '())
  )

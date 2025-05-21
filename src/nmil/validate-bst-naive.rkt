#lang racket/base

(require (only-in racket/list empty? permutations))

#|

  Data structures for a text editor:

  gap buffer:
  - keep text two separate portions of memory, copying between them.
  - cursor is at the end of the left buffer, inserting = adding to first buffer
  - second buffer is the text behind the cursor
  - moving cursor = copying first buffer <-> second buffer (can be delayed until insertion is done)

  - gap buffer implementation using a list of pages before cursor, list of pages after cursor
    (head of gap_pre_buffers is page with cursor) lines + text before cursor
    (head of gap_post_buffers is page right after cursor) <- text + lines after cursor

  - IDEA: no gap buffer, use screen data only, storing lines into memory when scrolling out of visibility?
          => there cannot be invisible data (scrolled out left and right)? => need for line break
             alternative: left|right invisible chars need to be saved/rendered efficiently <- is this really possible / not overly complex?
          ADVANTAGE: no dual changes (memory model + render)
          PROBLEM: undo/redo?

  - IDEA: gap buffer modification
          put current edit line (when actually editing, not if moving)
              into one extra page (text before cursor at start of page, text behind cursor, end of page),
              text is plain, without any encoding (like leading spaces or length data)
              cursor = ptr in page, typing = inserting text at cursor position
                                    erasing = deleting text before cursor position
              [<text before cursor>| ........  <text behind cursor>]
                                   /\        /\
                  pointer to cursor            pointer to text behind cursor
          once editing a line is done, it is copied into the regular text buffers
          format: [len][#leading-spaces]characters[len][0],
                  with len = total length of this line (including leading spaces) [stored two times, to do fast scanning|moving
                       #leading-spaces = number of spaces before first char starts
                       characters = actual text
                       0 = zero, marking end of line (for scanning)
                  lines are always kept completely on one page!

  - IDEA: keep current line# up to date <- needs to be updated with insertions / deletions
  - IDEA: keep line anchors (limited number of pointers to strings with their line number) <- need to be updated once lines are inserted (or invalidated)
  - IDEA: keep line anchors for all visible lines

    fast implementation:
    - render-line 'a starting from column 'b [to column 'c] at visual line 'd (two variants: 1. for currently edited line, 2. regular encoded text line)
    - scroll up/down 'n lines
    - scroll left/right 'n columns

    - insert/delete char
    - insert/delete line
    - insert/delete block
    - undo/redo (keep modifications + undo data)

  - multiple cursors (are not supported for fast operation)

  - IDEA: use mil data structures to implement the above
          => native array for currently edited line <- indexed access + copy left<->right from cursor
             tree of strings for current buffer text
             path to current line (for moving cursor line-wise through the text) = search previous/next leaf based on current node-path
             (e.g. line of a buffer = cell-pair (car = int, leading spaces + strlen, cdr = pointer to native string/array))
             buffer = n-ary tree of lines <- binary tree has advantage of using car/cdr pair only <- start out with this
             (rebalance tree once in a while)
             undo/redo = persistent tree data structure?, undo/redo organized in n-ary tree itself.
             <- when to discard? (running low on memory, discard most distant (tree distance) nodes)
                siblings are ordered last touched first

  => data structures to implement:
     b-tree (persistent) <- keep somewhat balanced? balancing tree will enlarge undo/redo buffer usage!
     - eval b-tree compared to n-ary tree for source code (line) meta data (or multiple lines on one leaf?)
     - avl tree ? (https://en.wikipedia.org/wiki/AVL_tree)
     - read-black tree maybe, to keep it somewhat balanced ?
       how to keep the color on a car/cdr node? (could be kept in the reference count byte e.g. bit 7)
       tree may contain pointer to other tree nodes (cons pairs) or native arrays (leading spaces, total len, character data). no payload bit is free.
       red,black nodes may be used from different page sets. => there is a function to determine whether a cons pair comes from a "red" (or a "black") page => changing color = moving cons cell to other page?
    n-ary tree (non-persistent)
|#

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

(A.2) V1 < v:   o   
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
              c=V1 o   v 
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

(define (btree-depth node)
  (cond [(not (mpair? node)) 0]
        [else
         (define lb-node (mcdr node))
         (define l (mcar lb-node))
         (define r (mcdr lb-node))
         (add1 (max (btree-depth l) (btree-depth r)))]))

(define (btree-validate node (print-error #f))
  (define node-must-be-pair (mpair? node))
  (when (and print-error (not node-must-be-pair))
    displayln (format "failed rule: node must be pair, ~a" node))

  (define lb-node (mcdr node))
  (define l (mcar lb-node))
  (define r (mcdr lb-node))

  (define comparator-must-be-int (integer? (mcar node)))
  (when (and print-error (not comparator-must-be-int))
    (displayln (format "failed rule: comparator must be an integer, ~a" node)))

  (define left-is-integer-or-pair
    (or (integer? l)
      (and (mpair? l)
         (btree-validate l print-error))))
  (when (and print-error (not left-is-integer-or-pair))
    (displayln (format "failed rule: left is integer or pair, ~a" node)))

  (define right-is-integer-or-pair-or-empty
    (or (integer? l)
      (empty? r)
      (and (mpair? l)
         (btree-validate l print-error))))
  (when (and print-error (not right-is-integer-or-pair-or-empty))
    (displayln (format "failed rule: right is integer or pair or empty, ~a" node)))

  (define max-of-left-matches-c-value
    (= (mcar node)
       (btree-max-value l)))
  (when (and print-error (not max-of-left-matches-c-value))
    (displayln (format "failed rule: max of left matches c-value ~a" node)))

  (and
   node-must-be-pair
   comparator-must-be-int
   left-is-integer-or-pair
   right-is-integer-or-pair-or-empty
   max-of-left-matches-c-value))


(define (btree-add-value la-node value)
  (define lb-node (mcdr la-node))
  (define l (mcar lb-node))
  (define r (mcdr lb-node))
  (cond
    ;; A.1
    [(and (not (mpair? l))
        (empty? r)
        (< value (mcar la-node)))
     (mcons value (mcons value l))]
    ;; A.2
    [(and ;; (not (mpair? l))
        (empty? r)
        (<= (mcar la-node) value))
     (mcons (mcar la-node) (mcons l value))]
    ;; B.1
    [(and (not (mpair? l))
        (not (mpair? r))
        (< value (mcar la-node)))
     (mcons (mcar la-node) (mcons (mcons value (mcons value l)) r))]
    ;; B.2
    [(and (not (mpair? l))
        (not (mpair? r))
        (< value r))
     (mcons value (mcons (mcons l (mcons l value)) r))]
    ;; B.3
    [(and (not (mpair? l))
        (not (mpair? r))
        (< r value))
     (mcons r (mcons (mcons l (mcons l r)) value))]
    ;; C.1
    ;; [(and (mpair? l)
    ;;     (empty? r)
    ;;     (< value (mcar la-node)))
    ;;  (mcons (mcar la-node) (mcons (btree-add-value l value) r))]
    ;; C.2
    ;; [(and (mpair? l)
    ;;     (empty? r)
    ;;     (<= (mcar la-node) value))
    ;;  (mcons (mcar la-node) (mcons l value))]
    ;; D.1
    [(and (mpair? l)
        ;; (not (mpair? r))
        (< value (mcar la-node)))
     (mcons (mcar la-node) (mcons (btree-add-value l value) r))]
    ;; D.2
    [(and (mpair? l)
        (not (mpair? r))
        (<= (mcar la-node) value)
        (< value r))
     (mcons (mcar la-node) (mcons l (mcons value (mcons value r))))]
    ;; D.3
    [(and (mpair? l)
        (not (mpair? r))
        (<= (mcar la-node) value)
        (< r value))
     (mcons (mcar la-node) (mcons l (mcons r (mcons r value))))]
    ;; E.1
    ;; [(and (mpair? l)
    ;;     (mpair? r)
    ;;     (< value (mcar l)))
    ;;  (mcons (mcar la-node) (mcons (btree-add-value l value) r))]
    [(and (mpair? l)
        (mpair? r)
        (<= (mcar l) value)
        (< value (btree-max-value r)))
     (mcons (mcar la-node) (mcons l (btree-add-value r value)))]
    [(and (mpair? l)
        (mpair? r)
        (<= (btree-max-value r) value))
     (mcons (mcar la-node) (mcons l (mcons (btree-max-value r) (mcons r value))))]
    [else (raise-user-error "unknown case")]))

(define (btree-to-list node (result (list)))
  (cond [(empty? node) result]
        [(not (mpair? node)) (cons node result)]
        [else
         (define lb-node (mcdr node))
         (define l (mcar lb-node))
         (define r (mcdr lb-node))
         (cond [(not (mpair? l)) (cons l (btree-to-list r))]
               [else (append (btree-to-list l) (btree-to-list r))])]))

(define (btree-to-balanced-tree--values-to-nodes values (node-list '()))
  (cond [(empty? values) (reverse node-list)]
        [(and (car values)
            (not (empty? (cdr values))))
          (btree-to-balanced-tree--values-to-nodes
           (cddr values)
           (cons (mcons (car values) (mcons (car values) (cadr values))) node-list))
         ]
        [else 
         (btree-to-balanced-tree--values-to-nodes
          (cdr values)
          (cons (btree-make-root (car values)) node-list))]))

(define (btree-to-balanced-tree--combine-nodes nodes (result-nodes '()))
  (cond [(and (not (empty? nodes))
            (empty? (cdr nodes))
            (empty? result-nodes))
         (car nodes)]
        [(empty? nodes)
         (btree-to-balanced-tree--combine-nodes (reverse result-nodes))]
        [(and (not (empty? nodes))
            (car nodes)
            (not (empty? (cdr nodes))))
         (btree-to-balanced-tree--combine-nodes
          (cddr nodes)
          (cons
           (mcons (btree-max-value (car nodes)) (mcons (car nodes) (cadr nodes)))
           result-nodes))
         ]
        [else
         (btree-to-balanced-tree--combine-nodes
          (cdr nodes)
          (cons
           (car nodes)
           result-nodes))]))

(define (btree-to-balanced-tree values)
  (btree-to-balanced-tree--combine-nodes
   (btree-to-balanced-tree--values-to-nodes values)))

(module+ test #| btree add value |#
  (check-equal?
   (btree-to-balanced-tree (list 1))
   (mcons 1 (mcons 1 '())))
  (check-true (btree-validate (btree-to-balanced-tree (list 1))))
  (check-equal? (btree-depth (btree-to-balanced-tree (list 1)))
                1)

  (check-equal?
   (btree-to-balanced-tree (list 1 2))
   (mcons 1 (mcons 1 2)))
  (check-true (btree-validate (btree-to-balanced-tree (list 1 2))))
  (check-equal? (btree-depth (btree-to-balanced-tree (list 1 2)))
                1)

  (check-equal?
   (btree-to-balanced-tree (list 1 2 3))
   (mcons 2 (mcons (mcons 1 (mcons 1 2)) (mcons 3 (mcons 3 '())))))
  (check-true (btree-validate (btree-to-balanced-tree (list 1 2 3))))
  (check-equal? (btree-depth (btree-to-balanced-tree (list 1 2 3)))
                2)

  (check-equal?
   (btree-to-balanced-tree (list 1 2 3 4))
   (mcons 2 (mcons (mcons 1 (mcons 1 2)) (mcons 3 (mcons 3 4)))))
  (check-true (btree-validate (btree-to-balanced-tree (list 1 2 3 4))))
  (check-equal? (btree-depth (btree-to-balanced-tree (list 1 2 3 4)))
                2)

  (check-equal?
   (btree-to-balanced-tree (list 1 2 3 4 5))
   (mcons 4 (mcons (mcons 2 (mcons (mcons 1 (mcons 1 2)) (mcons 3 (mcons 3 4)))) (mcons 5 (mcons 5 '())))))
  (check-true (btree-validate (btree-to-balanced-tree (list 1 2 3 4 5))))
  (check-equal? (btree-depth (btree-to-balanced-tree (list 1 2 3 4 5)))
                3)

  (check-equal?
   (btree-to-balanced-tree (list 1 2 3 4 5 6))
   (mcons 4 (mcons (mcons 2 (mcons (mcons 1 (mcons 1 2)) (mcons 3 (mcons 3 4)))) (mcons 5 (mcons 5 6)))))
  (check-true (btree-validate (btree-to-balanced-tree (list 1 2 3 4 5 6))))
  (check-equal? (btree-depth (btree-to-balanced-tree (list 1 2 3 4 5 6)))
                3)

  (check-equal?
   (btree-to-balanced-tree (list 1 2 3 4 5 6 7))
   (mcons 4 (mcons (mcons 2 (mcons (mcons 1 (mcons 1 2)) (mcons 3 (mcons 3 4)))) (mcons 6 (mcons (mcons 5 (mcons 5 6)) (mcons 7 (mcons 7 '())))))))
  (check-true (btree-validate (btree-to-balanced-tree (list 1 2 3 4 5 6 7))))
  (check-equal? (btree-depth (btree-to-balanced-tree (list 1 2 3 4 5 6 7)))
                3)

  (check-equal?
   (btree-to-balanced-tree (list 1 2 3 4 5 6 7 8))
   (mcons 4 (mcons (mcons 2 (mcons (mcons 1 (mcons 1 2)) (mcons 3 (mcons 3 4)))) (mcons 6 (mcons (mcons 5 (mcons 5 6)) (mcons 7 (mcons 7 8)))))))
  (check-true (btree-validate (btree-to-balanced-tree (list 1 2 3 4 5 6 7 8))))
  (check-equal? (btree-depth (btree-to-balanced-tree (list 1 2 3 4 5 6 7 8)))
                3)

  (check-true (btree-validate (mcons 5 (mcons 5 null))))
  (check-equal? (btree-add-value (mcons 5 (mcons 5 null)) 3)
                (mcons 3 (mcons 3 5)))
  (check-true (btree-validate (mcons 3 (mcons 3 5))))

  (check-true (btree-validate (mcons 2 (mcons 2 null))))
  (check-equal? (btree-add-value (mcons 2 (mcons 2 null)) 3)
                (mcons 2 (mcons 2 3)))
  (check-true (btree-validate (mcons 2 (mcons 2 3))))


  (check-true (btree-validate (mcons 4 (mcons 4 5))))
  (check-equal? (btree-add-value (mcons 4 (mcons 4 5)) 3)
                (mcons 4 (mcons (mcons 3 (mcons 3 4)) 5)))
  (check-true (btree-validate (mcons 4 (mcons (mcons 3 (mcons 3 4)) 5))))


  (check-true (btree-validate (mcons 2 (mcons 2 5))))
  (check-equal? (btree-add-value (mcons 2 (mcons 2 5)) 3)
                (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5)))
  (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5))))


  (check-true (btree-validate (mcons 1 (mcons 1 2))))
  (check-equal? (btree-add-value (mcons 1 (mcons 1 2)) 3)
                (mcons 2 (mcons (mcons 1 (mcons 1 2)) 3)))
  (check-true (btree-validate (mcons 2 (mcons (mcons 1 (mcons 1 2)) 3))))


  (check-true (btree-validate (mcons 5 (mcons (mcons 4 (mcons 4 5)) null))))
  (check-equal? (btree-add-value (mcons 5 (mcons (mcons 4 (mcons 4 5)) null)) 3)
                (mcons 5 (mcons (mcons 4 (mcons (mcons 3 (mcons 3 4)) 5)) null)))
  (check-true (btree-validate (mcons 5 (mcons (mcons 4 (mcons (mcons 3 (mcons 3 4)) 5)) null))))


  (check-true (btree-validate (mcons 5 (mcons (mcons 2 (mcons 2 5)) null))))
  (check-equal? (btree-add-value (mcons 5 (mcons (mcons 2 (mcons 2 5)) null)) 3)
                (mcons 5 (mcons (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5)) null)))
  (check-true (btree-validate (mcons 5 (mcons (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5)) null))))


  (check-true (btree-validate (mcons 5 (mcons (mcons 2 (mcons 2 5)) null))))
  (check-equal? (btree-add-value (mcons 5 (mcons (mcons 2 (mcons 2 5)) null)) 6)
                (mcons 5 (mcons (mcons 2 (mcons 2 5)) 6)))
  (check-true (btree-validate (mcons 5 (mcons (mcons 2 (mcons 2 5)) 6))))


  (check-true (btree-validate (mcons 5 (mcons (mcons 2 (mcons 2 5)) 6))))
  (check-equal? (btree-add-value (mcons 5 (mcons (mcons 2 (mcons 2 5)) 6)) 3)
                (mcons 5 (mcons (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5)) 6)))
  (check-true (btree-validate (mcons 5 (mcons (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5)) 6))))


  (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) 6))))
  (check-equal? (btree-add-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) 6)) 5)
                (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))))
  (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6))))))


  (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5))))
  (check-equal? (btree-add-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5)) 6)
                (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))))
  (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6))))))


  (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6))))))
  (check-equal? (btree-add-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))) 1)
                (mcons 3 (mcons (mcons 2 (mcons (mcons 1 (mcons 1 2)) 3)) (mcons 5 (mcons 5 6)))))
  (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons (mcons 1 (mcons 1 2)) 3)) (mcons 5 (mcons 5 6))))))


  (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6))))))
  (check-equal? (btree-add-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))) 4)
                (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons (mcons 4 (mcons 4 5)) 6)))))
  (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons (mcons 4 (mcons 4 5)) 6))))))


  (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6))))))
  (check-equal? (btree-add-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))) 7)
                (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 6 (mcons (mcons 5 (mcons 5 6)) 7)))))
  (check-true (btree-validate (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 6 (mcons (mcons 5 (mcons 5 6)) 7))))))


  (check-true (btree-validate (mcons 4 (mcons (mcons 1 (mcons (mcons 0 (mcons 0 1)) (mcons 2 (mcons 2 4)))) 5))))
  (check-equal? (btree-add-value (mcons 4 (mcons (mcons 1 (mcons (mcons 0 (mcons 0 1)) (mcons 2 (mcons 2 4)))) 5)) 3)
                (mcons 4 (mcons (mcons 1 (mcons (mcons 0 (mcons 0 1)) (mcons 3 (mcons (mcons 2 (mcons 2 3)) 4)))) 5)))
  (check-true (btree-validate (mcons 4 (mcons (mcons 1 (mcons (mcons 0 (mcons 0 1)) (mcons 3 (mcons (mcons 2 (mcons 2 3)) 4)))) 5))))

  ;; ;; costly test, enable for major changes
  ;; (for-each
  ;;  (lambda (number-list)
  ;;    (define btree (btree-build-with number-list))
  ;;    (unless (btree-validate btree #t)
  ;;      (displayln (format "original number list: ~a" number-list))
  ;;      (displayln (format "failed to validate tree: ~a" btree))))
  ;;  (permutations (list 1 2 3 4 5 6 7 8 9 10)))

  (check-equal?
   (btree-to-list (btree-build-with (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16)))
   (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))

  (check-equal?
   (btree-to-list (btree-build-with (list 16 15 14 13 12 11 10 9 8 7 6 5 4 3 2 1)))
   (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))

  (check-equal?
   (btree-to-list (btree-build-with (list 12 3 16 1 5 2 11 10 8 7 13 15 4 14 6 9)))
   (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16))

  (check-equal? (btree-to-list (mcons 4 (mcons (mcons 1 (mcons (mcons 0 (mcons 0 1)) (mcons 3 (mcons (mcons 2 (mcons 2 3)) 4)))) 5)))
                (list 0 1 2 3 4 5))

  (check-equal? (btree-depth (mcons 4 (mcons (mcons 1 (mcons (mcons 0 (mcons 0 1)) (mcons 3 (mcons (mcons 2 (mcons 2 3)) 4)))) 5)))
                4)

  (check-equal? (btree-depth (mcons 1 (mcons (mcons 0 (mcons 0 1)) (mcons 3 (mcons (mcons 2 (mcons 2 3)) 4)))))
                3)

  (check-equal? (btree-depth (mcons 3 (mcons (mcons 2 (mcons 2 3)) 4)))
                2)

  (check-equal? (btree-depth (mcons 5 (mcons 5 null)))
                1)

  (check-equal? (btree-depth (mcons 5 (mcons 5 6)))
                1))


(define (btree-build-with values)
  (foldl (lambda (node val)
           (btree-add-value val node))
         (btree-make-root (car values))
         (cdr values)))

(define (btree-max-value la-node)
  (cond [(empty? la-node) null]
        [(not (mpair? la-node)) la-node]
        [else
         (define cv (mcar la-node))
         (define r (mcdr (mcdr la-node)))
         (cond [(empty? r) cv]
               [else (btree-max-value r)])]))

(module+ test #| btree max value |#
  (check-equal? (btree-max-value (mcons 5 (mcons 5 null)))
                5)

  (check-equal? (btree-max-value (mcons 2 (mcons 2 null)))
                2)

  (check-equal? (btree-max-value (mcons 4 (mcons 4 5)))
                5)

  (check-equal? (btree-max-value (mcons 2 (mcons 2 5)))
                5)

  (check-equal? (btree-max-value (mcons 1 (mcons 1 2)))
                2)

  (check-equal? (btree-max-value (mcons 5 (mcons (mcons 4 (mcons 4 5)) null)))
                5)

  (check-equal? (btree-max-value (mcons 5 (mcons (mcons 2 (mcons 2 5)) null)))
                5)

  (check-equal? (btree-max-value (mcons 5 (mcons (mcons 2 (mcons 2 5)) null)))
                5)

  (check-equal? (btree-max-value (mcons 5 (mcons (mcons 2 (mcons 2 5)) 6)))
                6)

  (check-equal? (btree-max-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) 6)))
                6)

  (check-equal? (btree-max-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) 5)))
                5)

  (check-equal? (btree-max-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))))
                6)

  (check-equal? (btree-max-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))))
                6)

  (check-equal? (btree-max-value (mcons 3 (mcons (mcons 2 (mcons 2 3)) (mcons 5 (mcons 5 6)))))
                6))

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

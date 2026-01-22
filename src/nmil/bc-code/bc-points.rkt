#lang racket/base

#|
   this file is implementing some functions around a
   (defs point
    (x int)
    (y int)
    (color string))


   xdist between two points:   x2-x1
   ydist between two points:   y2-y1
   equal (not comparing color)

|#

(require (only-in racket/list flatten)
         "../../6510.rkt"
         (only-in "../../tools/6510-interpreter.rkt" memory-list)
         (only-in "../vm-bc-opcode-definitions.rkt" bc)
         (only-in "../vm-interpreter.rkt" vm-interpreter))

(module+ test #|  |#
  (require (only-in "../test-utils.rkt"
                    regression-test)
           "./test-utils.rkt"))

(define POINT_CREATE ;; x :: y :: color -> point struct
  (list
   (label POINT_CREATE)
          (byte 1)
          (bc PUSH_B) (byte 3)
          (bc ALLOC_ARA)
          (bc POP_TO_RA_AF)
          (bc POP_TO_RA_AF)
          (bc POP_TO_RA_AF)
          (bc PUSH_RA)
          (bc RET)))

(module+ test #| point create |#
  (define point-create-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
       (bc CALL) (word-ref POINT_CREATE)
       (bc BREAK))
      POINT_CREATE)
     ))

  (regression-test
   point-create-state
   "100 500 -> point create"
   (check-equal? (vm-stack->strings point-create-state)
                 (list "stack holds 2 items"
                       (format "ptr[1] $~a02  (rt)" (format-hex-byte PAGE_AVAIL_0))
                       "ptr NIL"))
   (check-equal? (vm-slot->string point-create-state (+ PAGE_AVAIL_0_W #x02))
                 "ptr[1] cell-array of len 3")
   (check-equal? (peek point-create-state(+ PAGE_AVAIL_0_W #x02))
                 1
                 "reference count is 1")
   (check-equal? (map (lambda (offset) (vm-cell-at->string point-create-state (+ PAGE_AVAIL_0_W offset) #f #t))
                      (list 04 06 08))
                 (list "int $01f4" "int $0064" "int $0000")
                 "the first three elements of the array are decimal 500, 100, 0"))

  (define point-create-n-pop-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
       (bc CALL) (word-ref POINT_CREATE)
       (bc POP)
       (bc BREAK))
      POINT_CREATE)
     ))

  (regression-test
   point-create-n-pop-state
   "100 500 -> point create -> pop"
   (check-equal? (vm-stack->strings point-create-n-pop-state)
                 (list "stack is empty or tos=nil"))
   (check-equal? (memory-list  point-create-n-pop-state (+ PAGE_AVAIL_0_W #x02))
                 (list #x0a)
                 "next free is @0e")))

(define POINT_XDIST ;; point1 :: point2 -> int
  (list
   (label POINT_XDIST)
          (byte 0)
          (bc GET_AF_0)    ;; pop_to_ra, get_ra_af_0
          (bc SWAP)
          (bc GET_AF_0)    ;; pop_to_ra, get_ra_af_0
          (bc ISUB)
          (bc RET)))

(module+ test #| point xdist |#
  (define point-xdist-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 250)
       (bc PUSH_I) (word 600)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc CALL) (word-ref POINT_XDIST)
       (bc BREAK))
      POINT_CREATE
      POINT_XDIST)
     ))

  (regression-test
   point-xdist-1-state
   "point 250 600, point 100 500 -> point x distance"
   (check-equal? (vm-stack->strings point-xdist-1-state)
                 (list "stack holds 2 items"
                       "int $0064  (rt)"
                       "ptr NIL"))))

(define POINT_YDIST ;; point1 :: point2 -> int
  (list
   (label POINT_YDIST)
          (byte 0)
          (bc GET_AF_1)
          (bc SWAP)
          (bc GET_AF_1)
          (bc ISUB)
          (bc RET)))

(module+ test #| point ydist |#
  (define point-ydist-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 250)
       (bc PUSH_I) (word 600)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc CALL) (word-ref POINT_YDIST)
       (bc BREAK))
      POINT_CREATE
      POINT_YDIST)
     ))

  (regression-test
   point-ydist-1-state
   "point 250 600, point 100 500 -> y distance"
   (check-equal? (vm-stack->strings point-ydist-1-state)
                 (list "stack holds 2 items"
                       "int $0096  (rt)"
                       "ptr NIL"))))


(define POINT_EQUAL ;; point1 :: point2 -> bool
  (list
   (label POINT_EQUAL)
          (byte 0)
          (bc POP_TO_RA)
          (bc POP_TO_RB)
          (bc GET_RA_AF_0)
          (bc SWAP_RA_RB)
          (bc GET_RA_AF_0)
          (bc CELL_EQ_P)
          (bc F_P_RET_F)
          (bc GET_RA_AF_1)
          (bc SWAP_RA_RB)
          (bc GET_RA_AF_1)
          (bc CELL_EQ_P)
          (bc RET)))

(module+ test #| point equal |#
  (define point-equal-1-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc CALL) (word-ref POINT_EQUAL)
       (bc BREAK))
      POINT_CREATE
      POINT_EQUAL)
     ))

  (regression-test
   point-equal-1-state
   "point 100 500, point 100 500 -> point euqal?"
   (check-equal? (vm-stack->strings point-equal-1-state)
                 (list "stack holds 2 items"
                       "int $0001  (rt)"
                       "ptr NIL"))

   (inform-check-equal? (cpu-state-clock-cycles point-equal-1-state)
                        9886))

  (define point-equal-2-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 499)
       (bc CALL) (word-ref POINT_CREATE)

       (bc CALL) (word-ref POINT_EQUAL)
       (bc BREAK))
      POINT_CREATE
      POINT_EQUAL)
     ))

  (regression-test
   point-equal-2-state
   "point 100 500, point 100 499 -> point equal?"
   (check-equal? (vm-stack->strings point-equal-2-state)
                 (list "stack holds 2 items"
                       "int $0000  (rt)"
                       "ptr NIL")))

  (define point-equal-3-state
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 199)
       (bc PUSH_I) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
       (bc CALL) (word-ref POINT_CREATE)

       (bc CALL) (word-ref POINT_EQUAL)
       (bc BREAK))
      POINT_CREATE
      POINT_EQUAL)
     ))

  (regression-test
   point-equal-3-state
   "point 199 500, point 100 500 -> point equal?"
   (check-equal? (vm-stack->strings point-equal-3-state)
                 (list "stack holds 2 items"
                       "int $0000  (rt)"
                       "ptr NIL"))))

(module+ test #| memory check points |#
  (define mem-point-create-state-1
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100)
       (bc PUSH_I) (word 500)
       (bc CALL) (word-ref POINT_CREATE)
       (bc BREAK))
      POINT_CREATE)
     ))

  (regression-test
   mem-point-create-state-1
   "point create -> memory usage is?"
   (check-equal? (vm-slot->string mem-point-create-state-1 (+ PAGE_AVAIL_0_W #x02))
                 "ptr[1] cell-array of len 3")
   (check-equal? (peek mem-point-create-state-1 (+ PAGE_AVAIL_0_W #x02))
                 1
                 "reference count is 1")
   (check-equal? (map (lambda (offset) (vm-cell-at->string mem-point-create-state-1 (+ PAGE_AVAIL_0_W offset) #f #t))
                      (list 04 06 08))
                 (list "int $01f4" "int $0064" "int $0000")
                 "the first three elements of the array are decimal 500, 100, 0"))

  (define mem-point-create-state-2
    (run-bc-wrapped-in-test
     (append
      (list
       (bc PUSH_I0) ;; color is int0 (string will be implemented later)
       (bc PUSH_I) (word 100) ;; x
       (bc PUSH_I) (word 500) ;; y
       (bc CALL) (word-ref POINT_CREATE)
       (bc POP)            ;; discard created point slot and gc
       (bc BREAK))
      POINT_CREATE)
     ))

  (regression-test
   mem-point-create-state-2
   "point created -> pop -> memory usage?"
   (check-equal? (peek mem-point-create-state-2 (+ PAGE_AVAIL_0_W #x02))
                 #x0a
                 "next free slot on this page starts here (eager collection of array)")
   (check-equal? (peek mem-point-create-state-2 (+ PAGE_AVAIL_0_W #xfe))
                 #x02
                 "first free slot is here again")))

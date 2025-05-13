#lang typed/racket/base

#|review: ignore|#
#|  review does show several false positives |#

#|

Generator translating a mil module to a stack-virtual-machine

|#

(require (only-in "../cisc-vm/stack-virtual-machine.rkt" make-vm make-function-def run-until-break list->cell-list-ptr cell-byte- PUSH_I CALL BRK NIL_CELL vm--value-stack))
(require (only-in "./svm-compiler.rkt" svm-compile svm-generate generation-artifact--bytes make-generation-artifact))
(require (only-in "./parse.rkt" m-fun-def m-val-def))

(module+ test #| require test utils |#
  (require typed/rackunit))


;; TODO: find a way to define values and have them in a value-array of the vm (in globals)
;; IDEA: value definitions may call functions (which do not use this global value transitively)
;;       ==> each value as a flag, weather it is already evaluated
;;           (during interpretation of the globals during compile): running into a non evaluated global will start a thread to evaluate this global
;;           if recursion is encountered (global currently evaluated is required for function to finish) this is aborted with an error
;; SIMPLE IMPLEMENTATION: only vm functions may be used (e.g. car cdr cons byte+ ...)
;; GOOD IMPLEMENTATION: also runtime functions may be used
;; SOPHISTICATED IMPL: user functions of this module may be used
;; INCREDIBLE IMPL: all functions may be used


(module+ test
  ;; (svm-generate (svm-compile (m-val-def test-list cell* "values for reverse-test" (list 5 6 10 20))))
  )

;; DONE: PUSH_GLOBAL does push values of the value-array onto the value stack
;; TODO: parse a list top definitions [of function (header), constant definitions] to generate a tables for function-id -> int, value-constant -> int
;; IDEA: parse m-module
;; IDEA: import from other modules
;; IDEA: generate vm from module (importing other modules as needed)


(module+ test
  (define test-tail-recursion--value-stack
    (list (list->cell-list-ptr (list (cell-byte- 5) (cell-byte- 6) (cell-byte- 10) (cell-byte- 20)))
          NIL_CELL))

  (define test-tail-recursion--run-until-break
    (run-until-break
     (make-vm
      #:options (list ) ;;  'trace
      #:value-stack  test-tail-recursion--value-stack
      #:functions
      (vector-immutable
       (make-function-def
        #:byte-code (vector-immutable PUSH_I   1 0 ;; function index 1
                                      CALL
                                      BRK))
       (make-function-def
        #:parameter-count 2 ;; param0 = original list, param1 = nil (reversed list in the end)
        #:byte-code (generation-artifact--bytes
                     (svm-generate
                      (svm-compile
                       (m-fun-def (reverse (a-list cell*) (b-list cell* '()) -> cell*
                                           "reverse a-list, consing it into b-list")
                                  (if (nil? a-list)
                                      b-list
                                      (reverse (cdr a-list) (cons (car a-list) b-list)))))
                      (make-generation-artifact))))))))

  (check-equal? (vm--value-stack test-tail-recursion--run-until-break)
                (list (list->cell-list-ptr (list  (cell-byte- 20) (cell-byte- 10) (cell-byte- 6) (cell-byte- 5))))
                "tos is reversed list"))

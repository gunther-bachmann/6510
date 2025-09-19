#lang racket/base

#|

combines all implementations of the runtime byte code implementations

|#

(require (only-in racket/list
                  flatten)
         "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "./ext.rkt"
                  BC_EXT1_CMD
                  VM_INTERPRETER_OPTABLE_EXT1_LB
                  VM_INTERPRETER_OPTABLE_EXT1_HB)
         (only-in "./push_local.rkt"
                  BC_WRITE_LOCAL_SHORT
                  BC_PUSH_LOCAL_SHORT
                  PUSH_RT_WRITE_LOCAL_bc_enc)
         (only-in "./push_n_pop.rkt"
                  BC_PUSH_B))

(provide (all-from-out "./push_local.rkt")
         (all-from-out "./ext.rkt")
         (all-from-out "./push_n_pop.rkt"))

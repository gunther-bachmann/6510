#lang racket/base

#|

combines all implementations of the runtime byte code implementations

|#

(require "../../6510.rkt")
(require (only-in "../../ast/6510-resolver.rkt" add-label-suffix))
(require (only-in racket/list flatten))

(require (only-in "./ext.rkt"
                  BC_EXT1_CMD
                  VM_INTERPRETER_OPTABLE_EXT1_LB
                  VM_INTERPRETER_OPTABLE_EXT1_HB))

(require (only-in "./push_local.rkt"
                  BC_WRITE_LOCAL_SHORT
                  BC_PUSH_LOCAL_SHORT
                  PUSH_RT_WRITE_LOCAL_bc_enc))

(require (only-in "./push_n_pop.rkt"
                  BC_PUSH_B))

(provide (all-from-out "./push_local.rkt")
         (all-from-out "./ext.rkt")
         (all-from-out "./push_n_pop.rkt"))

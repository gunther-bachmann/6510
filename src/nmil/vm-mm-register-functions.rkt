#lang racket/base

(require "../6510.rkt")
(require (only-in "./vm-memory-map.rkt"
                  TAGGED_NIL
                  ZP_RP
                  ZP_RT
                  VM_MEMORY_MANAGEMENT_CONSTANTS))
(require (only-in "./vm-inspector-utils.rkt"
                  vm-cell-at-nil?
                  vm-rega->string
                  vm-regt->string))

(provide WRITE_NIL_TO_RT
         WRITE_NIL_TO_RP
         WRITE_INT_AY_TO_RT
         CP_RA_TO_RT
         CP_RA_TO_RZ
         CP_RT_TO_RA
         CP_RT_TO_RP
         CP_RT_TO_RZ
         CP_RZ_TO_RT)

(module+ test
  (require  "../6510-test-utils.rkt")
  (require "./vm-memory-manager-test-utils.rkt")

  (define test-runtime
    (append
     WRITE_NIL_TO_RT
     WRITE_NIL_TO_RP
     WRITE_INT_AY_TO_RT
     CP_RA_TO_RT
     CP_RA_TO_RZ
     CP_RT_TO_RA
     CP_RT_TO_RP
     CP_RT_TO_RZ
     CP_RZ_TO_RT
     VM_MEMORY_MANAGEMENT_CONSTANTS
     (list (label VM_INITIALIZE_MEMORY_MANAGER) (RTS)))))

;; write NIL into register, not checking its content (no dec-refcnt)
;; input:  -
;; output: RT (RP) = NIL
(define WRITE_NIL_TO_RT
  (list
   (label WRITE_NIL_TO_RT)
          (LDA !<TAGGED_NIL)
          (STA ZP_RT)
          (LDA !>TAGGED_NIL)
          (STA ZP_RT+1)
          (RTS)))

(define WRITE_NIL_TO_RP
  (list
   (label WRITE_NIL_TO_RP)
          (LDA !<TAGGED_NIL)
          (STA ZP_RP)
          (LDA !>TAGGED_NIL)
          (STA ZP_RP+1)
          (RTS)))

(module+ test #| WRITE_NIL_TO_Rx|#
  (define write-nil-to-rp
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR WRITE_NIL_TO_RP)))

  (check-true (vm-cell-at-nil? write-nil-to-rp ZP_RP))

  (define write-nil-to-rt
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR WRITE_NIL_TO_RT)))

  (check-true (vm-cell-at-nil? write-nil-to-rt ZP_RT)))

;; input:  RA
;; output: RT (copy of RA)
(define CP_RA_TO_RT
  (list
   (label CP_RA_TO_RT)
   (label CP_RA_TO_RT__VALUE) ;;just value, no tagged byte
          (LDA ZP_RA+1)
          (STA ZP_RT+1)
          (LDA ZP_RA)
          (STA ZP_RT)
          (RTS)))

;; input:  RA
;; output: RZ (copy of RA)
(define CP_RA_TO_RZ
  (list
   (label CP_RA_TO_RZ)
          (LDA ZP_RA+1)
          (STA ZP_RZ+1)
          (LDA ZP_RA)
          (STA ZP_RZ)
          (RTS)))

;; input:  RT
;; output: RZ (copy of RT)
(define CP_RT_TO_RZ
  (list
   (label CP_RT_TO_RZ)
          (LDA ZP_RT+1)
          (STA ZP_RZ+1)
          (LDA ZP_RT)
          (STA ZP_RZ)
          (RTS)))

;; input:  RZ
;; output: RT (copy of RZ)
(define CP_RZ_TO_RT
  (list
   (label CP_RZ_TO_RT)
          (LDA ZP_RZ+1)
          (STA ZP_RT+1)
          (LDA ZP_RZ)
          (STA ZP_RT)
          (RTS)))

;; input:  RT
;; output: RP (copy of RT)
(define CP_RT_TO_RP
  (list
   (label CP_RT_TO_RP)
          (LDA ZP_RT+1)
          (STA ZP_RP+1)
          (LDA ZP_RT)
          (STA ZP_RP)
          (RTS)))

;; input:  RT
;; output: RA (copy of RT)
(define CP_RT_TO_RA
  (list
   (label CP_RT_TO_RA)
   (label CP_RT_TO_RA__VALUE) ;;just value, no tagged byte
          (LDA ZP_RT+1)
          (STA ZP_RA+1)
          (LDA ZP_RT)
          (STA ZP_RA)
          (RTS)))

(module+ test #| vm-cp-rt-to-ra |#

  (define vm-cp-rt-to-ra-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
    (JSR WRITE_INT1_TO_RT)
    (JSR CP_RT_TO_RA)))

  (check-equal? (vm-rega->string vm-cp-rt-to-ra-state)
                "int $0001")
  (check-equal? (vm-regt->string vm-cp-rt-to-ra-state)
                "int $0001"))

;; write the given int in A/Y into RT, ignoring what was in RT (no dec-refcnt)
;; input:  A = lowbyte of int (0..255), written into high byte of cell register RT
;;         Y = highbyte (0.31), written into lowbyte and tagged lowbyte of cell register
;;         X = (0 = RT, 2 = RA)
;; usage:  A, X, Y
;; output: RT = cell-int
(define WRITE_INT_AY_TO_RT
  (list
   (label WRITE_INTm1_TO_RT)
          (LDA !$ff) ;; int lowbyte = ff
          (LDY !$7f) ;; #b[0]111 11[11] = $1f for int high byte
          (BNE ENC_WRITE_AY_TO_RT)

   (label WRITE_INT1_TO_RT)
          (LDA !$01)
          (BNE WRITE_INT_A_TO_RT)

   (label WRITE_INT0_TO_RT)
          (LDA !$00)

   (label WRITE_INT_A_TO_RT)
          (LDY !$03) ;; #b[0]000 00[11] = high byte of int  0
   (label ENC_WRITE_AY_TO_RT)
          (STY ZP_RT)
          (STA ZP_RT+1)
          (RTS)

   (label WRITE_INT_AY_TO_RT)
          (STA ZP_RT+1)
          (TYA)      ;; #b???x xxxx
          (SEC)
          (ROL)      ;; #b??xx xxx1
          (SEC)
          (ROL)      ;; #b?xxx xx11
          (AND !$7f) ;; #xb0xxx xx11 (mask out top bit!)
          (STA ZP_RT) ;; encoded tagged byte of int goes into first memory cell, even though it is the high-byte part of int
          (RTS)))

(module+ test #| vm_write_int_ay_to_rt |#
  (define vm-write-int-ay-to-rt-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDA !$01)
     (LDY !$02)
     (JSR WRITE_INT_AY_TO_RT)))

  (check-equal? (vm-regt->string vm-write-int-ay-to-rt-state)
                "int $0201"))

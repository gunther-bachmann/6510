#lang racket/base

;; IDEA: reduce to the minimal needed, make the ones used more often fast, make the others generic (see swap)

(provide WRITE_NIL_TO_RT        ;; write constant NIL into RT
         WRITE_NIL_TO_RP        ;; write constant NIL into RP
         WRITE_INT_AY_TO_RT     ;; write integer constant into Rx
         WRITE_INT0_TO_RT       ;; write int 0 to RT
         WRITE_INT1_TO_RT       ;; write int 1 to RT
         WRITE_INTm1_TO_RT      ;; write int -1 to RT

         CP_RA_TO_RT            ;; copy register RA -> RT
         CP_RA_TO_RZ            ;; copy register RA -> RZ
         CP_RA_TO_RB            ;; RA -> RB
         CP_RB_TO_RZ            ;; RB -> RZ
         CP_RC_TO_RZ            ;; RC -> RZ

         CP_RT_TO_RA            ;; RT -> RA
         CP_RT_TO_RB            ;; RT -> RB
         CP_RT_TO_RZ            ;; RT -> RZ
         CP_RT_TO_RP            ;; RT -> RP
         CP_RZ_TO_RT            ;; RZ -> RT

         SWAP_ZP_WORD           ;; swap 16 bits of two zero page locations (in A and X)
         SWAP_RA_RB             ;; RA <-> RB

         vm-register-functions-code)

#|

  list of functions of generic register operations (RT, RP, RZ, RA, RB, RC ...)

 |#

(require "../../6510.rkt"
         (only-in "../../ast/6510-relocator.rkt"
                  code-len)
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol)
         (only-in "../vm-inspector-utils.rkt"
                  vm-cell-at-nil?
                  vm-rega->string
                  vm-regb->string
                  vm-regt->string
                  vm-regz->string)
         (only-in "./vm-memory-map.rkt"
                  TAGGED_NIL
                  TAGGED_INT_0_LB
                  ZP_RP
                  ZP_RT
                  ZP_RA
                  ZP_RC
                  ZP_RZ
                  VM_MEMORY_MANAGEMENT_CONSTANTS))

(module+ test
  (require  "../../6510-test-utils.rkt"
            (only-in "../../tools/6510-interpreter.rkt" memory-list)
            "./vm-memory-manager-test-utils.rkt")

  (define test-runtime
    (append
     WRITE_NIL_TO_RT
     WRITE_NIL_TO_RP
     WRITE_INT_AY_TO_RT
     CP_RA_TO_RT
     CP_RA_TO_RZ
     CP_RT_TO_RA
     CP_RT_TO_RB
     CP_RT_TO_RP
     CP_RT_TO_RZ
     CP_RZ_TO_RT
     SWAP_ZP_WORD

     VM_MEMORY_MANAGEMENT_CONSTANTS
     (list (label VM_INIT_MEMORY_MANAGER) (RTS)))))

;; @DC-FUN: WRITE_NIL_TO_RT, group: register
;; write NIL into register, not checking its content (no dec-refcnt)
;; input:  -
;; output: RT (RP) = NIL
(define-vm-function WRITE_NIL_TO_RT
  (list
          (LDX !<TAGGED_NIL)
          (STX ZP_RT)
          (LDX !>TAGGED_NIL)
          (STX ZP_RT+1)
          (RTS)))

(define-vm-function WRITE_NIL_TO_RP
  (list
          (LDX !<TAGGED_NIL)
          (STX ZP_RP)
          (LDX !>TAGGED_NIL)
          (STX ZP_RP+1)
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

;; @DC-FUN: CP_RA_TO_RT, group: register
;; copy RA -> RT
;; input:  RA
;; output: RT (copy of RA)
(define-vm-function CP_RA_TO_RT
  (list
          (LDX ZP_RA+1)
          (STX ZP_RT+1)
          (LDX ZP_RA)
          (STX ZP_RT)
          (RTS)))

;; @DC-FUN: CP_RA_TO_RZ, group: register
;; copy RA -> RZ
;; input:  RA
;; output: RZ (copy of RA)
(define-vm-function CP_RA_TO_RZ
  (list
          (LDX ZP_RA+1)
          (STX ZP_RZ+1)
          (LDX ZP_RA)
          (STX ZP_RZ)
          (RTS)))

;; @DC-FUN: CP_RB_TO_RZ, group: register
;; copy RB -> RZ
;; input:  RB
;; output: RZ (copy of RA)
(define-vm-function CP_RB_TO_RZ
  (list
          (LDX ZP_RB+1)
          (STX ZP_RZ+1)
          (LDX ZP_RB)
          (STX ZP_RZ)
          (RTS)))

;; @DC-FUN: CP_RC_TO_RZ, group: register
;; copy RC -> RZ
;; input:  RC
;; output: RZ (copy of RA)
(define-vm-function CP_RC_TO_RZ
  (list
          (LDX ZP_RC+1)
          (STX ZP_RZ+1)
          (LDX ZP_RC)
          (STX ZP_RZ)
          (RTS)))

;; @DC-FUN: CP_RT_TO_RZ, group: register
;; copy RT -> RZ
;; input:  RT
;; output: RZ (copy of RT)
;;         X = low byte of RT
(define-vm-function CP_RT_TO_RZ
  (list
          (LDX ZP_RT+1)
          (STX ZP_RZ+1)
          (LDX ZP_RT)
          (STX ZP_RZ)
          (RTS)))

;; @DC-FUN: CP_RZ_TO_RT, group: register
;; copy RZ -> RT
;; input:  RZ
;; output: RT (copy of RZ)
(define-vm-function CP_RZ_TO_RT
  (list
          (LDX ZP_RZ+1)
          (STX ZP_RT+1)
          (LDX ZP_RZ)
          (STX ZP_RT)
          (RTS)))

;; @DC-FUN: CP_RT_TO_RP, group: register
;; copy RT -> RP
;; input:  RT
;; output: RP (copy of RT)
(define-vm-function CP_RT_TO_RP
  (list
          (LDX ZP_RT+1)
          (STX ZP_RP+1)
          (LDX ZP_RT)
          (STX ZP_RP)
          (RTS)))

;; @DC-FUN: CP_RT_TO_RA, group: register
;; copy RT -> RA
;; input:  RT
;; output: RA (copy of RT)
(define-vm-function CP_RT_TO_RA
  (list
          (LDX ZP_RT+1)
          (STX ZP_RA+1)
          (LDX ZP_RT)
          (STX ZP_RA)
          (RTS)))

;; @DC-FUN: CP_RT_TO_RB, group: register
;; copy RT -> RB
;; input:  RT
;; output: RB (copy of RT)
(define-vm-function CP_RT_TO_RB
  (list
          (LDX ZP_RT+1)
          (STX ZP_RB+1)
          (LDX ZP_RT)
          (STX ZP_RB)
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

(define WRITE_INT0_TO_RT '())
(define WRITE_INT1_TO_RT '())
(define WRITE_INTm1_TO_RT '())
;; @DC-FUN: WRITE_INT_AY_TO_RT, group: register
;; write the given int in A/Y into RT, ignoring what was in RT (no dec-refcnt)
;; input:  A = lowbyte of int (0..255), written into high byte of cell register RT
;;         Y = highbyte (0.31), written into lowbyte and tagged lowbyte of cell register
;;         X = (0 = RT, 2 = RA)
;; usage:  A, X, Y
;; output: RT = cell-int
(define-vm-function-wol WRITE_INT_AY_TO_RT
  (list
   (label WRITE_INTm1_TO_RT)
          (LDA !$ff) ;; int lowbyte = ff
          (LDY !$ff) ;; #b1111 11[11] = $1f for int high byte
          (BNE ENC_WRITE_AY_TO_RT)

   (label WRITE_INT1_TO_RT)
          (LDA !$01)
          (BNE WRITE_INT_A_TO_RT)

   (label WRITE_INT0_TO_RT)
          (LDA !$00)

   (label WRITE_INT_A_TO_RT)
          (LDY !TAGGED_INT_0_LB) ;; #b0000 00[11] = high byte of int  0
   (label ENC_WRITE_AY_TO_RT)
          (STY ZP_RT)
          (STA ZP_RT+1)
          (RTS)

   (label WRITE_INT_AY_TO_RT)
          (STA ZP_RT+1)
          (TYA)      ;; #b???x xxxx
          (ASL A)
          (ASL A)
          (ORA !$03)
          (STA ZP_RT) ;; encoded tagged byte of int goes into first memory cell, even though it is the high-byte part of int
          (RTS)))

(module+ test #| vm_write_int_ay_to_rt |#
  (define vm-write-int-ay-to-rt-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR WRITE_INTm1_TO_RT)
     (JSR CP_RT_TO_RA)
     (JSR WRITE_INT0_TO_RT)
     (JSR CP_RT_TO_RB)
     (JSR WRITE_INT1_TO_RT)
     (JSR CP_RT_TO_RZ)
     (LDA !$01)
     (LDY !$02)
     (JSR WRITE_INT_AY_TO_RT)))

  (check-equal? (vm-regt->string vm-write-int-ay-to-rt-state)
                "int $0201"
                "WRITE_INT_AY_TO_RT encodes A and Y with tag byte")
  (check-equal? (vm-rega->string vm-write-int-ay-to-rt-state)
                "int $3fff"
                "WRITE_INTm1_TO_RT writes int -1")
  (check-equal? (vm-regb->string vm-write-int-ay-to-rt-state)
                "int $0000"
                "WRITE_INT0_TO_RT writes int 0")
  (check-equal? (vm-regz->string vm-write-int-ay-to-rt-state)
                "int $0001"
                "WRITE_INT1_TO_RT write int 1"))

;; @DC-FUN: SWAP_ZP_WORD, group: register
;; swap 16 bits of two zero page locations
;; e.g. swapping RA with RB: A = !ZP_RA, X = !ZP_RB
;; input:  A = zero-page address 1
;;         X = zero page address 2
;; usage:  A, X, Y, TEMP..TEMP4
;; output: swapped zero page 16 bit values
(define-vm-function SWAP_ZP_WORD ;; 33 bytes
   (list
           (LDY !$00)
           (STY ZP_TEMP2)
           (STA ZP_TEMP)
           (STY ZP_TEMP4)
           (STX ZP_TEMP3)

           (LDA (ZP_TEMP3),y)
           (TAX)

           (LDA (ZP_TEMP),y)
           (STA (ZP_TEMP3),y)
           (INY)
           (LDA (ZP_TEMP3),y)
           (PHA)

           (LDA (ZP_TEMP),y)
           (STA (ZP_TEMP3),y)

           (PLA)
           (STA (ZP_TEMP),y)
           (DEY)
           (TXA)
           (STA (ZP_TEMP),y)
           (RTS)))

(module+ test #| swap-zp-word |#
  (define swap-zp-word-t0
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (LDX !$1e)
     (STX ZP_RA)
     (INX)
     (STX ZP_RA+1)
     (INX)
     (STX ZP_RC)
     (INX)
     (STX ZP_RC+1)

     (LDA !ZP_RC)
     (LDX !ZP_RA)
     (JSR SWAP_ZP_WORD)))

  (check-equal? (memory-list swap-zp-word-t0 ZP_RA (+ 1 ZP_RA))
                (list #x20 #x21)
                "swaps rc (20 21) with ra (1e 1f)")
  (check-equal? (memory-list swap-zp-word-t0 ZP_RC (+ 1 ZP_RC))
                (list #x1e #x1f)
                "swaps rc (20 21) with ra (1e 1f)"))

(define-vm-function CP_RA_TO_RB
   (list
           (LDX ZP_RA)
           (STX ZP_RB)
           (LDX ZP_RA+1)
           (STX ZP_RB+1)
           (RTS)))

(define-vm-function SWAP_RA_RB ;; 17 bytes
   (list
           (LDA ZP_RB)
           (LDY ZP_RB+1)

           (LDX ZP_RA)
           (STX ZP_RB)
           (LDX ZP_RA+1)
           (STX ZP_RB+1)

           (STA ZP_RA)
           (STY ZP_RA+1)
           (RTS)))

(define vm-register-functions-code
  (append
   WRITE_NIL_TO_RT
   WRITE_NIL_TO_RP
   WRITE_INT_AY_TO_RT
   ;; WRITE_INT0_TO_RT
   ;; WRITE_INT1_TO_RT
   ;; WRITE_INTm1_TO_RT
   CP_RA_TO_RT
   CP_RA_TO_RZ
   CP_RB_TO_RZ
   CP_RC_TO_RZ
   CP_RT_TO_RA
   CP_RT_TO_RP
   CP_RT_TO_RZ
   CP_RZ_TO_RT
   CP_RT_TO_RB
   SWAP_ZP_WORD
   CP_RA_TO_RB
   SWAP_RA_RB))

(module+ test #| code len |#
  (inform-check-equal? (code-len vm-register-functions-code)
                       247))

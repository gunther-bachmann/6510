#lang racket/base

(provide WRITE_NIL_TO_RT        ;; write constant NIL into RT
         WRITE_NIL_TO_RP
         WRITE_INT_AY_TO_RT     ;; write integer constant into Rx
         WRITE_INT0_TO_RT
         WRITE_INT1_TO_RT
         CP_RA_TO_RT            ;; copy regiser from RA to RT
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
         SWAP_RA_RB

         vm-register-functions-code)


#|

  list of functions of generic register operations (RT, RP, RZ, RA, RB, RC ...)

 |#

(require "../../6510.rkt"
         (only-in "../../ast/6510-resolver.rkt"
                  add-label-suffix)
         (only-in "../../ast/6510-relocator.rkt"
                  code-len)
         (only-in "../vm-inspector-utils.rkt"
                  vm-cell-at-nil-n?
                  vm-rega-n->string
                  vm-regt-n->string)
         (only-in "./vm-memory-map.rkt"
                  TAGGED_NIL
                  ZP_RP
                  ZP_RT
                  ZP_RA
                  ZP_RC
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
     CP_RT_TO_RP
     CP_RT_TO_RZ
     CP_RZ_TO_RT
     SWAP_ZP_WORD

     VM_MEMORY_MANAGEMENT_CONSTANTS
     (list (label VM_INITIALIZE_MEMORY_MANAGER) (RTS)))))

;; @DC-FUN: WRITE_NIL_TO_RT, group: register
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

  (check-true (vm-cell-at-nil-n? write-nil-to-rp ZP_RP))

  (define write-nil-to-rt
    (compact-run-code-in-test-
     #:runtime-code test-runtime
     (JSR WRITE_NIL_TO_RT)))

  (check-true (vm-cell-at-nil-n? write-nil-to-rt ZP_RT)))

;; @DC-FUN: CP_RA_TO_RT, group: register
;; copy RA -> RT
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

;; @DC-FUN: CP_RA_TO_RZ, group: register
;; copy RA -> RZ
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

;; @DC-FUN: CP_RB_TO_RZ, group: register
;; copy RB -> RZ
;; input:  RB
;; output: RZ (copy of RA)
(define CP_RB_TO_RZ
  (list
   (label CP_RB_TO_RZ)
          (LDA ZP_RB+1)
          (STA ZP_RZ+1)
          (LDA ZP_RB)
          (STA ZP_RZ)
          (RTS)))

;; @DC-FUN: CP_RC_TO_RZ, group: register
;; copy RC -> RZ
;; input:  RC
;; output: RZ (copy of RA)
(define CP_RC_TO_RZ
  (list
   (label CP_RC_TO_RZ)
          (LDA ZP_RC+1)
          (STA ZP_RZ+1)
          (LDA ZP_RC)
          (STA ZP_RZ)
          (RTS)))

;; @DC-FUN: CP_RT_TO_RZ, group: register
;; copy RT -> RZ
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

;; @DC-FUN: CP_RZ_TO_RT, group: register
;; copy RZ -> RT
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

;; @DC-FUN: CP_RT_TO_RP, group: register
;; copy RT -> RP
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

;; @DC-FUN: CP_RT_TO_RA, group: register
;; copy RT -> RA
;; input:  RT
;; output: RA (copy of RT)
(define CP_RT_TO_RA
  (list
   (label CP_RT_TO_RA)
          (LDA ZP_RT+1)
          (STA ZP_RA+1)
          (LDA ZP_RT)
          (STA ZP_RA)
          (RTS)))

;; @DC-FUN: CP_RT_TO_RB, group: register
;; copy RT -> RB
;; input:  RT
;; output: RB (copy of RT)
(define CP_RT_TO_RB
  (list
   (label CP_RT_TO_RB)
          (LDA ZP_RT+1)
          (STA ZP_RB+1)
          (LDA ZP_RT)
          (STA ZP_RB)
          (RTS)))

(module+ test #| vm-cp-rt-to-ra |#

  (define vm-cp-rt-to-ra-state
    (compact-run-code-in-test-
     #:runtime-code test-runtime
    (JSR WRITE_INT1_TO_RT)
    (JSR CP_RT_TO_RA)))

  (check-equal? (vm-rega-n->string vm-cp-rt-to-ra-state)
                "int $0001")
  (check-equal? (vm-regt-n->string vm-cp-rt-to-ra-state)
                "int $0001"))

(define WRITE_INT0_TO_RT '())
(define WRITE_INT1_TO_RT '())
;; @DC-FUN: WRITE_INT_AY_TO_RT, group: register
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

  (check-equal? (vm-regt-n->string vm-write-int-ay-to-rt-state)
                "int $0201"))

;; @DC-FUN: SWAP_ZP_WORD, group: register
;; swap 16 bits of two zero page locations
;; e.g. swapping RA with RB: A = !ZP_RA, X = !ZP_RB
;; input:  A = zero-page address 1
;;         X = zero page address 2
;; usage:  A, X, Y, TEMP..TEMP4
;; output: swapped zero page 16 bit values
(define SWAP_ZP_WORD ;; 33 bytes
  (add-label-suffix
   "__" "SWAP_ZP_WORD"
   (list
    (label SWAP_ZP_WORD)
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
           (RTS))))

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
                "originally $1e $1f")
  (check-equal? (memory-list swap-zp-word-t0 ZP_RC (+ 1 ZP_RC))
                (list #x1e #x1f)
                "originally $20 $21"))

(define CP_RA_TO_RB
  (add-label-suffix
   "__" "CP_RA_TO_RB"
   (list
    (label CP_RA_TO_RB)
           (LDX ZP_RA)
           (STX ZP_RB)
           (LDX ZP_RA+1)
           (STX ZP_RB+1)
           (RTS))))

(define SWAP_RA_RB ;; 17 bytes
  (add-label-suffix
   "__" "SWAP_RA_RB"
   (list
    (label SWAP_RA_RB)
           (LDA ZP_RB)
           (LDY ZP_RB+1)

           (LDX ZP_RA)
           (STX ZP_RB)
           (LDX ZP_RA+1)
           (STX ZP_RB+1)

           (STA ZP_RA)
           (STY ZP_RA+1)
           (RTS))))

(define vm-register-functions-code
  (append
   WRITE_NIL_TO_RT        ;; write constant NIL into RT
   WRITE_NIL_TO_RP
   WRITE_INT_AY_TO_RT     ;; write integer constant into Rx
   ;; WRITE_INT0_TO_RT
   ;; WRITE_INT1_TO_RT
   CP_RA_TO_RT            ;; copy regiser from RA to RT
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
                       249))

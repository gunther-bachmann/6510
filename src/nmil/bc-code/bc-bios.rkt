#lang racket/base

(module+ test
  (require
   (only-in "../test-utils.rkt"
            regression-test)
  "./test-utils.rkt"))

(module+ test #| bios print |#
  (define bios-print-state
    (run-bc-wrapped-in-test
     (append
      (list
              ;; copy data into native array
              (bc NATIVE)
              (LDA !$04)
              (JSR ALLOC_NATARR_TO_RA)
              (JSR CP_RA_TO_RT)
              (LDA !<DATA)
              (STA ZP_RP)
              (LDA !>DATA)
              (STA ZP_RP+1)
              (LDY !5)
       (label LOOP)
              (LDA (ZP_RP),y)
              (STA (ZP_RT),y)
              (DEY)
              (CPY !$01)
              (BNE LOOP)
              (JSR RETURN_TO_BC)

              ;; actual test
              (bc PUSH_B) (byte 23) ;; row
              (bc SWAP)
              (bc PUSH_B) (byte 5) ;; col
              (bc SWAP)
              (bc BIOS) (byte $00)
              (bc BREAK)

       (label DATA)
              (byte $00 $00)           ;; buffer to allow for same y indexing
              (byte $01 $02 $03 $04))) ;; native array with ABCD
    #f))

  (regression-test
   bios-print-state
   "print string at"
   (check-equal? (memory-list- bios-print-state (+ (* 40 23) #x0400 5) 4)
                 (list 1 2 3 4))))

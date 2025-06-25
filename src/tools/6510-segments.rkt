#lang racket/base

#|

define code/data segments for a c64 program that are used for generating a loader/linker

|#

(provide (struct-out c64-segment)
         (struct-out c64-relocation-info)
         (struct-out c64-resolution-info)
         (struct-out c64-resolution-symbols)
         width?
         segment-type?
         resolution-type?)

(require (only-in racket/contract/base struct-guard/c and/c list/c)
         (only-in "../6510-utils.rkt" word/c  byte/c))

(define (width? w)
  (memq w (list 'byte 'word)))

(define (segment-type? l)
  (memq l (list 'pinned 'relocatable)))

(define (resolution-type? r)
  (memq r (list 'relative 'absolute)))

;; information needed to resolve open references to other segments
(struct c64-resolution-info
  (-offset      ;; offset within this segments byte array
   -width       ;; width to adjust
   -symbol      ;; the actual symbol to be resolved (and present in other segment)
   -type        ;; type of reference
   )
  #:transparent
  #:guard (struct-guard/c word/c width? string? resolution-type?))

;; information needed to relocate a segment (all references resolved, just relocating)
(struct c64-relocation-info
  (-offset      ;; offset into the segments byte array
   -width       ;; width to adjust
   -adjustment  ;; adjustment relative to own offset
   )
  #:transparent
  #:guard (struct-guard/c word/c width? word/c))

(struct c64-resolution-symbols
  (-symbol      ;; symbol to resolve
   -offset      ;; offset into this segment
   )
  #:transparent
  #:guard (struct-guard/c
           string?
           word/c))

;; define a self contained code segment
(struct c64-segment
  (-type             ;; t
   -location         ;; location where this segment should be located if its type is 'pinned
   -relocation-info  ;; data that needs adjustment because of relocation
   -resolution-info  ;; data that needs to be resolved to other segments
   -provided-symbols ;; data used by other segments to resolve against
   -length           ;; length of byte array
   -bytes            ;; bytes list
   )
  #:transparent
  #:guard (struct-guard/c
           segment-type?
           word/c
           (list/c c64-relocation-info?)
           (list/c c64-resolution-info?)
           (list/c c64-resolution-symbols?)
           word/c
           (list/c byte?))
  )

;; placing/copying a segment somewhere into the code includes the following steps
;; - copy all bytes to the new location
;; - go over all relocation info, adjusting the references
;; - go over all resolution info, resolving references to other segments


(require "../6510.rkt")
(require (only-in "../ast/6510-resolver.rkt"
                  add-label-suffix))

;; A/X (l/h) bytes with starting descriptor of the format
;; mem at [xa]:  lb-len hb-len lb-src hb-src lb-tar hb-tar <- should start with highest mem region to minimize change
;;               ...
;;               00 00  <- last entry has len 0
(define LOOPED_COPY_REGION
  (add-label-suffix
   "__" "__LOOPED_COPY_REGION"
  (list
   (label LOOPED_COPY_REGION)
          (STA $F8)
          (STX $F9)

   (label OUTER_LOOP__)
          (LDY !$00)
          (LDA ($F8),y)
          (BNE LEN_NOT_ZERO__)
          (INY)
          (LDA ($F8),y)
          (BEQ DONE__)

   (label LEN_NOT_ZERO__)
          (LDY !$00)
          (LDX !$00)

   (label INNER_LOOP__)
          (LDA ($F8),y)
          (STA $FA,x)
          (INY)
          (INX)
          (CPY !$06)
          (BNE INNER_LOOP__)
          (CLC)
          (TYA)
          (ADC $F8)
          (STA $F8)
          (BCC NO_INC_PAGE__)
          (INC $F9)
   (label NO_INC_PAGE__)

          ;; now copy region fields are filled
          (JSR COPY_REGION)
          (JMP OUTER_LOOP__)

   (label DONE__)
          (RTS))))

(module+ test #| looped-copy-region |#
  (require (only-in  "../nmil/vm-memory-manager-test-utils.rkt" list-with-label-suffix))
  (define looped-copy-region-01
    (run-code-in-test-on-code
     (append
     (list-with-label-suffix
      #:org  (org #x0804)
      #:mock (list (label CHAR_OUT))
             ;; (JMP CTRL_SECTION)

             (org #x0900)
      (label TEST_COUNTERS)
             (byte 0)
      (label COPY_DESCRIPTOR)
             (byte 06 00 00 #x10 00 #x80)
             (byte 06 00 00 #x20 00 #xc0)
             (byte 00 00)

             ;; (word-const CHAR_OUT $ffd2)

      (label TEST_ENTRY)
      (label CTRL_SECTION)
             ;; now copy sections to right place
             (LDA !<COPY_DESCRIPTOR)
             (LDX !>COPY_DESCRIPTOR)
             (JSR LOOPED_COPY_REGION)

             (JSR $8000)
             (JSR $c000)
             (BRK)

             (org #x1000)
      (label SECTION_ONE)
             (LDA !48)
             (JSR CHAR_OUT)
             (RTS)
             (org #x2000)
      (label SECTION_TWO)
             (LDA !49)
             (JSR CHAR_OUT)
             (RTS))
     LOOPED_COPY_REGION
     COPY_REGION)
     ))

  (check-equal? (memory-list looped-copy-region-01 #x0900 #x0900)
                (list 02)
                "CHAR_OUT has been called two times!")
  (check-equal? (memory-list looped-copy-region-01 #x1000 #x1002)
                (list 169 48 32)
                "load and jump command")
  (check-equal? (memory-list looped-copy-region-01 #x2000 #x2002)
                (list 169 49 32)
                "load and jump command"))

;; copy region from: zp:  fc/fd (low/high), to fe/ff (low/high), size fa/fb (low/high)
;; starting from the end, then page wise (with 0 offset first) => there should be no page overlap
;; source and target must not be on the zero page to not fall victim to zp wrap around
(define COPY_REGION
  (add-label-suffix
   "__" "__COPY_REGION"
  (list
   (label COPY_REGION)
          (LDA $FD)
          (CLC)
          (ADC $FB)
          (STA $FD)
          (BCC NO_INC_SRC_HB__)
          (INC $FD)
   (label NO_INC_SRC_HB__)

          (LDA $FF)
          (CLC)
          (ADC $FB)
          (STA $FF)
          (BCC NO_INC_TAR_HB__)
          (INC $FF)

   (label NO_INC_TAR_HB__)
          (INC $FB) ;; => so BNE on outer loop works exactly the number of times wanted

          (LDY !$00)
          (LDA ($FC),y)
          (STA ($FE),y)

          (LDY $FA)
          (BEQ NEXT_PAGE_ITER__)

   (label OUTER_LOOP__)
   (label INNER_LOOP__)
          (LDA ($FC),y)
          (STA ($FE),y)
          (DEY)
          (BNE INNER_LOOP__)

   (label NEXT_PAGE_ITER__)
          (DEC $FD)
          (DEC $FF)
          (DEC $FB)
          (BNE OUTER_LOOP__) ;; y = 0, copies ,0 then ,ff then ,fe ...

   (label DONE__)
          (RTS))))

(module+ test #| copy-region |#
  (require (only-in "../nmil/vm-memory-manager-test-utils.rkt" run-code-in-test-on-code))
  (require (only-in "../tools/6510-interpreter.rkt" memory-list))
  (require "../6510-test-utils.rkt")

  (define copy-region-state
    (run-code-in-test-on-code
     (append
      (list
       (org #x00f7)
             (JMP TEST_ENTRY)
       (org #x00fa)
             (word #x000a) ;; copy 10 bytes
             (word #xB000) ;; from b000
             (word #xF1FA) ;; to f1fa
       (org #xa000)
       (label TEST_ENTRY)
              (JSR COPY_REGION)
              (BRK)

       (org #xb000)
              (byte 1 2 3 4 5 6 7 8 9 10)
       (org #xc000))
      COPY_REGION)
     ))

  (check-equal? (memory-list copy-region-state #xf1fa (+ #xf1fa 9))
                (list 1 2 3 4 5 6 7 8 9 10))

  (define copy-region-state-02
    (run-code-in-test-on-code
     (append
      (list
       (org #x00f7)
             (JMP TEST_ENTRY)
       (org #x00fa)
             (word #x010a) ;; copy 266 bytes
             (word #xAFFC) ;; from affc
             (word #xF1FA) ;; to f1fa
       (org #xa000)
       (label TEST_ENTRY)
              (JSR COPY_REGION)
              (BRK)

       (org #xaffc)
              (byte 1 2 3 4 5 6 7 8 9 10)
       (org #xb080)
              (byte 1 2 3 4 5 6 7 8 9 10)
       (org #xb0fc)
              (byte 1 2 3 4 5 6 7 8 9 10)
       (org #xc000))
      COPY_REGION)
     ))

  (check-equal? (memory-list copy-region-state-02 #xf1fa (+ #xf1fa 9))
                (list 1 2 3 4 5 6 7 8 9 10))
  (check-equal? (memory-list copy-region-state-02 #xf27e (+ #xf27e 9))
                (list 1 2 3 4 5 6 7 8 9 10))
  (check-equal? (memory-list copy-region-state-02 #xf2fa (+ #xf2fa 9))
                (list 1 2 3 4 5 6 7 8 9 10)))

(module+ test #| program |#
  (require "../ast/6510-assembler.rkt")
  (require "../tools/6510-prg-generator.rkt")

  (define orgloc 2064)

  (define program
    (append
     (list
      (org #x0810)
      (label START)
             (LDA !<PAYLOAD)
             (STA $FC)
             (LDA !>PAYLOAD)
             (STA $FD)
             (LDA !$00)
             (STA $FE)
             (LDA !$C0)
             (STA $FF)
             (LDA !25)
             (STA $FA)
             (LDA !$00)
             (STA $FB)
             (JSR COPY_REGION)
             (JMP $C000))
     COPY_REGION
     (list
      (label PAYLOAD)
      (org #xc000)
             (LDY STRING_DATA)
      (label LOOP_STRING)
             (LDA STRING_DATA,y)
             (JSR $FFD2)
             (DEY)
             (BNE LOOP_STRING)
             (RTS)
      (label STRING_DATA)
             (byte 12)
             (asc "!DLROW OLLEH")) ))

  (define raw-bytes
    (apply append
           (map cdr
                (assembly-code-list-org-code-sequences
                 (new-assemble-to-code-list program)))))

  (define generated-d64-program
    (begin
      (create-prg raw-bytes orgloc "loader.prg")
      (create-image-with-program raw-bytes orgloc "loader.prg" "loader.d64" "."))))

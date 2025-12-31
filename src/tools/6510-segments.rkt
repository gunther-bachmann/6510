#lang racket/base
#|
  TODO: think about reducing dependencies (currently hardcoded based on #x0080 location!
        vm_interpreter_zp --(VM_OPERATIONS_TABLE)-->
                          <-(VM_ZP_PC)
                          <-(VM_INTERPRETER)
                          <-(VM_INTERPRETER_INC_PC)
|#

#|

define code/data segments for a c64 program that are used for generating a loader/linker

currently the following test programs are created
  bc-tramp:
  - loading and initializing byte code interpreter
  - executing a simple byte code routine
  - printing A in the upper left corner of the text screen
  - cursor etc. is garbled since zero page is used in non intended ways!
  - return to basic (although reset is necessary)

  loader:
  - use code relocator for loading native code
  - print hello world, return to basic

  loader2:
  - use code relocator for loading native code
  - resolve labels within each relocated section
  - print out 0HELLO! (from one section)
  - print out 1HI! (from another section)

  trampoline:
  - use trampoline loading code
  - use two sections, one printing 0HELLO!
  - the other 1HI! and referencing the first section routine in its copied to location
  => output is: 0HELLO!1HI!0HELLO! then returning back to BASIC

|#

(require (only-in racket/contract/base
                  struct-guard/c
                  and/c
                  list/c
                  listof)
         (only-in racket/list flatten empty? take)
         "../6510.rkt"
         (only-in "../ast/6510-resolver.rkt"
                  add-label-suffix))

(provide (struct-out c64-segment)
         (struct-out c64-relocation-info)
         (struct-out c64-resolution-info)
         (struct-out c64-resolution-symbols)
         width?
         segment-type?
         resolution-type?)

(module+ test
  (require  racket/hash
           "../6510-test-utils.rkt"
           "../ast/6510-assembler.rkt"
           (only-in "../nmil/vm-bc-opcode-definitions.rkt"
                    full-extended-optable-lb
                    full-extended-optable-hb
                    full-interpreter-opcode-table
                    bc)
           (only-in "../nmil/vm-interpreter-loop.rkt"
                    VM_INTERPRETER_ZP)
           (only-in "../nmil/vm-interpreter.rkt"
                    just-vm-interpreter)
           (only-in "../nmil/vm-runtime/vm-memory-manager.rkt"
                    vm-memory-manager-code)
           (only-in  "../nmil/vm-runtime/vm-memory-manager-test-utils.rkt"
                     list-with-label-suffix
                     run-code-in-test-on-code)
           ;; (only-in "../nmil/vm-runtime/vm-pages.rkt"
           ;;          VM_INITIAL_MM_REGS
           ;;          VM_PAGE_SLOT_DATA)
           (only-in "../tools/6510-interpreter.rkt"
                    memory-list)
           "../tools/6510-prg-generator.rkt"))

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
           (listof c64-relocation-info?)
           (listof c64-resolution-info?)
           (listof c64-resolution-symbols?)
           word/c
           (listof byte?))
  )

;; placing/copying a segment somewhere into the code includes the following steps
;; - copy all bytes to the new location
;; - go over all relocation info, adjusting the references
;; - go over all resolution info, resolving references to other segments


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
  ;; load two code sections (located in the program loaded to 0800)
  ;; into c000 and 8000 that print out 0 and 1 (and some more) on the screen
  ;; absolute and relative references within these sections are resolved
  ;; with the target location in mind, copy descriptor is resolved,
  ;; using the src locations of the code to be copied
  (define (looped-copy-region-01 for-test)
      (append
       (if for-test
           (list)
           (list   (org #x0810)
                   (word-const CHAR_OUT $ffd2)
                   (JMP CTRL_SECTION)))
       LOOPED_COPY_REGION
       COPY_REGION
       (if for-test
           (list
                   (org #x0900)
            (label TEST_COUNTERS)
                   (byte 0)
            (label COPY_DESCRIPTOR)
                   (byte 27 00 00 #x10 00 #x80)
                   (byte 24 00 00 #x20 00 #xc0)
                   (byte 00 00))
           (flatten
            (list
            (label COPY_DESCRIPTOR)
                   (byte 27 00)
                   (byte-ref <SECTION_ONE)
                   (byte-ref >SECTION_ONE)
                   (byte 00 #x80)
                   (byte 24 00)
                   (byte-ref <SECTION_TWO)
                   (byte-ref >SECTION_TWO)
                   (byte 00 #xc0)
                   (byte 00 00))))
       (list
            (label TEST_ENTRY)
            (label CTRL_SECTION)
            ;; now copy sections to right place
                   (LDA !<COPY_DESCRIPTOR)
                   (LDX !>COPY_DESCRIPTOR)
                   (JSR LOOPED_COPY_REGION)

                   (JSR $8000)
                   (JSR $c000))
       (if for-test
           (list   (BRK)
                   (org #x1000))
           (list   (RTS)
                   (org #x8000) ;; this should only be active for resolving references within section on, not for using the byte reference in the COPY_DESCRIPTOR!
                   ))
       (list
            (label SECTION_ONE)
                   (LDA !48)
                   (JSR CHAR_OUT)
                   (LDY STRING1_MSG)
            (label LOOP_OUT_1)
                   (LDA STRING1_MSG,y)
                   (JSR CHAR_OUT)
                   (DEY)
                   (BNE LOOP_OUT_1)
                   (RTS)
            (label STRING1_MSG)
                   (byte 6)
                   (asc "!OLLEH"))
       (if for-test
           (list   (org #x2000))
           (list   (org #xc000)
                   ))
       (list
            (label SECTION_TWO)  ;; this should only be active for resolving references within section on, not for using the byte reference in the COPY_DESCRIPTOR!
                   (LDA !49)
                   (JSR CHAR_OUT)
                   (LDY STRING2_MSG)
            (label LOOP_OUT_2)
                   (LDA STRING2_MSG,y)
                   (JSR CHAR_OUT)
                   (DEY)
                   (BNE LOOP_OUT_2)
                   (RTS)
            (label STRING2_MSG)
                   (byte 3)
                   (asc "!IH"))))

  (define looped-copy-region-01t
    (run-code-in-test-on-code
     (apply
      list-with-label-suffix
      (looped-copy-region-01 #t)
      #:provide-own-test-entry-label #t
      #:org  (org #x0810)
      #:mock (list (label CHAR_OUT)))))

  (check-equal? (memory-list looped-copy-region-01t #x0900 #x0900)
                (list 11)
                "CHAR_OUT has been called eleven times!")
  (check-equal? (memory-list looped-copy-region-01t #x1000 #x1002)
                (list 169 48 32)
                "load and jump command were copied to the right location")
  (check-equal? (memory-list looped-copy-region-01t #x2000 #x2002)
                (list 169 49 32)
                "load and jump command were copied to the expected location")

  (define looped-copy-region-prg
    (looped-copy-region-01 #f))

  (define (remove-all-but-first-org-cmd ast-commands)
    (unless (empty? ast-commands)
      (append (list (car ast-commands))
              (filter (lambda (cmd) (not (ast-org-command? cmd))) (cdr ast-commands)))))

  ;; use first code segment, resolving all labels based in the first (and only that) org directives
  ;; use all other code segments, resolving them based on their respective org directives
  ;; this is necessary to create the copy descriptors correctly (located in first code segment)
  ;; and resolve references in the other code segments based on the requested target location
  ;; they are actually (finally) located at
  (define looped-copy-region-raw-bytes
    (let ([sections-as-loaded
           (cdar (assembly-code-list-org-code-sequences
                  (new-assemble-to-code-list (remove-all-but-first-org-cmd looped-copy-region-prg))))]
          [remaining-sections-located-right
            (flatten (map cdr (cdr (assembly-code-list-org-code-sequences
                                    (new-assemble-to-code-list looped-copy-region-prg)))))])
      (append
       (take  sections-as-loaded (- (length sections-as-loaded) (length remaining-sections-located-right)))
       remaining-sections-located-right)))

  (define basic-org-loc 2064)
  (define looped-copy-region-generated-d64-program
    (begin
      (create-prg looped-copy-region-raw-bytes basic-org-loc "loader2.prg")
      (create-image-with-program looped-copy-region-raw-bytes basic-org-loc "loader2.prg" "loader2.d64" "loader2")))

  (define segment-1-assembly
    (new-assemble-to-code-list
     (list
             (org #x8000)

             (word-const CHAR_OUT $ffd2)

      (label SECTION_ONE)
             (LDA !48)
             (JSR CHAR_OUT)
             (LDY STRING1_MSG)
      (label LOOP_OUT_1)
             (LDA STRING1_MSG,y)
             (JSR CHAR_OUT)
             (DEY)
             (BNE LOOP_OUT_1)
             (RTS)
      (label STRING1_MSG)
             (byte 6)
             (asc "!OLLEH"))))

  (define raw-bytes-segment-1
    (cdar (assembly-code-list-org-code-sequences segment-1-assembly)))

  (define segment-2-assembly
    (new-assemble-to-code-list
     (list
             (org #xc000)

             (word-const CHAR_OUT $ffd2)

      (label SECTION_TWO)
             (LDA !49)
             (JSR CHAR_OUT)
             (LDY STRING2_MSG)
      (label LOOP_OUT_2)
             (LDA STRING2_MSG,y)
             (JSR CHAR_OUT)
             (DEY)
             (BNE LOOP_OUT_2)
             (JSR SECTION_ONE) ;; reference to final location of section1
             (RTS)
      (label STRING2_MSG)
             (byte 3)
             (asc "!IH"))

     (assembly-code-list-labels segment-1-assembly)))

  (define raw-bytes-segment-2
    (cdar (assembly-code-list-org-code-sequences segment-2-assembly)))

  (define segment-1-descriptor
    (segment->copy-descriptor
     (c64-segment 'pinned ;; type
                  #x8000  ;; location
                  '()       ;; reloc info
                  '()       ;; resolution info
                  '()       ;; resolution symbols
                  (length raw-bytes-segment-1)
                  raw-bytes-segment-1)
     "SECTION_ONE"))  ;; <- where do I get source location from, e.g. depends on number of segments? (could be a label)

  (define segment-2-descriptor
    (segment->copy-descriptor
     (c64-segment 'pinned ;; type
                  #xc000  ;; location
                  '()       ;; reloc info
                  '()       ;; resolution info
                  '()       ;; resolution symbols
                  (length raw-bytes-segment-2)
                  raw-bytes-segment-2)
     "SECTION_TWO"))

  (define trampoline
    (append
     (list
             (org #x0810)
      (label CALLED_FROM_BASIC)
             (LDA !<COPY_DESCRIPTOR)
             (LDX !>COPY_DESCRIPTOR)
             (JSR LOOPED_COPY_REGION)

             ;; add code to initialize memory manage and interpreter
             (JSR $8000)
             (JSR $c000)
             (RTS)

     ;; byte code could go here

      (label COPY_DESCRIPTOR))
     ;; make this section dynamic?
     segment-1-descriptor ;; <- use section one as parameter
     segment-2-descriptor
     (list (byte 0 0)) ;; end mark
     LOOPED_COPY_REGION
     COPY_REGION
     ;; make this section dynamic?
     (list (label "SECTION_ONE")
           (ast-bytes-cmd '() raw-bytes-segment-1)
           (label "SECTION_TWO")
           (ast-bytes-cmd '() raw-bytes-segment-2))))

  (define trampoline-code
    (cdar (assembly-code-list-org-code-sequences (new-assemble-to-code-list trampoline))))

  (define generated-trampoline
    (begin
      (create-prg trampoline-code #x0810 "trampoline.prg")
      (create-image-with-program trampoline-code #x0810 "trampoline.prg" "trampoline.d64" "trampoline")))

  ;; idea
  ;; @cdc0 mm-regs
  ;; (define mem-data
  ;;   (new-assemble-to-code-list (append (list (org #xcdc0) (byte-const ZP_VM_PC #x85)))))
  ;; (define raw-mem-data
  ;;   (cdar (assembly-code-list-org-code-sequences mem-data)))

  ;; @c000 runtime + memory management etc
  (define vm-runtime
    (new-assemble-to-code-list
     (append (list (org #x2800)
                   (byte-const ZP_VM_PC #x85) ;; #x80 + 5
                   )
             vm-memory-manager-code)
     ;; (assembly-code-list-labels mem-data)
     ))

  (define raw-vm-runtime
    (cdar
     (assembly-code-list-org-code-sequences
      vm-runtime)))

  (check-true (> (- #x3000 #x2800)
                 (length raw-vm-runtime))
              "vm runtime must fit into 2k")

  ;; @9000 just bc interpreter
  (define bc-interpreter
    (new-assemble-to-code-list
       (append (list (org #x2000))
               full-interpreter-opcode-table ;; aligned to xx00 (page aligned)
               (list ;; (org #x2100)
                     ;; (word-const VM_INTERPRETER_OPTABLE $ce00)
                     (word-const VM_INTERPRETER $0084)
                     (word-const VM_INTERPRETER_INC_PC $0080)
                     (byte-const ZP_VM_PC #x85) ;; #x80 + 5

                     (JSR VM_INTERPRETER_INIT_AX)
                     (JSR VM_INITIALIZE_MEMORY_MANAGER)
                     ;; (JSR VM_INITIALIZE_CALL_FRAME)
                     (JMP VM_INTERPRETER))
               just-vm-interpreter
               full-extended-optable-hb
               full-extended-optable-lb)
       (assembly-code-list-labels vm-runtime)))

  (define raw-bc-interpreter
    (cdar
     (assembly-code-list-org-code-sequences
      bc-interpreter)))

  (define zp-interpreter-loop
    (new-assemble-to-code-list
     (append VM_INTERPRETER_ZP)
     (assembly-code-list-labels bc-interpreter)))

  (define raw-zp-interpreter-loop
    (cdar
     (assembly-code-list-org-code-sequences
      zp-interpreter-loop)))

  (check-true (> (- #x2800 #x2000)
                 (length raw-bc-interpreter))
              "bc interpreter must fit into 1k")

  ;; @ce00
  ;; (define raw-bc-jump-table
  ;;   (cdar
  ;;    (assembly-code-list-org-code-sequences
  ;;     (new-assemble-to-code-list
  ;;      (append (list (org #xcdc0)) full-interpreter-opcode-table)
  ;;      (assembly-code-list-labels bc-interpreter) ;; (need to add labels collected by interpreter)
  ;;      ))))
  ;; @cf00

  (define (byte-code-loader byte-codes)
    (append
     (list
             (org #x0810)
      (label CALLED_FROM_BASIC)
             (LDA !<COPY_DESCRIPTOR)
             (LDX !>COPY_DESCRIPTOR)
             (JSR LOOPED_COPY_REGION)

             ;; add code to initialize memory manage and interpreter
             (LDA !<BC_START) ;; lb start of available memory
             (LDX !>BC_START) ;; hb  start of available memory
             (JMP $2100)      ;; initialize memory manager & interpreter & start interpreter

      (label BC_START)
             (ast-bytes-cmd '() byte-codes)

             (bc PUSH_B) (byte $ff)
             (bc PUSH_B) (byte $02)
             (bc BADD)
             (bc POKE_B) (byte $00 $04) ;; poke result $01 into $0400 (first character on screen)

             (bc NATIVE)

      ;;        (LDY STRING1_MSG)
      ;; (label LOOP_OUT_1)
      ;;        (LDA STRING1_MSG,y)
      ;;        (JSR $FFD2)
      ;;        (DEY)
      ;;        (BNE LOOP_OUT_1)
      ;;        (RTS)
      ;; (label STRING1_MSG)
      ;;        (byte 6)
      ;;        (asc "!OLLEH")

             (RTS)           ;; return to basic, but since zero page is used by interpreter, don't expect anything to work

      (label COPY_DESCRIPTOR)) ;; all data following here is discarded when interpreter is started and regarded as free
      ;; use (bc interpreter) in 9000-9fff (4k)
      ;; use (memory manager) in c000-cfff (4k)
     (segment->copy-descriptor
      (c64-segment 'pinned ;; type
                   #x0080  ;; location
                   '()       ;; reloc info
                   '()       ;; resolution info
                   '()       ;; resolution symbols
                   (length raw-zp-interpreter-loop)
                   raw-zp-interpreter-loop)
      "SECTION_ZI")
     (segment->copy-descriptor
      (c64-segment 'pinned ;; type
                   #x2000  ;; location
                   '()       ;; reloc info
                   '()       ;; resolution info
                   '()       ;; resolution symbols
                   (length raw-bc-interpreter)
                   raw-bc-interpreter)
      "SECTION_BC")
     (segment->copy-descriptor
      (c64-segment 'pinned ;; type
                   #x2800  ;; location
                   '()       ;; reloc info
                   '()       ;; resolution info
                   '()       ;; resolution symbols
                   (length raw-vm-runtime)
                   raw-vm-runtime)
      "SECTION_RT")
     ;; (segment->copy-descriptor
     ;;  (c64-segment 'pinned ;; type
     ;;               #xcdc0  ;; location
     ;;               '()       ;; reloc info
     ;;               '()       ;; resolution info
     ;;               '()       ;; resolution symbols
     ;;               (length raw-mem-data)
     ;;               raw-mem-data)
     ;;  "SECTION_MD")
     ;; (segment->copy-descriptor
     ;;  (c64-segment 'pinned ;; type
     ;;               #xce00  ;; location
     ;;               '()       ;; reloc info
     ;;               '()       ;; resolution info
     ;;               '()       ;; resolution symbols
     ;;               (length raw-bc-jump-table)
     ;;               raw-bc-jump-table)
     ;;  "SECTION_BCJT")
     (list (byte 0 0)) ;; end mark
     LOOPED_COPY_REGION
     COPY_REGION
     (list (label "SECTION_ZI")
           (ast-bytes-cmd '() raw-zp-interpreter-loop)
           (label "SECTION_BC")
           (ast-bytes-cmd '() raw-bc-interpreter) ;; <-- this part is too large to fit into c000-#xcdbf => split byte codes
           (label "SECTION_RT")
           (ast-bytes-cmd '() raw-vm-runtime)
           ;; (label "SECTION_MD")
           ;; (ast-bytes-cmd '() raw-mem-data)
           ;; (label "SECTION_BCJT")
           ;; (ast-bytes-cmd '() raw-bc-jump-table) ;;
           )))

  (define bc-code-trampoline
    (cdar
     (assembly-code-list-org-code-sequences
      (new-assemble-to-code-list
       (byte-code-loader (list))))))

  (define generated-bc-trampoline
    (begin
      (create-prg bc-code-trampoline #x0810 "bc-tramp.prg")
      (create-image-with-program bc-code-trampoline #x0810 "bc-tramp.prg" "bc-tramp.d64" "bc-tramp"))))


;; create a copy descriptor for a segment as ast commands (to be resolved by assembler)
(define (segment->copy-descriptor seg label-str)
  (list
   (ast-bytes-cmd
    '()
    (list (low-byte (c64-segment--length seg))
          (high-byte (c64-segment--length seg))))
   (ast-unresolved-bytes-cmd
    '() '()
    (ast-resolve-byte-scmd label-str 'low-byte))
   (ast-unresolved-bytes-cmd
    '() '()
    (ast-resolve-byte-scmd label-str 'high-byte))
   (ast-bytes-cmd
    '()
    (list (low-byte (c64-segment--location seg))
          (high-byte (c64-segment--location seg))))))

;; create a c64 prg basic loader for all segments
(define (segments->prg segments prg-name)
  '())

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
  (define copy-region-state
    (run-code-in-test-on-code
     (append
      (list
       (org #x00f7)
             (JMP NTEST_ENTRY)
       (org #x00fa)
             (word #x000a) ;; copy 10 bytes
             (word #xB000) ;; from b000
             (word #xF1FA) ;; to f1fa
       (org #xa000)
       (label NTEST_ENTRY)
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
             (JMP NTEST_ENTRY)
       (org #x00fa)
             (word #x010a) ;; copy 266 bytes
             (word #xAFFC) ;; from affc
             (word #xF1FA) ;; to f1fa
       (org #xa000)
       (label NTEST_ENTRY)
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

(module+ test #| loader.prg |#
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


;; TODO: now implement transforming segment definitions -> code
;; TODO: define segments for current virtual machine implementation of VM
;; TODO: define bytecode to run for VM to execute

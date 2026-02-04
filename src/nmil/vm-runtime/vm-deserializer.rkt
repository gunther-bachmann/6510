#lang racket/base

(provide)

#|

 serialize/deserialize data from heap <-> byte-stream

 ideas: encoding should not recurse but loop (no additional memory requirements)
        decoding should not recurse but loop (no additional memory requirements)

 encoding

 - native array  1000 0001 llll llll l*bytes

 open (not implemented)
 - byte          0000 0001 bbbb bbbb
 - int           iiii ii11 iiii iiii
 - bcd           01cc cc01 bbbb aaaa
 - cell-array    1000 0101 llll llll l*cells (encoded as written here)
 - ptr           1ppp ppp0 oooo oooo reference within encoding to page and offset (page=0..63) => max 16k data
 - nil-ptr       0000 0000


 |#


(require "../../6510.rkt"
         (only-in "../vm-definition-utils.rkt"
                  define-vm-function
                  define-vm-function-wol))

(module+ test #| require |#
  (require (only-in racket/string
                    string-replace)
           (only-in uuid
                    uuid-string)
           "../../6510-test-utils.rkt"
           (only-in "../../ast/6510-relocator.rkt"
                    estimated-code-len)
           (only-in "../../tools/6510-interpreter.rkt"
                    cpu-state-clock-cycles
                    peek-word-at-address
                    memory-list
                    memory-list-)
           "./vm-memory-manager-test-utils.rkt"
           "../vm-inspector-utils.rkt"
           (only-in "./vm-memory-map.rkt"
                    VM_MEMORY_MANAGEMENT_CONSTANTS
                    ZP_RT
                    ZP_RA
                    ZP_RP
                    ZP_RZ)
           (only-in "./vm-memory-manager.rkt"
                    vm-memory-manager-code))

  (define PAGE_AVAIL_0 #xcb)
  (define PAGE_AVAIL_1 #xca)
  (define PAGE_AVAIL_1_W #xca00)

  (define start-of-free-test-memory #xb000) ;; used for data check and transfer during the test

  (define test-runtime
    (append
     DESERIAL_ADDY_TO_ZP
     VM_DESERIALIZE_NATIVE_ARRAY
     VM_SERIALIZE_NATIVE_ARRAY
     VM_DESERIALIZE_CELL_ARRAY

     (list (byte-const ZP_VM_PC #x85))
     VM_MEMORY_MANAGEMENT_CONSTANTS
     vm-memory-manager-code)))

;; NO DEC REFCOUNT ON ANYTHING THAT IS IN RA!
;; input:  ZP_RP pointer to memory to read from
;; modifies: ZP_TEMP + ALLOC_NATARR_TO_RA
;; output: ZP_RA reference to  native array read  (with refcount = 0, please inc if usage is wanted!)
(define-vm-function VM_DESERIALIZE_NATIVE_ARRAY
  (list
          (LDY !0)
          (LDA !$81) ;; serial tag byte fo native array
          (CMP (ZP_RP),y)
          (BNE ERROR__)
          (LDY !$01)
          (LDA (ZP_RP),y) ;; read len field
          (PHA)
          (JSR ALLOC_NATARR_TO_RA)

          ;; store pointer into original data (to resolve backreferences later)
          (LDA ZP_RA)
          (LDY !$00)
          (STA (ZP_RP),y)
          (LDA ZP_RA+1)
          (INY)
          (STA (ZP_RP),y)

          (PLA)         ;; get length
          (CLC)
          (ADC !$02)    ;; calc last index + 1
          (STA ZP_TEMP) ;; store last index + 1 for cpy in loop
          (INY)         ;; start of payload for both is @2

   (label LOOP__)
          (LDA (ZP_RP),y)
          (STA (ZP_RA),y)
          (INY)
          (CPY ZP_TEMP)
          (BNE LOOP__)

          (JMP DESERIAL_ADDY_TO_ZP)

   (label ERROR__)
          (BRK)))

(module+ test #| deserialize native array|#
  (define deserialize-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
            (LDA !<SERIALIZED_DATA)
            (STA ZP_RP)
            (LDA !>SERIALIZED_DATA)
            (STA ZP_RP+1)

            (JSR VM_DESERIALIZE_NATIVE_ARRAY)
            (BRK)

            (ast-org-command '() start-of-free-test-memory) ;; select some memory section that is unique for this test (for easy access in test)
     (label SERIALIZED_DATA)
            (byte $81)
            (byte $04)
            (byte $01 $02 $03 $04)
            (byte $05) ;; should not be copied, since native array has length 4
            ))

  (define deserialize-test-rt (peek-word-at-address deserialize-test ZP_RA))
  (check-equal? (vm-slot->string deserialize-test deserialize-test-rt)
                "ptr[0] native-array of len 4"
                "rt points to a native array of len 4")
  (check-equal? (memory-list- deserialize-test (+ 2 deserialize-test-rt) 5)
                (list 1 2 3 4 14)
                "native array content is 1 2 3 4, whereas14 is already the offset to the next free m1 slot")
  (check-equal? (memory-list- deserialize-test start-of-free-test-memory 2)
                (list (low-byte deserialize-test-rt) (high-byte deserialize-test-rt))
                "the original bytes are overwritten with the pointer of the allocated array")
  (check-equal? (memory-list- deserialize-test ZP_RP 2)
                (int->bytes (+ start-of-free-test-memory 6))))

;; input:  ZP_RA pointer to native array
;;         ZP_RP pointer to memory to write to
;; output: memory zp_rp points to is filled with serialized data
(define-vm-function VM_SERIALIZE_NATIVE_ARRAY
  (list
          (LDY !0)
          (LDA !$81) ;; serial tag byte fo native array
          (STA (ZP_RP),y)
          (INY)
          (LDA (ZP_RA),y) ;; read type/len field
          (AND !$7f)      ;; extract len
          (STA (ZP_RP),y)
          (CLC)
          (ADC !$01)
          (STA ZP_TEMP)
   (label LOOP__)
          (INY)
          (LDA (ZP_RA),y)
          (STA (ZP_RP),y)
          (CPY ZP_TEMP)
          (BNE LOOP__)

          (RTS)))

(module+ test #| serialize native array |#
  (define serialize-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
            (LDA !$04) ;; array len 4 bytes
            (JSR ALLOC_NATARR_TO_RA)
            (LDY ZP_RAI)

            ;; fill array with 4 values 1 2 3 4
            (LDA !$01)
      (label LOOP__)
            (STA (ZP_RA),y)
            (INY)
            (CLC)
            (ADC !$01)
            (CPY !$6)
            (BNE LOOP__)

            ;; now serialize
            (LDA !<SERIALIZED_DATA)
            (STA ZP_RP)
            (LDA !>SERIALIZED_DATA)
            (STA ZP_RP+1)

            (JSR VM_SERIALIZE_NATIVE_ARRAY)
            (BRK)

            (ast-org-command '() start-of-free-test-memory)
     (label SERIALIZED_DATA)
            (byte $00)
            (byte $00)
            (byte $00 $00 $00 $00)
            (byte $ff) ;; should not be overwritten, since native array has length 4
            ))

  (check-equal? (memory-list- serialize-test start-of-free-test-memory 7)
                (list #x81 #x04 1 2 3 4 #xff)
                "serialized native array follows format and does not overwrite ff marker at end"))

;; NO DEC REFCOUNT ON ANYTHING THAT IS IN RA!
;; input:  ZP_RP pointer to memory to read from
;;         ZP_RZ original start of deserialization for resolving ptrs
;; modifies: ZP_TEMP, ZP_TEMP2, ZP_TEMP3 + ALLOC_CELL_ARRAY_TO_RA
;; output: ZP_RA pointer to cell array (with refcount = 0, please inc if usage is wanted!)
(define-vm-function VM_DESERIALIZE_CELL_ARRAY
  (list
          (LDY !0)
          (LDA !$85) ;; serial tag for cell array
          (CMP (ZP_RP),y)
          (BNE ERROR__)
          (LDY !$01)
          (LDA (ZP_RP),y) ;; read len field
          (STA LOOP_COUNTER__)
          (JSR ALLOC_CELL_ARRAY_TO_RA)

          ;; store allocated array over mem for later referral
          (LDA ZP_RA)
          (LDY !$00)
          (STA (ZP_RP),y)
          (LDA ZP_RA+1)
          (INY)
          (STA (ZP_RP),y)

          ;; now loop over the cells and store them
          ;; since all ptrs can be resolved by reading from the offset w/i ZP_RP, no recursion required
          (LDY !$02) ;; start of payload in stream and in cell-array
   (label LOOP__)
          (LDA (ZP_RP),y) ;; tagged low byte
          (BEQ ATOM_CELL_NONLSRD__) ;; it is nil
          (LSR)
          (BCC POINTER_CELL__)
   (label ATOM_CELL__)
          (ROL) ;; restore original tagged byte
   (label ATOM_CELL_NONLSRD__)
          (STA (ZP_RA),y)
          (INY)
          (LDA (ZP_RP),y)
          (STA (ZP_RA),y)
          (INY)
          (BNE NEXT__) ;; always jump

   (label POINTER_CELL__)
          ;; A = 01pp pppp
          ;; these two bytes are page and offset into ZP_RZ, where the ptr to the allocated object actually is
          (PHA)
          (INY)
          (LDA (ZP_RP),y)        ;; offset
          (CLC)
          (ADC ZP_RZ)
          (STA ZP_TEMP)
          (PLA)
          (AND !$3f)            ;; now A = page offset to ZP_RZ
          (ADC ZP_RZ+1)         ;;
          (STA ZP_TEMP+1)       ;; now zp_temp points to the right
          (STY ZP_TEMP3)        ;; keep y (currently on high byte, because it got incremented)
          (LDY !$00)
          (LDA (ZP_TEMP),y)
          (TAX)                 ;; keep low byte in X
          (INY)
          (LDA (ZP_TEMP),y)     ;; keep high byte in A
          (LDY ZP_TEMP3)        ;; restore y for writing into ZP_RP

          ;; increase refcount of ptr used
          (STA inc_abs__+2)
   (label inc_abs__)
          (INC $cf00,x)         ;; ref-count because of this pointer usage

          ;; store ptr into cell-array
          (STA (ZP_RA),y)       ;; store high byte
          (TXA)                 ;; get low byte
          (DEY)                 ;; go back to low byte position
          (STA (ZP_RA),y)       ;; store low byte
          (INY)                 ;; move to next cell
          (INY)

   (label NEXT__)
          ;; decode next cell from ZP_RP
          (DEC LOOP_COUNTER__)
          (BNE LOOP__)
          (JMP DESERIAL_ADDY_TO_ZP)

   (label LOOP_COUNTER__)
          (byte $01)

   (label ERROR__)
          (BRK)))

(module+ test #| deserialize cell array |#
  (define deserialize-cell-array-test
    (compact-run-code-in-test-
     #:debug #f
     #:runtime-code test-runtime
            (LDA !<SERIALIZED_DATA)
            (STA ZP_RP)
            (STA ZP_RZ)
            (LDA !>SERIALIZED_DATA)
            (STA ZP_RP+1)
            (STA ZP_RZ+1)

            (JSR VM_DESERIALIZE_NATIVE_ARRAY)
            (JSR VM_DESERIALIZE_CELL_ARRAY)

            (BRK)

            (ast-org-command '() start-of-free-test-memory) ;; select some memory section that is unique for this test (for easy access in test)
     (label SERIALIZED_DATA)
            (byte $81) ;; native array
            (byte $04) ;; len 4
            (byte $01 $02 $03 $04)

            (byte $85) ;; cell array
            (byte $03) ;; len 3
            (byte $01 $05) ;; cell byte value 5
            (byte $80 $00) ;; cell ptr to offset 0 (where native array is)
            (byte $03 $20) ;; cell int value 32

            (byte $05) ;; should not be copied, since cell array has length 3
     ))

  (check-equal? (vm-rega->string deserialize-cell-array-test)
                (format "ptr[0] $~a02" (byte->hex-string PAGE_AVAIL_1))
                "reg ra points to first slot on page avail 1")
  (check-equal? (vm-slot->string deserialize-cell-array-test (+ PAGE_AVAIL_1_W 02))
                "ptr[0] cell-array of len 3"
                "reg ra points to slot with cell-array of len 3")
  (check-equal? (map (lambda (loc) (vm-cell-at->string deserialize-cell-array-test (+ PAGE_AVAIL_1_W loc)))
                     (list  04 06 08))
                (list "byte $05"
                      (format "ptr[1] $~a02" (byte->hex-string PAGE_AVAIL_0))
                      "int $0020")
                "cell-array has a byte, a pointer to a native array and an int"))

(define-vm-function DESERIAL_ADDY_TO_ZP
  (list
          (TYA)
          (CLC)
          (ADC ZP_RP)
          (STA ZP_RP)
          (BCC skip_page_inc__)
          (INC ZP_RP+1)
   (label skip_page_inc__)

          (LDA ZP_RA+1)
          (LDX ZP_RA)
          (STA dec_abs__+2)
   (label dec_abs__)
          (DEC $cf00,x)

          (RTS)))

(define vm-deserializer-code
  (append
   DESERIAL_ADDY_TO_ZP
   VM_DESERIALIZE_CELL_ARRAY
   VM_DESERIALIZE_NATIVE_ARRAY
   VM_SERIALIZE_NATIVE_ARRAY))

(module+ test #| code len |#
  (inform-check-equal? (estimated-code-len vm-deserializer-code)
                       216
                       "length of deserializer"))

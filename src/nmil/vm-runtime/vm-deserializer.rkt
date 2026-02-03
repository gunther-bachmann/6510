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
                    memory-list)
           "./vm-memory-manager-test-utils.rkt"
           (only-in "./vm-memory-map.rkt"
                    VM_MEMORY_MANAGEMENT_CONSTANTS
                    ZP_RT
                    ZP_RA
                    ZP_RP)
           (only-in "./vm-memory-manager.rkt"
                    vm-memory-manager-code))

  (define test-runtime
    (append
     VM_DESERIALIZE_NATIVE_ARRAY
     VM_SERIALIZE_NATIVE_ARRAY

     (list (byte-const ZP_VM_PC #x85))
     VM_MEMORY_MANAGEMENT_CONSTANTS
     vm-memory-manager-code)))

;; input:  ZP_RP pointer to memory to read from
;; output: ZP_RA reference to  native array read
(define-vm-function VM_DESERIALIZE_NATIVE_ARRAY
  (list
          (LDY !0)
          (LDA !$81) ;; serial tag byte fo native array
          (CMP (ZP_RP),y)
          (BNE ERROR__)
          (LDA ZP_RA)
          (BEQ CONT__)
          (JSR DEC_REFCNT_M1_SLOT_RA)
   (label CONT__)
          (LDY !$01)
          (LDA (ZP_RP),y) ;; read len field
          (PHA)
          (JSR ALLOC_NATARR_TO_RA)
          (PLA)
          (CLC)
          (ADC !$02)
          (STA ZP_TEMP) ;; index for last field + 1
          (LDY !$02)    ;; start of payload for both is @2
   (label LOOP__)
          (LDA (ZP_RP),y)
          (STA (ZP_RA),y)
          (INY)
          (CPY ZP_TEMP)
          (BNE LOOP__)

          (RTS)

   (label ERROR__)
          (BRK)))

(module+ test #| deserialize |#
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

     (label SERIALIZED_DATA)
            (byte $81)
            (byte $04)
            (byte $01 $02 $03 $04)
            (byte $05) ;; should not be copied, since native array has length 4
            ))

  (define deserialize-test-rt (peek-word-at-address deserialize-test ZP_RA))
  (check-equal? (memory-list deserialize-test (+ 1 deserialize-test-rt) (+ 6 deserialize-test-rt))
                (list #b10000100 1 2 3 4 14)))

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

(module+ test #| deserialize |#
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

            (org #xb000)
     (label SERIALIZED_DATA)
            (byte $00)
            (byte $00)
            (byte $00 $00 $00 $00)
            (byte $ff) ;; should not be overwritten, since native array has length 4
            ))

  (check-equal? (memory-list serialize-test #xb000 #xb006)
                (list #x81 #x04 1 2 3 4 #xff)))

(define vm-deserializer-code
  (append
   VM_DESERIALIZE_NATIVE_ARRAY
   VM_SERIALIZE_NATIVE_ARRAY))

(module+ test #| code len |#
  (inform-check-equal? (estimated-code-len vm-deserializer-code)
                       75
                       "length of deserializer"))

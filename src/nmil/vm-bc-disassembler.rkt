#lang racket/base

#|

disassembler for byte code

|#

(require (only-in "../util.rkt" bytes->int format-hex-byte format-hex-word))
(require (only-in "../tools/6510-disassembler.rkt" info-for-label))

(provide disassembler-byte-code--byte-count
         disassemble-byte-code)

;; get count of bytes belonging to the given bc command
(define (disassembler-byte-code--byte-count bc (bc_p1 0))
  (cond [(memq bc (list #x68                ;; call
                        #x0c)) 3]           ;; push int
        [(memq bc (list #x18                ;; branch on true,
                        #x1a                ;; branch on false,
                        #x2e                ;; push byte
                        #x3a                ;; branch if not zero
                        #x5e                ;; branch if  zero
                        #x78                ;; goto
                        #xa8)) 2]           ;; BC_DEC_RBI_NZ_P_BRA
        [(= bc #x04)                        ;; ext command
         (cond [else 2])]                   ;; default is 2 bytes (for ext command)
        [else 1]))                          ;; default is 1 byte (for regular byte code command)

;; return disassembled string for bc (and byte 1, byte 2 thereafter)
(define (disassemble-byte-code bc (bc_p1 0) (bc_p2 0) #:labels (labels (hash)))
  (cond
    [(= bc #x00) "push l0"]
    [(= bc #x02) "push l1"]
    [(= bc #x04) "push l2"]
    [(= bc #x06) "push l3"]
    [(= bc #x08) ;; ext command
     (cond
       [(= bc_p1 #x01) "x max int"]
       [(= bc_p1 #x02) "x inc int"]
       [(= bc_p1 #x03) "x gc free-list"]
       [else "x unknown"])]
    [(= bc #x0a) "reserved"]
    [(= bc #x0c) (format "push int ~a" (format-hex-word (bytes->int bc_p1 bc_p2)))]
    [(= bc #x0e) "int?"]
    [(= bc #x10) "write l0"]
    [(= bc #x12) "write l1"]
    [(= bc #x14) "write l2"]
    [(= bc #x16) "write l3"]
    [(= bc #x18)
     (format "branch on true? by $~a" (format-hex-byte bc_p1))]
    [(= bc #x1a)
     (format "branch on false? by $~a" (format-hex-byte bc_p1))]
    [(= bc #x1c) "ret on false?"]
    [(= bc #x1e) "dup"]
    [(= bc #x20) "pop to l0"]
    [(= bc #x22) "pop to l1"]
    [(= bc #x24) "pop to l2"]
    [(= bc #x26) "pop to l3"]
    [(= bc #x28) "push nil"]
    [(= bc #x2a) "push array field"]
    [(= bc #x2c) "pop to array field"]
    [(= bc #x2e) (format "push byte $~a" (format-hex-byte bc_p1))]
    [(= bc #x30) "write to l0"]
    [(= bc #x32) "write to l1"]
    [(= bc #x34) "write to l2"]
    [(= bc #x36) "write to l3"]
    [(= bc #x38) "byte inc"]
    [(= bc #x3a) (format "not zero? -> branch by ~a" (format-hex-byte bc_p1))]
    [(= bc #x40) "reserved"]
    [(= bc #x42) "nil?"]
    [(= bc #x44) "int 0?"]
    [(= bc #x46) "byte add"]
    [(= bc #x48) "byte > ?"]
    [(= bc #x4a) "native"]
    [(= bc #x50) "reserved"]
    [(= bc #x52) "cddr"]
    [(= bc #x54) "break"]
    [(= bc #x56) "swap"]
    [(= bc #x58) "pop"]
    [(= bc #x5a) "pair?"]
    [(= bc #x5c) "true? -> ret"]
    [(= bc #x5e) (format "zerp? -> branch ~a" (format-hex-byte bc_p1))]
    [(= bc #x60) "set array field 0"]
    [(= bc #x62) "set array field 1"]
    [(= bc #x64) "set array field 2"]
    [(= bc #x66) "set array field 3"]
    [(= bc #x68) (format "call ~a  ~a" (format-hex-word (add1 (bytes->int bc_p1 bc_p2))) (info-for-label (number->string (bytes->int bc_p1 bc_p2) 16) labels))] ;; add 1 because byte code starts there (after #locals)
    [(= bc #x6a) "tail call"]
    [(= bc #x6c) "byte dec"]
    [(= bc #x6e) "cons"]
    [(= bc #x70) "push int 0"]
    [(= bc #x72) "push int 1"]
    [(= bc #x74) "push int 2"]
    [(= bc #x76) "push int -1"]
    [(= bc #x78) (format "goto ~a" (format-hex-byte bc_p1))]
    [(= bc #x7a) "ret"]
    [(= bc #x7c) "nop"]
    [(= bc #x7e) "cdr"]
    [(= bc #x80) "zero? ret and pop 1"]
    [(= bc #x82) "zero? ret and pop 2"]
    [(= bc #x84) "zero? ret and pop 3"]
    [(= bc #x86) "zero? ret and pop 4"]
    [(= bc #x88) "coons"]
    [(= bc #x8a) "swap RA<->RB"]
    [(= bc #x8c) "pop to rb"]
    [(= bc #x8e) "push ra"]
    [(= bc #x90) "set (ra),0"]
    [(= bc #x90) "set (ra),1"]
    [(= bc #x90) "set (ra),2"]
    [(= bc #x90) "set (ra),3"]
    [(= bc #x98) "alloc array to ra"]
    [(= bc #x9a) "push (ra),rai"]
    [(= bc #x9c) "pop to (ra),rai"]
    [(= bc #x9e) "pop byte to rai"]
    [(= bc #xa0) "push (car l0) "]
    [(= bc #xa2) "push (car l1) "]
    [(= bc #xa4) "push (car l2) "]
    [(= bc #xa6) "push (car l3) "]
    [(= bc #xa8) "dec rbi, not zero? -> branch ~a" (format-hex-byte bc_p1)]
    [(= bc #xaa) "write ra"]
    [(= bc #xac) "write to rai"]
    [(= bc #xae) "dec rai"]
    [(= bc #xb0) "nil? -> ret l0 pop 1"]
    [(= bc #xb2) "nil? -> ret l0 pop 2"]
    [(= bc #xb4) "nil? -> ret l0 pop 3"]
    [(= bc #xb6) "nil? -> ret l0 pop 4"]
    [(= bc #xb8) "write to rbi"]
    [(= bc #xba) "car"]
    [(= bc #xbc) "int -"]
    [(= bc #xbe) "int +"]
    [(= bc #xc0) "not zero? ret pop 0"]
    [(= bc #xc2) "not zero? ret pop 1"]
    [(= bc #xc4) "not zero? ret pop 2"]
    [(= bc #xc6) "not zero? ret pop 3"]
    [(= bc #xc8) "int > ?"]
    [(= bc #xca) "inc rai"]
    [(= bc #xcc) "byte < ?"]
    [(= bc #xce) "pop to ra"]
    [(= bc #xd0) "push (cdr l0)"]
    [(= bc #xd2) "push (cdr l1)"]
    [(= bc #xd4) "push (cdr l2)"]
    [(= bc #xd6) "push (cdr l3)"]
    [(= bc #xd8) "reserved"]
    [(= bc #xda) "reserved"]
    [(= bc #xdc) "reserved"]
    [(= bc #xde) "reserved"]
    [(= bc #xe0) "caar"]
    [(= bc #xe2) "reserved"]
    [(= bc #xe4) "reserved"]
    [(= bc #xe6) "cadr"]
    [(= bc #xe8) "reserved"]
    [(= bc #xea) "reserved"]
    [(= bc #xec) "cdar"]
    [(= bc #xee) "reserved"]
    [(= bc #xf0) "get array field 0"]
    [(= bc #xf2) "get array field 1"]
    [(= bc #xf4) "get array field 2"]
    [(= bc #xf6) "get array field 3"]
    [(= bc #xf8) "get (ra),0"]
    [(= bc #xaa) "get (ra),1"]
    [(= bc #xfc) "get (ra),2"]
    [(= bc #xfe) "get (ra),3"]
    [else "unknown bc"]))

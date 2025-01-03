#lang racket/base

(require (only-in "../util.rkt" bytes->int format-hex-byte format-hex-word))

(provide disassembler-byte-code--byte-count
         disassemble-byte-code)

;; get count of bytes belonging to the given bc command
(define (disassembler-byte-code--byte-count bc)
  (cond [(memq bc (list #x34)) 3] ;; call
        [(memq bc (list #x0c #x0d)) 2] ;; branch on true
        [else 1]))

;; return disassembled string for bc (and byte 1, byte 2 thereafter)
(define (disassemble-byte-code bc bc_p1 bc_p2)
  (define byte-code-t2 (arithmetic-shift (if (> bc 127) (bitwise-and #x78 bc) (bitwise-and #x7f bc)) 1))
  (cond
    [(= byte-code-t2 #x00)
     (define n (arithmetic-shift (bitwise-and #x6 bc) -1))
     (if (= 1 (bitwise-and bc #x01))
         (format "write from local #~a" n)
         (format "push local #~a" n))]
    [(= byte-code-t2 #x02) "nop"]
    [(= byte-code-t2 #x04) "brk"]
    [(= byte-code-t2 #x06) "swap"]
    [(= byte-code-t2 #x0e) "int?"]
    [(= byte-code-t2 #x12) "push nil"]
    [(= byte-code-t2 #x14) "pair?"]
    [(= byte-code-t2 #x16) "ret on true?"]
    [(= byte-code-t2 #x18)
     (format "branch on true? by $~a" (format-hex-byte bc_p1))]
    [(= byte-code-t2 #x1a)
     (format "branch on false? by $~a" (format-hex-byte bc_p1))]
    [(= byte-code-t2 #x20)
     (define n (arithmetic-shift (bitwise-and #x6 bc) -1))
     (if (= 1 (bitwise-and bc #x01))
         (format "write to local #~a" n)
         (format "pop to local #~a" n))]
    [(= byte-code-t2 #x30)
     (define n (arithmetic-shift (bitwise-and #x6 bc) -1))
     (if (= 1 (bitwise-and bc #x01))
         (format "nil? ret param #~a" n)
         (format "nil? ret local #~a" n))]
    [(= byte-code-t2 #x42) "nil?"]
    [(= byte-code-t2 #x66) "return"]
    [(= byte-code-t2 #x68) (format "call $~a" (format-hex-word (bytes->int (+ 1 bc_p1) bc_p2)))] ;; add 2 because byte code starts there
    [(= byte-code-t2 #x6a) "tail call"]
    [(= byte-code-t2 #x70)
     (define n (bitwise-and bc #x03))
     (define is-int (= 0 (bitwise-and bc #x04)))
     (define num-str
       (cond [(= n 0) "0"]
             [(= n 1) "1"]
             [(= n 2) "2"]
             [(= n 3) "-1"]
             [else (raise-user-error "unexpected number encoding")]))
     (format "push ~a ~a" (if is-int "int" "byte") num-str)]
    [(= byte-code-t2 #x82) "cdr"]
    [(= byte-code-t2 #x84) "cons"]
    [(= byte-code-t2 #x86) "car"]
    [else "unknown bc"]))

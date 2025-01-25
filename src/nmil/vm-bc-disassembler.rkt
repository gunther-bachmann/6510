#lang racket/base

(require (only-in "../util.rkt" bytes->int format-hex-byte format-hex-word))

(provide disassembler-byte-code--byte-count
         disassemble-byte-code)

;; get count of bytes belonging to the given bc command
(define (disassembler-byte-code--byte-count bc (bc_p1 0))
  (cond [(memq bc (list #x34                ;; call
                        #x06)) 3]           ;; push int
        [(memq bc (list #x0c                ;; branch on true, 
                        #x0d                ;; branch on false,
                        #x32                ;; goto
                        #x05)) 2]           ;; push byte
        [(= bc #x04)                        ;; ext command
         (cond [else 2])]                   ;; default is 2 bytes (for ext command)
        [else 1]))                          ;; default is 1 byte (for regular byte code command)

;; return disassembled string for bc (and byte 1, byte 2 thereafter)
(define (disassemble-byte-code bc (bc_p1 0) (bc_p2 0))
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
    [(= byte-code-t2 #x08)
     (cond
       [(= bc_p1 #x01) "x max int"]
       [(= bc_p1 #x02) "x inc int"]
       [(= bc_p1 #x03) "x gc free-list"]
       [else "x unknown"])]
    [(= byte-code-t2 #x0a) (format "push byte $~a" (format-hex-byte bc_p1))]
    [(= byte-code-t2 #x0c) (format "push int $~a" (format-hex-word (bytes->int bc_p1 bc_p2)))]
    [(= byte-code-t2 #x0e) "int?"]
    [(= byte-code-t2 #x12) "push nil"]
    [(= byte-code-t2 #x14) "pair?"]
    [(= byte-code-t2 #x16) "ret on true?"]
    [(= byte-code-t2 #x18)
     (format "branch on true? by $~a" (format-hex-byte bc_p1))]
    [(= byte-code-t2 #x1a)
     (format "branch on false? by $~a" (format-hex-byte bc_p1))]
    [(= byte-code-t2 #x1c) "ret on false?"]
    [(= byte-code-t2 #x1e) "dup"]
    [(= byte-code-t2 #x20)
     (define n (arithmetic-shift (bitwise-and #x6 bc) -1))
     (if (= 1 (bitwise-and bc #x01))
         (format "write to local #~a" n)
         (format "pop to local #~a" n))]
    [(= byte-code-t2 #x22) "pop"]
    [(= byte-code-t2 #x30)
     (define n (arithmetic-shift (bitwise-and #x6 bc) -1))
     (if (= 1 (bitwise-and bc #x01))
         (format "nil? ret param #~a" n)
         (format "nil? ret local #~a" n))]
    [(= byte-code-t2 #x40)
     (define n (arithmetic-shift (bitwise-and #x0e bc) -1))
     (if (= 1 (bitwise-and bc #x01))
         (format "push local #~a, cdr" n)
         (format "push local #~a, car" n))]
    [(= byte-code-t2 #x42) "nil?"]
    [(= byte-code-t2 #x44) "int 0?"]
    [(= byte-code-t2 #x50)
     (define n (bitwise-and #x07 bc))
     (case n
       [(0) "caar"]
       [(1) "coons"]
       [(2) "cadr"]
       [(3) "condr"]
       [(4) "cdar"]
       [(5) "conar"]
       [(6) "cddr"]
       [(7) "unknown"])]
    [(= byte-code-t2 #x64) (format "goto $~a" (format-hex-byte bc_p1))]
    [(= byte-code-t2 #x66) "return"]
    [(= byte-code-t2 #x68) (format "call $~a" (format-hex-word (add1 (bytes->int bc_p1 bc_p2))))] ;; add 1 because byte code starts there (after #locals)
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
    [(= byte-code-t2 #x88) "coons"]
    [(= byte-code-t2 #xc2) "int -"]
    [(= byte-code-t2 #xc4) "int +"]
    [(= byte-code-t2 #xc6) "int > ?"]
    [else "unknown bc"]))

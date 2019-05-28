#lang racket

(require racket/fixnum)
(require "6510-utils.rkt")

(module+ test
  (require rackunit))

(provide cpu-state initialize-cpu peek poke run set-pc-in-state 6510-load)

(struct cpu-state (program-counter flags memory accumulator x-index y-index stack-pointer))

(define (initialize-cpu)
  (cpu-state 0 0 (make-fxvector 65536) 0 0 0 #xff))

(define (peek state memory-address)
  (fxvector-ref (cpu-state-memory state) memory-address))

(define (poke state address value)
  (fxvector-set! (cpu-state-memory state) address (byte value))
  state)

(module+ test
  (check-match (peek (poke (initialize-cpu) #xc000 17) #xc000)
               17))

(define (6510-load state memory-address program)
  (map (lambda (pair) (fxvector-set! (cpu-state-memory state) (first pair) (last pair)))
       (map list
            (sequence->list (in-range memory-address (+ memory-address (length program))))
            program))
  state)

(define (peek-pc state)
  (peek state (cpu-state-program-counter state)))

(define (peek-pc+1 state)
  (peek state (+ 1 (cpu-state-program-counter state))))

(define (peek-pc+2 state)
  (peek state (+ 2 (cpu-state-program-counter state))))

(define (set-pc-in-state state pc)
  (struct-copy cpu-state state [program-counter (word pc)]))

(define (run state)
  (if (not (eq? 0 (peek-pc state)))
      (run (execute-cpu-step state))
      state))

(define (interpret-rts state)
  (let* ([sp (cpu-state-stack-pointer state)]
         [low-ret (peek state (+ #x100 (byte (+ 2 sp))))]
         [high-ret (peek state (+ #x100 (byte (+ 1 sp))))]
         [new-state (struct-copy cpu-state state
                                 [program-counter (word (+ 1 (absolute high-ret low-ret)))]
                                 [stack-pointer (byte (+ sp 2))])])
    new-state))

(define (interpret-jsr-abs high low state)
  (case (absolute high low)
    [(#xFFD2) (display (string (integer->char (cpu-state-accumulator state))))
              (struct-copy cpu-state state [program-counter (word (+ 3 (cpu-state-program-counter state)))])]
    [else (let* ([new-program-counter (absolute high low)]
                 [return-address (+ 2 (cpu-state-program-counter state))]
                 [sp (cpu-state-stack-pointer state)]
                 [new-state (struct-copy cpu-state state
                                         [program-counter (word new-program-counter)]
                                         [stack-pointer (byte (- sp 2))])])
            (poke new-state (+ #x100 sp) (low-byte return-address))
            (poke new-state (+ #x100 (byte (- sp 1))) (high-byte return-address))
            new-state)])) ;; TODO: put return address onto the stack and jump to address

(define (interpret-jmp-abs high low state)
  (struct-copy cpu-state state
               [program-counter (word (absolute high low))]))

(define (interpret-adc-i immediate state)
  (let* ([old-accumulator (cpu-state-accumulator state)]
         [intermediate-accumulator (+ immediate old-accumulator)]
         [new-accumulator (if (carry-flag? state) (+ 1 intermediate-accumulator) intermediate-accumulator)]
         [overflow        (> (+ immediate old-accumulator) 255 )])
    (struct-copy cpu-state state
                 [program-counter (word (+ 2 (cpu-state-program-counter state)))]
                 [flags           (if overflow
                                      (-set-carry-flag (cpu-state-flags state))
                                      (cpu-state-flags state))]
                 [accumulator     (byte new-accumulator)])))

(define (interpret-lda-i immediate state)
  (struct-copy cpu-state state
               [program-counter (word (+ 2 (cpu-state-program-counter state)))]
               [accumulator     (byte immediate)]))

(define (carry-flag? state)
  (eq? 1 (bitwise-and 1 (cpu-state-flags state))))

(define (-set-carry-flag flags)
  (bitwise-xor 1 flags))

(define (-clear-carry-flag flags)
  (bitwise-and #xfe flags))

(define (set-carry-flag state)
  (struct-copy cpu-state state [flags (-set-carry-flag (cpu-state-flags state))]))

(define (print-state state)
  (printf "A = ~a~n" (cpu-state-accumulator state))
  (printf "X = ~a~n" (cpu-state-x-index state))
  (printf "Y = ~a~n" (cpu-state-y-index state))
  (printf "SP = ~a~n" (cpu-state-stack-pointer state))
  (printf "PC = ~a~n" (cpu-state-program-counter state))
  (printf "C = ~s" (if (carry-flag? state) "X" " " )))

(define (execute-cpu-step state)
  (case (peek-pc state)
    [(#x20) (interpret-jsr-abs (peek-pc+2 state) (peek-pc+1 state) state)]
    [(#x4C) (interpret-jmp-abs (peek-pc+2 state) (peek-pc+1 state) state)]
    [(#x60) (interpret-rts state)]
    [(#x69) (interpret-adc-i (peek-pc+1 state) state)]
    [(#xA9) (interpret-lda-i (peek-pc+1 state) state)]
    [else state]))

(module+ test
  (check-eq? (peek (6510-load (initialize-cpu) 10 (list #x00 #x10 #x00 #x11)) 11)
             16
             "immediate operand 1 is $10 = 16")

  (check-true (carry-flag? (set-carry-flag (initialize-cpu))) "after setting, carry is set")

  (check-false (carry-flag? (initialize-cpu)) "carry initially clear")

  (check-eq? (cpu-state-accumulator (execute-cpu-step (execute-cpu-step (6510-load (initialize-cpu) 0 (list #x69 #x80 #x69 #x81)))))
             1
             "accumulator should be 1 after adding 128 and 129 (overflow)")

  (check-true (carry-flag? (execute-cpu-step (execute-cpu-step (6510-load (initialize-cpu) 0 (list #x69 #x80 #x69 #x81)))))
              "carry should be set after adding 128 and 129"))

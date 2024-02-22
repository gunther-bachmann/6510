#lang racket

#|
  definition of the intermediate language machine
  |#

(require (only-in data/pvector pvector? make-pvector))
(require (only-in data/collection nth set-nth))
(require (only-in threading ~>>))

(module+ test #| require test utilities |#
  (require "../6510-test-utils.rkt"))

(struct il-cell () #:transparent)

(struct il-byte-cell il-cell
  (native-byte)
  #:transparent
  #:guard (struct-guard/c byte?))

;; generic command cell, may never be instantiated
(struct il-command-cell il-cell
  ()
  #:transparent)

;; push a native byte into the eval stack
(struct il-push-native-byte-cmd il-command-cell
  (byte)
  #:transparent
  #:guard (struct-guard/c byte?))

;; pop from the eval stack
(struct il-pop-cmd il-command-cell
  ()
  #:transparent)

;; stop execution of the vm
(struct il-break-cmd il-command-cell
  ()
  #:transparent)

(struct il-cell-stack
  (cells)
  #:transparent
  #:guard (struct-guard/c (listof il-cell?)))

;; push the given cell onto the stackm returning the new stack
(define (push-cell-iles cell stack)
  (struct-copy il-cell-stack
               stack
               [cells (cons cell (il-cell-stack-cells stack))]))

;; pop the tos from the stack and return a new stack
(define (pop-cell-iles stack)
  (struct-copy il-cell-stack
               stack
               [cells (cdr (il-cell-stack-cells stack))]))

;; get the op cell if the (eval) stack
(define (top-cell-iles stack)
  (define cells (il-cell-stack-cells stack))
  (if (empty? cells)
      '()
      (car cells)))

(struct il-machine
  (eval-stack    ;; stack with values for evaluation
   ip            ;; instruction pointer (index in instructions)
   instructions  ;; vector of instructions / program
   )
   #:transparent
  #:guard (struct-guard/c
           il-cell-stack?
           nonnegative-integer?
           pvector?))

;; create an empty machine
(define (create-machine)
  (il-machine (il-cell-stack '()) 0 (make-pvector 65536 (il-break-cmd))))

;; get current value (tos) of the evaluation stack
(define (get-current-value machine)
  (top-cell-iles (il-machine-eval-stack machine)))

(module+ test
  (check-true (null? (get-current-value (create-machine)))))

;; fetch and execute a single command
(define (execute-single-command machine)
  (define cmd  (nth (il-machine-instructions machine) (il-machine-ip machine)))
  (execute-command cmd machine))

;; execute a single command on the machine
(define (execute-command cmd machine)
  (unless (il-command-cell? cmd)
    (raise-user-error "cell cannot be executed, is no command cell"))
  (cond [(il-push-native-byte-cmd? cmd)
         (struct-copy il-machine
                      machine
                      [ip (add1 (il-machine-ip machine))]
                      [eval-stack (push-cell-iles (il-byte-cell (il-push-native-byte-cmd-byte cmd))
                                                  (il-machine-eval-stack machine))])]
        [(il-pop-cmd? cmd)
         (struct-copy il-machine
                      machine
                      [ip (add1 (il-machine-ip machine))]
                      [eval-stack (pop-cell-iles (il-machine-eval-stack machine))])]
        [else (raise-user-error "unknown command")]))

(module+ test
  (check-equal? (il-byte-cell-native-byte (get-current-value (execute-command (il-push-native-byte-cmd 17) (create-machine))))
                17))

(module+ test
  (check-equal? (il-byte-cell-native-byte (get-current-value (execute-command (il-push-native-byte-cmd 17) (create-machine))))
                17))

;; load the given command at idx into the machine
(define (load-command command idx machine)
  (struct-copy il-machine
               machine
               [instructions
                (set-nth (il-machine-instructions machine) idx command)]))

(module+ test
  (check-equal?
   (~>>
    (execute-single-command
     (load-command (il-push-native-byte-cmd 17)
                   0
                   (create-machine)))
    get-current-value
    il-byte-cell-native-byte)
   17))

;; load a list of commands starting from idx into the machine
(define (load-program command-list idx machine)
  (if (null? command-list)
      machine
      (load-program (cdr command-list)
                    (add1 idx)
                    (load-command (car command-list) idx machine))))


;; execute the next n commands of the machine
(define (execute-commands machine)
  (define cmd  (nth (il-machine-instructions machine) (il-machine-ip machine)))
  (if (il-break-cmd? cmd)
      machine
      (execute-commands (execute-single-command machine))))

;; execute the given list of commands loaded into the machine
(define (execute-program load-at command-list machine)
  (execute-commands
   (load-program command-list load-at machine)))

(module+ test
  (check-equal?
   (~>>
    (execute-program 0
                     (list (il-push-native-byte-cmd 17)
                           (il-push-native-byte-cmd 18)
                           (il-break-cmd))
                     (create-machine))
    get-current-value
    il-byte-cell-native-byte)
   18)

  (check-equal?
   (~>>
    (execute-program 0
                     (list (il-push-native-byte-cmd 17)
                           (il-push-native-byte-cmd 18)
                           (il-pop-cmd)
                           (il-break-cmd))
                     (create-machine))
    get-current-value
    il-byte-cell-native-byte)
   17))

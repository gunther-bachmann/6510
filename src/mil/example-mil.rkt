#lang racket

(require "./mil-structures.rkt")
(require "./mil-interpreter.rkt")

(define mil-definition-funx80 
  (mil-definition 
   'funx80 '()
   "a function that returns the value #x80"
   (mil-uint8 #x80)))


(define mil-module-a
  (mil-module 'a
              (list)
              (list)
              (list mil-definition-funx80)
              (list)
              (list)))

(module+ main
  (interpret (mil-list (list (mil-symbol 'display)
                               (mil-string "some ~a\n")
                               (mil-uint8 #x80)))
               (list))

  (interpret
   (mil-list (list (mil-symbol '+)
                   (mil-uint8 2)
                   (mil-list (list (mil-symbol 'funx80)))))
   (list (module-ctx mil-module-a))))





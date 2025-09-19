#lang info
(define name "s65")
(define gracket-launcher-libraries '("src/example/6510-example.rkt"))
(define gracket-launcher-names     '("6510 example"))

(define scribblings '(("6510-relocator.scrbl" ())))

(define deps '("base" "megaparsack-lib" "threading" "pvector" "cover" "ansi-color"))

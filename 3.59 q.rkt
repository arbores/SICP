(define tmp-op (current-output-port))
(current-output-port (open-output-string))
(load "3.5.2.rkt")
(current-output-port tmp-op)

(define (stream-expand num den radix)
  (stream-cons
   (quotient (* num radix) den)
   (stream-expand (remainder (* num radix) den) den radix)))

(stream-head (stream-expand 1 7 10) 10)
(stream-head (stream-expand 3 8 10) 10)

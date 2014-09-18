(define tmp-op1 (current-output-port))
(current-output-port (open-output-string))
(load "3.5.2.rkt")
(current-output-port tmp-op1)

(define (mul-series s1 s2)
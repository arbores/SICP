(define tmp-op2 (current-output-port))
(current-output-port (open-output-string))
(load "3.5.3.rkt")
(current-output-port tmp-op2)



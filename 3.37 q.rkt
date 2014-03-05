(define tmp-op (current-output-port))
(current-output-port (open-output-string))
(load "3.3.5.rkt")
(current-output-port tmp-op)
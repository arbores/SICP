(define tmp-op (current-output-port))
(current-output-port (open-output-string))
(load "3.5.2.rkt")
(current-output-port tmp-op)

(define s (stream-cons 1 (add-streams s s)))
(stream-head s 10)
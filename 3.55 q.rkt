(define tmp-op (current-output-port))
(current-output-port (open-output-string))
(load "3.5.2.rkt")
(current-output-port tmp-op)

(define (partial-sums stream)
  (stream-cons (stream-car stream)
               (add-streams (stream-cdr stream)
                            (partial-sums stream))))
(stream-head (partial-sums integers) 10)

(define tmp-op (current-output-port))
(current-output-port (open-output-string))
(load "3.5.1.rkt")
(current-output-port tmp-op)

(define (show x)
  (display x)
  (newline)
  x)

(define x (stream-map show (stream-enumerate-interval 0 10)))

(stream-ref x 5)
(stream-ref x 7)
  
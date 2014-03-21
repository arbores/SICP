(define tmp-op (current-output-port))
(current-output-port (open-output-string))
(load "3.5.1.rkt")
(current-output-port tmp-op)

(define (display-stream s)
  (if (stream-null? s)
      'done
      (begin
        (display (stream-car s))
        (newline)
        (display-stream (stream-cdr s)))))
(define sum 0)

(define (accum x)
  (set! sum (+ x sum))
  sum)

(define seq (stream-map accum (stream-enumerate-interval 1 20)))
(define y (stream-filter even? seq))
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))

(stream-ref y 7)
(display-stream z)
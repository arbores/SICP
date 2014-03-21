(define tmp-op (current-output-port))
(current-output-port (open-output-string))
(load "3.5.2.rkt")
(current-output-port tmp-op)

(define (mul-streams s1 s2)
  (stream-cons (* (stream-car s1) 
                  (stream-car s2))
               (mul-streams
                (stream-cdr s1)
                (stream-cdr s2))))

(define factorials 
  (stream-cons 1
               (mul-streams
                (stream-cdr integers)
                factorials)))

(stream-head factorials 10)
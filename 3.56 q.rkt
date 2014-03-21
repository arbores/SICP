(define tmp-op (current-output-port))
(current-output-port (open-output-string))
(load "3.5.2.rkt")
(current-output-port tmp-op)

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (stream-cons s1car (merge (stream-cdr s1) s2)))
                 ((> s1car s2car)
                  (stream-cons s2car (merge s1 (stream-cdr s2))))
                 (else
                  (stream-cons s1car (merge (stream-cdr s1)
                                            (stream-cdr s2)))))))))

(define S (stream-cons 1 (merge 
                          (scale-stream S 2)
                          (merge
                           (scale-stream S 3)
                           (scale-stream S 5)))))

(stream-head S 20)

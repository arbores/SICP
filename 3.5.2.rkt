(define tmp-op (current-output-port))
(current-output-port (open-output-string))
(load "3.5.1.rkt")
(current-output-port tmp-op)

;; p.193 - 
(define (integers-starting-from n)
  (stream-cons n (integers-starting-from (+ n 1))))
(define integers (integers-starting-from 1))


(define (divisible? x y) (= (remainder x y) 0))
(define no-sevens
  (stream-filter (lambda (x) (not (divisible? x 7)))
                 integers))

;fibonacci数
(define (fibgen a b)
  (stream-cons a (fibgen b (+ a b))))
(define fib (fibgen 0 1))

;Eratosthenesの篩
(define (sieve stream)
  (stream-cons
   (stream-car stream)
   (sieve (stream-filter
           (lambda (x)
             (not (divisible? x (stream-car stream))))
           (stream-cdr stream)))))
(define primes (sieve (integers-starting-from 2)))

;;p.194 - ストリームの暗黙の定義
(define (stream-map proc . streams)
  (if (stream-null? (car streams))
      the-empty-stream
      (stream-cons 
       (apply proc (map stream-car streams))
       (apply stream-map proc (map stream-cdr streams)))))
(define ones (stream-cons 1 ones))

(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define integers2 (stream-cons 1 (add-streams ones integers2)))

(define fibs2
  (stream-cons 0
               (stream-cons 1
                            (add-streams fibs2
                                         (stream-cdr fibs2)))))
                                         
(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

(define double (stream-cons 1 (scale-stream double 2)))


;;
(exec-commands 
 '((stream-ref no-sevens 100)
   (stream-head fib 10)
   (stream-ref primes 50)
   (stream-head integers2 10)
   (stream-head fibs2 10)
   (stream-head double 10)
   ))
               
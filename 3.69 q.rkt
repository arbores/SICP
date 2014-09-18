(define tmp-op2 (current-output-port))
(current-output-port (open-output-string))
(load "3.5.3.rkt")
(current-output-port tmp-op2)

(define (triples s t u)
  (stream-cons
   (list (stream-car s) (stream-car t) (stream-car u))
   (interleave
    (stream-map (lambda (x) (cons (stream-car s) x))
                (pairs t (stream-cdr u)))
    (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))

; s <= t <= u になってるかテスト
(andmap (lambda (x) (apply <= x))
        (stream-head (triples integers integers integers) 50))

(stream-head 
 (stream-filter (lambda (x) (= (+ (square (car x)) (square (cadr x)))
                               (square (caddr x))))
                (triples integers integers integers)) 
 5)


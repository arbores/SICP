;; 実行用
(define (exec-commands commands)
  (for-each (lambda (command)
              (printf "> ~a\n" command)
              (with-handlers ([exn:fail:user? (lambda (e) (print e))])
                (display (eval command))
                (newline)))
            commands))


(define-syntax stream-cons
  (syntax-rules()
    ((_ x y) (cons x (delay y)))))
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))
(define the-empty-stream '())
(define stream-null? null?)


;;p.188-191
(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (stream-cons (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (stream-cons (stream-car stream)
                      (stream-filter pred (stream-cdr stream))))
        (else
         (stream-filter pred (stream-cdr stream)))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (stream-cons low
                   (stream-enumerate-interval (+ 1 low) high))))



(define (memo-proc proc)
  (let ((already-done? false) (result false))
    (if (not already-done?)
        (begin
          (set! result (proc))
          (set! already-done? true)
          result)
        result)))
   
;; ほかの stream も作ってみる
(define (stream-nature)
  (let iter ((x 1))
    (stream-cons x
                 (iter (+ 1 x)))))

(define (stream-prime)
  (define (iter x primes)
    (cond ((zero? (remainder x (stream-car primes)))
           (iter (+ 2 x) (stream-prime)))
          ((> x (expt (stream-car primes) 2))
           (iter x (stream-cdr primes)))
          (else
           (stream-cons x
                        (iter (+ 2 x) (stream-prime))))))
  (stream-cons 2
               (iter 3 (stream-prime))))

(define (prime? x)
  (let iter((primes (stream-prime)))
    (cond ((= x (stream-car primes)) true)
          ((< x (stream-car primes)) false)
          (else (stream-cdr primes)))))

(define (stream-factorial n)
  (stream-cons (expt n 2)
               (stream-factorial (+ 1 n))))
   
(define (stream-fibonacci)
  (define (iter f_n-1 f_n-2)
    (let ((f_n (+ f_n-1 f_n-2)))
    (stream-cons f_n
                 (iter f_n f_n-1))))
  (stream-cons 1
               (stream-cons 1
                            (iter 1 1))))

(define (stream-catalan)
  (define (iter n c_n-1)
    (let ((c_n (* (/ (* 2 (+ (* 2 n) 1)) (+ n 2)) c_n-1)))
      (stream-cons c_n
                   (iter (+ 1 n) c_n))))
  (stream-cons 1
               (iter 1 1)))

(define (stream-head s n)
  (if (>= 0 n)
      '()
      (cons (stream-car s)
            (stream-head (stream-cdr s)
                         (- n 1)))))


;; 実行
(exec-commands
 '(
   (stream-car 
    (stream-cdr
     (stream-filter prime?
                    (stream-enumerate-interval 1000 100000))))
   (stream-head (stream-nature) 10)
   (stream-head (stream-prime) 10)
   (stream-head (stream-factorial 1) 10)
   (stream-head (stream-fibonacci) 10)
   (stream-head (stream-catalan) 10)
   ))


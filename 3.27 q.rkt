;; 
(define (exec-commands commands)
  (if (null? commands)
      'end
      (begin
        (display (format "> ~a\n" (car commands)))
        (with-handlers ([exn:fail:user? (lambda (e) (display e) (newline))])
          (display (eval (car commands))))
        (newline)
        (exec-commands (cdr commands)))))



;;
(define (mcaar ls) (mcar (mcar ls)))
(define (massoc key ls)
  (cond
    ((null? ls) false)
    ((eq? (mcaar ls) key) (mcar ls))
    (else (massoc key (mcdr ls)))))
(define (make-table)
  (let ((local-table (mcons '*table* '())))
    (define (insert! key value)
      (let ((pair (massoc key (mcdr local-table))))
        (if pair
            (set-mcdr! pair value)
            (set-mcdr! local-table (mcons (mcons key value)
                                          (mcdr local-table)))))
      local-table)
    
    (define (lookup key)
      (let ((result (massoc key (mcdr local-table))))
        (and result (mcdr result))))
    
    (define (dispatch m)
      (case m
        ((insert!) insert!)
        ((lookup) lookup)
        (else (raise-user-error "Unknown operation -- TABLE" m))))
    dispatch))
      
(define (apply-generic op object . args) (apply (object op) args))
(define (insert! table key value) (apply-generic 'insert! table key value))
(define (lookup table key) (apply-generic 'lookup table key))
;;


(define (memorize f)
  (let ((table (make-table)))
    (lambda (n)
      (let ((previously-computed-result (lookup table n)))
        (or previously-computed-result
            (let ((result (f n)))
              (insert! table n result)
              result))))))


(define memo-fib
  (memorize (lambda (n)
              (cond ((= n 0) 0)
                    ((= n 1) 1)
                    (else (+ (memo-fib (- n 1))
                             (memo-fib (- n 2))))))))


(define (fib n)
  (cond ((= 0 n) 0)
        ((= 1 n) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; test
(require racket/trace)
(trace memo-fib)
(memo-fib 10)
(trace fib)
(fib 10)


(define (c+ x y)
  (let ((z (make-connector)))
    (adder x y z)
    z))

(define (c* x y)
  (let ((z (make-connector)))
    (multiplier x y z)
    z))

(define (c/ x y)
  (let ((z (make-connector)))
    (multiplier y z x)
    z))

(define (cv x)
  (let ((z (make-connector)))
    (set-value! z x 'cv)
    z))

(define (celsius-fahrenheit-converter2 x)
  (c+ (c* (c/ (cv 9) (cv 5))
          x)
      (cv 32)))

(define tmp-op (current-output-port))
(current-output-port (open-output-string))
(load "3.3.5.rkt")
(current-output-port tmp-op)

;;
;; 実行
;;

(exec-commands
 '(
   (define C (make-connector))
   (define F (celsius-fahrenheit-converter2 C))
   (probe "Celsius temp" C)
   (probe "Fahrenheit temp" F)
   (set-value! C 100 'user)
     ))
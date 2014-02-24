(define (exec-commands commands)
  (if (null? commands)
      'end
      (begin
        (display (format "> ~a\n" (car commands)))
        (with-handlers ([exn:fail:user? (lambda (e) (print e))])
          (print (eval (car commands))))
        (exec-commands (cdr commands)))))

;;
;;
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay  inverter-delay
                    (lambda ()
                      (set-signal! output new-value)))))
  (add-acction! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (raise-user-error "Invalid signal" s))))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)





;;
(exec-commands 
 '((define a (make-wire))
   (define b (make-wire))
   (define c (make-wire))
   (define d (make-wire))
   (define e (make-wire))
   (define s (make-wire))
   (or-gate a b d)
   (and-gate a b c)
   (inverter c e)
   (and-gate d e s)))
   
                




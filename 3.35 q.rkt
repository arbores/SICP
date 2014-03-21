(define (squarer a b)
  (define (process-new-value)
    (cond ((has-value? b)
           (if (< (get-value b) 0)
               (raise-user-error "square less than 0 -- SQUARER" (get-value b))
               (set-value! a 
                           (sqrt (get-value b))
                           me)))
          ((has-value? a)
           (set-value! b 
                       (* (get-value a) (get-value a))
                       me))
          (else 'ignore)))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (process-new-value))
  (define (me request)
    (case request
      ((I-have-a-value) (process-new-value))
      ((I-lost-my-value) (process-forget-value))
      (else (raise-user-error "Unknown operation -- SQUARER" request))))
  (connect a me)
  (connect b me)
  me)


;;
;;
;;
;;
;;

(define tmp-op (current-output-port))
(current-output-port (open-output-string))
(load "3.3.5.rkt")
(current-output-port tmp-op)

;;
;;実行
;;
(exec-commands
 '(
   (define a (make-connector))
   (define b (make-connector))
   (squarer a b)
   (probe "a" a)
   (probe "b" b)
   (set-value! a 10 'user)
   (set-value! b 1 'user)
   (forget-value! a 'user)
   (set-value! b 35 'user)
   (forget-value! b 'user)
   (set-value! b -50 'user)
   )) 
(define (average a b c)
  (define (process-new-value)
    (cond ((and (has-value? a) (has-value? b))
           (set-value! c
                       (/ (+ (get-value a) (get-value b)) 2)
                       me))
          ((and (has-value? a) (has-value? c))
           (set-value! b
                       (- (* 2 (get-value c)) (get-value a))
                       me))
          ((and (has-value? b) (has-value? c))
           (set-value! a
                       (- (* 2 (get-value c)) (get-value b))
                       me))))
  (define (process-forget-value)
    (forget-value! a me)
    (forget-value! b me)
    (forget-value! c me)
    (process-new-value))
  (define (me request)
    (case request
      ((I-have-a-value) (process-new-value))
      ((I-lost-my-value) (process-forget-value))
      (else (error "Unknown operation -- AVERAGE" request))))
  (connect a me)
  (connect b me)
  (connect c me)
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
   (define c (make-connector))
   (average a b c)
   (probe "a" a)
   (probe "b" b)
   (probe "c" c)
   (set-value! a 10 'user)
   (set-value! b 300 'user)
   (forget-value! a 'user)
   (set-value! c 100 'user)
   (forget-value! b 'user)
   (set-value! a 50 'user)
   )) 
(define (exec-commands commands)
  (if (null? commands)
      'end
      (begin
        (printf "\n> ~a\n" (car commands))
        (with-handlers ([exn:fail:user? (lambda (e) (print e))])
          (print (eval (car commands))))
        (exec-commands (cdr commands)))))
(define (mmemq v lst)
  (cond ((null? lst) false)
        ((eq? v (mcar lst)) (mcdr lst))
        (else (mmemq v (mcdr lst)))))
;;
;; Connector
;;
(define (make-connector)
  (begin
    (define (for-each-except exception procedure list)
      (define (loop items)
        (cond ((null? items) 'done)
              ((eq? (mcar items) exception) (loop (mcdr items)))
              (else (procedure (mcar items))
                    (loop (mcdr items)))))
      (loop list))
  (let ((value false) (informant false) (constraints '()))
    (define (set-my-value newval setter)
      (cond ((not (has-value? me))
             (set! value newval)
             (set! informant setter)
             (for-each-except setter
                              inform-about-value
                              constraints))
            ((not (= value newval))
             (raise-user-error "Constradiction" (list value newval)))
            (else 'ignored)))
    (define (forget-my-value retractor)
      (if (eq? retractor informant)
          (begin (set! informant false)
                 (for-each-except retractor
                                  inform-about-no-value
                                  constraints))
          'ignoreed))
    
    (define (connect new-constraint)
      (if (not (mmemq new-constraint constraints))
          (set! constraints
                (mcons new-constraint constraints)))
      (if (has-value? me)
          (inform-about-value new-constraint))
      'done)
    
    (define (me request)
      (case request
        ((has-value?) (if informant true false))
        ((get-value) value)
        ((set-value!) set-my-value)
        ((forget-value!) forget-my-value)
        ((connect) connect)
        (else (raise-user-error "Unkonwn opration -- CONNECTOR" request))))
    me)))
(define (set-value! connector new-value informant) 
  ((connector 'set-value!) new-value informant))
(define (get-value connector) (connector 'get-value))
(define (has-value? connector) (connector 'has-value?))
(define (forget-value! connector retractor)
  ((connector 'forget-value!) retractor))
(define (connect connector new-constraint)
  ((connector 'connect) new-constraint))
  
    
;;
;;Primitive Constraints
;;
(define (adder a1 a2 sum)
  (define (process-new-value)
    (cond ((and (has-value? a1) (has-value? a2))
           (set-value! sum
                       (+ (get-value a1) (get-value a2))
                       me))
          ((and (has-value? a1) (has-value? sum))
           (set-value! a2
                       (- (get-value sum) (get-value a1))
                       me))
          ((and (has-value? a2) (has-value? sum))
           (set-value! a1
                       (- (get-value sum) (get-value a2))
                       me))))
  (define (process-forget-value)
    (forget-value! sum me)
    (forget-value! a1 me)
    (forget-value! a2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (raise-user-error "Unkown request -- ADDER" request))))
  (connect a1 me)
  (connect a2 me)
  (connect sum me)
  me)

(define (multiplier m1 m2 product)
  (define (process-new-value)
    (cond ((or (and (has-value? m1) (= (get-value m1) 0))
               (and (has-value? m2) (= (get-value m2) 0)))
           (set-value! product 0))
          ((and (has-value? m1) (has-value? m2))
           (set-value! product 
                       (* (get-value m1) (get-value m2))
                       me))
          ((and (has-value? m1) (has-value? product))
           (set-value! m2
                       (/ (get-value product) (get-value m1))
                       me))
          ((and (has-value? m2) (has-value? product))
           (set-value! m1
                       (/ (get-value product) (get-value m2))
                       me))))
  (define (process-forget-value)
    (forget-value! product me)
    (forget-value! m1 me)
    (forget-value! m2 me)
    (process-new-value))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (raise-user-error "Unknown operation -- MULTIPLIER" request))))
  (connect m1 me)
  (connect m2 me)
  (connect product me)
  me)

(define (constant value connector)
  (define (me request)
    (raise-user-error "Unknown request -- CONSTANT" request))
  (connect connector me)
  (set-value! connector value me)
  me)

(define (probe name connector)
  (define (print-probe value)
    (display "probe: ")
    (display name)
    (display " = ")
    (display value)
    (newline))
  
  (define (process-new-value)
    (print-probe (get-value connector)))
  (define (process-forget-value)
    (print-probe "?"))
  (define (me request)
    (cond ((eq? request 'I-have-a-value)
           (process-new-value))
          ((eq? request 'I-lost-my-value)
           (process-forget-value))
          (else
           (raise-user-error "Unknown request -- PROBE" request))))
  (connect connector me)
  me)
           
           

(define (inform-about-value constraint)
  (constraint 'I-have-a-value))
(define (inform-about-no-value constraint)
  (constraint 'I-lost-my-value))
  
  
  


;;
;; 制約ネットワーク作成
(define (celsius-fahrenheit-converter c f)
  (let ((u (make-connector))
        (v (make-connector))
        (w (make-connector))
        (x (make-connector))
        (y (make-connector)))
    (multiplier c w u)
    (multiplier v x u)
    (adder v y f)
    (constant 9 w)
    (constant 5 x)
    (constant 32 y)
    'ok))


;;
;;実行
;;
(exec-commands
 '(
   (define C (make-connector))
   (define F (make-connector))
   (celsius-fahrenheit-converter C F)
   (probe "Celsius temp" C)
   (probe "Fahrenheit temp" F)
   (set-value! C 25 'user)
   (set-value! F 212 'user)
   (forget-value! C 'user)
   (set-value! F 212 'user)
   ))
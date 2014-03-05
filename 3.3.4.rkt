(define (exec-commands commands)
  (if (null? commands)
      'end
      (begin
        (display (format "> ~a\n" (car commands)))
        (with-handlers ([exn:fail:user? (lambda (e) (print e))])
          (print (eval (car commands))))
        (exec-commands (cdr commands)))))
(define (mcaar lst) (mcar (mcar lst)))
(define (mcdar x) (mcdr (mcar x)))
(define (to-string-agenda agenda)
  (string-append (format "{~a " (mcar agenda))
                 (let iter((mlst (mcdr agenda)))
                   (if (null? mlst)
                       "}"
                       (string-append (format "{~a . ~a}" (mcaar mlst) (len (mcdar mlst)))
                                      (iter (mcdr mlst)))))))

;;
;; Queue
;;
(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (set-front-ptr! pair)
      (set! front-ptr pair))
    (define (set-rear-ptr! pair)
      (set! rear-ptr pair))
    
    (define (empty?) (null? front-ptr))
    (define (front)
      (if (empty?)
          (raise-user-error "FRONT-QUEUE called with an empty queue" front-ptr)
          (mcar front-ptr)))
    
    (define (insert! item)
      (let ((new-pair (mcons item '())))
        (if (empty?)
            (begin
              (set-front-ptr! new-pair)
              (set-rear-ptr! new-pair))
            (begin
              (set-mcdr! rear-ptr new-pair)
              (set-rear-ptr! new-pair)))
        front-ptr))
    
    (define (delete!)
      (if (empty?)
          (raise-user-error "DELETE! called with an empty queue" front-ptr)
          (set-front-ptr! (mcdr front-ptr)))
      front-ptr)
    
    (define (len) (let iter((ls front-ptr))
                    (if (null? ls)
                        0
                        (+ 1 (iter (mcdr ls))))))
    
    (define (dispatch m)
      (case m
        ((empty?) empty?)
        ((front) front)
        ((insert!) insert!)
        ((delete!) delete!)
        ((len) len)
        (else (raise-user-error "invalid operation -- QUEUE" m))))
    dispatch))
(define (apply-generic op object . args) 
  (if (null? args)
      ((object op))
      (apply (object op) args)))
(define (empty-queue? queue) (apply-generic 'empty? queue))
(define (front-queue queue) (apply-generic 'front queue))
(define (insert-queue! queue item) (apply-generic 'insert! queue item))
(define (delete-queue! queue) (apply-generic 'delete! queue))
(define (len queue) (apply-generic 'len queue))


;;
;; wire 実装
;;
(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (call-each procedures)
      (if (null? procedures)
          'done
          (begin
            ((car procedures))
            (call-each (cdr procedures)))))
    
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
          (begin (set! signal-value new-value)
                 (call-each action-procedures))
          'done))
    
    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))
    
    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (raise-user-error "Unknown operation -- WIRE" m))))
    dispatch))
      
(define (get-signal wire) (wire 'get-signal))
(define (set-signal! wire new-value) ((wire 'set-signal!) new-value))
(define (add-action! wire action-procedure) ((wire 'add-action!) action-procedure))
            

;;
;; 次第書き
;;
(define (make-time-segment time queue) (mcons time queue))
(define (segment-time s) (mcar s))
(define (segment-queue s) (mcdr s))
(define (make-agenda) (mcons 0 null))
(define (current-time agenda) (mcar agenda))
(define (set-current-time! agenda time) (set-mcar! agenda time))
(define (segments agenda) (mcdr agenda))
(define (set-segments! agenda segments) (set-mcdr! agenda segments))
(define (first-segment agenda) (mcar (segments agenda)))
(define (rest-segments agenda) (mcdr (segments agenda)))
(define (empty-agenda? agenda) (null? (segments agenda)))
(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (mcar segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (mcar segments)) time)
        (insert-queue! (segment-queue (mcar segments))
                       action)
        (let ((rest (mcdr segments)))
          (if (belongs-before? rest)
              (set-mcdr!
               segments
               (mcons (make-new-time-segment time action)
                     (mcdr segments)))
              (add-to-segments! rest))))
    ;(display (format "add:~a\n" (to-string-agenda agenda)))
    )
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
        (set-segments!
         agenda
         (mcons (make-new-time-segment time action)
               segments))
        (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
    (delete-queue! q)
    (if (empty-queue? q)
        (set-segments! agenda (rest-segments agenda))))
  (display (format "del:~a\n" (to-string-agenda agenda)))
  )

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
      (raise-user-error "Agenda is empty --- FIRST-AGENDA-ITEM")
      (let ((first-seg (first-segment agenda)))
        (set-current-time! agenda (segment-time first-seg))
        (front-queue (segment-queue first-seg)))))
      
(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))
(define (propagate)
  (if (empty-agenda? the-agenda)
      'done 
      (let ((first-item (first-agenda-item the-agenda)))
              (first-item)
              (remove-first-agenda-item! the-agenda)
              (propagate))))

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (newline)
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display " New-value = ")
                 (display (get-signal wire)))))

;;
;; 論理素子
;;
(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay  inverter-delay
                    (lambda ()
                      (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (raise-user-error "Invalid signal" s))))

(define (logical-and x1 x2)
  (if (zero? x1)
      0
      (if (zero? x2)
          0
          1)))

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

(define (logical-or x1 x2) 
  (if (zero? x1)
      (if (zero? x2)
          0
          1)
      1))

(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                  (lambda ()
                    (set-signal! output new-value)))))
  (add-action! a1 or-action-procedure)
  (add-action! a2 or-action-procedure)
  'ok)

(define (half-adder a b s c)
  (let ((d (make-wire))
        (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))


;;
(require racket/trace)
(exec-commands 
 '((define the-agenda (make-agenda))
   (define inverter-delay 2)
   (define and-gate-delay 3)
   (define or-gate-delay 5)
   (define input-1 (make-wire))
   (define input-2 (make-wire))
   (define sum (make-wire))
   (define carry (make-wire))
   (probe 'sum sum)
   (probe 'carry carry)
   (half-adder input-1 input-2 sum carry)
   (set-signal! input-1 1)   
   (propagate)
   (set-signal! input-2 1)
   (propagate)
   ))




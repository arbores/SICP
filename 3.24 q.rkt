(define (mcaar x) (mcar (mcar x)))
(define (mlist . ls)
  (if (null? ls)
      null
      (mcons (car ls) (apply mlist (cdr ls)))))
(define (massoc key records same-key?)
  (cond ((null? records) false)
        ((same-key? (mcaar records) key) (mcar records))
        (else
         (massoc key (mcdr records) same-key?))))

(define (make-table same-key?)
  (define (make-record key-2 value) (mcons key-2 value))
  (define (make-subtable key-1 key-2 value)
    (mlist key-1 (make-record key-2 value)))
  (define (new-records key-2 value subtable)
    (mcons (make-record key-2 value)
           (mcdr subtable)))
  (define (new-subtables key-1 key-2 value local-table)
    (mcons (make-subtable key-1 key-2 value)
           (mcdr local-table)))
  
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table) same-key?)))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable) same-key?)))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table) same-key?)))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable) same-key?)))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable (new-records key-2 value subtable))))
            (set-mcdr! local-table (new-subtables key-1 key-2 value local-table))))
      local-table)
    (define (dispatch m)
      (case m
        ((lookup-proc) lookup)
        ((insert-proc!) insert!)
        (else (raise-user-error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table equal?))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

  
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
(exec-commands
 '((put 'letters 'a 10)
   (put 'letters 'b 12)
   (put 'math '+ 21)
   (put 'math '- 21)
   (get 'letters 'a)
   (get 'math '-)
   (get 'g '+)
   (get 'math 'a)))
          
          
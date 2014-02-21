(define (mcaar x) (mcar (mcar x)))
(define (mlist . ls)
  (if (null? ls)
      null
      (mcons (car ls) (apply mlist (cdr ls)))))
(define (massoc key records)
  (cond ((null? records) false)
        ((equal? (mcaar records) key) (mcar records))
        (else
         (massoc key (mcdr records)))))

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable (mcons (mcons key-2 value)
                                             (mcdr subtable)))))
            (set-mcdr! local-table (mcons (mlist key-1
                                                 (mcons key-2 value))
                                          (mcdr local-table)))))
      local-table)
    (define (dispatch m)
      (case m
        ((lookup-proc) lookup)
        ((insert-proc!) insert!)
        (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

  
;;
(define (exec-commands commands)
  (if (null? commands)
      'end
      (begin
        (display (format "> ~a\n" (car commands)))
        (with-handlers ([exn:fail? (lambda (e) (display e) (newline))])
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
          
          
  
  
    
    
            
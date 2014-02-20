(define (mcaar x) (mcar (mcar x)))

(define (lookup key table)
  (let ((record (my-assoc key (mcdr table))))
    (if record
        (mcdr record)
        false)))

(define (my-assoc key records)
  (cond ((null? records) false)
        ((equal? key (mcaar records)) (mcar records))
        (else
         (my-assoc key (mcdr records)))))

(define (insert! key value table)
  (let ((record (my-assoc key (mcdr table))))
    (if record
        (set-mcdr! record value)
        (set-mcdr! table
                  (mcons (mcons key value)
                         (mcdr table)))))
  table)

(define (make-table)
  (mcons '*table* '()))

;;
(define (exec-commands ls)
  (if (not (null? ls))
      (begin
        (display (format "> ~a\n" (car ls)))
        (display 
         (with-handlers ([exn:fail?
                          (lambda (e) (display e)(newline))])
           (eval (car ls))))
        (newline)
        (exec-commands (cdr ls)))))
(exec-commands   
 '((define table (make-table))
   (insert! 'a 10 table)
   (insert! 'b 12 table)
   (insert! 'a 20 table)
   (lookup 'a table)
   (lookup 'b table)))
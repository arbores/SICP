;;二次元の表 p.157
(define (mcaar x) (mcar (mcar x)))
(define (mlist . ls)
  (if (null? ls)
      null
      (mcons (car ls) (apply mlist (cdr ls)))))

(define (my-assoc key records)
  (cond ((null? records) false)
        ((equal? key (mcaar records)) (mcar records))
        (else
         (my-assoc key (mcdr records)))))

(define (make-table)
  (mcons '*table* '()))

(define (lookup key-1 key-2 table)
  (let ((subtable (my-assoc key-1 (mcdr table))))
    (if subtable
        (let ((record (my-assoc key-2 (mcdr subtable))))
          (if record
              (mcdr record)
              false))
        false)))

(define (insert! key-1 key-2 value table)
  (let ((subtable (my-assoc key-1 (mcdr table))))
    (if subtable
        (let ((record (my-assoc key-2 (mcdr subtable))))
          (if record
              (set-mcdr! record value)
              (set-mcdr! subtable (mcons (mcons key-2 value)
                                         (mcdr subtable)))))
        (set-mcdr! table
                   (mcons (mlist key-1
                                 (mcons key-2 value))
                          (mcdr table)))))
  table)

;;
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
   (insert! 'math '+ 43 table)
   (insert! 'math '- 45 table)
   (insert! 'math '* 42 table)
   (insert! 'letters 'a 97 table)
   (insert! 'letters 'b 98 table)
   (lookup 'letters 'a table)
   (lookup 'math '* table)))
  
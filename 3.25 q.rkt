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



;; 本体
(define (make-table same-key?)
  (define (make-record key value) (mcons key value))
  (define (new-records key value subtable)
    (mcons (make-record key value)
           (mcdr subtable)))
  (define (make-subtable keys value)
    (if (null? (cdr keys))
        (make-record (car keys) value)
        (mlist (car keys) 
               (make-subtable (cdr keys) value))))
  (define (new-subtables keys value subtable)
    (mcons (make-subtable keys value)
           (mcdr subtable)))
  (define (partition-keys-value keys-value)
    (if (null? (cdr keys-value))
        (values '() (car keys-value))
        (let-values (((keys value) 
                      (partition-keys-value (cdr keys-value))))
          (values (cons (car keys-value) keys) value))))

  (let ((local-table (mlist '*table*)))
    (define (lookup key1 . other-keys)
      (let iter((keys (cons key1 other-keys)) (table local-table))
        (let ((subtable (massoc (car keys) (mcdr table) same-key?)))
          (cond ((not subtable) false)
                ((= 1 (length keys)) (mcdr subtable))
                (else
                 (iter (cdr keys) subtable))))))
                    
    (define (insert! key1 key2-or-value . keys-and-value)
      (let-values (((keys value)
                    (partition-keys-value (append (list key1 key2-or-value) keys-and-value))))
        (let iter((keys keys) (table local-table))
          (let ((subtable (massoc (car keys) (mcdr table) same-key?)))
            (if (= 1 (length keys))
                (if subtable
                    (set-mcdr! subtable value)
                    (set-mcdr! table (new-records (car keys) value table)))
                (if subtable
                    (iter (cdr keys) subtable)
                    (set-mcdr! table (new-subtables keys value table)))))))
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

  
;;テスト
(exec-commands
 '((put 'letters 'a 10)
   (put 'letters 'b 12)
   (put 'math '+ 21)
   (put 'math '- 21)
   (get 'letters 'a)
   (get 'math '-)
   (get 'g '+)
   (get 'math 'a)
   (put 'one 1)
   (put 'a 'b 'c 'd 100)
   (put 'a 'b 'e 50)
   (put 'a 'b 'f 51)
   (get 'one)
   (get 'a 'b 'c 'd)
   (get 'a 'b 'f)
   (get 'math)))
          
          
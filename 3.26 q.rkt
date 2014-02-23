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
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
(define (mcadr x) (mcar (mcdr x)))
(define (mcaddr x) (mcar (mcdr (mcdr x))))
(define (mlist . ls) (accumulate mcons '() ls))

;; tree
;; key は key-type　に合うものだけ
;; 
(define (make-tree-map =? <? >?)
  (let ((local-tree '()))
    (define (entry tree) (mcar tree))
    (define (left-branch tree) (mcadr tree))
    (define (right-branch tree) (mcaddr tree))
    (define (make-tree entry left-branch right-branch)
      (mlist entry left-branch right-branch))
    (define (make-node key value) (mcons key value))      
    (define (get-key node) (mcar node))
    (define (get-value node) (mcdr node))
    (define (make-symbol->string-op op)
      (lambda (x y)
        (op (symbol->string x) (symbol->string y))))
      
    (define (exist? key)
      (let iter((tree local-tree))
        (cond ((null? tree) false)
              ((=? key (get-key (entry tree))) true)
              ((<? key (get-key (entry tree))) (iter (left-branch tree)))
              ((>? key (get-key (entry tree))) (iter (right-branch tree)))
              (else (raise-user-error "Can not reach hear -- exist?" key)))))
      
    (define (insert! key value)
      (set! local-tree
            (let iter((tree local-tree))
              (cond ((null? tree) (make-tree (make-node key value) '() '()))
                    ((=? key (get-key (entry tree))) 
                     (make-tree (make-node key value)
                                (left-branch tree)
                                (right-branch tree)))
                    ((<? key (get-key (entry tree))) 
                     (make-tree (entry tree)
                                (iter (left-branch tree))
                                (right-branch tree)))
                    ((>? key (get-key (entry tree))) 
                     (make-tree (entry tree)
                                (left-branch tree)
                                (iter (right-branch tree)))))))
      local-tree)
    
    (define (lookup key)
      (let iter((tree local-tree))
        (cond ((null? tree) false)
              ((=? key (get-key (entry tree))) (get-value (entry tree)))
              ((<? key (get-key (entry tree))) (iter (left-branch tree)))
              ((>? key (get-key (entry tree))) (iter (right-branch tree))))))
                                       
    (define (dispatch m)
      (case m
        ((exist?) exist?)
        ((insert!) insert!)
        ((lookup) lookup)
        ((local-tree) local-tree)
        (else (raise-user-error "Unkown operation -- TREE-MAP" m))))
    dispatch))


(define (apply-generic op object args)
  (apply (object op) args))
(define (insert! object . keys-and-value) (apply-generic 'insert! object keys-and-value))
(define (lookup object . keys) (apply-generic 'lookup object keys))
(define (exist? object . keys) (apply-generic 'exist? object keys))
      
(define (make-symbol->string-op op)
  (lambda (x y)
    (op (symbol->string x) (symbol->string y))))




;;table
(define (make-table key=? key<? key>?)
  (define (make-init-tree-map)
    (make-tree-map key=? key<? key>?))
  (define (partition-keys-and-value keys-and-value)
    (if (null? (cdr keys-and-value))
        (values '() (car keys-and-value))
        (let-values (((keys value) (partition-keys-and-value (cdr keys-and-value))))
          (values (cons (car keys-and-value) keys)
                  value))))

  (let ((local-tree (make-init-tree-map)))
    (define (lookup . keys)
      (let iter((keys keys) (tree local-tree))
        (cond ((null? keys) tree)
              ((procedure? tree)
               (iter (cdr keys) ((tree 'lookup) (car keys))))
              (else false))))

    (define (insert! key1 key2-or-value . keys-and-value)
      (let-values (((keys value) (partition-keys-and-value (cons key1 (cons key2-or-value keys-and-value)))))
        (let iter((keys keys) (tree local-tree))
          (if (= 1 (length keys))
              ((tree 'insert!) (car keys) value)
              (let ((subtree ((tree 'lookup) (car keys))))
                (if (and subtree (procedure? subtree))
                    (iter (cdr keys) subtree)
                    (let ((new-subtree (make-init-tree-map)))
                      ((tree 'insert!) (car keys) new-subtree)
                      (iter (cdr keys) new-subtree)))))))
      (local-tree 'local-tree))
    
    (define (dispatch m)
      (case m
        ((lookup) lookup)
        ((insert!) insert!)
        (else (raise-user-error "Unkown operation -- TABLE" m))))
    
    dispatch))
    
;;; test
(exec-commands
 '((define t1 (make-tree-map  
               (make-symbol->string-op string=?)
               (make-symbol->string-op string<?)
               (make-symbol->string-op string>?)))
   (insert! t1 'ta 10)
   (lookup t1 'ta)
   (lookup t1 'tb)
   (exist? t1 'a)
   (lookup t1 'a)
   (insert! t1 'a 10)
   (exist? t1 'a)
   (lookup t1 'a)
   (insert! t1 'a 13)
   (lookup t1 'a)
   (insert! t1 'b 12)
   (insert! t1 'c 15)
   (lookup t1 'b)
   ))
  
(exec-commands
 '((define table (make-table
               (make-symbol->string-op string=?)
               (make-symbol->string-op string<?)
               (make-symbol->string-op string>?)))
   (insert! table 'ta 10)
   (lookup table 'ta)
   (lookup table 'tb)
   (insert! table 'ta 's-a 15)
   (lookup table 'ta 's-a)
   (insert! table 'tb 10)
   (lookup table 'tb 's-a)
   (insert! table 'a 'b 'c 'd 100)
   (lookup table 'a 'b 'c 'd)
   (lookup table 'b 'c 'd)
   ))
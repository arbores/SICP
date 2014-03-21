(define tmp-op (current-output-port))
(current-output-port (open-output-string))
(load "3.5.1.rkt")
(current-output-port tmp-op)


(define (stream-map proc . argstreams)
  (if (foldr (lambda (v s)
               (or s (stream-null? v)))
             false
             argstreams)
      the-empty-stream
      (stream-cons
       (apply proc (map stream-car argstreams))
       (apply stream-map 
              (cons proc (map stream-cdr argstreams))))))
      


;;
;; 実行
;;

(exec-commands
 '(
   (stream-map (lambda (i p) p)
               (stream-enumerate-interval 0 10)
               (stream-prime))
   ))
(define tmp-op2 (current-output-port))
(current-output-port (open-output-string))
(load "3.5.3.rkt")
(current-output-port tmp-op2)


(define (stream-limit stream tolerance)
  (let iter((before-val (stream-car stream))
            (stream (stream-cdr stream)))
    (if (< (abs (- (stream-car stream) before-val))
           tolerance)
        (stream-car stream)
        (iter (stream-car stream)
              (stream-cdr stream)))))

;;;
(exec-commands
 '((stream-limit (sqrt-stream 2) 0.01)
   ))
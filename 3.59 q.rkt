(define tmp-op1 (current-output-port))
(current-output-port (open-output-string))
(load "3.5.2.rkt")
(current-output-port tmp-op1)

(define (integrate-series s)
  (stream-map / s integers))

(define exp-series
  (stream-cons 1 (integrate-series exp-series)))

(define cosine-series
  (stream-cons 1 (stream-map - (integrate-series sine-series))))

(define sine-series
  (stream-cons 0 (integrate-series cosine-series)))
  
(define (exec-commands commands)
  (for-each (lambda (command)
              (printf "> ~a\n" command)
              (with-handlers ([exn:fail:user? (lambda (e) (print e))])
                (display (eval command))
                (newline)))
            commands))

(exec-commands
 '((stream-head (integrate-series ones) 10)
   (stream-head exp-series 10)
   (stream-head cosine-series 10)
   (stream-head sine-series 10)
   ))
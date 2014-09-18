(define tmp-op2 (current-output-port))
(current-output-port (open-output-string))
(load "3.5.3.rkt")
(current-output-port tmp-op2)

; interleave で混ぜるストリームに縦列の分も含める
(define (pairs s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    (interleave
     ;縦列のストリーム
     (stream-map (lambda (x) (list x (stream-car t)))
                 (stream-cdr s))
     (pairs (stream-cdr s) (stream-cdr t))))))


(stream-head (pairs integers integers) 20)
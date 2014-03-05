
(define (ripple-carry-adder1 a-list b-list c-in sum-list c)
  (foldl (lambda (a b sum c-in)
           (let ((c-out (make-wire)))
             (full-adder a b c-in sum c-out)
             c-out))
         c-in
         a-list b-list sum-list))
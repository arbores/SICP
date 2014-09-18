(define tmp-op1 (current-output-port))
(current-output-port (open-output-string))
(load "3.5.2.rkt")
(current-output-port tmp-op1)

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))
(define (sqrt-stream x)
  (define guesses
    (stream-cons 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
    guesses)

(define (partial-sums streams)
  (stream-cons (stream-car streams)
               (add-streams (stream-cdr streams)
                            (partial-sums streams))))

(define (pi-summands n)
  (stream-cons (/ 1.0 n)
               (stream-map - (pi-summands (+ n 2)))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

;p.199 並び加速
(define (euler-transform s)
  (let ((s0 (stream-ref s 0))
        (s1 (stream-ref s 1))
        (s2 (stream-ref s 2)))
    (stream-cons (- s2 (/ (square (- s2 s1))
                          (+ s0 (* -2 s1) s2)))
                 (euler-transform (stream-cdr s)))))

;タブロー：ストリームのストリームを作成
(define (make-tableau transform s)
  (stream-cons s
               (make-tableau transform
                             (transform s))))

(define (accelerated-sequence transform s)
  (stream-map stream-car
              (make-tableau transform s)))


;;p.201 対の無限ストリーム
;２つの数の和が素数になるものを取り出す、をストリームにするなら
(define (sum-primes)
  (stream-filter (lambda (pair)
                  (prime? (+ (car pair) (cadr pair))))
                 ;interleave のため要素の順が上の横列ほど多いようになってる
                (pairs integers integers)))

; ストリーム s,t の2次元配列の上三角になるストリームを作る
(define (pairs s t)
  (stream-cons
   (list (stream-car s) (stream-car t))
   (interleave  ;stream-append
    ;体格以降の横一列分のストリーム作成
    (stream-map (lambda (x) (list (stream-car s) x))
                (stream-cdr t))
    ;つぎの対角行列の位置、つまり右斜め下が次の開始点
    (pairs (stream-cdr s) (stream-cdr t)))))

;s1 と s2 のストリームを並べてつなげたストリームを作る
;s1 が無限ストリームだと、s2の要素には到達しない。
(define (stream-append s1 s2)
  (if (stream-null? s1)
      s2 
      (stream-cons (stream-car s1)
                   (stream-append (stream-cdr s1) s2))))

;s1 と s2 のストリームから交互に要素を並べたストリームを作る
;stream-append を無限ストリームでも適切なようにしたもの
(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (stream-cons (stream-car s1)
                   (interleave s2 (stream-cdr s1)))))



(exec-commands
 '(
   (stream-head (sqrt-stream 2) 10)
   (stream-head pi-stream 10)
   (stream-head (euler-transform pi-stream) 10)
   (stream-head (accelerated-sequence euler-transform pi-stream) 10)
   (stream-head (sum-primes) 15)
   ))
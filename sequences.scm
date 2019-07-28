
(define triangular-numbers
  (s-accumulate + 0 (s-iter 1+ 1)))

(define s-factorial
  (let ((factorials (s-accumulate * 1 (s-iter 1+ 1))))
    (lambda (n)
      (s-ref factorials n))))

(define s-choose
  (let ((binomials (s-iter (lambda (x)
			     (map +
				  (cons 0 x)
				  (append x '(0))))
			   '(1))))
    (lambda (n k)
      (list-ref (s-ref binomials n)
		k))))


(define s:e^x
  (s:cons 1 (s:integrate s:e^x)))

(define s:cos
  (s:cons 1 (s:negate (s:integrate s:sin))))

(define s:sin
  (s:cons 0 (s:integrate s:cos)))

(define s:pi
  (s:accumulate + 1
		(s:map /
		       (s:cycle '(-1 1))
		       (s:iter (curry + 2) 3.))))

(define s:palindromes
  (lambda ()
    (s:merge-sorted (s:map (lambda (k)
                             (let ((ds (digits k)))
                               (digits->integer
                                (append ds (cdr (reverse ds))))))
                           (nats (s:iter 1+ 1)))
                    (s:map (lambda (k)
                             (let ((ds (digits k)))
                               (digits->integer
                                (append ds (reverse ds)))))
                           (nats (s:iter 1+ 1))))))

(define binomials
  (lambda ()
    (s:iter (lambda (x)
	      (s:map + (s:cons 0 x) x))
	    (s:cons 1 (s:constant 0)))))

(define factorials
  (lambda ()
    (s:accumulate * 1 (s:iter 1+ 1))))

(define s:factorial
  (let ((factorials (factorials)))
    (lambda (n)
      (s:ref factorials n))))

(define s:choose
  (let ((binomials (binomials)))
    (lambda (n k)
      (let ((j (min k (- n k))))
	(if (< j 0)
	    0
	    (s:ref (s:ref binomials n) j))))))


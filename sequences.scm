
(define s:factorial
  (let ((factorials (s:accumulate * 1 (s:iter 1+ 1))))
    (lambda (n)
      (s:ref factorials n))))

(define s:choose
  (let ((binomials (s:iter (lambda (x)
			     (s:map + (s:cons 0 x) x))
			   (s:cons 1 (s:constant 0)))))
    (lambda (n k)
      (let ((j (min k (- n k))))
	(if (< j 0)
	    0
	    (s:ref (s:ref binomials n)
		   j))))))

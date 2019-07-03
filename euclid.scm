
;; extended euclid algorithm
(define algorithm-E
  (lambda (m n)
    (let recycle ((c m) (d n) (a0 0) (a1 1) (b0 1) (b1 0))
      (let* ((q (quotient c d)) (r (modulo c d)))
	(if (zero? r)
	    (list a0 b0 d)
	    (recycle d r (- a1 (* q a0)) a0 (- b1 (* q b0)) b0))))))

(define ax+by=gcd
  (lambda (x y)
    (cond ((zero? x) y)
	  ((zero? y) x)
	  (else (algorithm-E x y)))))

;; generalized cont frac
;; A, B start from n = -1
;; An = bn*An-1 + an*An-2, n >= 1
;; Bn = bn*Bn-1 + an*Bn-2, n >= 1
(define g:convergents
  (lambda (as bs)
    (letrec ((A (s:cons 1 (car bs)
			(s:map +
			       (s:map * (s:cdr bs) (s:cdr A))
			       (s:map * (s:cdr as) A))))
	     (B (s:cons 0 1
			(s:map +
			       (s:map * (s:cdr bs) (s:cdr B))
			       (s:map * (s:cdr as) B)))))
      (s:map /
	     (s:drop 1 A)
	     (s:drop 1 B)))))

(define convergents
  (lambda (bs)
    (g:convergents (s:constant 1) bs)))

(define (test-root-3)
  (s:take 20
	  (s:map * (s:constant 1.)
		 (convergents (s:cons 1 (s:cycle '(1 2)))))))

(define (test-root-2)
  (s:take 20
	  (s:map * (s:constant 1.)
		 (convergents (s:cons 1 (s:cycle '(2)))))))

(define (test-e n)
  (s:take n
	  (s:map log
		 (convergents
		  (s:cons 2 (s:map (lambda (n)
				     (case (mod n 3)
				       ((2) (* 2 (quotient (+ 2 n) 3)))
				       (else 1)))
				   (s:iter 1+ 1)))))))

(test-e 10)
(test-root-3)

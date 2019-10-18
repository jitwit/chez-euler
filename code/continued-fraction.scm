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
      (s:map / (s:cdr A) (s:cdr B)))))

(define convergents
  (lambda (bs)
    (g:convergents (s:constant 1) bs)))

(define cf:sqrt-rep
  (lambda (S)
    (let ((a0 (isqrt S)))
      (if (= (square a0) S)
	  `(,a0)
	  (let loop ((a a0) (m 0) (d 1) (X '()))
	    (if (= a (* 2 a0))
		(reverse (cons a X))
		(let* ((m* (- (* d a)
			      m))
		       (d* (/ (- S (square m*))
			      d))
		       (a* (quotient (+ a0 m*)
				     d*)))
		  (loop a* m* d* (cons a X)))))))))

(define cf:sqrt
  (lambda (S)
    (let* ((rep (cf:sqrt-rep S))
	   (a0 (car rep))
	   (as (cdr rep)))
      (if (null? as)
	  (s:constant a0)
	  (convergents (s:cons a0 (s:cycle as)))))))


(define-syntax s:cons
  (lambda (x)
    (syntax-case x ()
      ((_) #'())
      ((_ x) #'x)
      ((_ x y ... z)
       #'(cons* x y ...
		(delay z))))))

(define s:cdr
  (lambda (S)
    (let ((tl (cdr S)))
      (when (procedure? tl)
	(set-cdr! S (force tl))))
    (cdr S)))

(define s:ref
  (lambda (S i)
    (letrec ((aux (lambda (i S)
		    (case i
		      ((0) (car S))
		      (else (aux (1- i) (s:cdr S)))))))
      (aux i S))))

(define s:iter
  (lambda (f x0)
    (letrec ((aux (lambda (x)
		    (s:cons x (aux (f x))))))
      (aux x0))))

(define s:map
  (lambda (f . xs)
    (letrec ((aux (lambda (S)
		    (s:cons (apply f (map car S))
			    (aux (map s:cdr S))))))
      (aux xs))))

(define s:filter
  (lambda (predicate S)
    (letrec ((aux (lambda (S)
		    (let ((hd (car S))
			  (tl (s:cdr S)))
		      (if (predicate hd)
			  (s:cons hd (aux tl))
			  (aux tl))))))
      (aux S))))

(define s:take
  (lambda (n S)
    (letrec ((aux (lambda (n S)
		    (cond ((or (zero? n) (null? S)) '())
			  ((pair? S) (cons* (car S)
					    (aux (1- n) (s:cdr S))))
			  (else (list S))))))
      (aux n S))))

(define s:enumerate
  (lambda (S)
    (letrec ((aux (lambda (i S)
		    (s:cons (cons i (car S))
			    (aux (1+ i) (s:cdr S))))))
      (aux 0 S))))

(define s:drop
  (lambda (n S)
    (letrec ((aux (lambda (n S)
		    (cond ((or (zero? n) (null? S)) S)
			  ((pair? S) (aux (1- n) (s:cdr S)))
			  (else (list S))))))
      (aux n S))))

(define s:take-while
  (lambda (predicate S)
    (letrec ((aux (lambda (S)
		    (if (or (null? S)
			    (not (predicate (car S))))
			'()
			(s:cons (car S) (aux (s:cdr S)))))))
      (aux S))))

(define s:drop-while
  (lambda (predicate S)
    (letrec ((aux (lambda (S)
		    (if (or (null? S)
			    (not (predicate (car S))))
			S
			(aux (s:cdr S))))))
      (aux S))))

(define s:accumulate
  (lambda (f x0 S)
    (letrec ((aux (lambda (x S)
		    (s:cons x
			    (aux (f x (car S))
				 (s:cdr S))))))
      (aux x0 S))))

(define s:append
  (lambda (S . Ts)
    (cond ((null? Ts) S)
	  ((null? S) (apply s:append (car Ts) (s:cdr Ts)))
	  (else (s:cons (car S)
			(apply s:append (s:cdr S) Ts))))))

(define s:chunks
  (lambda (n S)
    (letrec ((aux (lambda (S)
		    (s:cons (s:take n S)
			    (aux (list-tail S n))))))
      (aux S))))

(define s:cycle
  (lambda (S)
    (letrec ((aux (lambda (T)
		    (if (null? T)
			(aux S)
			(s:cons (car T)
				(aux (cdr T)))))))
      (if (null? S)
	  S
	  (aux S)))))

(define s:constant
  (lambda (x)
    (s:cons x (s:constant x))))

(define s:scale
  (lambda (k S)
    (s:map (lambda (x)
	     (* x k))
	   S)))

(define s:negate
  (lambda (S)
    (s:map - S)))

(define s:integrate
  (lambda (S)
    (letrec ((aux (lambda (n S)
		    (s:cons (/ (car S) n)
			    (aux (1+ n) (s:cdr S))))))
      (aux 1 S))))

(define s:derive
  (lambda (S)
    (letrec ((aux (lambda (n S)
		    (s:cons (* n (car S))
			    (aux (1+ n) (s:cdr S))))))
      (s:cons 0 (aux 1 (s:cdr S))))))

(define s:interleave
  (lambda (S T)
    (letrec ((aux (lambda (S T)
		    (s:cons (car S)
			    (aux T (s:cdr S))))))
      (aux S T))))

(define s:convolve
  (lambda (S T)
    (letrec ((aux (lambda (S T)
		    (let ((s0 (car S))
			  (t0 (car T))
			  (st (s:cdr S))
			  (tt (s:cdr T)))
		      (s:cons (* s0 t0)
			      (s:map +
				     (s:scale t0 st)
				     (s:scale s0 tt)
				     (s:cons 0 (aux st tt))))))))
      (aux S T))))

(define s:square
  (lambda (S)
    (s:convolve S S)))



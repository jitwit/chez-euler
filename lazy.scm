(define-syntax s-cons
  (lambda (x)
    (syntax-case x ()
      ((_) #'())
      ((_ x) #'x)
      ((_ x y ...)
       #'(cons x (delay (s-cons y ...)))))))

(define s-cdr
  (lambda (S)
    (let ((tl (cdr S)))
      (when (procedure? tl)
	(set-cdr! S (force tl)))
      (cdr S))))

(define s-ref
  (lambda (S i)
    (case i
      ((0) (car S))
      (else (s-ref (s-cdr S) (1- i))))))

(define s-iter
  (lambda (f x0)
    (s-cons x0 (s-iter f (f x0)))))

(define s-map
  (lambda (f . xs)
    (s-cons (apply f (map car xs))
	    (apply s-map f (map s-cdr xs)))))

(define s-filter
  (lambda (predicate S)
    (let ((hd (car S))
	  (tl (s-cdr S)))
      (if (predicate hd)
	  (s-cons hd (s-filter predicate tl))
	  (s-filter predicate tl)))))

(define s-take
  (lambda (n S)
    (cond ((or (zero? n) (null? S)) '())
	  ((pair? S) (cons* (car S)
			    (s-take (1- n)
				    (s-cdr S))))
	  (else (list S)))))

(define s-drop
  (lambda (n S)
    (cond ((or (zero? n) (null? S)) S)
	  ((pair? S) (s-drop (1- n) (s-cdr S)))
	  (else (list S)))))

(define s-take-while
  (lambda (predicate S)
    (cond ((or (null? S) (not (predicate (car S)))) '())
	  (else (cons* (car S)
		       (s-take-while predicate
				     (s-cdr S)))))))

(define s-drop-while
  (lambda (predicate S)
    (cond ((or (null? S) (not (predicate (car S)))) S)
	  (else (s-drop-while predicate (s-cdr S))))))

(define s-accumulate
  (lambda (f x0 S)
    (s-cons x0
	    (s-accumulate f
			  (f x0 (car S))
			  (s-cdr S)))))

(define s-append
  (lambda (S . Ts)
    (cond ((null? Ts) S)
	  ((null? S) (apply s-append (car Ts) (s-cdr Ts)))
	  (else (s-cons (car S)
			(apply s-append (s-cdr S) Ts))))))

(define list->lazy
  (lambda (xs)
    (if (null? xs)
	xs
	(s-cons (car xs) (list->lazy (cdr xs))))))

(define s-constant
  (lambda (x)
    (s-cons x (s-constant x))))

(define y (s-iter cos 0))

(define even-fibs (s-filter even?
			    (letrec ((z (s-cons 0 (s-map + z (s-cons 1 z)))))
			      z)))

(define triangles
  (s-accumulate + 0 (s-iter 1+ 1)))




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
    (cond ((zero? x) (list 1 0 y))
	  ((zero? y) (list 0 1 x))
	  (else (algorithm-E x y)))))

(define coprime?
  (lambda (x y)
    (= 1 (gcd x y))))

(define divides?
  (lambda (d x)
    (zero? (modulo x d))))

(define inverse-modulo
  (lambda (x m)
    (let ((yx+bm=1 (ax+by=gcd x m)))
      (if (= 1 (list-ref yx+bm=1 2))
	  (mod (car yx+bm=1) m)
	  #f))))

(define chinese-remainder-theorem
  (lambda (a1 n1 a2 n2)
    (let-values (((m1 m2 gcd) (apply values (ax+by=gcd n1 n2))))
      (when (not (= gcd 1))
	(error 'crt "modulii not pairwise coprime" n1 n2))
      (mod (+ (* a1 m2 n2) (* a2 m1 n1))
           (* n1 n2)))))

(define crt-system
  (lambda (eqns)
    (fold-right (lambda (an1 an2)
		  (let ((a1 (car an1))
                        (a2 (car an2))
                        (n1 (cdr an1))
                        (n2 (cdr an2)))
		    (cons (chinese-remainder-theorem a1 n1 a2 n2)
			  (* n1 n2))))
		(car eqns)
		(cdr eqns))))

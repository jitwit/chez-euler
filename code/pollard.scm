;; Trial Division                                                             ;;
(define *cutoff* 1000000)

(define trial-division
  (lambda (N)
    (let loop ((ps (primes (isqrt N)))
	       (N N))
      (if (or (null? ps) (> (car ps) N))
	(if (= N 1) '() (list N))
	(let* ((p (car ps))
	       (q (fx/ N p)))
	  (if (= N (* q p))
	    (cons p (loop ps q))
	    (loop (cdr ps) N)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Rho-Pollard                                                                ;;
(define rho-method
  (lambda (x y k l n)
    (define (B2)
      (if (prime? n) n (B3)))
    (define (B3)
      (let ((g (gcd (- y x) n)))
	(cond ((= g 1) (B4))
	      ((or (= g n)) 'failed)
	      (else
	       (set! n (quotient n g))
	       (set! x (modulo x n))
	       (set! y (modulo y n))
	       (B2)))))
    (define (B4)
      (set! k (1- k))
      (when (zero? k)
	(set! y x)
	(set! l (* 2 l))
	(set! k l))
      (set! x (mod (1+ (* x x)) n))
      (B3))
    (B2)))

(define rho-pollard
  (lambda (N)
    (let ((n (rho-method 5 2 1 1 N)))
      (if (eq? n 'failed)
	'()
	(let ((q (quotient N n)))
	  (cond ((< q 2) (list n))
		((prime? q) (list n q))
		((< q *cutoff*) (cons n (trial-division q)))
		(else (cons n (rho-pollard q)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Hybrid                                                                     ;;
(define factorize
  (lambda (N)
    (let loop ((ps (primes (min N (isqrt *cutoff*)))) (N N))
      (if (null? ps)
	  (cond ((= N 1) '())
		((prime? N) (list N))
		((< N *cutoff*) (trial-division N))
		(else (rho-pollard N)))
	  (let* ((p (car ps))
		 (q (quotient N p)))
	    (if (= N (* q p))
		(cons* p (loop ps q))
		(loop (cdr ps) N)))))))

(define factors-tree ;; depends on patricia trees
  (lambda (N)
    (fold-right (lambda (x T)
		  (insert-with + x 1 T))
		empty-tree
		(factorize N))))

(define factorize-with-multiplicity ;; depends on patricia trees
  (lambda (N)
    (tree->alist (factors-tree N))))

(define prime-factors
  (lambda (N)
    (tree->keys (factors-tree N))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Divisors                                                                   ;;
(define combine-factors
  (lambda (x Y)
    (merge-sorted (map (lambda (y)
			 (* x y))
		       Y)
		  Y)))

(define divisors
  (lambda (N)
    (if (zero? N)
	'()
	(fold-right combine-factors '(1) (factorize N)))))

(define totient
  (lambda (N)
    (fold-right (lambda (x t)
		  (/ (* (1- x) t) x))
		N
		(tree->keys (factors-tree N)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Multiplicative group                                                       ;;

(define Z/nZ*
  (lambda (n)
    (define G (make-vector n #t))
    (define (sieve j p)
      (when (< j n)
	(vector-set! G j #f)
	(sieve (+ j p) p)))
    (for-each (lambda (p)
		(sieve p p))
	      (prime-factors n))
    (let loop ((x (1- n)) (Z/nZ '()))
      (cond ((zero? x) Z/nZ)
	    ((vector-ref G x) (loop (1- x) (cons x Z/nZ)))
	    (else (loop (1- x) Z/nZ))))))


(define miller-rabin
  (lambda (p)
    (define n (1- p))
    (define r (bitwise-first-bit-set n))
    (define d (ash n (- r)))
    (define trials 20) ;; sensible?
    (define (loop t k x)
      (define x^2 (expt-mod x 2 p))
      (cond ((<= k 0) 'composite)
	    ((= x^2 n) (witness (1+ t)))
	    (else (loop t (sub1 k) x^2))))
    (define (witness k)
      (let ((x (expt-mod (+ 2 (random (- p 3))) d p)))
	(cond ((= k trials) 'probable-prime)
	      ((or (= x n) (= x 1)) (witness (1+ k)))
	      (else (loop k (1- r) x)))))
    ;;      (random-seed 1)
    (cond ((> p 3) (witness 0))
	  ((= p 3) 'prime)
	  ((= p 2) 'prime)
	  (else 'composite))))


(define prime?
  (lambda (p)
    (case p
      ((2 3 5 7) #t)
      (else (not (or (even? p)
		     (divides? 3 p)
		     (divides? 5 p)
		     (divides? 7 p)
		     (eq? 'composite (miller-rabin p))))))))

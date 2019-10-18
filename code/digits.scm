
(define digit-fold
  (lambda (x g N)
    (letrec ((aux (lambda (N y)
		    (if (zero? N)
			y
			(aux (quotient N 10)
			     (g (modulo N 10) y))))))
      (aux N x))))

(define digits
  (lambda (N)
    (digit-fold '() cons N)))

(define digit-sum
  (lambda (N)
    (digit-fold 0 + N)))

(define digits->integer
  (lambda (digs)
    (fold-left (lambda (y x)
		 (+ x (* y 10)))
	       0
	       digs)))

(define pandigital?
  (lambda (digits)
    (andmap = (sort < digits) (iota (length digits)))))

(define palindrome?
  (lambda (N)
    (let ((digits (digits N)))
      (equal? digits (reverse digits)))))

(define digit-fold
  (lambda (g x N)
    (let aux ((N N) (y x))
      (if (zero? N)
          y
          (aux (quotient N 10)
               (g (modulo N 10) y))))))

(define digits
  (lambda (N)
    (digit-fold cons '() N)))

(define digit-sum
  (lambda (N)
    (digit-fold fx+ 0 N)))

(define digits->integer
  (lambda (digs)
    (fold-left (lambda (y x)
		 (+ x (* y 10)))
	       0
	       digs)))

(define pandigital?
  (lambda (digits)
    (andmap fx= (sort fx< digits) (iota (length digits)))))

(define palindrome?
  (lambda (N)
    (let ((digits (digits N)))
      (equal? digits (reverse digits)))))

(define (nth-digit n x)
  (fxmod (quotient x (expt 10 (fx1+ n))) 10))

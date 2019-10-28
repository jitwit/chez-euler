(import (prefix (patricia) t:))

(define p:degree
  (lambda (P)
    (car (or (t:maximum P)
	     '(0 . 0)))))

(define p:constant
  (lambda (k)
    (t:singleton 0 k)))

(define p:coefficient
  (lambda (n P)
    (t:lookup-with-default n 0 P)))

(define p:singleton
  (lambda (k n)
    (t:singleton n k)))

(define p:add
  (lambda (P Q)
    (t:merge-with + P Q)))

(define p:+
  (lambda Ps
    (fold-right p:add t:empty-tree Ps)))

(define p:multiply
  (lambda (P Q)
    (t:tree-ifold-right (lambda (n k R)
			  (p:add R (p:shift/scale Q n k)))
			t:empty-tree
			P)))

(define p:*
  (lambda polynomials
    (fold-right p:multiply (p:singleton 1 0) polynomials)))

(define p:^
  (lambda (P k)
    (if (zero? k)
	(p:constant 1)
	(let ((P^k/2 (p:^ P (quotient k 2))))
	  (if (even? k)
	      (p:* P^k/2 P^k/2)
	      (p:* P^k/2 P^k/2 P))))))

(define p:scale
  (lambda (P k)
    (t:tree-map (lambda (p)
		  (* k p))
		P)))

(define p:shift
  (lambda (P d)
    (t:tree-ifold-right (lambda (n k Q)
			  (t:insert (+ n d) k Q))
			t:empty-tree
			P)))

(define p:shift/scale
  (lambda (P d s)
    (t:tree-ifold-right (lambda (n k Q)
			  (t:insert (+ n d) (* s k) Q))
			t:empty-tree
			P)))

(define not-horner
  (lambda (P x)
    (t:tree-ifold-right (lambda (n k y)
			  (+ y (* k (expt x n))))
			0
			P)))

(define coefficients->polynomial
  (lambda (ks)
    (apply p:+ (map p:singleton ks (enumerate ks)))))

(define horner
  (lambda (P x)
    (let-values (((t d)
		  (apply values
			 (t:tree-ifold-right
			  (lambda (n k tpx)
			    (let-values (((t m) (apply values tpx)))
			      (list (+ (* t (expt x (- m n)))
				       k)
				    n)))
			  (list 0 (p:degree P)) ;; total, prev p:degree, x
			  P))))
      (* t (expt x d)))))

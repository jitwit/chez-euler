
(define blum-blum-shub
  (lambda (s0 M)
    (s:iter (lambda (x-n)
	      (mod (square x-n)
		   M))
	    s0)))



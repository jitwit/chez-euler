(define (legendre-symbol a p)
  (let ((a/p (expt-mod a (quotient (1- p) 2) p)))
    (if (< 1 a/p)
        -1
        a/p)))



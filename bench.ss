(load "euler.so")
(import (euler))
(define (pi N)
  (length (primes N)))

(for-each (lambda (N)
	    (time (pi (expt 10 N))))
	  (iota 10))

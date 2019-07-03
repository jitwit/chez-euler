(library (chez euler)
  (export primes
	  prime?
	  ax+by=gcd
	  inverse-modulo)
  (import (chezscheme))
  (include "eratosthenes.scm")
  (include "euclid.scm")
  (include "miller-rabin.scm"))

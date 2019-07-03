(library (chez euler)
  (export primes
	  prime?
	  ax+by=gcd)
  (import (chezscheme))
  (include "eratosthenes.scm")
  (include "euclid.scm")
  (include "miller-rabin.scm"))

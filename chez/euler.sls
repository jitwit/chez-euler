(library (chez euler)
  (export
   ;; sieves
   primes
   ;; removing for now because it's incorrect! boo!
   ;;   primes-in-range 
   totient-sieve
   moebius-sieve

   ;; factorization
   factorize
   factorize-with-multiplicity
   divisors

   ;; prime testing
   prime?
   
   ;; misc.
   ax+by=gcd
   inverse-modulo
   Omega
   omega)
  
  (import (chezscheme)
	  (chez patricia))

  (include "eratosthenes.scm")
  (include "euclid.scm")
  (include "miller-rabin.scm")
  (include "pollard.scm")
  
  )

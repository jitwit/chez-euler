(library (chez euler)
  (export
   ;; sieves
   primes
   ;; removing for now because it's incorrect! boo!
   ;;   primes-in-range 
   totient-sieve

   ;; factorization
   factorize

   ;; prime testing
   prime?
   
   ;; misc.
   ax+by=gcd
   inverse-modulo)
  
  (import (chezscheme))

  (include "eratosthenes.scm")
  (include "euclid.scm")
  (include "miller-rabin.scm")
  (include "pollard.scm")
  
  )

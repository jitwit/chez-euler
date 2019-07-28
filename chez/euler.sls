(eval-when (compile)
  (optimize-level 3))

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
   totient

   ;; prime testing
   prime?
   
   ;; misc.
   ax+by=gcd
   inverse-modulo
   Omega
   omega

   ;; streams
   s-cons
   s-cdr
   s-ref
   s-iter
   s-map
   s-filter
   s-take
   s-drop
   s-take-while
   s-drop-while
   s-constant
   s-append

   ;; stream related
   s-factorial
   s-choose
   *primes*
   )
  
  (import (chezscheme)
	  (chez patricia))

  (include "eratosthenes.scm")
  (include "euclid.scm")
  (include "miller-rabin.scm")
  (include "pollard.scm")
  (include "lazy.scm")
  (include "sequences.scm")
  
  )

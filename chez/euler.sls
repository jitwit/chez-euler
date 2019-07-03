(library (chez euler)
  (export
   ;; sieves
   primes
   primes-in-range

   ;; prime testing
   prime?
   
   ;; misc.
   ax+by=gcd
   inverse-modulo)
  (import (chezscheme))
  (include "eratosthenes.scm")
  (include "euclid.scm")
  (include "miller-rabin.scm"))

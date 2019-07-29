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
   s:cons
   s:cdr
   s:ref
   s:iter
   s:map
   s:filter
   s:take
   s:drop
   s:take-while
   s:drop-while
   s:constant
   s:scale
   s:append
   s:cycle
   s:chunks
   s:enumerate
   s:integrate
   s:derive
   s:negate
   s:interleave
   s:convolve
   s:square

   ;; stream related
   s:factorial
   s:choose
   s:e^x
   s:sin
   s:cos
   *primes*

   ;; polynomial related
   p:constant
   p:coefficient
   p:degree
   horner
   p:*
   p:+
   p:^
   p:scale
   p:shift
   p:shift/scale
   p:singleton
   list->polynomial

   ;; geometry
   point
   segment
   _x
   _y
   _start
   _end
   open-segment-intersection
   closed-segment-intersection

   ;; pseudo random sequences
   blum-blum-shub

   ;; generally useful procedures
   square
   cube
   compose
   curry
   push!
   pop!
   inc!
   dec!

   ;; sugar
   for/range
   )
  
  (import (chezscheme)
	  (chez patricia))

  (include "outils.scm")
  (include "eratosthenes.scm")
  (include "euclid.scm")
  (include "miller-rabin.scm")
  (include "pollard.scm")
  (include "lazy.scm")
  (include "sequences.scm")
  (include "polynomial.scm")
  (include "geometry.scm")
  (include "pseudo.scm")
  
  )

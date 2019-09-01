(eval-when (compile)
  (optimize-level 3))

(library (chez euler)
  (export primes     ;; sieves
	  primes-in-range
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
	  coprime?
	  divides?
	  Z/nZ*
	  chinese-remainder-theorem
	  crt-system
	  sgn

	  ;; lists
	  permutations
	  delete-single
	  sort-on
	  sort-on!
	  shuffle-list

	  ;; streams
	  s:cons
	  ;; 	  s:car use car!
	  s:cdr
	  s:ref
	  s:iter
	  s:map
	  s:filter
	  s:take
	  s:drop
	  s:take-while
	  s:drop-while
	  s:accumulate
	  s:find
	  s:constant
	  s:scale
	  s:append
	  s:join
	  s:bind
	  s:cycle
	  s:chunks
	  s:enumerate
	  s:integrate
	  s:derive
	  s:negate
	  s:interleave
	  s:convolve
	  s:square
	  s:tails
	  s:euler-transform
	  s:accelerate

	  ;; stream related
	  s:factorial
	  s:choose
	  binomials
	  factorials
	  s:e^x
	  s:sin
	  s:cos
	  s:pi
	  *primes*

	  ;; continued fractions
	  convergents
	  g:convergents
	  cf:sqrt

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
	  coefficients->polynomial
	  
	  ;; geometry
	  point
	  segment
	  segment-between
	  _x
	  _y
	  _start
	  _end
	  open-segment-intersection
	  closed-segment-intersection

	  ;; pseudo random sequences
	  blum-blum-shub

	  ;; combinatorial procedures
	  algorithm-P
	  shuffle!

	  ;; digit procedures
	  digit-fold
	  digits
	  digit-sum
	  digits->integer
	  pandigital?

	  ;; generally useful procedures
	  square
	  cube
	  compose
	  curry
	  flip
	  const
	  *machine-epsilon*
	  =~
	  average
	  iaverage
	  log-base
	  log-2
	  log-10
	  merge-sorted
	  
	  ;; sugar
	  for/range
	  push!
	  pop!
	  inc!
	  dec!
	  vector-inc!
	  vector-dec!
	  vector-modify!
	  vector-swap!)
  
  (import (chezscheme)
	  (chez patricia))

  (include "outils.scm")
  (include "digits.scm")
  (include "eratosthenes.scm")
  (include "euclid.scm")
  (include "miller-rabin.scm")
  (include "pollard.scm")
  (include "lazy.scm")
  (include "sequences.scm")
  (include "polynomial.scm")
  (include "geometry.scm")
  (include "pseudo.scm")
  (include "continued-fraction.scm")
  (include "combinatorics.scm")
  (include "permutations.scm")
  
  )

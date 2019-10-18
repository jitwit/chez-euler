(eval-when (compile)
  (optimize-level 3))

(library (euler)
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
	  delete-single
	  sort-on
	  sort-on!
	  shuffle-list
          nub-eq
          nub-equal

	  ;; streams
	  s:cons
	  ;; s:car just use car 
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
          s:merge-sorted
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
          s:palindromes
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
          algorithm-T
          combinations
          permutations
	  shuffle!

	  ;; digit procedures
	  digit-fold
	  digits
	  digit-sum
	  digits->integer
	  pandigital?
          palindrome?

	  ;; generally useful procedures
	  square
          square?
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
          display-ln
	  
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
  ;;  (include "permutations.scm")
  
  )

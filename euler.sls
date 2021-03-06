#!chezscheme

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
          count
          filter-map
          subsets
          cartesian-product
          shuffle
	  sort-on
          rank-on
          maximum-on
          maximums-on
	  sort-on!
          nub-eq
          nub-equal
          eq-histogram
          equal-histogram
          suffixes
          take-while
          drop-while
          group-with
          intersperse

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
          digit-palindrome?
          nth-digit

	  ;; generally useful procedures
	  square
          square?
	  cube
	  compose
          identity
	  curry
	  flip
	  const
	  *machine-epsilon*
	  =~
	  average
	  discrete-average
	  log-base
	  log-2
	  log-10
	  merge-sorted
          display-ln
          pi/4
          pi/2
          pi
          2pi

          ;; memoization
          defmemo
	  
	  ;; sugar
	  push!
	  pop!
	  inc!
	  dec!
	  vector-inc!
	  vector-dec!
	  vector-modify!
	  vector-swap!
	  fxvector-inc!
	  fxvector-dec!
	  fxvector-modify!
	  fxvector-swap!)
  
  (import (chezscheme)
	  (patricia))

  (include "code/outils.scm")
  (include "code/memoize.scm")
  (include "code/digits.scm")
  (include "code/eratosthenes.scm")
  (include "code/euclid.scm")
  (include "code/miller-rabin.scm")
  (include "code/pollard.scm")
  (include "code/lazy.scm")
  (include "code/sequences.scm")
  (include "code/polynomial.scm")
  (include "code/geometry.scm")
  (include "code/pseudo.scm")
  (include "code/continued-fraction.scm")
  (include "code/combinatorics.scm")
  (include "code/lists.scm")
  (include "code/statistics.scm")
  
  )

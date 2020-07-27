(print-gensym #f) (import (euler))

(define-syntax test-group
  (syntax-rules ()
    ((_ name test ...)
     (begin
       (newline) (display "checking ") (display name) (display "... ") (newline)
       (assert test) ...
       (display "...seems fine") (newline)))))

(test-group 'primes
	    (= 0 (length (primes (expt 2 0))))
	    (= 1 (length (primes (expt 2 1))))
	    (= 2 (length (primes (expt 2 2))))
	    (= 6 (length (primes (expt 2 4))))
	    (= 6542 (length (primes (expt 2 16))))
	    (= 1077871 (length (primes (expt 2 24)))))

(test-group 'totient-sieve
	    (equal? '#vfx(0 1 1 2 2 4 2 6 4 6 4) (totient-sieve 10)))

(test-group 'moebius-sieve
	    (equal? #vfx(0 1 -1 -1 0 -1 1 -1 0 0 1) (moebius-sieve 10)))

(test-group 'totient
	    (= 40 (totient 100))
	    (= (totient 123123) (apply * (map 1- (factorize 123123))))
	    (= (totient 49) (* 6 7))
	    (= (totient 100) (* 4 5 1 2)))

(test-group 'factorize
	    (equal? '(1) (factorize 1))
	    (andmap (lambda (prime)
		      (= 1 (length (factorize prime))))
		    (primes 1000))
	    (equal? '(11 11) (factorize 121))
	    (equal? '(2 2 5 5) (factorize 100)))

(test-group 'divisors
	    (equal? '(1) (divisors 1))
	    (andmap (lambda (prime)
		      (= 2 (length (divisors prime))))
		    (primes 1000))
	    (equal? '(2 2 5 5) (factorize 100)))

(test-group 'prime?
	    (equal? (primes 100000) (filter prime? (iota 100000))))

(test-group 'extended-euclid
	    (equal? '(0 1 3) (ax+by=gcd 3 0))
	    (equal? '(-3 2 1) (ax+by=gcd 5 8)))

(test-group 'inverse-modulo
	    (= 6 (inverse-modulo -1 7))
	    (= 7 (inverse-modulo 7 12))
	    (not (inverse-modulo 6 12)))

(test-group 'chinese-remainder
	    (equal? '(39 . 60)
		    (crt-system (map cons
				     '(0 3 4)
				     '(3 4 5)))))

(test-group 'multiplicative-group
	    (equal? (Z/nZ* 12) '(1 5 7 11))
	    (equal? (Z/nZ* 1993) (map 1+ (iota 1992)))
	    (equal? (length (Z/nZ* 123123)) (totient 123123)))

(test-group 'digits
	    (equal? '(1 2 3 1 2 3) (digits 123123))
	    (equal? '(-1 2 3 1 2 3) (digits -123123))
	    (equal? '(0) (digits 0))

	    (pandigital? '())
	    (pandigital? (iota 10))
	    (pandigital? (shuffle (iota 10)))
	    (not (pandigital? (remv 4 (iota 10))))

	    (digit-palindrome? 123321)
	    (digit-palindrome? 909))

(test-group 'permutations
	    (= 1 (length (permutations (iota 0))))
	    (= 1 (length (permutations (iota 1))))
	    (= 2 (length (permutations (iota 2))))
	    (= 120 (length (permutations (iota 5))))
	    (= 40320 (length (permutations (iota 8)))))

(test-group 'combinations
	    (= 1 (length (combinations (iota 0) 0)))
	    (= 1 (length (combinations (iota 4) 0)))
	    (= 6 (length (combinations (iota 4) 2)))
	    (= 1 (length (combinations (iota 4) 4)))
	    (= 0 (length (combinations (iota 4) 10)))
	    (= 184756 (length (combinations (iota 20) 10))))


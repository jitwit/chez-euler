(print-gensym #f) (import (euler))

(define-syntax test-group
  (syntax-rules ()
    ((_ name test ...)
     (begin
       (newline) (display "checking ") (display name) (display "... ") (newline)
       test ...
       (display "...seems fine") (newline)))))

(test-group 'primes
	    (assert (= 0 (length (primes (expt 2 0)))))
	    (assert (= 1 (length (primes (expt 2 1)))))
	    (assert (= 2 (length (primes (expt 2 2)))))
	    (assert (= 6 (length (primes (expt 2 4)))))
	    (assert (= 6542 (length (primes (expt 2 16)))))
	    (assert (= 1077871 (length (primes (expt 2 24))))))

(test-group 'totient-sieve
	    (assert (equal? '#vfx(0 1 1 2 2 4 2 6 4 6 4) (totient-sieve 10))))

(test-group 'moebius-sieve
	    (assert (equal? #vfx(0 1 -1 -1 0 -1 1 -1 0 0 1) (moebius-sieve 10))))

(test-group 'totient
	    (assert (= 40 (totient 100)))
	    (assert (= (totient 123123) (apply * (map 1- (factorize 123123)))))
	    (assert (= (totient 49) (* 6 7)))
	    (assert (= (totient 100) (* 4 5 1 2))))

(test-group 'factorize
	    (assert (equal? '(1) (factorize 1)))
	    (assert (andmap (lambda (prime)
			      (= 1 (length (factorize prime))))
			    (primes 1000)))
	    (assert (equal? '(11 11) (factorize 121)))
	    (assert (equal? '(2 2 5 5) (factorize 100))))

(test-group 'divisors
	    (assert (equal? '(1) (divisors 1)))
	    (assert (andmap (lambda (prime)
			      (= 2 (length (divisors prime))))
			    (primes 1000)))
	    (assert (equal? '(2 2 5 5) (factorize 100))))

(test-group 'prime?
	    (assert (equal? (primes 100000) (filter prime? (iota 100000)))))

(test-group 'extended-euclid
	    (assert (equal? '(0 1 3) (ax+by=gcd 3 0)))
	    (assert (equal? '(-3 2 1) (ax+by=gcd 5 8))))

(test-group 'inverse-modulo
	    (assert (= 6 (inverse-modulo -1 7)))
	    (assert (= 7 (inverse-modulo 7 12)))
	    (assert (not (inverse-modulo 6 12))))

(test-group 'chinese-remainder
	    (assert (equal? '(39 . 60)
			    (crt-system (map cons
					     '(0 3 4)
					     '(3 4 5))))))

(test-group 'multiplicative-group
	    (assert (equal? (Z/nZ* 12) '(1 5 7 11)))
	    (assert (equal? (Z/nZ* 1993) (map 1+ (iota 1992))))
	    (assert (equal? (length (Z/nZ* 123123)) (totient 123123))))

(test-group 'digits
	    (assert (equal? '(1 2 3 1 2 3) (digits 123123)))
	    (assert (equal? '(-1 2 3 1 2 3) (digits -123123)))
	    (assert (equal? '(0) (digits 0)))

	    (assert (pandigital? '()))
	    (assert (pandigital? (iota 10)))
	    (assert (pandigital? (shuffle (iota 10))))
	    (assert (not (pandigital? (remv 4 (iota 10)))))

	    (assert (digit-palindrome? 123321))
	    (assert (digit-palindrome? 909)))

(test-group 'permutations
	    (assert (= 1 (length (permutations (iota 0)))))
	    (assert (= 1 (length (permutations (iota 1)))))
	    (assert (= 2 (length (permutations (iota 2)))))
	    (assert (= 120 (length (permutations (iota 5)))))
	    (assert (= 40320 (length (permutations (iota 8))))))

(test-group 'combinations
	    (assert (= 1 (length (combinations (iota 0) 0))))
	    (assert (= 1 (length (combinations (iota 4) 0))))
	    (assert (= 6 (length (combinations (iota 4) 2))))
	    (assert (= 1 (length (combinations (iota 4) 4))))
	    (assert (= 0 (length (combinations (iota 4) 10))))
	    (assert (= 184756 (length (combinations (iota 20) 10)))))


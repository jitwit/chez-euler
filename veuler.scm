;; tests
(parameterize ((optimize-level 3))
  (load "eratosthenes.scm"))

(define test-prime-count
  (lambda (N pi)
    (format #t "testing: ~7a = (length (primes ~a))~%"
	    pi N)
    (assert (= pi (length (primes N))))))

;; https://en.wikipedia.org/wiki/Prime-counting_function
(define (run-prime-counts)
  (for-each test-prime-count
	    '(1
	      10
	      100
	      1000
	      10000
	      100000
	      1000000
	      10000000
	      100000000)
	    '(0
	      4
	      25
	      168
	      1229
	      9592
	      78498
	      664579
	      5761455)))

(time
 (run-prime-counts))

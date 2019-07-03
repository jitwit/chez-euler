(parameterize ((optimize-level 3))
  (load "chez/euler.sls"))
(import (chez euler))

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

(define test-extended-euclid
  (lambda (x y)
    (let-values (((s t d) (apply values (ax+by=gcd x y))))
      (let ((t1 `(= (gcd ,x ,y) ))
	    (t2 `(= ,d (+ (* ,s ,x) (* ,t ,y)))))
	(format #t "testing: ~a & ~a~%" t1 t2)
	(assert (eval `(and ,t1 ,t2)))))))

(define (run-extended-euclid)
  (for-each test-extended-euclid
	    '(1769 240 17 4 3)
	    '(551 46 12 2 8)))

(define test-miller-rabin
  (lambda (N)
    (format #t "testing: ~a = ~a~%"
	    `(length (primes ,N))
	    `(length (filter? prime? (cdr (iota ,N)))))
    (assert (= (length (primes N))
	       (length (filter prime? (cdr (iota N))))))))

(define (run-miller-rabin)
  (for-each test-miller-rabin '(1 10 100 1000 10000)))

(format #t "library: ~a~%~%" (library-exports '(chez euler)))
(time (run-extended-euclid))
(time (run-prime-counts))
(time (run-miller-rabin))
(format #t "all good~%")


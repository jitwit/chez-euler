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

(define (run-extended-euclid)
  (for-each test-extended-euclid
	    '(1769 240 17 4 3 0 12)
	    '(551 46 12 2 8 1 0)))

(define test-extended-euclid
  (lambda (x y)
    (let-values (((s t d) (apply values (ax+by=gcd x y))))
      (let ((t1 `(= (gcd ,x ,y) ))
	    (t2 `(= ,d (+ (* ,s ,x) (* ,t ,y)))))
	(format #t "testing: ~a & ~a~%" t1 t2)
	(assert (eval `(and ,t1 ,t2)))))))

(define test-inverse-mod
  (lambda (x m)
    (let ((x-1 (inverse-modulo x m)))
      (cond (x-1
	     (format #t "testing: ~a*~a = 1 in Z/~aZ~%" x x-1 m)
	     (assert (= 1 (mod (* x x-1) m))))
	    (else
	     (format #t "testing: ~a and ~a not coprime~%" x m)
	     (assert (not (= 1 (gcd x m)))))))))

(define (run-inverse-mod)
  (for-each test-inverse-mod
	    '(1 5 7 11 4)
	    '(12 12 12 12 12)))

(define test-miller-rabin
  (lambda (N)
    (format #t "testing: ~15a = ~a~%"
	    `(primes ,N)
	    `(filter prime? (cdr (iota ,N))))
    (assert (equal? (primes N) (filter prime? (cdr (iota N)))))))

(define (run-miller-rabin)
  (for-each test-miller-rabin '(1 10 100 1000 10000 100000)))

(format #t "library: ~a~%~%" (library-exports '(chez euler)))
(time (run-extended-euclid))
(time (run-prime-counts))
(time (run-miller-rabin))
(time (run-inverse-mod))
(format #t "all good~%")


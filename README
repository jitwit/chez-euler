This is a the test suite as well as some kind of documentation as well
as me learning about org mode.

# Tests

## load.scm

``` scheme
(parameterize ((optimize-level 3))
  (load "./load.scm"))
```

## eratosthenes.scm

Currently, there is an implementation of Eratosthenes sieve, a segmented
primes sieve, and a totient sieve.

### Prime Count

``` scheme
(define (pi n)
  (length (primes n)))
```

`pi` should agree with the
[chart](https://en.wikipedia.org/wiki/Prime-counting_function) at
wikipedia.

``` scheme
(for-each (lambda (n pi-n)
        (assert (= pi-n (pi (expt 10 n)))))
      (iota 8)
      '(0 4 25 168 1229 9592 78498 664579))
```

### Segmented vs Eratosthenes

``` scheme
(define (filtered-eratosthenes A B)
  (filter (lambda (prime) 
        (>= prime A))
      (primes B)))

```

If the primes outside of a range *A,B* are filtered out,
`(filtered-eratosthenes A B)` and `(primes-in-range A B)` should agree.

``` scheme
(for-each (lambda (A B)
        (assert (equal? (filtered-eratosthenes A B)
                (primes-in-range A B))))
      '(1 5 3 100 8000 10000 100000 12000 100)
      '(100 10 2 0 9000 20000 100100 12100 11000))
```

Testing currently fails for range 100 to 11000.

### Totients

Defining

``` scheme
(define (slowtient N)
  (length (filter (lambda (n) 
            (= 1 (gcd n N)))
          (iota N))))
```

we check the results of the totient sieve:

``` scheme
(let* ((limit 1000)
       (V (totient-sieve limit)))
  (for-each (lambda (k)
          (assert (= (slowtient k) (fxvector-ref V k))))
        (iota limit)))
```

## miller-rabin.scm

### Prime?

Being a simple implementation of the [Miller-Rabin primality
test](https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test),
using 40 trial rounds. We can check that

``` scheme
(define (slow-primes N)
  (filter prime? (iota N)))

```

agrees with eratosthenes:

``` scheme
(for-each (lambda (N)
        (assert (equal? (primes N) (slow-primes N))))
      '(0 1 3 10 100 1000 10000 100000))
```

## euclid.scm

Testing `ax+by=gcd` can be done by comparing the output gcd with the
library gcd, and verifying *s\*x+t\*y = gcd(x,y)*. `inverse-modolu` by
seeing if *s\*x = 1 (mod y)*:

``` scheme
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
```

# Project Euler examples

A solution to problem 10 from Project Euler:

``` scheme
(define (e:10 N)
  (apply + (primes N)))
```

``` example
142913828922
```

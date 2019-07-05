This is a the test suite as well as some kind of documentation as well
as me learning about org mode.

Tests
=====

load.scm
--------

Self-explanatory.

``` {.scheme exports="code" session="euler"}
(load "./load.scm")
```

eratosthenes.scm
----------------

Currently, there is an implementation of Eratosthenes sieve, a segmented
primes sieve, and a totient sieve.

### Prime Count

``` {.scheme exports="code" results="value" session="euler"}
(define (pi n)
  (length (primes n)))
```

`pi`{.scheme exports="code"} should agree with the
[chart](https://en.wikipedia.org/wiki/Prime-counting_function) at
wikipedia.

``` {.scheme exports="both" results="value" session="euler"}
(for-each (lambda (n pi-n)
        (assert (= pi-n (pi (expt 10 n)))))
      (iota 9)
      '(0 4 25 168 1229 9592 78498 664579 5761455))
```

``` {.example}
#t
```

### Segmented vs Eratosthenes

``` {.scheme exports="code" results="value" session="euler"}
(define (filtered-eratosthenes A B)
  (filter (lambda (prime) 
        (>= prime A))
      (primes B)))

```

If the primes outside of a range *A,B* are filtered out,
`(filtered-eratosthenes A B)`{.scheme exports="code"} and
`(primes-in-range A B)`{.scheme exports="code"} should agree.

``` {.scheme exports="both" results="value" session="euler"}
(for-each (lambda (A B)
        (assert (equal? (filtered-eratosthenes A B)
                (primes-in-range A B))))
      '(1 5 3 100 8000 10000 100000 12000)
      '(100 10 2 0 9000 20000 100100 12100))
```

Fails for case 100 to 11000...

### Totients

Defining

``` {.scheme exports="code" results="value" session="euler"}
(define (slowtient N)
  (length (filter (lambda (n) 
            (= 1 (gcd n N)))
          (iota N))))
```

we check the results of the totient sieve:

``` {.scheme exports="both" results="value" session="euler"}
(let* ((limit 1000)
       (V (totient-sieve limit)))
  (for-each (lambda (k)
          (assert (= (slowtient k) (fxvector-ref V k))))
        (iota limit)))
```

``` {.example}
#<void>
```

miller-rabin.scm
----------------

### Prime?

Being a simple implementation of the [Miller-Rabin primality
test](https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test),
using 40 trial rounds. We can check that

``` {.scheme exports="code" results="value" session="euler"}
(define (slow-primes N)
  (filter prime? (iota N)))

```

agrees with eratosthenes:

``` {.scheme exports="both" results="value" session="euler"}
(for-each (lambda (N)
        (assert (equal? (primes N) (slow-primes N))))
      '(0 1 3 10 100 1000 10000 100000))
```

``` {.example}
#<void>
```

euclid.scm
----------

Testing `ax+by=gcd` can be done by comparing the output gcd with the
library gcd, and verifying *s\*x+t\*y = gcd(x,y)*. `inverse-modulo` by
seeing if *s\*x = 1 (mod y)*:

``` {.scheme exports="both" results="value" session="euler"}
(for-each (lambda (x y)
        (let-values (((s t d) (apply values (ax+by=gcd x y))))
          (assert (and (= d (gcd x y))
               (= d (+ (* s x) (* t y)))))))
      '(1769 240 17 4 3 0 12)
      '(551 46 12 2 8 1 0))

(for-each (lambda (x m)
        (let ((y (inverse-modulo x m)))
          (if y
          (assert (= 1 (mod (* x y) m)))
          (not (= 1 (gcd x m))))))
      '(1 5 7 11 4)
      '(12 12 12 12 12))
```

pollard.scm
-----------

Factorize numbers large and small. For now I\'ll just randomly type out
a bunch of numbers and check that the product of `factorize` spits back
the input.

``` {.scheme exports="both" results="value" session="euler"}

(for-each (lambda (n)
        (assert (= n (apply * (factorize n)))))
      '(2 123 123123123123 25852 234987 128 123884756 128367453485765213 98374 123627436526418192370348545876))

```

``` {.example}
#t
```

Project Euler examples
======================

Find the sum of all the primes below two million.

Problem 3
---------

What is the largest prime factor of the number 600851475143 ?

``` {.scheme exports="both" session="euler"}
(define (e:3 N)
  (apply max (factorize N)))

(e:3 600851475143)
```

``` {.example}
6857
```

Problem 10
----------

``` {.scheme exports="code" session="euler"}
(define (e:10 N)
  (apply + (primes N)))
```

``` {.example}
142913828922
```

``` {.scheme exports="code" session="euler"}
(define (e:10 N)
  (apply + (primes N)))
```

Problem 69
----------

Find the value of n ≤ 1,000,000 for which n/φ(n) is a maximum.

``` {.scheme exports="both" session="euler"}
(define (e:69 N)
  (let ((T (totient-sieve N)))
    (car
     (sort (lambda (x y)
         (> (/ (+ x 0.) (fxvector-ref T x))
        (/ (+ y 0.) (fxvector-ref T y))))
       (cdr (iota (1+ N)))))))

(e:69 1000000)
```

``` {.example}
510510
```

Of course, a better way to solve this is by noticing that *n/φ(n)* is
the product over *p\|n* of *p/p-1*. This will be biggest when the *p*
are small and distinct.

``` {.scheme exports="both" session="euler"}
(define (better-e:69 N)
  (let loop ((ps (primes N)) (best 1))
    (let ((next (* (car ps) best)))
      (if (> next N)
      best
      (loop (cdr ps) next)))))

(better-e:69 1000000)

```

``` {.example}
510510
```

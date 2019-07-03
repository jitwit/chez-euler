;;    (fold-right (lambda (x W)
;;		  (logbit1 x W))
;;		0
;;		(filter (lambda (x)
;;			  (let ((y (+ 1 x x)))
;;			    (not (or (zero? (mod y 3))
;;				     (zero? (mod y 5))))))
;;			(iota 60)))

;; bits of wheel:
;; 100101101101001100101101101001100101101101001100101101101001
(define *wheel-235* 679255032393419625)

;; chezscheme fixnums have 60 bits.
;; rth bit in cth column corresponds to n = 120*c + 2*r+1
(define p:column
  (lambda (n)
    (fx/ (fx1- n) 120)))

(define p:row
  (lambda (n)
    (fxmod (ash n -1) 60)))

(define p:index
  (lambda (n)
    (values (p:column n) (p:row n))))

;; check bit for j
(define p:query
  (lambda (B j)
    (let-values (((c r) (p:index j)))
      (logbit? r (fxvector-ref B c)))))

;; clear bit for j
(define p:clear
  (lambda (B j)
    (let-values (((c r) (p:index j)))
      (let ((x (fxvector-ref B c)))
	(fxvector-set! B c (logbit0 r x))))))

;; set bit for j
(define p:mark
  (lambda (B j)
    (let-values (((c r) (p:index j)))
      (let ((x (fxvector-ref B c)))
	(fxvector-set! B c (logbit1 r x))))))

;; run eratosthenes sieve
(define eratosthenes
  (lambda (N)
    (define cutoff (fx1+ (p:column N)))
    (define bits (make-fxvector cutoff *wheel-235*))
    (define (clear 2*p j)
      (unless (fx> j N)
	(p:clear bits j)
	(clear 2*p (fx+ 2*p j))))
    (define (sieve p)
      (unless (fx> (fx* p p) N)
	(when (p:query bits p)
	  (clear (fx* 2 p) (fx* p p)))
	(sieve (fx+ p 2))))
    (p:clear bits 1) ;; 1 is not prime
    (p:mark bits 3) ;; 3 and 5 are, but were cleared by wheel
    (p:mark bits 5)
    (sieve 7) ;; sieve!
    bits))

;; extract primes in a list from the sieve
(define eratosthenes->primes
  (lambda (N)
    (define bits (eratosthenes N))
    (define (walk-2 k)
      (cond ((fx> k N) '())
	    ((p:query bits k) (cons k (walk-4 (fx+ k 2))))
	    (else (walk-4 (fx+ k 2)))))
    (define (walk-4 k)
      (cond ((fx> k N) '())
	    ((p:query bits k) (cons k (walk-2 (fx+ k 4))))
	    (else (walk-2 (fx+ k 4)))))
    (cons* 2 3 5 (walk-4 7))))

(define primes
  (lambda (N)
    (cond ((>= N 5) (eratosthenes->primes N))
	  ((>= N 3) '(2 3))
	  ((>= N 2) '(2))
	  (else '()))))

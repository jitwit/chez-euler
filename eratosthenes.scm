;; u8 bytevector indexing
(define u8:column
  (lambda (n)
    (ash n -3)))

(define u8:row
  (lambda (n)
    (logand n 7)))

(define u8:index
  (lambda (n)
    (values (u8:column n) (u8:row n))))

;; check bit for j
(define u8:prime?
  (lambda (B j)
    (let-values (((c r) (u8:index j)))
      (logbit? r (bytevector-u8-ref B c)))))

;; clear bit for j
(define u8:clear
  (lambda (B j)
    (let-values (((c r) (u8:index j)))
      (let ((x (bytevector-u8-ref B c)))
	(bytevector-u8-set! B c (logbit0 r x))))))

;; set bit for j
(define u8:mark
  (lambda (B j)
    (let-values (((c r) (u8:index j)))
      (let ((x (bytevector-u8-ref B c)))
	(bytevector-u8-set! B c (logbit1 r x))))))

;; run eratosthenes sieve
(define eratosthenes-sieve
  (lambda (N)
    (define cutoff (fx1+ (u8:column N)))
    (define bits (make-bytevector cutoff 255))
    (define (clear 2*p j)
      (unless (fx> j N)
	(u8:clear bits j)
	(clear 2*p (fx+ 2*p j))))
    (define (sieve p)
      (unless (fx> (fx* p p) N)
	(when (u8:prime? bits p)
	  (clear (* 2 p) (fx* p p)))
	(sieve (fx+ p 2))))
    (sieve 3)
    bits))

;; gather primes in a list. alternate incrementing p by 2 or 4
(define eratosthenes->primes
  (lambda (N)
    (define bits (eratosthenes-sieve N))
    (define (walk k dk)
      (cond ((fx> k N) '())
	    ((u8:prime? bits k) (cons k (walk (fx+ k dk) (fx- 6 dk))))
	    (else (walk (fx+ k dk) (fx- 6 dk)))))
    (cons* 2 3 (walk 5 2))))

(define primes
  (lambda (N)
    (cond ((> N 4) (eratosthenes->primes N))
	  (else (filter (lambda (p)
			  (> N p))
			'(2 3))))))

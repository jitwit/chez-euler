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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eratosthenes sieve
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

;; gather primes in a list
(define run-eratosthenes
  (lambda (N)
    (define bits (eratosthenes-sieve N))
    (define (walk k dk) ;; alternate dk = 2 or 4
      (cond ((fx> k N) '())
	    ((u8:prime? bits k) (cons k (walk (fx+ k dk) (fx- 6 dk))))
	    (else (walk (fx+ k dk) (fx- 6 dk)))))
    (cons* 2 3 (walk 5 2))))

(define primes
  (lambda (N)
    (cond ((> N 4) (run-eratosthenes N))
	  (else (filter (lambda (p)
			  (> N p))
			'(2 3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Segmented sieve
(define first-odd-multiple-above
  (lambda (x n)
    (let ((y (fx* x (fx/ (fx+ x n -1) x))))
      (if (fxeven? y)
	  (fx- (fx+ y x) n)
	  (fx- y n)))))

(define segmented-sieve
  (lambda (A B)
    (define cutoff (+ 2 (u8:column (- B A -1))))
    (define bits (make-bytevector cutoff 255))
    (define (clear j dj)
      (when (fx< j (fx- B A))
	(u8:clear bits j)
	(clear (+ j dj) dj)))
    (define (walk j dj)
      (cond ((> j B) '())
	    ((u8:prime? bits (- j A)) (cons j (walk (+ j dj) (- 6 dj))))
	    (else (walk (+ j dj) (- 6 dj)))))
    
    (for-each (lambda (p)
		(clear (first-odd-multiple-above p A)
		       (fx* p 2)))
	      (cdr (primes (isqrt B))))
    (case (mod A 6)
      ((0) (walk (+ A 1) 4))
      ((1) (walk A 4))
      ((2) (walk (+ A 3) 2))
      ((3) (walk (+ A 2) 2))
      ((4) (walk (+ A 1) 2))
      ((5) (walk A 2)))))

(define primes-in-range
  (lambda (A B)
    (cond ((< B A) '())
	  ((< A 3) (primes B))
	  (else (segmented-sieve A B)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Totient sieve
(define totient-sieve
  (lambda (n)
    (define V (make-fxvector (fx1+ n)))
    (define (initialize j)
      (when (fx<= j n)
	
	(if (fxeven? j)
	    (fxvector-set! V j (ash j -1))
	    (fxvector-set! V j j))
	(initialize (fx1+ j))))
    (define (loop p j)
      (when (fx<= j n)
	(let ((phi-j (fxvector-ref V j)))
	  (fxvector-set! V j (fx/ (fx* phi-j (fx1- p)) p))
	  (loop p (fx+ p j)))))
    (define (walk j)
      (cond ((fx> j n) V)
	    ((fx= j (fxvector-ref V j))
	     (loop j j)
	     (walk (fx1+ j)))
	    (else
	     (walk (fx1+ j)))))
    
    (initialize 1)
    (walk 3)))

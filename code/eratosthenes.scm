;;; bit vector
(define u8:column
  (lambda (n)
    (fxsrl n 4)))

(define u8:row
  (lambda (n)
    (fxlogand (fxsrl n 1) 7)))

(define u8:index
  (lambda (n)
    (values (u8:column n) (u8:row n))))

(define u8:prime?
  (lambda (B j)
    (fxlogbit? (u8:row j) (bytevector-u8-ref B (u8:column j)))))

(define u8:clear
  (lambda (B j)
    (let-values (((c r) (u8:index j)))
      (bytevector-u8-set! B c (fxlogbit0 r (bytevector-u8-ref B c))))))

(define u8:mark
  (lambda (B j)
    (let-values (((c r) (u8:index j)))
      (let ((x (bytevector-u8-ref B c)))
	(bytevector-u8-set! B c (fxlogbit1 r x))))))

;;; Eratosthenes sieve
(define eratosthenes-sieve
  (lambda (N)
    (define cutoff (fx1+ (u8:column N)))
    (define bits (make-bytevector cutoff 255))
    (define (clear 2*p j)
      (unless (fx> j N)
	(u8:clear bits j)
	(clear 2*p (fx+ 2*p j))))
    (define (sieve p dp)
      (unless (fx> (fx* p p) N)
	(when (u8:prime? bits p)
	  (clear (fxsll p 1) (fx* p p)))
	(sieve (fx+ p dp) (fx- 6 dp))))
    (sieve 5 2)
    bits))

(define select-1/5
  (lambda (N)
    (case (fxmod N 6)
      ((0) (values (fx1- N) 4))
      ((1) (values N 2))
      ((2) (values (fx- N 1) 2))
      ((3) (values (fx- N 2) 2))
      ((4) (values (fx- N 3) 2))
      ((5) (values N 4)))))

;; gather primes in a list
(define run-eratosthenes
  (lambda (N)
    (define bits (eratosthenes-sieve N))
    (define (walk k dk ps) ;; alternate dk = 2 or 4
      (cond ((fx< k 6) (cons* 2 3 5 ps))
	    ((u8:prime? bits k) (walk (fx- k dk) (fx- 6 dk) (cons k ps)))
	    (else (walk (fx- k dk) (fx- 6 dk) ps))))
    (define-values (k dk) (select-1/5 N))
    (walk k dk '())))

(define primes
  (lambda (N)
    (cond ((> N 6) (run-eratosthenes N))
          (else (filter (lambda (p) (<= p N))
                        '(2 3 5))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Segmented sieves                                                           ;;
(define first-odd-multiple-above
  (lambda (n x)
    (let ((y (fx* x (fx/ (fx+ x n -1) x))))
      (if (fxeven? y)
	  (fx- (fx+ y x) n)
	  (fx- y n)))))

(define segmented-sieve
  (lambda (A B)
    (define cutoff (fx1+ (u8:column (- B A -1))))
    (define bits (make-bytevector cutoff 255))
    (define (clear j dj)
      (when (fx< j (fx- B A))
	(u8:clear bits j)
	(clear (+ j dj) dj)))
    (define (walk j dj)
      (cond ((> j B) '())
	    ((u8:prime? bits (- j A)) (cons j (walk (+ j dj) (- 6 dj))))
	    (else (walk (+ j dj) (- 6 dj)))))
    (for-all (lambda (p)
               (clear (first-odd-multiple-above A p)
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

(define (*primes* . interval-width)
  (let* ((width (if (null? interval-width)
		    100000
		    (car interval-width)))
	 (j 1)
	 (ps (primes width)))
    (lambda ()
      (when (null? ps)
	(set! ps (primes-in-range (* width j) (* width (1+ j))))
	(inc! j))
      (pop! ps))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Totient sieve                                                              ;;
(define totient-sieve
  (lambda (n)
    (define V (make-fxvector (fx1+ n)))
    (define (initialize j)
      (when (fx<= j n)
	(if (fxeven? j)
	    (fxvector-set! V j (fxsrl j 1))
	    (fxvector-set! V j j))
	(initialize (fx1+ j))))
    (define (loop p j)
      (when (fx<= j n)
	(let ((phi-j (fxvector-ref V j)))
	  (fxvector-set! V j (fx/ (fx* phi-j (fx1- p)) p))
	  (loop p (fx+ p j)))))
    (define (walk j)
      (cond ((fx> j n) V)
	    ((fx= j (fxvector-ref V j)) ;; j is prime, so loop
	     (loop j j)
	     (walk (fx1+ j)))
	    (else
	     (walk (fx1+ j)))))
    (initialize 1)
    (walk 3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Moebius sieve                                                              ;;
;; moebius(n) = { 0, n divisible by a square                                  ;;
;;              { (-1)^k, n has k distinct prime divisors                     ;;

;; Lioen and Van de Lune sieve:
;; for n = 1 to N: mu = 1
;; for p <= root N: for multiples of p, mu = -p * mu
;; for p <= root N: for multiples of p^2 mu = 0
;; for n = 1 to N: if |mu(n)| /= n then mu(n) = -mu(n)
;; for n = 1 to N: mu(n) = sign(mu(n)).
(define moebius-sieve
  (lambda (n)
    (define M (make-fxvector (fx1+ n) 1))
    (define (M1 j p)
      (unless (fx> j n)
	(fxvector-set! M j (fx* (fx- p) (fxvector-ref M j)))
	(M1 (fx+ j p) p)))
    (define (M2 j p^2)
      (unless (fx> j n)
	(fxvector-set! M j 0)
	(M2 (fx+ j p^2) p^2)))
    (define (M3 j)
      (unless (fx> j n)
	(let ((m-j (fxvector-ref M j)))
          (let ((s-j (sgn m-j)))
            (if (fx= (fxabs m-j) j)
                (fxvector-set! M j s-j)
                (fxvector-set! M j (fx- s-j)))))
	(M3 (fx1+ j))))
    (for-each (lambda (p)
                (M1 p p)
                (let ((p^2 (fx* p p)))
                  (M2 p^2 p^2)))
              (primes (isqrt n)))
    (fxvector-set! M 0 0)
    (M3 1)
    M))
; improve with page 3? https://arxiv.org/pdf/1610.08551.pdf
;  which comes from https://arxiv.org/pdf/1108.0135.pdf

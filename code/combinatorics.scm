;;; Plain changes. Visit permutations of vector A
(define algorithm-P
  (lambda (A visit)
    (define n (vector-length A))
    (define j)
    (define s)
    (define q)
    (define C (make-fxvector (fx1+ n) 0))
    (define O (make-fxvector (fx1+ n) 1))
    (define (P2)
      (visit A)
      (P3))
    (define (P3)
      (set! j n)
      (set! s 0)
      (P4))
    (define (P4)
      (set! q (fx+ (fxvector-ref O j)
                   (fxvector-ref C j)))
      (cond ((fx< q 0) (P7))
	    ((fx= q j) (P6))
	    (else (P5))))
    (define (P5)
      (vector-swap! A
                    (fx+ j s (- (fxvector-ref C j)) -1)
                    (fx+ j s (- q) -1))
      (fxvector-set! C j q)
      (P2))
    (define (P6)
      (unless (fx= j 1)
	(inc! s)
	(P7)))
    (define (P7)
      (fxvector-set! O j (fx- (fxvector-ref O j)))
      (dec! j)
      (P4))
    (P2)))

;;; Lexicographic Combinations. Knuth 7.2.1.3 pg 359.
(define algorithm-T
  (lambda (n t visit)
    (define j 0)
    (define x t)
    ;; index vector
    (define C (make-fxvector (fx+ t 3)))
    ;; visit vector
    (define (T1)
      ;; initialize
      (do ((i 1 (fx1+ i)))
          ((fx> i t)
           ;; set c-t+1 <- n
           ;; set c-t+2 <- 0
           (fxvector-set! C (fx1+ t) n)
           (fxvector-set! C (fx+ t 2) 0)
           (T2))
        ;; set c-j <- j - 1 for 1 <= j <= t
        (fxvector-set! C i (fx1- i))))
    (define (T2)
      ;; invariant: j is smallest index such that c-j+1 > j
      (visit C)
      (cond ((fx> j 0)
             (set! x j)
             (T6))
            (else (T3))))
    (define (T3)
      ;; easy case is first branch. test c1 + 1 < c2
      (cond ((fx< (fx1+ (fxvector-ref C 1)) (fxvector-ref C 2))
             (fxvector-set! C 1 (fx1+ (fxvector-ref C 1)))
             (T2))
            (else
             (set! j 2)
             (T4))))
    (define (T4)
      ;; find j
      ;; set c-j-1 <- j - 2. set x <- c-j + 1
      (fxvector-set! C (fx1- j) (fx- j 2))
      (set! x (fx1+ (fxvector-ref C j)))
      (cond ((fx= x (fxvector-ref C (fx1+ j)))
             (set! j (fx1+ j))
             (T4))
            (else (T5))))
    (define (T5)
      ;; terminate when t < j
      (unless (fx< t j)
        (T6)))
    (define (T6)
      (fxvector-set! C j x)
      (set! j (fx1- j))
      (T2))
    (T1)))

(define combinations
  (lambda (X k)
    (let ((V (if (vector? X) X (list->vector X)))
          (xs '()))
      (algorithm-T (vector-length V)
                   k
                   (lambda (C)
                     (do ((j k (fx1- j))
                          (lst '() (cons (vector-ref V (fxvector-ref C j))
                                         lst)))
                         ((fxzero? j) (push! lst xs)))))
      ;; note xs is in reverse lexicographic order
      xs)))

(define permutations
  (lambda (X)
    (if (null? X)
	'(()) ;; 0! = 1
	(let ((ps '()))
	  (algorithm-P (list->vector X)
		       (lambda (v)
			 (push! (vector->list v) ps)))
	  ps))))

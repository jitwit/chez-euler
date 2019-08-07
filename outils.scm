
(define *machine-epsilon*
  (let loop ((epsilon 1.))
    (if (= (+ 1. epsilon) 1.)
	(* epsilon 2)
	(loop (/ epsilon 2)))))

(define =~
  (lambda (x y)
    (< (abs (- x y))
       *machine-epsilon*)))

(define square
  (lambda (x)
    (* x x)))

(define cube
  (lambda (x)
    (* x x x)))

(define average
  (lambda (a b)
    (/ (+ a b) 2)))

(define iaverage
  (lambda (a b)
    (quotient (+ a b) 2)))

(define log-base
  (lambda (b x)
    (/ (log x)
       (log b))))

(define-syntax compose
  (lambda (x)
    (syntax-case x ()
      ((_) #'(lambda x x))
      ((_ g) #'g)
      ((_ f h ...)
       #'(let ((g (compose h ...)))
	   (lambda x
	     (call-with-values (lambda () (apply g x)) f)))))))

(define curry
  (lambda (f . x)
    (lambda y
      (apply f (append x y)))))

(define flip
  (lambda (f)
    (lambda (x y)
      (f y x))))

(define const
  (lambda (x)
    (lambda y
      x)))

(define-syntax for/range
  (lambda (x)
    (syntax-case x ()
      ((_ x a b e ...)
       #'(let loop ((x a))
	   (unless (= x b)
	     e ...
	     (loop (1+ x))))))))

(define-syntax inc!
  (lambda (x)
    (syntax-case x ()
      ((_ x)
       #'(set! x (1+ x))))))

(define-syntax dec!
  (lambda (x)
    (syntax-case x ()
      ((_ x)
       #'(set! x (1- x))))))

(define-syntax push!
  (lambda (x)
    (syntax-case x ()
      ((_ x xs)
       #'(set! xs (cons x xs))))))

(define-syntax pop!
  (lambda (x)
    (syntax-case x ()
      ((_ xs)
       #'(let ((x (car xs)))
	   (set! xs (cdr xs))
	   x)))))

(define vector-modify!
  (lambda (V j g)
    (vector-set! V j (g (vector-ref V j)))))

(define vector-inc!
  (lambda (V j)
    (vector-modify! V j 1+)))

(define vector-dec!
  (lambda (V j)
    (vector-modify! V j 1-)))

(define vector-swap!
  (lambda (V i j)
    (let ((t (vector-ref V i)))
      (vector-set! V i (vector-ref V j))
      (vector-set! V j t))))

(define merge-sorted
  (lambda (X Y)
    (cond ((null? X) Y)
	  ((null? Y) X)
	  (else
	   (let ((x (car X))
		 (y (car Y)))
	     (cond ((< x y) (cons x (merge-sorted (cdr X) Y)))
		   ((< y x) (cons y (merge-sorted X (cdr Y))))
		   (else (cons x (merge-sorted (cdr X) (cdr Y))))))))))

(define log-2
  (lambda (x)
    (log-base 2 x)))

(define log-10
  (lambda (x)
    (log-base 10 x)))



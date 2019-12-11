
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

(define square?
  (lambda (x)
    (exact? (sqrt x))))

(define cube
  (lambda (x)
    (* x x x)))

(define average
  (lambda (a b)
    (/ (+ a b) 2)))

(define discrete-average
  (lambda (a b)
    (ash (+ a b) -1)))

(define log-base
  (lambda (b x)
    (/ (log x)
       (log b))))

(define-syntax compose
  (lambda (x)
    (syntax-case x ()
      ((_) #'(lambda x x))
      ((_ g) #'g)
      ((_ f g ...)
       #'(lambda (x)
           (f ((compose g ...) x)))))))

(define display-ln
  (lambda (object)
    (display object) (newline)))

(define curry
  (lambda (f . x)
    (lambda y
      (apply f (append x y)))))

(define flip
  (lambda (f)
    (lambda (x y)
      (f y x))))

(define (identity x)
  x)

(define const
  (lambda (x)
    (lambda y
      x)))

(define-syntax inc!
  (syntax-rules ()
    ((_ x)
     (set! x (1+ x)))
    ((_ x dx)
     (set! x (+ x dx)))))

(define-syntax dec!
  (syntax-rules ()
    ((_ x)
     (set! x (1- x)))
    ((_ x dx)
     (set! x (- x dx)))))

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

(define fxvector-modify!
  (lambda (V j g)
    (fxvector-set! V j (g (fxvector-ref V j)))))

(define vector-inc!
  (lambda (V j)
    (vector-modify! V j 1+)))

(define fxvector-inc!
  (lambda (V j)
    (fxvector-modify! V j fx1+)))

(define fxvector-dec!
  (lambda (V j)
    (fxvector-modify! V j fx1-)))

(define vector-dec!
  (lambda (V j)
    (vector-modify! V j 1-)))

(define vector-swap!
  (lambda (V i j)
    (let ((t (vector-ref V i)))
      (vector-set! V i (vector-ref V j))
      (vector-set! V j t))))

(define fxvector-swap!
  (lambda (V i j)
    (let ((t (fxvector-ref V i)))
      (fxvector-set! V i (fxvector-ref V j))
      (fxvector-set! V j t))))

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

(define shuffle!
  (lambda (V)
    (let loop ((i (1- (vector-length V))))
      (unless (< i 1)
	(let ((j (random (1+ i))))
	  (vector-swap! V i j))
	(loop (1- i))))
    'done))

(define pi/4
  (/ pi/2 2))

(define pi/2
  (acos 0))

(define pi
  (* 2 pi/2))

(define 2pi
  (* 4 pi/2))

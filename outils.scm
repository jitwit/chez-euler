
(define square
  (lambda (x)
    (* x x)))

(define cube
  (lambda (x)
    (* x x x)))

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
       #'(if (null? xs) 
	     #f
	     (let ((x (car xs)))
	       (set! xs (cdr xs))
	       x))))))

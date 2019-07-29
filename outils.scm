
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




;; (define-syntax case
;;   (lambda (x)
;;     (syntax-case x ()
;;       [(_ e c1 c2 ...)
;;        #`(let ([t e])
;;            #,(let f ([c1 #'c1] [cmore #'(c2 ...)])
;;                (if (null? cmore)
;;                    (syntax-case c1 (else)
;;                      [(else e1 e2 ...) #'(begin e1 e2 ...)]
;;                      [((k ...) e1 e2 ...)
;;                       #'(if (memv t '(k ...)) (begin e1 e2 ...))])
;;                    (syntax-case c1 ()
;;                      [((k ...) e1 e2 ...)
;;                       #`(if (memv t '(k ...))
;;                             (begin e1 e2 ...)
;;                             #,(f (car cmore) (cdr cmore)))]))))])))

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

(define-syntax defmemo
  (syntax-rules ()
    ((_ (f x) body ...)
     (define f
       (let ((mem (make-hash-table))
             (g (lambda (x) body ...)))
         (lambda (y)
           (let ((f-y (hashtable-ref mem y #f)))
             (or f-y
                 (let ((f-y (g y)))
                   (hashtable-set! mem y f-y)
                   f-y)))))))
    ((_ (f x y ...) body ...)
     (define f
       (let ((mem (make-hashtable equal-hash equal?))
             (g (lambda (x y ...) body ...)))
         (lambda xy
	   (if (hashtable-contains? mem xy)
	       (hashtable-ref mem xy 'ohno)
               (let ((f-xy (apply g xy)))
                 (hashtable-set! mem xy f-xy)
                 f-xy))))))))

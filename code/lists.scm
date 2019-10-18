
(define delete-single
  (lambda (item lst)
    (cond ((null? lst) lst)
	  ((eq? item (car lst))
	   (cdr lst))
	  (else
	   (cons (car lst) (delete-single item (cdr lst)))))))

(define sort-on
  (lambda (X h)
    (sort (lambda (x y)
	    (< (h x)
	       (h y)))
	  X)))

(define rank-on
  (lambda (X h)
    (sort-on car
             (map (lambda (x)
                    (cons (h x) x))
                  X))))

(define sort-on!
  (lambda (X heur)
    (sort! (lambda (x y)
	     (< (heur x)
		(heur y)))
	   X)))

(define subsets
  (lambda (X)
    (if (null? X)
        '(())
        (let ((P-X (subsets (cdr X))))
          (append (map (lambda (U)
                         (cons (car X) U))
                       P-X)
                  P-X)))))

(define count
  (lambda (predicate X)
    (fold-left (lambda (cnt x)
                 (if (predicate x)
                     (fx1+ cnt)
                     cnt))
               0
               X)))

(define filter-map
  (lambda (predicate X)
    (fold-right (lambda (x xs)
                  (let ((p-x (predicate x)))
                    (if p-x
                        (cons p-x xs)
                        xs)))
                '()
                X)))

(define shuffle
  (lambda (X)
    (let ((V (list->vector X)))
      (shuffle! V)
      (vector->list V))))

(define nub-eq
  (lambda (X)
    (let ((table (make-hash-table)))
      (for-each (lambda (x)
                  (hashtable-set! table x #t))
                X)
      (vector->list
       (hashtable-keys table)))))

(define nub-equal
  (lambda (X)
    (let ((table (make-hashtable equal-hash equal?)))
      (for-each (lambda (x)
                  (hashtable-set! table x #t))
                X)
      (vector->list
       (hashtable-keys table)))))

(define eq-histogram
  (lambda (X)
    (let ((table (make-hash-table)))
      (for-each (lambda (x)
                  (hashtable-update! table
                                     x
                                     fx1+
                                     0))
                X)
      (vector->list
       (hashtable-cells table)))))

(define equal-histogram
  (lambda (X)
    (let ((table (make-hashtable equal-hash equal?)))
      (for-each (lambda (x)
                  (hashtable-update! table
                                     x
                                     fx1+
                                     0))
                X)
      (vector->list
       (hashtable-cells table)))))

(define suffixes
  (lambda (X)
    (if (null? X)
        '(())
        (cons X (suffixes (cdr X))))))

(define take-while
  (lambda (predicate X)
    (letrec ((aux (lambda (X)
                    (if (and (pair? X)
                             (predicate (car X)))
                        (cons (car X) (aux (cdr X)))
                        '()))))
      (aux X))))

(define drop-while
  (lambda (predicate X)
    (letrec ((aux (lambda (X)
                    (if (and (pair? X)
                             (predicate (car X)))
                        (aux (cdr X))
                        X))))
      (aux X))))

(define group-with
  (lambda (equivalence X)
    (letrec ((aux (lambda (X G g)
                    (if (pair? X)
                        (let ((x (car X)))
                          (if (equivalence g x)
                              (aux (cdr X)
                                   (cons x G)
                                   x)
                              (cons (reverse G) (grp X))))
                        (list (reverse G)))))
             (grp (lambda (X)
                    (if (pair? X)
                        (aux (cdr X)
                             (list (car X))
                             (car X))
                        '()))))
      (grp X))))
